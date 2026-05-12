{-# LANGUAGE GADTs, PostfixOperators #-}

-- allows this file to be treated as a module
module Lang where

import Control.Monad


-- ============================================================================== --
-- Language Abstract Syntax

data KUTypeLang where
  TNum :: KUTypeLang
  TBool :: KUTypeLang
  (:->:) :: KUTypeLang -> KUTypeLang -> KUTypeLang
  deriving (Show,Eq)

data KULang where
  Num :: Int -> KULang
  Boolean :: Bool -> KULang
  Id :: String -> KULang
  Plus :: KULang -> KULang -> KULang
  Minus :: KULang -> KULang -> KULang
  Mult :: KULang -> KULang -> KULang
  Div :: KULang -> KULang -> KULang
  Exp :: KULang -> KULang -> KULang
  Between :: KULang -> KULang -> KULang -> KULang
  Lambda :: String -> KUTypeLang -> KULang -> KULang
  App :: KULang -> KULang -> KULang
  If :: KULang -> KULang -> KULang -> KULang
  And :: KULang -> KULang -> KULang
  Or :: KULang -> KULang -> KULang
  Leq :: KULang -> KULang -> KULang
  IsZero :: KULang -> KULang
  Fix :: KULang -> KULang
  -- New Language Constructs for Sequencing
  Seq :: KULang -> KULang -> KULang
  deriving (Show,Eq)  

data KULangExt where
  NumX :: Int -> KULangExt
  BooleanX :: Bool -> KULangExt
  IdX :: String -> KULangExt  
  PlusX :: KULangExt -> KULangExt -> KULangExt
  MinusX :: KULangExt -> KULangExt -> KULangExt
  MultX :: KULangExt -> KULangExt -> KULangExt
  DivX :: KULangExt -> KULangExt -> KULangExt
  ExpX :: KULangExt -> KULangExt -> KULangExt
  BetweenX :: KULangExt -> KULangExt -> KULangExt -> KULangExt
  LambdaX :: String -> KUTypeLang -> KULangExt -> KULangExt
  AppX :: KULangExt -> KULangExt -> KULangExt 
  BindX :: String -> KUTypeLang -> KULangExt -> KULangExt -> KULangExt
  IfX :: KULangExt -> KULangExt -> KULangExt -> KULangExt
  AndX :: KULangExt -> KULangExt -> KULangExt
  OrX :: KULangExt -> KULangExt -> KULangExt
  LeqX :: KULangExt -> KULangExt -> KULangExt
  IsZeroX :: KULangExt -> KULangExt
  FixX :: KULangExt -> KULangExt
  -- New Language Constructs (just extended)
  SeqX :: KULangExt -> KULangExt -> KULangExt
  deriving (Show,Eq)

data KULangVal where
  NumV :: Int -> KULangVal
  BooleanV :: Bool -> KULangVal
  ClosureV :: String -> KULang -> EnvVal -> KULangVal
  deriving (Show,Eq)

type EnvVal = [(String,KULangVal)]
type Cont = [(String,KUTypeLang)]

-- ============================================================================== --
-- Reader & Helper Methods
data Reader e a = Reader (e -> Maybe a)

ask :: Reader a a 
ask = Reader $ \e -> Just e

runR :: Reader e a -> e -> Maybe a
runR (Reader f) e = f e 

local :: (e -> t) -> Reader t a -> Reader e a
local f r = Reader $ \e -> runR r (f e)

useClosure :: String -> KULangVal -> EnvVal -> EnvVal -> EnvVal
useClosure i v e _ = (i,v):e

useClosureType :: String -> KUTypeLang -> Cont -> Cont -> Cont
useClosureType i t c _ = (i,t):c

instance Monad (Reader e) where
    g >>= f = Reader $ \e -> 
        case runR g e of
      Nothing -> Nothing
      Just v  -> runR (f v) e

instance Functor (Reader e) where
    fmap f (Reader g) = Reader $ \e ->
        case g e of
        Nothing -> Nothing
        Just v  -> Just (f v)

instance Applicative (Reader e) where
    pure x = Reader $ \e -> Just x
    (Reader f) <*> (Reader g) = Reader $ \e ->
        case f e of
      Nothing -> Nothing
      Just h  ->
        case g e of
          Nothing -> Nothing
          Just x  -> Just (h x)

instance MonadFail (Reader e) where
  fail _ = Reader $ \_ -> Nothing

-- ============================================================================== --

-- Part 1 - Type Inference
typeof :: KULang -> Reader Cont KUTypeLang
typeof (Num x) = if x>= 0 then return TNum else fail "negative"
typeof (Boolean b) = return TBool
typeof (Plus l r) =
    do {
        TNum <- typeof l;
        TNum <- typeof r;
        return TNum;
    }
typeof (Minus l r) =
    do {
        TNum <- typeof l;
        TNum <- typeof r;
        return TNum;
    }
typeof (Mult l r) =
    do {
        TNum <- typeof l;
        TNum <- typeof r;
        return TNum;
    }
typeof (Div l r) =
    do {
        TNum <- typeof l;
        TNum <- typeof r;
        return TNum;
    }
typeof (Exp l r) =
    do {
        TNum <- typeof l;
        TNum <- typeof r;
        return TNum;
    }
typeof (And l r) =  
    do {
        TBool <- typeof l;
        TBool <- typeof r;
        return TBool;
    }
typeof (Or l r) =  
    do {
        TBool <- typeof l;
        TBool <- typeof r;
        return TBool;
    }
typeof (Leq l r) =  
    do {
        TNum <- typeof l;
        TNum <- typeof r;
        return TBool;
    }
typeof (IsZero v) = 
    do {
        TNum <- typeof v;
        return TBool;                        
    }
typeof (If c t e) =  
    do {
        TBool <- typeof c;
        t' <- typeof t; 
        e' <- typeof e; 
        if t' == e' then return t' else fail "mismatched if return values";
    }
typeof (Between a b c) =  
    do {
        TNum <- typeof a;
        TNum <- typeof b;
        TNum <- typeof c;
        return TBool;
    }
typeof (Id s) =
    do {
        cont <- ask;
        case (lookup s cont) of
            Just x -> return x
            Nothing -> fail "unbound variable"
    }
typeof (Lambda i t b) = 
    do {
        cont <- ask;
        -- using the type t of identifier i, determine type of body
        b' <- local (useClosureType i t cont) (typeof b);
        return ((:->:) t b');
    }
typeof (App f v) =
    do {
        -- current, where we use ((:->:) d r) to track type of functions
        ((:->:) d r) <- typeof f;
        v' <- typeof v;
        if v'==d then return r else fail "mismatch actual type with formal type";
    }
typeof (Fix f) = 
    do {
        ((:->:) d r) <- typeof f;
        return r;
    }
-- New type rules for sequencing
typeof (Seq l r) = 
    do {
        typeof l;
        typeof r;
    }

-- Part 2 - Evaluation
eval :: KULang -> Reader EnvVal KULangVal
eval (Num x) = if x<0 then fail "negative number" else return (NumV x)
eval (Boolean b) = return (BooleanV b)
eval (Plus l r) =
    do {
        (NumV x) <- eval l;
        (NumV y) <- eval r;
        return (NumV (x+y));
    }
eval (Minus l r) =
    do {
        (NumV x) <- eval l;
        (NumV y) <- eval r;
        let ret = x-y in
        if ret<0 then fail "negative number" else return (NumV ret);
    }
eval (Mult l r) =
    do {
        (NumV x) <- eval l;
        (NumV y) <- eval r;
        return (NumV (x*y));
    }
eval (Div l r) =
    do {
        (NumV x) <- eval l;
        (NumV y) <- eval r;
        if y==0 then fail "divide by 0" else return (NumV (quot x y));
    }
eval (Exp l r) =
    do {
        (NumV x) <- eval l;
        (NumV y) <- eval r;
        return (NumV (x^y));
    }
eval (And l r) =  
    do {
        (BooleanV x) <- eval l;
        (BooleanV y) <- eval r;
        return (BooleanV (x&&y));
    }
eval (Or l r) =  
    do {
        (BooleanV x) <- eval l;
        (BooleanV y) <- eval r;
        return (BooleanV (x||y));
    }
eval (Leq l r) =  
    do {
        (NumV x) <- eval l;
        (NumV y) <- eval r;
        return (BooleanV (x<=y));
    }
eval (IsZero v) = 
    do {
        (NumV v') <- eval v;
        return (BooleanV (v'==0));                 
    }
eval (If c t e) =  
    do {
        (BooleanV c') <- eval c;
        if c' then eval t else eval e;
    }
eval (Between a b c) =  
    do {
        (NumV x) <- eval a;
        (NumV y) <- eval b;
        (NumV z) <- eval c;
        return (BooleanV (x < y && y < z));
    }
eval (Id s) =
    do {
        env <- ask;
        case (lookup s env) of
            Just x -> return x
            Nothing -> fail "unbound variable"
    }
eval (Lambda i t b) = 
    do {
        env <- ask;
        return (ClosureV i b env);
    }
eval (App f v) =
    do {
        (ClosureV i b e) <- eval f;
        v' <- eval v;
        local (useClosure i v' e) (eval b);
    }
eval (Fix f) = 
    do {
        (ClosureV i b e) <- eval f;
        --using subst:
            --type of lambda here doesn't matter, so just use TNum
        eval (subst i (Fix (Lambda i TNum b)) b)
    }
-- New evaluation rules for sequencing
eval (Seq l r) =
    do {
        eval l;
        eval r;
    }

-- Part 2.5 - Add Bind through Elaboration
elabTerm :: KULangExt -> KULang 
elabTerm (NumX n) = (Num n)
elabTerm (BooleanX b) = (Boolean b)
elabTerm (PlusX l r) = (Plus (elabTerm l) (elabTerm r))
elabTerm (MinusX l r) = (Minus (elabTerm l) (elabTerm r)) 
elabTerm (MultX l r) = (Mult (elabTerm l) (elabTerm r)) 
elabTerm (DivX l r) = (Div (elabTerm l) (elabTerm r)) 
elabTerm (ExpX l r) = (Exp (elabTerm l) (elabTerm r)) 
elabTerm (BetweenX a b c) = (Between (elabTerm a) (elabTerm b) (elabTerm c))
elabTerm (IfX c t e) = (If (elabTerm c) (elabTerm t) (elabTerm e))
elabTerm (AndX l r) = (And (elabTerm l) (elabTerm r))
elabTerm (OrX l r) = (Or (elabTerm l) (elabTerm r))
elabTerm (LeqX l r) = (Leq (elabTerm l) (elabTerm r))
elabTerm (IsZeroX v) = (IsZero (elabTerm v))
elabTerm (LambdaX i t b) = (Lambda i t (elabTerm b))
elabTerm (AppX f v) = (App (elabTerm f) (elabTerm v))
elabTerm (IdX i) = (Id i)
elabTerm (BindX i t v b) = (App (Lambda i t (elabTerm b)) (elabTerm v))
elabTerm (FixX f) = (Fix (elabTerm f))
elabTerm (SeqX l r) = (Seq (elabTerm l) (elabTerm r))

-- Part 3 - Add the Fixed Point Operator

-- Part 3.1 - Add subst function to use with fix
subst :: String -> KULang -> KULang -> KULang
subst i v (Num x) = (Num x)
subst i v (Boolean b) = (Boolean b)
subst i v (Plus l r) = Plus (subst i v l) (subst i v r)
subst i v (Minus l r) = Minus (subst i v l) (subst i v r) 
subst i v (Mult l r) = Mult (subst i v l) (subst i v r) 
subst i v (Div l r) = Div (subst i v l) (subst i v r) 
subst i v (Exp l r) = Exp (subst i v l) (subst i v r) 
subst i v (And l r) = And (subst i v l) (subst i v r) 
subst i v (Or l r) = Or (subst i v l) (subst i v r) 
subst i v (Leq l r) = Leq (subst i v l) (subst i v r) 
subst i v (IsZero n) = IsZero (subst i v n) 
subst i v (If c t e) = If (subst i v c) (subst i v t) (subst i v e)
subst i v (Between a b c) = Between (subst i v a) (subst i v b) (subst i v c)
-- like bind, we should end if lambda defines new var that shadows ours, otherwise continue
subst i v (Lambda i' t b) = if i==i'
     then (Lambda i' t b)
     else (Lambda i' t (subst i v b))
subst i v (App f v') = App (subst i v f) (subst i v v')
subst i v (Id i') = if i==i' then v else (Id i')
subst i v (Fix f) = Fix (subst i v f)
subst i v (Seq l r) = Seq (subst i v l) (subst i v r)

-- Part 4 - Interpretation
interpret :: KULangExt -> Maybe KULangVal
-- not sure why, but had to use indentations instead of semicolon :/
interpret e = 
    do
        let e' = elabTerm e
        t <- runR (typeof e') []    -- if type checking fails, returns nothing and early exits
        runR (eval e') []