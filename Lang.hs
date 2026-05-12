{-# LANGUAGE GADTs, PostfixOperators #-}

-- allows this file to be treated as a module
module Lang where

-- ============================================================================== --
-- Language Abstract Syntax

data KUTypeLang where
  TNum :: KUTypeLang
  TBool :: KUTypeLang
  (:->:) :: KUTypeLang -> KUTypeLang -> KUTypeLang
  -- New Types for storage
  TLoc :: KUTypeLang
  TTop :: KUTypeLang
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
  -- New Language Constructs for storage
  New :: KULang -> KULang                   -- create new storage location and store value of KULang
  Deref :: KULang -> KULang                 -- get value stored at location
  Set :: KULang -> KULang -> KULang         -- assign value of second
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
  NewX :: KULangExt -> KULangExt
  DerefX :: KULangExt -> KULangExt
  SetX :: KULangExt -> KULangExt -> KULangExt
  deriving (Show,Eq)

data KULangVal where
  NumV :: Int -> KULangVal
  BooleanV :: Bool -> KULangVal
  ClosureV :: String -> KULang -> EnvVal -> KULangVal
  -- New Language Values for storage
  LocV :: Int -> KULangVal
  deriving (Show,Eq)

type EnvVal = [(String,KULangVal)]
type Cont = [(String,KUTypeLang)]
-- New constructs for storage
type Loc = Int
type StoreFunc = Loc -> Maybe KULangVal
type Store = (Loc, StoreFunc)

-- ============================================================================== --
-- Storage helpers

deref :: StoreFunc -> Loc -> Maybe KULangVal
deref s l = s l

derefStore :: Store -> Loc -> Maybe KULangVal
derefStore (i,s) l = deref s l

set :: StoreFunc -> Loc -> KULangVal -> StoreFunc
set s l v = 
   \m -> if m==l then (Just v) else s m

setStore :: Store -> Loc -> KULangVal -> Store
setStore (i,s) l v = (i, (set s l v))

newStore :: Store -> KULangVal -> Store
newStore (i,s) v = ((i+1), (set s i v))             -- returns new store with incremented location tracker and updated Store Function

-- Initializers for StoreFunc, Store
initStoreFunc :: StoreFunc
initStoreFunc x = Nothing

initStore :: Store
initStore = (0,initStoreFunc)
-- ============================================================================== --

-- Part 1 - Type Inference
typeof :: Cont -> KULang -> (Maybe KUTypeLang) 
typeof c (Num x) = if x>= 0 then return TNum else Nothing
typeof c (Boolean b) = return TBool
typeof c (Plus l r) =
    do {
        TNum <- typeof c l;
        TNum <- typeof c r;
        return TNum;
    }
typeof c (Minus l r) =
    do {
        TNum <- typeof c l;
        TNum <- typeof c r;
        return TNum;
    }
typeof c (Mult l r) =
    do {
        TNum <- typeof c l;
        TNum <- typeof c r;
        return TNum;
    }
typeof c (Div l r) =
    do {
        TNum <- typeof c l;
        TNum <- typeof c r;
        return TNum;
    }
typeof c (Exp l r) =
    do {
        TNum <- typeof c l;
        TNum <- typeof c r;
        return TNum;
    }
typeof c (And l r) =  
    do {
        TBool <- typeof c l;
        TBool <- typeof c r;
        return TBool;
    }
typeof c (Or l r) =  
    do {
        TBool <- typeof c l;
        TBool <- typeof c r;
        return TBool;
    }
typeof c (Leq l r) =  
    do {
        TNum <- typeof c l;
        TNum <- typeof c r;
        return TBool;
    }
typeof c (IsZero v) = 
    do {
        TNum <- typeof c v;
        return TBool;                        
    }
typeof c (If c' t e) =  
    do {
        TBool <- typeof c c';
        t' <- typeof c t; 
        e' <- typeof c e; 
        if t' == e' then return t' else Nothing
    }
typeof c (Between a b c') =  
    do {
        TNum <- typeof c a;
        TNum <- typeof c b;
        TNum <- typeof c c';
        return TBool;
    }
typeof c (Id s) = lookup s c
typeof c (Lambda i t b) = 
    do {
        -- using the type t of identifier i, determine type of body
        b' <- typeof ((i, t):c) b;
        return ((:->:) t b');
    }
typeof c (App f v) =
    do {
        -- current, where we use ((:->:) d r) to track type of functions
        ((:->:) d r) <- typeof c f;
        v' <- typeof c v;
        if v'==d then return r else Nothing
    }
typeof c (Fix f) = 
    do {
        ((:->:) d r) <- typeof c f;
        return r;
    }
-- New type rules for sequencing
typeof c (Seq l r) = 
    do {
        typeof c l;
        typeof c r;
    }
-- New type rules for storage
typeof c (New v) = fail "not implemented yet"
typeof c (Deref v) = fail "not implemented yet"
typeof c (Set l r) = fail "not implemented yet"

-- Part 2 - Evaluation
eval :: EnvVal -> KULang -> (Maybe KULangVal)
eval e (Num x) = if x<0 then Nothing else return (NumV x)
eval e (Boolean b) = return (BooleanV b)
eval e (Plus l r) =
    do {
        (NumV x) <- eval e l;
        (NumV y) <- eval e r;
        return (NumV (x+y));
    }
eval e (Minus l r) =
    do {
        (NumV x) <- eval e l;
        (NumV y) <- eval e r;
        let ret = x-y in
        if ret<0 then Nothing else return (NumV ret);
    }
eval e (Mult l r) =
    do {
        (NumV x) <- eval e l;
        (NumV y) <- eval e r;
        return (NumV (x*y));
    }
eval e (Div l r) =
    do {
        (NumV x) <- eval e l;
        (NumV y) <- eval e r;
        if y==0 then Nothing else return (NumV (quot x y));
    }
eval e (Exp l r) =
    do {
        (NumV x) <- eval e l;
        (NumV y) <- eval e r;
        return (NumV (x^y));
    }
eval e (And l r) =  
    do {
        (BooleanV x) <- eval e l;
        (BooleanV y) <- eval e r;
        return (BooleanV (x&&y));
    }
eval e (Or l r) =  
    do {
        (BooleanV x) <- eval e l;
        (BooleanV y) <- eval e r;
        return (BooleanV (x||y));
    }
eval e (Leq l r) =  
    do {
        (NumV x) <- eval e l;
        (NumV y) <- eval e r;
        return (BooleanV (x<=y));
    }
eval e (IsZero v) = 
    do {
        (NumV v') <- eval e v;
        return (BooleanV (v'==0));                 
    }
eval e (If c t e') =  
    do {
        (BooleanV c') <- eval e c;
        if c' then eval e t else eval e e';
    }
eval e (Between a b c) =  
    do {
        (NumV x) <- eval e a;
        (NumV y) <- eval e b;
        (NumV z) <- eval e c;
        return (BooleanV (x < y && y < z));
    }
eval e (Id s) = lookup s e
eval e (Lambda i t b) = return (ClosureV i b e);
eval e (App f v) =
    do {
        (ClosureV i b e') <- eval e f;
        v' <- eval e v;
        eval ((i, v'):e') b
    }
eval e (Fix f) = 
    do {
        (ClosureV i b e') <- eval e f;
        -- Question: should I use closure env or outside env?
        eval e' (subst i (Fix (Lambda i TNum b)) b)
    }
-- New evaluation rules for sequencing
eval e (Seq l r) =
    do {
        eval e l;
        eval e r;
    }
-- New evaluation rules for storage
eval e (New v) = fail "not implemented yet"
eval e (Deref v) = fail "not implemented yet"
eval e (Set l r) = fail "not implemented yet"

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
elabTerm (NewX v) = (New (elabTerm v))
elabTerm (DerefX v) = (Deref (elabTerm v))
elabTerm (SetX l r) = (Set (elabTerm l) (elabTerm r))

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
subst i v (New v') = New (subst i v v')
subst i v (Deref v') = Deref (subst i v v')
subst i v (Set l r) = Set (subst i v l) (subst i v r)

-- Part 4 - Interpretation
interpret :: KULangExt -> Maybe KULangVal
interpret e =
    do
        let e' = elabTerm e
        t <- typeof [] e'
        eval [] e'