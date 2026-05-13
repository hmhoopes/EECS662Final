# EECS 662 Final Project: Sequence and Storage
The ***two*** features we added for our final are **Sequencing** and **Storage**. We started off using Henry's project 5 submission.
#### Team Info
Members:
1. Henry Michael Hoopes
2. Nabeel Ahmad
## Sequencing
### Examples
`TODO`
### Implementation
Sequencing was added through **Extension**, where we added constructs to the abstract syntax and changed the original `typeof`/`eval` functions.

Language Constructs:
```haskell
data KULang where
    ...
    Seq :: KULang -> KULang -> KULang
```

For `typeof`:
```haskell
typeof :: Cont -> KULang -> (Maybe KUTypeLang) 
...
typeof (Seq l r) = 
    do {
        typeof l;
        typeof r;
    }
```

For `eval`:
```haskell
eval :: Store -> EnvVal -> KULang -> (Maybe (Store, KULangVal))
...
eval (Seq l r) =
    do {
        eval l;
        eval r;
    }
```

In addition, some small changes were added to `KULangExt`, `subst`, & `elab`
## Storage
### Examples
`TODO`
### Implementation
Like Sequencing, Storage was added through **Extension**

Language Constructs:
```haskell
data KUTypeLang where
    ...
    TLoc :: KUTypeLang
    TTop :: KUTypeLang

data KULang where
    ...
    New :: KULang -> KULang
    Deref :: KULang -> KULang
    Set :: KULang -> KULang -> KULang

data KULangVal where
    ...
    LocV :: Int -> KULangVal
```

Typing Helpers:
```haskell
-- Succeeds if t is the expected type OR TTop
checkType :: KUTypeLang -> Maybe KUTypeLang -> Maybe KUTypeLang
checkType expected (Just TTop) = Just expected
checkType expected t =
    do {
        t' <- t;
        if t' == expected then Just expected else Nothing
    }
```

For `typeof`:
```haskell
typeof :: Cont -> KULang -> (Maybe KUTypeLang) 
...
typeof c (Plus l r) =
    do {
        checkType TNum (typeof c l);
        checkType TNum (typeof c r);
        return TNum;
    }
...
typeof c (And l r) =  
    do {
        checkType TBool (typeof c l);
        checkType TBool (typeof c r);
        return TBool;
    }
...
typeof c (If c' t e) =  
    do {
        checkType TBool (typeof c c');
        t' <- typeof c t; 
        e' <- typeof c e; 
        -- if both match, return type. 
        -- if either is TTop, assume it matches the other, so return the other's type.
        -- if both don't match and aren't TTop, fail.
        if (t' == e') then return t' else if (t' == TTop) then return e' else if (e' == TTop) then return t' else Nothing
    }
...
typeof c (Lambda i t b) = 
    do {
        -- using the type t of identifier i, determine type of body
        b' <- typeof ((i, t):c) b;
        -- Prevent user from typing arguments as TTop, since it is only intended for use with storage 
        -- and allowing it here would cause problems with type inference in function application
        if t == TTop then Nothing else return ((:->:) t b');
    }
typeof c (App f v) =
    do {
        -- current, where we use ((:->:) d r) to track type of functions
        ((:->:) d r) <- typeof c f;
        v' <- typeof c v;
        -- if actual is TTop, assume it matches formal
        -- otherwise, check to see if they match, and if not fail
        if (v' == TTop) then return r else if (v' == d) then return r else Nothing
    }
...
typeof c (New v) = 
    do {
        v' <- typeof c v;
        return TLoc;
    }
typeof c (Deref l) = 
    do {
        checkType TLoc (typeof c l);
        return TTop;
    }
typeof c (Set l v) = 
    do {
        checkType TLoc (typeof c l);
        typeof c v;
    }
```

Storage Helpers:
```haskell
type Loc = Int
type StoreFunc = Loc -> Maybe KULangVal
type Store = (Loc, StoreFunc)

deref :: StoreFunc -> Loc -> Maybe KULangVal
deref s l = s l

set :: StoreFunc -> Loc -> KULangVal -> StoreFunc
set s l v = 
   \m -> if m==l then (Just v) else s m

initStoreFunc :: StoreFunc
initStoreFunc x = Nothing

initStore :: Store
initStore = (0,initStoreFunc)
```

For `eval`:
```haskell
eval :: Store -> EnvVal -> KULang -> (Maybe (Store, KULangVal))
...
eval s e (New v) =
    do {
        ((l, sFunc), v') <- eval s e v;
        return ((l+1, (set sFunc l v')), (LocV l))
    }
eval s e (Deref l) = 
    do {
        ((l', sFunc), (LocV loc)) <- eval s e l;
        v' <- deref sFunc loc;
        return ((l', sFunc), v')
    }
eval s e (Set l v) =
    do {
        (s', (LocV loc)) <- eval s e l;
        ((l', sFunc), v') <- eval s' e v;
        return ((l', (set sFunc loc v')), v')
    }
```

In addition, some small changes were added to `KULangExt`, `subst`, & `elab`
## Running
To run the project, run the command `ghci All.hs` in the main directory. Then, you can run typeof, eval, elab, and interpret there. 

Additionally, there is automatic testing for the project. To test base functionality (all functionality required by project 5), run `test` after running `ghci All.hs`. To test our added features, run `testFeat` after running `ghci All.hs`

## TODO
### Tasks
1. Add examples to README.md