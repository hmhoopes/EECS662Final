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

For typeof:
```haskell
typeof :: Cont -> KULang -> (Maybe KUTypeLang) 
...
typeof (Seq l r) = 
    do {
        typeof l;
        typeof r;
    }
```

For eval:
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

Helpers:
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

For typeof:
```haskell
typeof :: Cont -> KULang -> (Maybe KUTypeLang) 
...
typeof c (New v) = 
    do {
        v' <- typeof c v;
        return TLoc;
    }
typeof c (Deref l) = 
    do {
        TLoc <- typeof c l;
        return TTop;
    }
typeof c (Set l v) = 
    do {
        TLoc <- typeof c l;
        typeof c v;
    }
```

For eval:
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
1. Add examples, explanation to README.md