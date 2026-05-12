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
typeof :: KULang -> Reader Cont KUTypeLang
...
typeof (Seq l r) = 
    do {
        typeof l;
        typeof r;
    }
```

For eval:
```haskell
eval :: KULang -> Reader EnvVal KULangVal
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
`TODO`
## Running
To run the project, run the command `ghci All.hs` in the main directory. Then, you can run typeof, eval, elab, and interpret there. 

Additionally, there is automatic testing for the project. To test base functionality (all functionality required by project 5), run `test` after running `ghci All.hs`. To test our added features, run `testFeat` after running `ghci All.hs`

## Questions
**ASK BEFORE SUBMITTING**
1. Double check that eval should return result of last step, typeof should return type of last step
2. ...