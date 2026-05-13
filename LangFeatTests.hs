{-# LANGUAGE GADTs, PostfixOperators #-}

-- allows this file to be treated as a module
module LangFeatTests where

import Lang

-- ============================================================================== --
-- Test cases for added features (sequencing, storage)

-- ======================= Sequencing tests ======================= --
-- Concrete Syntax: 1+2;3+4
seq1 = (Seq (Plus (Num 1) (Num 2)) (Plus (Num 3) (Num 4)))
seq1_type = Just (TNum)
seq1_res = Just (NumV 7)
seq1X = (SeqX (PlusX (NumX 1) (NumX 2)) (PlusX (NumX 3) (NumX 4)))
seq1_interp = Just (NumV 7)

-- Concrete Syntax: Bind x = 1+2 in x+4;x+5
seq2 = (App (Lambda "x" TNum (Seq (Plus (Id "x") (Num 4)) (Plus (Id "x") (Num 5)))) (Plus (Num 1) (Num 2)))
seq2_type = Just (TNum)
seq2_res = Just (NumV 8)
seq2X = (BindX "x" TNum (PlusX (NumX 1) (NumX 2)) (SeqX (PlusX (IdX "x") (NumX 4)) (PlusX (IdX "x") (NumX 5))))
seq2_interp = Just (NumV 8)

-- Concrete Syntax: 1+2;3+4;5+6;7+8
seq3 = (Seq (Seq (Seq (Plus (Num 1) (Num 2)) (Plus (Num 3) (Num 4))) (Plus (Num 5) (Num 6))) (Plus (Num 7) (Num 8)))
seq3_type = Just (TNum)
seq3_res = Just (NumV 15)
seq3X = (SeqX (SeqX (SeqX (PlusX (NumX 1) (NumX 2)) (PlusX (NumX 3) (NumX 4))) (PlusX (NumX 5) (NumX 6))) (PlusX (NumX 7) (NumX 8)))
seq3_interp = Just (NumV 15)

-- ======================= Storage tests ======================= --
storage1 = (App (Lambda "x" TLoc (Deref (Id "x"))) (New (Num 42)))
storage1_type = Just (TTop)
storage1_res = Just (NumV 42)
storage1X = (BindX "x" TLoc (NewX (NumX 42)) (DerefX (IdX "x")))
storage1_interp = Just (NumV 42)

storage2 = (App (Lambda "x" TLoc (Seq (Set (Id "x") (Num 43)) (Deref (Id "x")))) (New (Num 42)))
storage2_type = Just (TTop)
storage2_res = Just (NumV 43)
storage2X = (BindX "x" TLoc (NewX (NumX 42)) (SeqX (SetX (IdX "x") (NumX 43)) (DerefX (IdX "x"))))
storage2_interp = Just (NumV 43)

storage3 = (App (Lambda "x" TLoc (Seq (Set (Id "x") (Num 43)) (Plus (Num 1) (Deref (Id "x"))))) (New (Num 42)))
storage3_type = Just (TNum)
storage3_res = Just (NumV 44)
storage3X = (BindX "x" TLoc (NewX (NumX 42)) (SeqX (SetX (IdX "x") (NumX 43)) (PlusX (NumX 1) (DerefX (IdX "x")))))
storage3_interp = Just (NumV 44)

storage4 = (Lambda "x" TTop (Id "x"))
storage4_type = Nothing
storage4_res = Just (ClosureV "x" (Id "x") [])
storage4X = (LambdaX "x" TTop (IdX "x"))
storage4_interp = Nothing

storage5 = (App storage4 (Deref (New (Num 42))))
storage5_type = Nothing
storage5_res = Just (NumV 42)
storage5X = (AppX storage4X (DerefX (NewX (NumX 42))))
storage5_interp = Nothing

storage6 = (App storage4 (New (Num 42)))
storage6_type = Nothing
storage6_res = Just (LocV 0)
storage6X = (AppX storage4X (NewX (NumX 42)))
storage6_interp = Nothing

-- Case where we cannot determine if If is valid since condition is TTop at type inference time, but at runtime is a TNum.
storage_if1 = (App (Lambda "x" TLoc (If (Deref (Id "x")) (Num 1) (Num 2))) (New (Num 42)))
storage_if1_type = Just (TNum)
storage_if1_res = Nothing
storage_if1X = (BindX "x" TLoc (NewX (NumX 42)) (IfX (DerefX (IdX "x")) (NumX 1) (NumX 2)))
storage_if1_interp = Nothing

storage_if2 = (App (Lambda "x" TLoc (If (Deref (Id "x")) (Num 1) (Num 2))) (New (Boolean False)))
storage_if2_type = Just (TNum)
storage_if2_res = Just (NumV 2)
storage_if2X = (BindX "x" TLoc (NewX (BooleanX False)) (IfX (DerefX (IdX "x")) (NumX 1) (NumX 2)))
storage_if2_interp = Just (NumV 2)

storage_if3 = (App (Lambda "x" TLoc (If (Deref (Id "x")) (Num 1) (Num 2))) (New (Boolean True)))
storage_if3_type = Just (TNum)
storage_if3_res = Just (NumV 1)
storage_if3X = (BindX "x" TLoc (NewX (BooleanX True)) (IfX (DerefX (IdX "x")) (NumX 1) (NumX 2)))
storage_if3_interp = Just (NumV 1)

-- Odd case: what should be done with the type here? what if it ends up not matching?
-- Can be written this way, but doesn't match how elab unfolds Binds: 
--      storage_if4 = (App (App (Lambda "y" TLoc (Lambda "x" TLoc (If (Deref (Id "x")) (Deref (Id "y")) (Num 2)))) (New (Num 42))) (New (Boolean True)))
storage_if4 = App (Lambda "y" TLoc (App (Lambda "x" TLoc (If (Deref (Id "x")) (Deref (Id "y")) (Num 2))) (New (Boolean True)))) (New (Num 42))
storage_if4_type = Just (TNum)
storage_if4_res = Just (NumV 42)
storage_if4X = (BindX "y" TLoc (NewX (NumX 42)) (BindX "x" TLoc (NewX (BooleanX True)) (IfX (DerefX (IdX "x")) (DerefX (IdX "y")) (NumX 2))))
storage_if4_interp = Just (NumV 42)

-- Similar to above, we cannot determine if the result from if is valid since we are using TTop as one of the cases' return types.
storage_if5 = App (Lambda "y" TLoc (App (Lambda "x" TLoc (If (Deref (Id "x")) (Deref (Id "y")) (Num 2))) (New (Boolean True)))) (New (Boolean False))
storage_if5_type = Just (TNum)
storage_if5_res = Just (BooleanV False)
storage_if5X = (BindX "y" TLoc (NewX (BooleanX False)) (BindX "x" TLoc (NewX (BooleanX True)) (IfX (DerefX (IdX "x")) (DerefX (IdX "y")) (NumX 2))))
storage_if5_interp = Just (BooleanV False)

-- Similar to above, but this results in failure
storage_if6 = App (Lambda "y" TLoc (App (Lambda "x" TLoc (Plus (Num 1) (If (Deref (Id "x")) (Deref (Id "y")) (Num 2)))) (New (Boolean True)))) (New (Boolean False))
storage_if6_type = Just (TNum)
storage_if6_res = Nothing
storage_if6X = (BindX "y" TLoc (NewX (BooleanX False)) (BindX "x" TLoc (NewX (BooleanX True)) (PlusX (NumX 1) (IfX (DerefX (IdX "x")) (DerefX (IdX "y")) (NumX 2)))))
storage_if6_interp = Nothing

storage_if7 = App (Lambda "y" TLoc (App (Lambda "x" TLoc (Plus (Num 1) (If (Deref (Id "x")) (Deref (Id "y")) (Num 2)))) (New (Boolean True)))) (New (Num 42))
storage_if7_type = Just (TNum)
storage_if7_res = Just (NumV 43)
storage_if7X = (BindX "y" TLoc (NewX (NumX 42)) (BindX "x" TLoc (NewX (BooleanX True)) (PlusX (NumX 1) (IfX (DerefX (IdX "x")) (DerefX (IdX "y")) (NumX 2)))))
storage_if7_interp = Just (NumV 43)

storage_if8 = App (Lambda "z" TLoc (App (Lambda "y" TLoc (App (Lambda "x" TLoc (Plus (Num 1) (If (Deref (Id "x")) (Deref (Id "y")) (Deref (Id "z"))))) (New (Boolean False)))) (New (Num 42)))) (New (Num 32))
storage_if8_type = Just (TNum)
storage_if8_res = Just (NumV 33)
storage_if8X = (BindX "z" TLoc (NewX (NumX 32)) (BindX "y" TLoc (NewX (NumX 42)) (BindX "x" TLoc (NewX (BooleanX False)) (PlusX (NumX 1) (IfX (DerefX (IdX "x")) (DerefX (IdX "y")) (DerefX (IdX "z")))))))
storage_if8_interp = Just (NumV 33)

storage_if9 = App (Lambda "z" TLoc (App (Lambda "y" TLoc (App (Lambda "x" TLoc (If (Deref (Id "x")) (Deref (Id "y")) (Deref (Id "z")))) (New (Boolean False)))) (New (Num 42)))) (New (Num 32))
storage_if9_type = Just (TTop)
storage_if9_res = Just (NumV 32)
storage_if9X = (BindX "z" TLoc (NewX (NumX 32)) (BindX "y" TLoc (NewX (NumX 42)) (BindX "x" TLoc (NewX (BooleanX False)) (IfX (DerefX (IdX "x")) (DerefX (IdX "y")) (DerefX (IdX "z"))))))
storage_if9_interp = Just (NumV 32)

-- Similar to above, since both are using TTop as the return type, we cannot detect the future failure when using plus with boolean values.
storage_if10 = App (Lambda "z" TLoc (App (Lambda "y" TLoc (App (Lambda "x" TLoc (Plus (Num 1) (If (Deref (Id "x")) (Deref (Id "y")) (Deref (Id "z"))))) (New (Boolean False)))) (New (Num 42)))) (New (Boolean False))
storage_if10_type = Just (TNum)
storage_if10_res = Nothing
storage_if10X = (BindX "z" TLoc (NewX (BooleanX False)) (BindX "y" TLoc (NewX (NumX 42)) (BindX "x" TLoc (NewX (BooleanX False)) (PlusX (NumX 1) (IfX (DerefX (IdX "x")) (DerefX (IdX "y")) (DerefX (IdX "z")))))))
storage_if10_interp = Nothing

-- showcases potential uses of set
storage_set1 = App (Lambda "y" TLoc (App (Lambda "x" TLoc (Seq (Set (Id "y") (If (Deref (Id "x")) (Deref (Id "y")) (Num 2))) (Deref (Id "y")))) (New (Boolean False)))) (New (Num 42))
storage_set1_type = Just (TTop)
storage_set1_res = Just (NumV 2)
storage_set1X = (BindX "y" TLoc (NewX (NumX 42)) (BindX "x" TLoc (NewX (BooleanX False)) (SeqX (SetX (IdX "y") (IfX (DerefX (IdX "x")) (DerefX (IdX "y")) (NumX 2))) (DerefX (IdX "y")))))
storage_set1_interp = Just (NumV 2)

-- showcases nested storage
doubleloc1 = App (Lambda "x" TLoc (App (Lambda "y" TLoc (Deref (Id "y"))) (New (Id "x")))) (New (Num 42))
doubleloc1_type = Just (TTop)
doubleloc1_res = Just (LocV 0)
doubleloc1X = (BindX "x" TLoc (NewX (NumX 42)) (BindX "y" TLoc (NewX (IdX "x")) (DerefX (IdX "y"))))
doubleloc1_interp = Just (LocV 0)

doubleloc2 = App (Lambda "x" TLoc (App (Lambda "y" TLoc (Id "y")) (New (Id "x")))) (New (Num 42))
doubleloc2_type = Just (TLoc)
doubleloc2_res = Just (LocV 1)
doubleloc2X = (BindX "x" TLoc (NewX (NumX 42)) (BindX "y" TLoc (NewX (IdX "x")) (IdX "y")))
doubleloc2_interp = Just (LocV 1)

doubleloc3 = App (Lambda "x" TLoc (App (Lambda "y" TLoc (Deref (Deref (Id "y")))) (New (Id "x")))) (New (Num 42))
doubleloc3_type = Just (TTop)
doubleloc3_res = Just (NumV 42)
doubleloc3X = (BindX "x" TLoc (NewX (NumX 42)) (BindX "y" TLoc (NewX (IdX "x")) (DerefX (DerefX (IdX "y")))))
doubleloc3_interp = Just (NumV 42)
-- ============================================================================== --
-- Code to run all tests and output results

testCases             = [seq1, seq2, seq3, storage1, storage2, storage3, storage4, storage5, storage6, storage_if1, storage_if2, storage_if3,
                        storage_if4, storage_set1, storage_if5, storage_if6, storage_if7, storage_if8, storage_if9, storage_if10, doubleloc1, 
                        doubleloc2, doubleloc3]
typeResultsExpected   = [seq1_type, seq2_type, seq3_type, storage1_type, storage2_type, storage3_type, storage4_type, storage5_type, 
                        storage6_type, storage_if1_type, storage_if2_type, storage_if3_type, storage_if4_type, storage_set1_type, storage_if5_type, 
                        storage_if6_type, storage_if7_type, storage_if8_type, storage_if9_type, storage_if10_type, doubleloc1_type, doubleloc2_type, 
                        doubleloc3_type]
evalResultsExpected   = [seq1_res, seq2_res, seq3_res, storage1_res, storage2_res, storage3_res, storage4_res, storage5_res, storage6_res, 
                        storage_if1_res, storage_if2_res, storage_if3_res, storage_if4_res, storage_set1_res, storage_if5_res, storage_if6_res, 
                        storage_if7_res, storage_if8_res, storage_if9_res, storage_if10_res, doubleloc1_res, doubleloc2_res, doubleloc3_res]
testCasesExt          = [seq1X, seq2X, seq3X, storage1X, storage2X, storage3X, storage4X, storage5X,storage6X,storage_if1X,storage_if2X, 
                        storage_if3X, storage_if4X, storage_set1X, storage_if5X, storage_if6X, storage_if7X, storage_if8X, storage_if9X, 
                        storage_if10X, doubleloc1X, doubleloc2X, doubleloc3X]
interpResultsExpected = [seq1_interp, seq2_interp, seq3_interp, storage1_interp, storage2_interp, storage3_interp, storage4_interp, 
                        storage5_interp, storage6_interp, storage_if1_interp, storage_if2_interp, storage_if3_interp, storage_if4_interp, 
                        storage_set1_interp, storage_if5_interp, storage_if6_interp, storage_if7_interp, storage_if8_interp, storage_if9_interp,
                        storage_if10_interp, doubleloc1_interp, doubleloc2_interp, doubleloc3_interp]

compareLists :: Eq a => [a] -> [a] -> [Bool]
compareLists list1 list2 = zipWith (==) list1 list2

typeResults = (map (typeof []) testCases)
typeDiff = compareLists typeResults typeResultsExpected

testEval :: KULang -> Maybe KULangVal
testEval x = 
    do {
        (s, v) <- eval initStore [] x;
        return v
    }

evalResults = (map testEval testCases)
evalDiff = compareLists evalResults evalResultsExpected

elabResults = (map elabTerm testCasesExt)
elabDiff = compareLists elabResults testCases

interpResults = (map interpret testCasesExt)
interpDiff = compareLists interpResults interpResultsExpected

testFeat = do
    let typeSuccess = typeResults == typeResultsExpected
    putStr "Type Inference Success:"
    print (typeSuccess)
    if (not typeSuccess) then print(typeDiff) else putStrLn "----------------------------"
    let evalSuccess = evalResults == evalResultsExpected
    putStr "Eval Success:"
    print (evalSuccess)
    if (not evalSuccess) then print(evalDiff) else putStrLn "----------------------------"
    let elabSuccess = elabResults == testCases
    putStr "Elab Success:"
    print (elabSuccess)
    if (not elabSuccess) then print(elabDiff) else putStrLn "----------------------------"
    let interpSuccess = interpResults == interpResultsExpected
    putStr "Interpret Success:"
    print (interpSuccess)
    if (not interpSuccess) then print(interpDiff) else putStrLn "----------------------------"