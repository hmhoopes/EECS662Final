{-# LANGUAGE GADTs, PostfixOperators #-}

-- allows this file to be treated as a module
module LangFeatTests where

import Lang

-- ============================================================================== --
-- Test cases for added features (sequencing, storage)

-- Sequencing tests
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
-- Storage tests

-- ============================================================================== --
-- Code to run all tests and output results

testCases = [seq1, seq2, seq3]
typeResultsExpected = [seq1_type, seq2_type, seq3_type]
evalResultsExpected = [seq1_res, seq2_res, seq3_res]
testCasesExt = [seq1X, seq2X, seq3X]
interpResultsExpected = [seq1_interp, seq2_interp, seq3_interp]

compareLists :: Eq a => [a] -> [a] -> [Bool]
compareLists list1 list2 = zipWith (==) list1 list2

testTypeof :: KULang -> Maybe KUTypeLang
testTypeof x = runR (typeof x) []

typeResults = (map (testTypeof) testCases)
typeDiff = compareLists typeResults typeResultsExpected

testEval :: KULang -> Maybe KULangVal
testEval x = runR (eval x) []

evalResults = (map (testEval) testCases)
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