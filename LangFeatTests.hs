{-# LANGUAGE GADTs, PostfixOperators #-}

-- allows this file to be treated as a module
module LangFeatTests where

import Lang

-- ============================================================================== --
-- Test cases for added features (sequencing, storage)

-- Sequencing tests
seq1 = (Seq (Plus (Num 1) (Num 2)) (Plus (Num 3) (Num 4)))
seq1_type = Just (TNum)
seq1_res = Just (NumV 7)
seq1X = (SeqX (PlusX (NumX 1) (NumX 2)) (PlusX (NumX 3) (NumX 4)))
seq1_interp = Just (NumV 7)


-- Storage tests

-- ============================================================================== --
-- Code to run all tests and output results

testCases = [seq1]
typeResultsExpected = [seq1_type]
evalResultsExpected = [seq1_res]
testCasesExt = [seq1X]
interpResultsExpected = [seq1_interp]

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