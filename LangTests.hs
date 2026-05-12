{-# LANGUAGE GADTs, PostfixOperators #-}

-- allows this file to be treated as a module
module LangTests where

import Lang

-- ============================================================================== --
-- Test cases for base

fail_div = (Div (Num 1) (Num 0))
fail_div_type = Just (TNum)
fail_div_res = Nothing
fail_divX = (DivX (NumX 1) (NumX 0))
fail_div_interp = Nothing

fail_minus = (Minus (Num 1) (Num 2))
fail_minus_type = Just (TNum)
fail_minus_res = Nothing
fail_minusX = (MinusX (NumX 1) (NumX 2))
fail_minus_interp = Nothing

fail_neg = (Plus (Num 1) (Num (-2)))
fail_neg_type = Nothing
fail_neg_res = Nothing
fail_negX = (PlusX (NumX 1) (NumX (-2)))
fail_neg_interp = Nothing

fail_exp = (Exp (Num 2) (Num (-1)))
fail_exp_type = Nothing
fail_exp_res = Nothing
fail_expX = (ExpX (NumX 2) (NumX (-1)))
fail_exp_interp = Nothing

plus = (Plus (Num 1) (Num 2))
plus_type = Just (TNum)
plus_res = Just (NumV 3)
plusX = (PlusX (NumX 1) (NumX 2))
plus_interp = Just (NumV 3)

minus = (Minus (Num 4) (Num 3))
minus_type = Just (TNum)
minus_res = Just (NumV 1)
minusX = (MinusX (NumX 4) (NumX 3))
minus_interp = Just (NumV 1)

mult = (Mult (Num 3) (Num 4))
mult_type = Just (TNum)
mult_res = Just (NumV 12)
multX = (MultX (NumX 3) (NumX 4))
mult_interp = Just (NumV 12)

div1 = (Div (Num 1) (Num 2))
div1_type = Just (TNum)
div1_res = Just (NumV 0)
div1X = (DivX (NumX 1) (NumX 2))
div1_interp = Just (NumV 0)

div2 = (Div (Num 6) (Num 3))
div2_type = Just (TNum)
div2_res = Just (NumV 2)
div2X = (DivX (NumX 6) (NumX 3))
div2_interp = Just (NumV 2)

exp1 = (Exp (Num 2) (Num 0))
exp1_type = Just (TNum)
exp1_res = Just (NumV 1)
exp1X = (ExpX (NumX 2) (NumX 0))
exp1_interp = Just (NumV 1)

exp2 = (Exp (Num 2) (Num 1))
exp2_type = Just (TNum)
exp2_res = Just (NumV 2)
exp2X = (ExpX (NumX 2) (NumX 1))
exp2_interp = Just (NumV 2)

exp3 = (Exp (Num 2) (Num 2))
exp3_type = Just (TNum)
exp3_res = Just (NumV 4)
exp3X = (ExpX (NumX 2) (NumX 2))
exp3_interp = Just (NumV 4)

combined1 = Plus (Plus (Plus (Plus (Plus (Plus (Plus (Plus 
            (Plus (Num 1) (Num 2)) (Num 3)) (Num 4)) (Num 5))
            (Num 6)) (Num 7)) (Num 8)) (Num 9)) (Num 10)
combined1_type = Just (TNum)
combined1_res =     Just (NumV 55)  
combined1X = PlusX (PlusX (PlusX (PlusX (PlusX (PlusX (PlusX (PlusX 
            (PlusX (NumX 1) (NumX 2)) (NumX 3)) (NumX 4)) (NumX 5))
            (NumX 6)) (NumX 7)) (NumX 8)) (NumX 9)) (NumX 10)
combined1_interp =     Just (NumV 55)  

combined2 = Minus (Plus (Minus (Plus (Minus (Plus (Minus 
            (Plus (Minus (Num 1) (Num 2)) (Num 3)) (Num 4))
            (Num 5)) (Num 6)) (Num 7)) (Num 8)) (Num 9)) (Num 10)
combined2_type = Just (TNum)
combined2_res =       Nothing   
combined2X = MinusX (PlusX (MinusX (PlusX (MinusX (PlusX (MinusX 
            (PlusX (MinusX (NumX 1) (NumX 2)) (NumX 3)) (NumX 4))
            (NumX 5)) (NumX 6)) (NumX 7)) (NumX 8)) (NumX 9)) (NumX 10)
combined2_interp =       Nothing   

combined3 = Exp (Num 2) (Plus (Num 1) (Num 6))
combined3_type = Just (TNum)
combined3_res = Just (NumV 128)
combined3X = ExpX (NumX 2) (PlusX (NumX 1) (NumX 6))
combined3_interp = Just (NumV 128)

combined4 = Exp (Div (Num 14) (Num 2)) (Minus (Plus (Plus (Plus 
            (Num 1) (Num 1)) (Num 1)) (Num 1)) (Num 2))
combined4_type = Just (TNum)
combined4_res =     Just (NumV 49)
combined4X = ExpX (DivX (NumX 14) (NumX 2)) (MinusX (PlusX (PlusX (PlusX 
            (NumX 1) (NumX 1)) (NumX 1)) (NumX 1)) (NumX 2))
combined4_interp =     Just (NumV 49)

combined5 = Plus (Minus (Plus (Minus (Plus (Minus (Plus (Minus 
            (Plus (Num 1) (Num 2)) (Num 3)) (Num 5)) (Num 4)) (Num 6)) 
            (Num 7)) (Num 9)) (Num 8)) (Num 10)
combined5_type = Just (TNum)
combined5_res =   Just (NumV 11)     
combined5X = PlusX (MinusX (PlusX (MinusX (PlusX (MinusX (PlusX (MinusX 
            (PlusX (NumX 1) (NumX 2)) (NumX 3)) (NumX 5)) (NumX 4)) (NumX 6)) 
            (NumX 7)) (NumX 9)) (NumX 8)) (NumX 10)
combined5_interp =   Just (NumV 11)     

fail_and = (And (Boolean False) (Num 1))
fail_and_type = Nothing
fail_and_res = Nothing
fail_andX = (AndX (BooleanX False) (NumX 1))
fail_and_interp = Nothing

fail_and2 = (And (Num 1) (Boolean False))
fail_and2_type = Nothing
fail_and2_res = Nothing
fail_and2X = (AndX (NumX 1) (BooleanX False))
fail_and2_interp = Nothing

good_and = (And (Boolean False) (Boolean True))
good_and_type = Just (TBool)
good_and_res = Just (BooleanV False)
good_andX = (AndX (BooleanX False) (BooleanX True))
good_and_interp = Just (BooleanV False)

good_and2 = (And (Boolean True) (Boolean True))
good_and2_type = Just (TBool)
good_and2_res = Just (BooleanV True)
good_and2X = (AndX (BooleanX True) (BooleanX True))
good_and2_interp = Just (BooleanV True)

fail_or = (Or (Boolean False) (Num 1))
fail_or_type = Nothing
fail_or_res = Nothing
fail_orX = (OrX (BooleanX False) (NumX 1))
fail_or_interp = Nothing

fail_or2 = (Or (Num 1) (Boolean False))
fail_or2_type = Nothing
fail_or2_res = Nothing
fail_or2X = (OrX (NumX 1) (BooleanX False))
fail_or2_interp = Nothing

good_or = (Or (Boolean False) (Boolean True))
good_or_type = Just (TBool)
good_or_res = Just (BooleanV True)
good_orX = (OrX (BooleanX False) (BooleanX True))
good_or_interp = Just (BooleanV True)

good_or2 = (Or (Boolean False) (Boolean False))
good_or2_type = Just (TBool)
good_or2_res = Just (BooleanV False)
good_or2X = (OrX (BooleanX False) (BooleanX False))
good_or2_interp = Just (BooleanV False)

fail_ifz = (IsZero (Boolean False))
fail_ifz_type = Nothing
fail_ifz_res = Nothing
fail_ifzX = (IsZeroX (BooleanX False))
fail_ifz_interp = Nothing

good_ifz = (IsZero (Minus (Num 1) (Num 1)))
good_ifz_type = Just (TBool)
good_ifz_res =Just (BooleanV True)
good_ifzX = (IsZeroX (MinusX (NumX 1) (NumX 1)))
good_ifz_interp =Just (BooleanV True)

good_ifz2 = (IsZero (Minus (Num 3) (Num 1)))
good_ifz2_type = Just (TBool)
good_ifz2_res =Just (BooleanV False)
good_ifz2X = (IsZeroX (MinusX (NumX 3) (NumX 1)))
good_ifz2_interp =Just (BooleanV False)

fail_leq = (Leq (Boolean False) (Num 1))
fail_leq_type = Nothing
fail_leq_res =Nothing
fail_leqX = (LeqX (BooleanX False) (NumX 1))
fail_leq_interp =Nothing

good_leq = (Leq (Num 1) (Num 1))
good_leq_type = Just (TBool)
good_leq_res =Just (BooleanV True)
good_leqX = (LeqX (NumX 1) (NumX 1))
good_leq_interp =Just (BooleanV True)

good_leq2 = (Leq (Num 3) (Num 1))
good_leq2_type = Just (TBool)
good_leq2_res =Just (BooleanV False)
good_leq2X = (LeqX (NumX 3) (NumX 1))
good_leq2_interp =Just (BooleanV False)

good_leq3 = (Leq (Num 1) (Num 4))
good_leq3_type = Just (TBool)
good_leq3_res =Just (BooleanV True)
good_leq3X = (LeqX (NumX 1) (NumX 4))
good_leq3_interp =Just (BooleanV True)

fail_btwn = (Between (Boolean False) (Num 1) (Num 3))
fail_btwn_type = Nothing
fail_btwn_res =Nothing
fail_btwnX = (BetweenX (BooleanX False) (NumX 1) (NumX 3))
fail_btwn_interp =Nothing

good_btwn = (Between (Num 1) (Num 1) (Num 1))
good_btwn_type = Just (TBool)
good_btwn_res =Just (BooleanV False)
good_btwnX = (BetweenX (NumX 1) (NumX 1) (NumX 1))
good_btwn_interp =Just (BooleanV False)

good_btwn2 = (Between (Num 0) (Num 1) (Num 2))
good_btwn2_type = Just (TBool)
good_btwn2_res =Just (BooleanV True)
good_btwn2X = (BetweenX (NumX 0) (NumX 1) (NumX 2))
good_btwn2_interp =Just (BooleanV True)

good_btwn3 = (Between (Num 1) (Num 0) (Num 4))
good_btwn3_type = Just (TBool)
good_btwn3_res =Just (BooleanV False)
good_btwn3X = (BetweenX (NumX 1) (NumX 0) (NumX 4))
good_btwn3_interp =Just (BooleanV False)

fail_if = (If (Num 1) (Num 1) (Num 3))
fail_if_type = Nothing
fail_if_res =Nothing
fail_ifX = (IfX (NumX 1) (NumX 1) (NumX 3))
fail_if_interp =Nothing

good_if = (If (Boolean True) (Num 1) (Num 3))
good_if_type = Just (TNum)
good_if_res =Just (NumV 1)
good_ifX = (IfX (BooleanX True) (NumX 1) (NumX 3))
good_if_interp =Just (NumV 1)

good_if2 = (If (Boolean False) (Num 1) (Num 2))
good_if2_type = Just (TNum)
good_if2_res =Just (NumV 2)
good_if2X = (IfX (BooleanX False) (NumX 1) (NumX 2))
good_if2_interp =Just (NumV 2)

good_if3 = (If (Boolean True) (Boolean False) (Boolean True))
good_if3_type = Just (TBool)
good_if3_res =Just (BooleanV False)
good_if3X = (IfX (BooleanX True) (BooleanX False) (BooleanX True))
good_if3_interp =Just (BooleanV False)

good_if4 = (If (Boolean True) (Boolean False) (Num 1))
good_if4_type = Nothing
good_if4_res =Just (BooleanV False)
good_if4X = (IfX (BooleanX True) (BooleanX False) (NumX 1))
good_if4_interp = Nothing


undeclared_id1 = (Plus (Num 1) (Id "hello"))
undeclared_id1_type = Nothing
undeclared_id1_res = Nothing
undeclared_id1X = (PlusX (NumX 1) (IdX "hello"))
undeclared_id1_interp = Nothing

-- new tests made for this assignment

lambda1 = (Lambda "x" (TNum) (Num 1))
lambda1_type = Just ((:->:) TNum TNum)
lambda1_res = Just (ClosureV "x" (Num 1) [])
lambda1X = (LambdaX "x" (TNum) (NumX 1))
lambda1_interp = Just (ClosureV "x" (Num 1) [])

lambda2 = (Lambda "x" (TNum) (Plus (Id "x") (Num 2)))
lambda2_type = Just ((:->:) TNum TNum)
lambda2_res = Just (ClosureV "x" (Plus (Id "x") (Num 2)) [])
lambda2X = (LambdaX "x" (TNum) (PlusX (IdX "x") (NumX 2)))
lambda2_interp = Just (ClosureV "x" (Plus (Id "x") (Num 2)) [])

lambda3 = (Lambda "x" (TBool) (Plus (Id "x") (Num 2)))
lambda3_type = Nothing
lambda3_res = Just (ClosureV "x" (Plus (Id "x") (Num 2)) [])
lambda3X = (LambdaX "x" (TBool) (PlusX (IdX "x") (NumX 2)))
lambda3_interp = Nothing

lambda4 = (Lambda "x" (TBool) (Plus (Id "y") (Num 2)))
lambda4_type = Nothing
lambda4_res = Just (ClosureV "x" (Plus (Id "y") (Num 2)) [])
lambda4X = (LambdaX "x" (TBool) (PlusX (IdX "y") (NumX 2)))
lambda4_interp = Nothing

lambda5 = (Lambda "x" (TNum) (IsZero (Minus (Id "x") (Num 2))))
lambda5_type = Just ((:->:) TNum TBool)
lambda5_res = Just (ClosureV "x" ((IsZero (Minus (Id "x") (Num 2)))) [])
lambda5X = (LambdaX "x" (TNum) (IsZeroX (MinusX (IdX "x") (NumX 2))))
lambda5_interp = Just (ClosureV "x" ((IsZero (Minus (Id "x") (Num 2)))) [])

lambda6 = (Lambda "f" ((:->:) TNum TNum) ((Id "f")))
lambda6_type = Just ((:->:) ((:->:) TNum TNum) ((:->:) TNum TNum))
lambda6_res = Just (ClosureV "f" ((Id "f")) [])
lambda6X = (LambdaX "f" ((:->:) TNum TNum) ((IdX "f")))
lambda6_interp = Just (ClosureV "f" ((Id "f")) [])

app1 = (App lambda2 (Num 1))
app1_type = Just (TNum)
app1_res = Just (NumV 3)
app1X = (AppX lambda2X (NumX 1))
app1_interp = Just (NumV 3)

app2 = (App lambda2 (Boolean False))
app2_type = Nothing
app2_res = Nothing
app2X = (AppX lambda2X (BooleanX False))
app2_interp = Nothing

app3 = (App lambda2 (Plus (Num 1) (Boolean False)))
app3_type = Nothing
app3_res = Nothing
app3X = (AppX lambda2X (PlusX (NumX 1) (BooleanX False)))
app3_interp = Nothing

app4 = (App lambda5 (Num 1))
app4_type = Just (TBool)
app4_res = Nothing
app4X = (AppX lambda5X (NumX 1))
app4_interp = Nothing

app5 = (App lambda5 (Num 3))
app5_type = Just (TBool)
app5_res = Just (BooleanV False)
app5X = (AppX lambda5X (NumX 3))
app5_interp = Just (BooleanV False)

app6 = (App lambda5 (Num 2))
app6_type = Just (TBool)
app6_res = Just (BooleanV True)
app6X = (AppX lambda5X (NumX 2))
app6_interp = Just (BooleanV True)

app7 = (Lambda "f" ((:->:) TNum TNum) (App (Id "f") (Num 2)))
app7_type = Just ((:->:) ((:->:) TNum TNum) (TNum))
app7_res = Just (ClosureV "f" (App (Id "f") (Num 2)) [])
app7X = (LambdaX "f" ((:->:) TNum TNum) (AppX (IdX "f") (NumX 2)))
app7_interp = Just (ClosureV "f" (App (Id "f") (Num 2)) [])

scopefunc1 = (App 
                (Lambda "f" (TNum)
                    (App
                        (Lambda "n" (TNum)
                            (Plus (Id "f") (Id "n" )) 
                        ) 
                        (Num 2)
                    )
                ) 
                (App 
                    (Lambda "n" (TNum)
                        (Plus (Id "n") (Id "n"))
                    )
                    (Num 1) 
                ) 
            )
scopefunc1_type = Just TNum
scopefunc1_res = Just (NumV 4)
scopefunc1X = (BindX "f" (TNum) (BindX "n" (TNum) (NumX 1) (PlusX (IdX "n") (IdX "n"))) (BindX "n" (TNum) (NumX 2) (PlusX (IdX "f") (IdX "n"))))
scopefunc1_interp = Just (NumV 4)

scopefunc2 = (App 
                (Lambda "f" ((:->:) TNum TNum)
                    (App
                        (Lambda "n" (TNum)
                            (App (Id "f") (Id "n" )) 
                        ) 
                        (Num 2)
                    )
                ) 
                (App 
                    (Lambda "n" (TNum)
                        (Lambda "x" (TNum)
                            (Plus (Id "x") (Id "n"))
                        )
                    )
                    (Num 1) 
                ) 
            )
scopefunc2_type = Just TNum
scopefunc2_res = Just (NumV 3)
scopefunc2X = (BindX "f" ((:->:) TNum TNum) (BindX "n" (TNum) (NumX 1) (LambdaX "x" (TNum) (PlusX (IdX "x") (IdX "n")))) (BindX "n" (TNum) (NumX 2) (AppX (IdX "f") (IdX "n"))))
scopefunc2_interp = Just (NumV 3)

testFib = interpret ( 
                BindX "fib" ((:->:) TNum TNum)
                    (FixX (LambdaX "g" ((:->:) TNum TNum)
                        (LambdaX "x" TNum 
                            (IfX (LeqX (IdX "x") (NumX 1))
                                (IdX "x")
                                (PlusX 
                                    (AppX (IdX "g") (MinusX (IdX "x") (NumX 1)))
                                    (AppX (IdX "g") (MinusX (IdX "x") (NumX 2)))
                                )
                            )
                        )
                    ))
                    (AppX (IdX "fib") (NumX 2))) == 
                Just (NumV 1)

testIsEven = interpret ( 
                BindX "isEven" ((:->:) TNum TBool)
                    (FixX (LambdaX "g" ((:->:) TNum TBool)
                        (LambdaX "x" TNum
                            (IfX (IsZeroX (IdX "x"))
                                (BooleanX True)
                                (IfX (LeqX (IdX "x") (NumX 1))
                                    (BooleanX False)
                                    (AppX (IdX "g") (MinusX (IdX "x") (NumX 2)))
                                )
                            )
                        )
                    ))
                    (AppX (IdX "isEven") (NumX 67))) == 
                Just (BooleanV False)

-- ============================================================================== --
-- Code to run all tests and output results

testCases = [fail_div, fail_minus, fail_neg, fail_exp, plus, minus, mult, div1, div2, exp1, exp2, 
             exp3, combined1, combined2, combined3, combined4, combined5, fail_and, fail_and2,
             good_and, good_and2, fail_or, fail_or2, good_or, good_or2, fail_ifz, good_ifz, good_ifz2, fail_leq, 
             good_leq, good_leq2, good_leq3, fail_btwn, good_btwn, good_btwn2, good_btwn3, fail_if, good_if, 
             good_if2, good_if3, good_if4, undeclared_id1, lambda1, lambda2, lambda3, lambda4, lambda5, app1,
             app2, app3, app4, app5, lambda6, app6, app7, scopefunc1, scopefunc2]

typeResultsExpected =  [fail_div_type, fail_minus_type, fail_neg_type, fail_exp_type, plus_type,
                        minus_type, mult_type, div1_type, div2_type, exp1_type, exp2_type, exp3_type,
                        combined1_type, combined2_type, combined3_type, combined4_type, combined5_type, 
                        fail_and_type, fail_and2_type, good_and_type, good_and2_type, fail_or_type, 
                        fail_or2_type, good_or_type, good_or2_type, fail_ifz_type, good_ifz_type, 
                        good_ifz2_type, fail_leq_type, good_leq_type, good_leq2_type, good_leq3_type, 
                        fail_btwn_type, good_btwn_type, good_btwn2_type, good_btwn3_type, fail_if_type, 
                        good_if_type, good_if2_type, good_if3_type, good_if4_type, undeclared_id1_type,
                        lambda1_type, lambda2_type, lambda3_type, lambda4_type, lambda5_type, app1_type,
                        app2_type, app3_type, app4_type, app5_type, lambda6_type, app6_type, app7_type,
                        scopefunc1_type, scopefunc2_type]

evalResultsExpected =  [fail_div_res, fail_minus_res, fail_neg_res, fail_exp_res, plus_res,
                        minus_res, mult_res, div1_res, div2_res, exp1_res, exp2_res, exp3_res,
                        combined1_res, combined2_res, combined3_res, combined4_res, combined5_res, 
                        fail_and_res, fail_and2_res, good_and_res, good_and2_res, fail_or_res, 
                        fail_or2_res, good_or_res, good_or2_res, fail_ifz_res, good_ifz_res, 
                        good_ifz2_res, fail_leq_res, good_leq_res, good_leq2_res, good_leq3_res, 
                        fail_btwn_res, good_btwn_res, good_btwn2_res, good_btwn3_res, fail_if_res, 
                        good_if_res, good_if2_res, good_if3_res, good_if4_res, undeclared_id1_res,
                        lambda1_res, lambda2_res, lambda3_res, lambda4_res, lambda5_res, app1_res,
                        app2_res, app3_res, app4_res, app5_res, lambda6_res, app6_res, app7_res,
                        scopefunc1_res, scopefunc2_res]

testCasesExt = [fail_divX, fail_minusX, fail_negX, fail_expX, plusX, minusX, multX, div1X, div2X, exp1X, 
                exp2X, exp3X, combined1X, combined2X, combined3X, combined4X, combined5X, fail_andX, fail_and2X,
                good_andX, good_and2X, fail_orX, fail_or2X, good_orX, good_or2X, fail_ifzX, good_ifzX, good_ifz2X,
                fail_leqX, good_leqX, good_leq2X, good_leq3X, fail_btwnX, good_btwnX, good_btwn2X, good_btwn3X, 
                fail_ifX, good_ifX, good_if2X, good_if3X, good_if4X, undeclared_id1X, lambda1X, lambda2X, 
                lambda3X, lambda4X, lambda5X, app1X, app2X, app3X, app4X, app5X, lambda6X, app6X, app7X, scopefunc1X, 
                scopefunc2X]

interpResultsExpected = [fail_div_interp, fail_minus_interp, fail_neg_interp, fail_exp_interp, plus_interp,
                        minus_interp, mult_interp, div1_interp, div2_interp, exp1_interp, exp2_interp, exp3_interp,
                        combined1_interp, combined2_interp, combined3_interp, combined4_interp, combined5_interp, 
                        fail_and_interp, fail_and2_interp, good_and_interp, good_and2_interp, fail_or_interp, 
                        fail_or2_interp, good_or_interp, good_or2_interp, fail_ifz_interp, good_ifz_interp, 
                        good_ifz2_interp, fail_leq_interp, good_leq_interp, good_leq2_interp, good_leq3_interp, 
                        fail_btwn_interp, good_btwn_interp, good_btwn2_interp, good_btwn3_interp, fail_if_interp, 
                        good_if_interp, good_if2_interp, good_if3_interp, good_if4_interp, undeclared_id1_interp,
                        lambda1_interp, lambda2_interp, lambda3_interp, lambda4_interp, lambda5_interp, app1_interp,
                        app2_interp, app3_interp, app4_interp, app5_interp, lambda6_interp, app6_interp, app7_interp,
                        scopefunc1_interp, scopefunc2_interp]

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

test = do
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
    putStr "Fib Test Success:"
    print (testFib)
    putStrLn "----------------------------"
    putStr "IsEven Test Success:"
    print (testIsEven)
    putStrLn "----------------------------"