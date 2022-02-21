module Assignment2Tests


// Test cases for Problem 1

// > checkAllPatterns (Let (PPair (PVar "x", PPair (PUnderscore, PVar "x")), Pair (Num 1, Pair (Num 2, Num 3)), Num 4));;
// val it: bool = false
// > checkAllPatterns (Let (PPair (PVar "x", PVar "y"), Pair (Num 1, Pair (Num 2, Num 3)), Let (PPair (PUnderscore, PVar "x"), Var "y", Num 4)));;
// val it: bool = true
// > checkAllPatterns (Let (PPair (PVar "x", PVar "y"), Pair (Num 1, Num 2), Let (PPair (PVar "z", PVar "z"), Pair (Num 3, Num 4), Num 5)));;
// val it: bool = false
// > checkAllPatterns (Pair (Var "x", Var "x"));;
// val it: bool = true
// > checkAllPatterns (Pair (Num 1, Let (PPair (PVar "x", PVar "x"), Num 2, Num 3)));;
// val it: bool = false
// > checkAllPatterns (Pair (Num 1, Let (PPair (PVar "x", PUnderscore), Num 2, Num 3)));;
// val it: bool = true


// Test cases for Problem 2

// > lex "let _ = x in 3";;
// val it: token list = [LET; UNDERSCORE; EQUAL; NAME "x"; IN; INT 3]
// > lex "let x, y = (1, 2) in 3";;
// val it: token list = [LET; NAME "x"; COMMA; NAME "y"; EQUAL; LPAR; INT 1; COMMA; INT 2; RPAR; IN; INT 3]
// > lex "let ((_, x), (z, w)) = p in x + z + w";;
// val it: token list = [LET; LPAR; LPAR; UNDERSCORE; COMMA; NAME "x"; RPAR; COMMA; LPAR; NAME "z"; COMMA; NAME "w"; RPAR; RPAR; EQUAL; NAME "p"; IN; NAME "x"; PLUS; NAME "z"; PLUS; NAME "w"]


// Test cases for Problem 3
// For the tests that raise an exception, it doesn't matter what the
// message says.

// > parse [LET; UNDERSCORE; EQUAL; NAME "x"; IN; INT 3];;
// val it: expr = Let (PUnderscore, Var "x", Num 3)
// > parse [LET; NAME "x"; COMMA; NAME "y"; EQUAL; LPAR; INT 1; COMMA; INT 2; RPAR; IN; INT 3];;
// val it: expr = Let (PPair (PVar "x", PVar "y"), Pair (Num 1, Num 2), Num 3)
// > parse [LET; LPAR; LPAR; UNDERSCORE; COMMA; NAME "x"; RPAR; COMMA; LPAR; NAME "z"; COMMA; NAME "w"; RPAR; RPAR; EQUAL; NAME "p"; IN; NAME "x"; PLUS; NAME "z"; PLUS; NAME "w"];;
// val it: expr = Let (PPair (PPair (PUnderscore, PVar "x"), PPair (PVar "z", PVar "w")), Var "p", Plus (Var "x", Plus (Var "z", Var "w")))
// > parse [LET; NAME "x"; COMMA; NAME "y"; COMMA; NAME "z"; EQUAL; INT 1];;
// System.Exception: let without equals sign
// > parse [LET; LPAR; NAME "x"; COMMA; NAME "y"; RPAR; COMMA; NAME "z"; EQUAL; INT 1; IN; INT 2];;
// val it: expr = Let (PPair (PPair (PVar "x", PVar "y"), PVar "z"), Num 1, Num 2)
// > parse [LET; INT 1; EQUAL; INT 2; IN; INT 3];;
// System.Exception: not a pattern
// > parse [LET; LPAR; NAME "x"; COMMA; NAME "y"; PLUS; INT 1; RPAR; EQUAL; INT 5; IN; INT 6];;
// System.Exception: left paren without right paren
// > parse [LET; LPAR; LPAR; NAME "x"; COMMA; LPAR; LPAR; NAME "y"; RPAR; RPAR; RPAR; RPAR; EQUAL; NAME "p"; IN; NAME "q"; PLUS; NAME "r"];;
// val it: expr = Let (PPair (PVar "x", PVar "y"), Var "p", Plus (Var "q", Var "r"))


// Test cases for Problem 4
// For the tests that raise an exception, it doesn't matter what the
// message says. For those that add several bindings to the environment,
// the order of the new bindings isn't important.

// > patternMatch PUnderscore (VNum 1) [];;
// val it: envir = []
// > patternMatch (PVar "x") (VNum 1) [];;
// val it: envir = [("x", VNum 1)]
// > patternMatch (PPair (PUnderscore, PUnderscore)) (VNum 1) [];;
// System.Exception: expected a pair, but given an int
// > patternMatch (PPair (PUnderscore, PVar "a")) (VPair (VNum 1, VPair (VNum 10, VNum 20))) [];;
// val it: envir = [("a", VPair (VNum 10, VNum 20))]
// > patternMatch (PPair (PUnderscore, PVar "a")) (VPair (VNum 1, VPair (VNum 10, VNum 20))) [("x", VNum 50); ("y", VPair (VNum 51, VNum 52))];;
// val it: envir = [("a", VPair (VNum 10, VNum 20)); ("x", VNum 50); ("y", VPair (VNum 51, VNum 52))]
// > patternMatch (PPair (PUnderscore, PPair (PVar "a", PVar "b"))) (VPair (VNum 1, VPair (VNum 10, VNum 20))) [];;
// val it: envir = [("b", VNum 20); ("a", VNum 10)]
// > patternMatch (PPair (PPair (PUnderscore, PUnderscore), PPair (PVar "a", PVar "b"))) (VPair (VNum 1, VPair (VNum 10, VNum 20))) [];;
// System.Exception: expected a pair, but given an int

// > eval (Let (PVar "x", Num 1, Plus (Var "x", Var "x"))) [];;
// val it: value = VNum 2
// > eval (Let (PVar "x", Num 1, Let (PVar "y", Num 2, Plus (Var "x", Var "y")))) [];;
// val it: value = VNum 3
// > eval (Let (PPair (PVar "x", PVar "y"), Pair (Num 1, Num 2), Let (PPair (PVar "z", PPair (PVar "w", PUnderscore)), Pair (Num 1, Pair (Var "x", Var "y")), Plus (Var "z", Var "w")))) [];;
// val it: value = VNum 2
// > eval (Let (PPair (PVar "x", PVar "y"), Pair (Num 1, Num 2), Let (PPair (PVar "y", PVar "z"), Pair (Num 3, Num 4), Let (PVar "z", Var "x", Plus (Var "x", Plus (Var "y", Var "z")))))) [];;
// val it: value = VNum 5
// > eval (Let (PPair (PUnderscore, PUnderscore), Num 1, Num 2)) [];;
// System.Exception: expected a pair, but given an int
// > eval (Let (PVar "x", Pair (Num 1, Pair (Num 2, Num 3)), Let (PPair (PUnderscore, PVar "y"), Var "x", Pair (Num 5, Var "y")))) [];;
// val it: value = VPair (VNum 5, VPair (VNum 2, VNum 3))


// Test cases for Problem 5

// > eval (nexprToExpr (NPlus (NVar "x", NNum 1))) ["x", VNum 100];;
// val it: value = VNum 101
// > eval (nexprToExpr (NPair (NPlus (NNum 1, NNum 2), NTimes (NNum 3, NNum 4)))) [];;
// val it: value = VPair (VNum 3, VNum 12)
// > eval (nexprToExpr (NPair (NPlus (NNum 1, NNum 2), NTimes (NNum 3, NNum 4)))) [];;
// val it: value = VPair (VNum 3, VNum 12)
// > eval (nexprToExpr (NFst (NPair (NNum 1, NNum 2)))) [];;
// val it: value = VNum 1
// > eval (nexprToExpr (NSnd (NPair (NNum 1, NNum 2)))) [];;
// val it: value = VNum 2
// > eval (nexprToExpr (NLet ("x", NPair (NNum 4, NNum 5), NTimes (NFst (NVar "x"), NSnd (NVar "x"))))) [];;
// val it: value = VNum 20
// > eval (nexprToExpr (NFst (NSnd (NFst (NVar "x"))))) ["x", VPair (VPair (VPair (VNum 1, VNum 2), VPair (VNum 3, VNum 4)), VNum 5)];;
// val it: value = VNum 3

// > neval (exprToNexpr (Let (PPair (PVar "x", PUnderscore), Pair (Num 3, Num 4), Let (PPair (PVar "x", PUnderscore), Pair (Num 1, Num 2), Var "x")))) [];;
// val it: value = VNum 1
// > neval (exprToNexpr (Let (PPair (PPair (PVar "x", PVar "y"), PPair (PVar "z", PVar "w")), Var "p", Plus (Var "x", Times (Var "z", Plus (Var "y", Var "w")))))) ["p", VPair (VPair (VNum 1, VNum 2), VPair (VNum 3, VNum 4))];;
// val it: value = VNum 19
// > neval (exprToNexpr (Pair (Let (PPair (PVar "x", PVar "y"), Var "p", Plus (Var "x", Var "y")), Var "p"))) ["p", VPair (VNum 10, VNum 11)];;
// val it: value = VPair (VNum 21, VPair (VNum 10, VNum 11))


// Test cases for Problem 6

// > reval (rcomp (NFst (NFst (NVar "x")))) [] ["x", [2;2;1;33;1;32;2;1;31;1;30]];;
// val it: int = 30
// > reval (rcomp (NSnd (NSnd (NVar "x")))) [] ["x", [2;2;1;33;1;32;2;1;31;1;30]];;
// val it: int = 33
// > reval (rcomp (NSnd (NFst (NVar "x")))) [] ["x", [2;2;1;33;1;32;2;1;31;1;30]];;
// val it: int = 31
// > reval (rcomp (NFst (NPair (NNum 1, NNum 2)))) [] [];;
// val it: int = 1
// > reval (rcomp (NSnd (NPair (NNum 1, NNum 2)))) [] [];;
// val it: int = 2
// > reval (rcomp (NFst (NFst (NSnd (NPair (NNum 1, NPair (NSnd (NVar "x"), NFst (NVar "x")))))))) [] ["x", [2;2;1;33;1;32;2;1;31;1;30]];;
// val it: int = 32


