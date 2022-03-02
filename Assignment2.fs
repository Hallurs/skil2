// T-501-FMAL, Spring 2022, Assignment 2

(*
STUDENT NAMES HERE: ...
Hallur Hermannsson Aspar
Úlfur Ingólfsson 
*)


module Assignment2

type pattern =
    | PUnderscore
    | PVar of string
    | PPair of pattern * pattern

type expr =
    | Var of string
    | Let of pattern * expr * expr
    | Pair of expr * expr
    | Num of int
    | Plus of expr * expr
    | Times of expr * expr

let paren b s = if b then  "(" + s + ")" else s

let prettyprint (e : expr) : string =
    let rec prettyprintPattern (p : pattern) (acc : int) : string =
        match p with
        | PUnderscore -> "_"
        | PVar x -> x
        | PPair (p1, p2) -> paren (1 <= acc) (prettyprintPattern p1 1 + ", " + prettyprintPattern p2 1)
    let rec prettyprintExpr (e : expr) (acc : int) : string =
        match e with
        | Var x -> x
        | Let (p, erhs, ebody) ->
             paren (3 <= acc) ("let " + prettyprintPattern p 0 + " = " + prettyprintExpr erhs 2 + " in " + prettyprintExpr ebody 2)
        | Pair (e1, e2) ->
             paren (3 <= acc) (prettyprintExpr e1 3 + ", " + prettyprintExpr e2 3)
        | Num i -> string i
        | Plus (e1, e2) ->
             paren (4 <= acc) (prettyprintExpr e1 3 + " + " + prettyprintExpr e2 4)
        | Times (e1, e2) ->
             paren (7 <= acc) (prettyprintExpr e1 6 + " * " + prettyprintExpr e2 7)
    prettyprintExpr e 0

let rec varsInPattern (p : pattern) : string list =
    match p with
    | PUnderscore -> []
    | PVar x -> [x]
    | PPair (p1, p2) -> varsInPattern p1 @ varsInPattern p2

let freeVars (e : expr) : string list =
    let rec freeVars' e bound =
        match e with
        | Var x -> if List.exists (fun y -> x = y) bound then [] else [x]
        | Let (p, erhs, ebody) ->
            freeVars' erhs bound @ freeVars' ebody (varsInPattern p @ bound)
        | Pair (e1, e2) -> freeVars' e1 bound @ freeVars' e2 bound
        | Num _ -> []
        | Plus (e1, e2) -> freeVars' e1 bound @ freeVars' e2 bound
        | Times (e1, e2) -> freeVars' e1 bound @ freeVars' e2 bound
    freeVars' e []

let freshVar (root : string) (used : string list) : string =
    let rec freshVar' counter =
        let candidate = if counter = 0 then root else sprintf "%s%i" root counter
        if List.exists (fun x -> candidate = x) used
        then freshVar' (counter + 1)
        else candidate
    freshVar' 0


let rec checkPattern (p : pattern) : bool =
    let rec checkPattern' seen p =
        match p with
        | PUnderscore -> Some seen
        | PVar x -> if List.exists (fun y -> x = y) seen then None else Some (x :: seen)
        | PPair (p1, p2) ->
            match checkPattern' seen p1 with
            | None -> None
            | Some seen -> checkPattern' seen p2
    checkPattern' [] p <> None


////////////////////////////////////////////////////////////////////////
// Problem 1                                                          //
////////////////////////////////////////////////////////////////////////

// (Write the function checkAllPatterns.)

let rec checkAllPatterns (e : expr) : bool =
    match e with
    | Var x -> checkPattern (PVar (x))
    | Let (expression,e2,e3) ->
        checkPattern expression && checkAllPatterns e2 && checkAllPatterns e3
    | Num i -> true
    | Pair (p1,p2) ->
        checkAllPatterns p1 && checkAllPatterns p2
    | Plus (k1,k2) ->
        checkAllPatterns k1 && checkAllPatterns k2
    | Times (t1,t2) ->
        checkAllPatterns t1 && checkAllPatterns t2
    


checkAllPatterns (Let (PPair (PVar "x", PPair (PUnderscore, PVar "x")), Pair (Num 1, Pair (Num 2, Num 3)), Num 4));;
// val it: bool = false
checkAllPatterns (Let (PPair (PVar "x", PVar "y"), Pair (Num 1, Pair (Num 2, Num 3)), Let (PPair (PUnderscore, PVar "x"), Var "y", Num 4)));;
// val it: bool = true
checkAllPatterns (Let (PPair (PVar "x", PVar "y"), Pair (Num 1, Num 2), Let (PPair (PVar "z", PVar "z"), Pair (Num 3, Num 4), Num 5)));;
// val it: bool = false
checkAllPatterns (Pair (Var "x", Var "x"));;
// val it: bool = true
checkAllPatterns (Pair (Num 1, Let (PPair (PVar "x", PVar "x"), Num 2, Num 3)));;
// val it: bool = false
checkAllPatterns (Pair (Num 1, Let (PPair (PVar "x", PUnderscore), Num 2, Num 3)));;
// val it: bool = true

type token =
    | NAME of string
    | LET | EQUAL | IN
    | INT of int
    | PLUS | TIMES
    | LPAR | RPAR
    | COMMA | UNDERSCORE
    | ERROR of char


let string2Chars (s : string) : char list =
    let rec helper cs i =
        if i = 0 then cs else let i = i - 1 in helper (s.[i] :: cs) i
    helper [] (String.length s)
let isDigit c = '0' <= c && c <= '9'
let digit2Int (c : char) = int c - int '0'
let isLowercaseLetter c = 'a' <= c && c <= 'z'
let isUppercaseLetter c = 'A' <= c && c <= 'Z'
let isLetter c = isLowercaseLetter c || isUppercaseLetter c
let word2Token (s : string) : token =
    match s with
    | "let" -> LET
    | "in"  -> IN
    | _     -> NAME s



////////////////////////////////////////////////////////////////////////
// Problem 2                                                          //
////////////////////////////////////////////////////////////////////////

// (Modify the function tokenize to handle commas and underscores.) 

let rec tokenize (cs : char list) : token list =
    match cs with
    | [] -> []
    | '+'::cs  -> PLUS :: tokenize cs
    | '*'::cs  -> TIMES :: tokenize cs
    | '='::cs  -> EQUAL :: tokenize cs
    | ' '::cs  -> tokenize cs
    | '\t'::cs -> tokenize cs
    | '\n'::cs -> tokenize cs
    | '('::cs  -> LPAR :: tokenize cs
    | ')'::cs  -> RPAR :: tokenize cs
    | '_'::cs  -> UNDERSCORE :: tokenize cs
    | ','::cs  -> COMMA :: tokenize cs
    | c::cs when isDigit c -> tokenizeInt cs (digit2Int c)
    | c::cs when isLowercaseLetter c -> tokenizeWord cs (string c)
    | c::cs -> ERROR c :: tokenize cs
and tokenizeInt cs (acc : int) =
    match cs with
    | c::cs when isDigit c -> tokenizeInt cs (acc * 10 + digit2Int c)
    | _ -> INT acc :: tokenize cs
and tokenizeWord cs (acc : string) =
    match cs with
    | c::cs when isLetter c || isDigit c -> tokenizeWord cs (acc + string c)
    | _ -> word2Token acc :: tokenize cs

let lex s = tokenize (string2Chars s)

lex "let _ = x in 3";;
// val it: token list = [LET; UNDERSCORE; EQUAL; NAME "x"; IN; INT 3]
lex "let x, y = (1, 2) in 3";;
// val it: token list = [LET; NAME "x"; COMMA; NAME "y"; EQUAL; LPAR; INT 1; COMMA; INT 2; RPAR; IN; INT 3]
lex "let ((_, x), (z, w)) = p in x + z + w";;
// val it: token list = [LET; LPAR; LPAR; UNDERSCORE; COMMA; NAME "x"; RPAR; COMMA; LPAR; NAME "z"; COMMA; NAME "w"; RPAR; RPAR; EQUAL; NAME "p"; IN; NAME "x"; PLUS; NAME "z"; PLUS; NAME "w"]

////////////////////////////////////////////////////////////////////////
// Problem 3                                                          //
////////////////////////////////////////////////////////////////////////

// (Modify the functions parsePattern and parseSimplePattern
// to handle a prefix of the token list corresponding to a pair resp.
// the underscore.)

let rec parseExpr (ts : token list) : expr * token list =
    let e1, ts = parseSum ts
    match ts with
    | COMMA :: ts ->
        let e2, ts = parseSum ts
        Pair (e1, e2), ts
    | _ -> e1, ts
and parseSum (ts : token list) : expr * token list =
    let e1, ts = parseSummand ts
    match ts with
    | PLUS :: ts ->
        let e2, ts = parseSum ts
        Plus (e1, e2), ts
    | _ -> e1, ts
and parseSummand (ts : token list) : expr * token list =
    let e1, ts = parseFactor ts
    match ts with
    | TIMES :: ts ->
        let e2, ts = parseSummand ts
        Times (e1, e2), ts
    | _ -> e1, ts
and parseFactor (ts : token list) : expr * token list =
    match ts with
    | NAME x :: ts -> (Var x, ts)
    | LET :: ts ->
        let p, ts = parsePattern ts
        match ts with
        | EQUAL :: ts ->
            let (erhs, ts) = parseExpr ts
            match ts with
            | IN :: ts ->
                let ebody, ts = parseExpr ts
                Let (p, erhs, ebody), ts
            | _ -> failwith "let without in"
        | _ -> failwith "let without equals sign"
    | INT i :: ts -> Num i, ts
    | LPAR :: ts ->
        let e, ts = parseExpr ts
        match ts with
        | RPAR :: ts -> e, ts
        | _ -> failwith "left paren without right paren"
    | _  -> failwith "not a factor"
and parsePattern (ts : token list) : pattern * token list =
    let psx, ts = parseSimplePattern ts
    match ts with
    | COMMA :: ts -> 
        let psy, ts = parseSimplePattern ts
        PPair (psx, psy), ts
    | _ -> psx, ts
and parseSimplePattern (ts : token list) : pattern * token list =
    match ts with
    | UNDERSCORE :: ts -> PUnderscore, ts
    | NAME x :: ts -> PVar x, ts
    | LPAR :: ts ->
        let p, ts = parsePattern ts
        match ts with
        | RPAR :: ts -> p, ts
        | _ -> failwith "left paren without right paren"
    | _  -> failwith "not a pattern"


let parse (ts : token list) : expr =
    let e, ts = parseExpr ts
    if ts = [] then e else failwithf "unconsumed tokens"
let lexParse (s : string) : expr = parse (lex s)

parse [LET; UNDERSCORE; EQUAL; NAME "x"; IN; INT 3];;
// val it: expr = Let (PUnderscore, Var "x", Num 3)
parse [LET; NAME "x"; COMMA; NAME "y"; EQUAL; LPAR; INT 1; COMMA; INT 2; RPAR; IN; INT 3];;
// val it: expr = Let (PPair (PVar "x", PVar "y"), Pair (Num 1, Num 2), Num 3)
parse [LET; LPAR; LPAR; UNDERSCORE; COMMA; NAME "x"; RPAR; COMMA; LPAR; NAME "z"; COMMA; NAME "w"; RPAR; RPAR; EQUAL; NAME "p"; IN; NAME "x"; PLUS; NAME "z"; PLUS; NAME "w"];;
// val it: expr = Let (PPair (PPair (PUnderscore, PVar "x"), PPair (PVar "z", PVar "w")), Var "p", Plus (Var "x", Plus (Var "z", Var "w")))
parse [LET; NAME "x"; COMMA; NAME "y"; COMMA; NAME "z"; EQUAL; INT 1];;
// System.Exception: let without equals sign
parse [LET; LPAR; NAME "x"; COMMA; NAME "y"; RPAR; COMMA; NAME "z"; EQUAL; INT 1; IN; INT 2];;
// val it: expr = Let (PPair (PPair (PVar "x", PVar "y"), PVar "z"), Num 1, Num 2)
parse [LET; INT 1; EQUAL; INT 2; IN; INT 3];;
// System.Exception: not a pattern
parse [LET; LPAR; NAME "x"; COMMA; NAME "y"; PLUS; INT 1; RPAR; EQUAL; INT 5; IN; INT 6];;
// System.Exception: left paren without right paren
parse [LET; LPAR; LPAR; NAME "x"; COMMA; LPAR; LPAR; NAME "y"; RPAR; RPAR; RPAR; RPAR; EQUAL; NAME "p"; IN; NAME "q"; PLUS; NAME "r"];;
// val it: expr = Let (PPair (PVar "x", PVar "y"), Var "p", Plus (Var "q", Var "r"))





type value =
    | VPair of value * value
    | VNum of int
type envir = (string * value) list

let rec lookup x env =
    match env with
    | []          -> failwith (x + " not found")
    | (y, v)::env -> if x = y then v else lookup x env
let rec erase x env =
    match env with
    | []          -> failwith (x + " not found")
    | (y, v)::env -> if x = y then env else (y, v) :: erase x env

let addValues (v1 : value) (v2 : value) =
  match v1, v2 with
  | VNum i1 , VNum i2 -> VNum (i1 + i2)
  | _ -> failwith "can only add numbers"
let mulValues (v1 : value) (v2 : value) =
  match v1, v2 with
  | VNum i1 , VNum i2 -> VNum (i1 * i2)
  | _ -> failwith "can only multiply numbers"



////////////////////////////////////////////////////////////////////////
// Problem 4                                                          //
////////////////////////////////////////////////////////////////////////

// (Write the function patternMatch.)

let rec patternMatch (p : pattern) (v : value) (env : envir) : envir =
    match p,v with
    | PUnderscore, _ -> env
    | PPair(PUnderscore,PUnderscore), _ -> failwith "expected a pair, but given an int"
    | _ , VNum 0 -> failwith "expected a positive number but got a 0" 
    | PVar x, v -> (x, v)::env
    | PPair(ehrs, ebody), VPair(x,y) ->
        let xval = patternMatch ehrs x env
        let renv1 =   xval @ env
        patternMatch ebody y renv1
    
        


patternMatch PUnderscore (VNum 1) [];;
// val it: envir = []
patternMatch (PVar "x") (VNum 1) [];;
// val it: envir = [("x", VNum 1)]
patternMatch (PPair (PUnderscore, PUnderscore)) (VNum 1) [];;
// System.Exception: expected a pair, but given an int
patternMatch (PPair (PUnderscore, PVar "a")) (VPair (VNum 1, VPair (VNum 10, VNum 20))) [];;
// val it: envir = [("a", VPair (VNum 10, VNum 20))]
patternMatch (PPair (PUnderscore, PVar "a")) (VPair (VNum 1, VPair (VNum 10, VNum 20))) [("x", VNum 50); ("y", VPair (VNum 51, VNum 52))];;
// val it: envir = [("a", VPair (VNum 10, VNum 20)); ("x", VNum 50); ("y", VPair (VNum 51, VNum 52))]
patternMatch (PPair (PUnderscore, PPair (PVar "a", PVar "b"))) (VPair (VNum 1, VPair (VNum 10, VNum 20))) [];;
// val it: envir = [("b", VNum 20); ("a", VNum 10)]
patternMatch (PPair (PPair (PUnderscore, PUnderscore), PPair (PVar "a", PVar "b"))) (VPair (VNum 1, VPair (VNum 10, VNum 20))) [];;
// System.Exception: expected a pair, but given an int


// (Complete the function eval.)
let rec eval (e : expr) (env : envir) : value =
    match e with
    | Var x -> lookup x env
    | Let (p, erhs, ebody) -> 
        let banv = patternMatch p (eval erhs env) env
        let renv2 = banv @ env
        eval ebody renv2
    | Pair (e1, e2) -> VPair (eval e1 env, eval e2 env)
    | Num i -> VNum i
    | Plus (e1, e2) -> addValues (eval e1 env) (eval e2 env)
    | Times (e1, e2) -> mulValues (eval e1 env) (eval e2 env)

let run e = eval e []

eval (Let (PVar "x", Num 1, Plus (Var "x", Var "x"))) [];;
// val it: value = VNum 2
eval (Let (PVar "x", Num 1, Let (PVar "y", Num 2, Plus (Var "x", Var "y")))) [];;
// val it: value = VNum 3
eval (Let (PPair (PVar "x", PVar "y"), Pair (Num 1, Num 2), Let (PPair (PVar "z", PPair (PVar "w", PUnderscore)), Pair (Num 1, Pair (Var "x", Var "y")), Plus (Var "z", Var "w")))) [];;
// val it: value = VNum 2
eval (Let (PPair (PVar "x", PVar "y"), Pair (Num 1, Num 2), Let (PPair (PVar "y", PVar "z"), Pair (Num 3, Num 4), Let (PVar "z", Var "x", Plus (Var "x", Plus (Var "y", Var "z")))))) [];;
// val it: value = VNum 5
eval (Let (PPair (PUnderscore, PUnderscore), Num 1, Num 2)) [];;
// System.Exception: expected a pair, but given an int
eval (Let (PVar "x", Pair (Num 1, Pair (Num 2, Num 3)), Let (PPair (PUnderscore, PVar "y"), Var "x", Pair (Num 5, Var "y")))) [];;
// val it: value = VPair (VNum 5, VPair (VNum 2, VNum 3))


type nexpr =
    | NVar of string
    | NLet of string * nexpr * nexpr
    | NPair of nexpr * nexpr
    | NFst of nexpr
    | NSnd of nexpr
    | NNum of int
    | NPlus of nexpr * nexpr
    | NTimes of nexpr * nexpr

let nprettyprint (e : nexpr) : string =
    let rec nprettyprintExpr (e : nexpr) (acc : int) : string =
        match e with
        | NVar x -> x
        | NLet (x, erhs, ebody) ->
             paren (3 <= acc) ("let " + x + " = " + nprettyprintExpr erhs 2 + " in " + nprettyprintExpr ebody 2)
        | NPair (e1, e2) ->
             paren (3 <= acc) (nprettyprintExpr e1 3 + " , " + nprettyprintExpr e2 3)
        | NNum i -> string i
        | NPlus (e1, e2) ->
             paren (4 <= acc) (nprettyprintExpr e1 3 + " + " + nprettyprintExpr e2 4)
        | NTimes (e1, e2) ->
             paren (7 <= acc) (nprettyprintExpr e1 6 + " * " + nprettyprintExpr e2 7)
        | NFst e -> paren (8 <= acc) ("fst " + nprettyprintExpr e 8)
        | NSnd e -> paren (8 <= acc) ("snd " + nprettyprintExpr e 8)
    nprettyprintExpr e 0

let rec neval (e : nexpr) (env : envir) : value =
    match e with
    | NVar x -> lookup x env
    | NLet (x, erhs, ebody) ->
         let xval = neval erhs env
         let env1 = (x , xval) :: env
         neval ebody env1
    | NPair (e1, e2) -> VPair (neval e1 env, neval e2 env)
    | NFst e ->
        match neval e env with
        | VPair (v1, v2) -> v1
        | _ -> failwith "expected a pair"
    | NSnd e ->
        match neval e env with
        | VPair (v1, v2) -> v2
        | _ -> failwith "expected a pair"
    | NNum i -> VNum i
    | NPlus (e1, e2) -> addValues (neval e1 env) (neval e2 env)
    | NTimes (e1, e2) -> mulValues (neval e1 env) (neval e2 env)



////////////////////////////////////////////////////////////////////////
// Problem 5                                                          //
////////////////////////////////////////////////////////////////////////

// (i) (Write the function nexprToExpr.)

let rec nexprToExpr (e : nexpr) : expr = failwith "Not implemented"


// (ii) (Complete the function bindPattern used by exprToNexpr.)

let rec bindPattern (p : pattern) (rhs : nexpr) (body : nexpr) : nexpr =
  match p with
  | PUnderscore -> body
  | PVar x -> NLet (x, rhs, body)
  | PPair (p1, p2) -> failwith "Not implemented"

let rec exprToNexpr (e : expr) : nexpr =
  match e with
  | Var x -> NVar x
  | Let (p, erhs, ebody) ->
      let toMatch = freshVar "toMatch" (varsInPattern p @ freeVars ebody)
      let nrhs = exprToNexpr erhs
      let nbody = exprToNexpr ebody
      let boundBody = bindPattern p (NVar toMatch) nbody
      NLet (toMatch, nrhs, boundBody)
  | Pair (e1, e2) -> NPair (exprToNexpr e1, exprToNexpr e2)
  | Num i -> NNum i
  | Plus (e1, e2) -> NPlus (exprToNexpr e1, exprToNexpr e2)
  | Times (e1, e2) -> NTimes (exprToNexpr e1, exprToNexpr e2)




type renvir = (string * int list) list
type rinstr =
    | RLoad of string
    | RStore of string
    | RErase of string
    | RNum of int
    | RAdd
    | RMul
    | RPair
    | RUnpair
    | RPop
    | RSwap
type rcode = rinstr list
type stack = int list

let rec reval (inss : rcode) (stk : stack) (renv : renvir) : int =
    let rec popValue stk =
        match stk with
        | 1 :: i :: stk -> [1; i], stk
        | tag :: stk ->
            let (vs, stk) = popValues tag stk
            tag :: vs, stk
        | _ -> failwith "reval: Too few operands on stack"
    and popValues tag stk =
        if tag < 0 then failwith "reval: Negative tag"
        else if tag = 0 then ([], stk)
        else
            let (v, stk) = popValue stk
            let (vs, stk) = popValues (tag - 1) stk
            (v @ vs, stk)

    //printfn "%A" stk
    match inss, stk with
    | [], 1 :: i :: _ -> i
    | [], _ :: i :: _ -> failwith "reval: Result is not a number!"
    | [], [] -> failwith "reval: No result on stack!"
    | RLoad x :: inss, stk -> reval inss (lookup x renv @ stk) renv
    | RStore x :: inss, stk ->
        let v, stk = popValue stk
        reval inss stk ((x, v) :: renv)
    | RErase x :: inss, stk -> reval inss stk (erase x renv)
    | RNum i :: inss, stk -> reval inss (1 :: i :: stk) renv
    | RAdd :: inss, 1 :: i2 :: 1 :: i1 :: stk -> reval inss (1 :: (i1+i2) :: stk) renv
    | RAdd :: inss, _ :: _ :: _ :: _ :: stk -> failwith "reval: expected two numbers"
    | RMul :: inss, 1 :: i2 :: 1 :: i1 :: stk -> reval inss (1 :: (i1*i2) :: stk) renv
    | RMul :: inss, _ :: _ :: _ :: _ :: stk -> failwith "reval: expected two numbers"
    | RPair :: inss, stk -> reval inss (2 :: stk) renv
    | RUnpair :: inss, 2 :: stk -> reval inss stk renv
    | RUnpair :: inss, _ :: stk -> failwith "reval: expected a pair"
    | RPop :: inss, stk ->
        let _, stk = popValue stk
        reval inss stk renv
    | RSwap :: inss, stk ->
        let v1, stk = popValue stk
        let v2, stk = popValue stk
        reval inss (v2 @ v1 @ stk) renv
    | _ -> failwith "reval: too few operands on stack"



////////////////////////////////////////////////////////////////////////
// Problem 6                                                          //
////////////////////////////////////////////////////////////////////////

(*
ANSWER 6(i) HERE:
// [1;40;] = 40
// [2;1;43;2;1;42;1;41;] = ((41, 42), 43)
// [2;2;1;48;1;47;2;1;46;1;45] = ((45, 46), (47, 48))
// [1;40;2;1;43;2;1;42;1;41;2;2;1;48;1;47;2;1;46;1;45] = 40, ((41, 42), 43), ((45, 46), (47, 48))

    FIRST:  40
    SECOND: ((41, 42), 43)
    THIRD:  ((45, 46), (47, 48))

    ANSWER: 40, ((41, 42), 43), ((45, 46), (47, 48))
*)

(*
ANSWER 6(ii) HERE:
1) [1; 701]
2) [2; 1; 702; 1; 701]
3) [2; 2; 1; 702; 1; 701; 1; 700]
4) [2; 1; 703; 2; 2; 1; 702; 1; 701; 1; 700]
*)


// (iii) (Write the functions rcompPair, rcompFst, rcompSnd used by rcomp.)

let rcompPair (r1 : rcode) (r2 : rcode) : rcode =
    r1 @ r2 @ [RPair]
let rcompFst (r : rcode) : rcode =
    r @ [RUnpair] @ [RPop]
let rcompSnd (r : rcode) : rcode =
    r @ [RUnpair]

let rec rcomp (e : nexpr) : rcode =
    match e with
    | NVar x -> [RLoad x]
    | NLet (x, erhs, ebody) -> rcomp erhs @ [RStore x] @ rcomp ebody @ [RErase x]
    | NPair (e1, e2) -> rcompPair (rcomp e1) (rcomp e2)
    | NFst e -> rcompFst (rcomp e)
    | NSnd e -> rcompSnd (rcomp e)
    | NNum i -> [RNum i]
    | NPlus (e1, e2) -> rcomp e1 @ rcomp e2 @ [RAdd]
    | NTimes (e1, e2) -> rcomp e1 @ rcomp e2 @ [RMul]

reval (rcomp (NFst (NFst (NVar "x")))) [] ["x", [2;2;1;33;1;32;2;1;31;1;30]];;
// val it: int = 30
reval (rcomp (NSnd (NSnd (NVar "x")))) [] ["x", [2;2;1;33;1;32;2;1;31;1;30]];;
// val it: int = 33
reval (rcomp (NSnd (NFst (NVar "x")))) [] ["x", [2;2;1;33;1;32;2;1;31;1;30]];;
// val it: int = 31
reval (rcomp (NFst (NPair (NNum 1, NNum 2)))) [] [];;
// val it: int = 1
reval (rcomp (NSnd (NPair (NNum 1, NNum 2)))) [] [];;
// val it: int = 2
reval (rcomp (NFst (NFst (NSnd (NPair (NNum 1, NPair (NSnd (NVar "x"), NFst (NVar "x")))))))) [] ["x", [2;2;1;33;1;32;2;1;31;1;30]];;
// val it: int = 32

// (ii) Functions for the answers to 6ii
reval (rcomp (NNum 701)) [] [];;
// [1; 701]
reval (rcomp (NPair (NNum 701, NNum 702))) [] [];;
// [2; 1; 702; 1; 701]
reval (rcomp (NPair((NNum 700, NPair (NNum 701, NNum 702))))) [] [];;
// [2; 2; 1; 702; 1; 701; 1; 700]
reval (rcomp (NPair(NPair((NNum 700, NPair (NNum 701, NNum 702))), NNum 703))) [] [];;
// [2; 1; 703; 2; 2; 1; 702; 1; 701; 1; 700]

//  (i) Functions for the answer to 6i
reval (rcomp (NNum 40)) [] [];;
// [1; 40]
reval (rcomp (NPair(NPair(NNum 41, NNum 42), NNum 43))) [] [];;
// [2; 1; 43; 2; 1; 42; 1; 41]
reval (rcomp (NPair(NPair(NNum 45, NNum 46), NPair(NNum 47, NNum 48)))) [] [];;
// [2; 2; 1; 48; 1; 47; 2; 1; 46; 1; 45]

//  all 3 values together
//  [1; 40; 2; 1; 43; 2; 1; 42; 1; 41; 2; 2; 1; 48; 1; 47; 2; 1; 46; 1; 45]

