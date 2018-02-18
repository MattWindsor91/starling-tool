/// <summary>
///     Test module for the pretty printer module.
/// </summary>
module Starling.Tests.Pretty

open NUnit.Framework
open Starling
open Starling.Tests.TestUtils
open Starling.Core.Var
open Starling.Lang.AST
open Starling.Lang.AST.Pretty

/// <summary>
///     Tests for <see cref="printExpression"/>.
/// </summary>
module ExpressionTests =
    let check expr str =
        Core.Pretty.printUnstyled (printExpression (freshNode expr)) ?=? str

    [<Test>]
    let ``expression '5' is printed correctly`` () =
        check (Num 5L) "5"

    [<Test>]
    let ``expression '(6 / bar)' is printed correctly`` () =
        check
            (BopExpr(Div, freshNode <| Num 6L, freshNode <| Identifier "bar"))
            "(6 / bar)"

    [<Test>]
    let ``expression '((1 + 2) * 3)' is printed correctly`` () =
        check
            (BopExpr
                (Mul,
                 freshNode <| BopExpr
                    (Add,
                     freshNode (Num 1L),
                     freshNode (Num 2L)),
                 freshNode (Num 3L)))
            "((1 + 2) * 3)"

/// <summary>
///     Tests for <see cref="printSolverExpression"/>.
/// </summary>
module SymbolicTests =
    let check sym str =
        Core.Pretty.printUnstyled (printSolverExpression sym) ?=? str

    [<Test>]
    let ``the empty symbol %{} is printed correctly`` () =
        check [] "%{}"

    [<Test>]
    let ``the symbol %{hello, world} is printed correctly`` () =
        check [ freshNode (SEString "hello, world") ] "%{hello, world}"

    [<Test>]
    let ``the split symbol %{hello, world} is printed correctly`` () =
        check
            [ freshNode (SEString "hello,")
              freshNode (SEString " ")
              freshNode (SEString "world") ]
            "%{hello, world}"

    [<Test>]
    let ``the symbol %{[|2|] + [|2|] = [|5|]} is printed correctly`` () =
        check
            [ freshNode (SEArg (freshNode (Num 2L)))
              freshNode (SEString " + ")
              freshNode (SEArg (freshNode (Num 2L)))
              freshNode (SEString " = ")
              freshNode (SEArg (freshNode (Num 5L))) ]
            "%{[|2|] + [|2|] = [|5|]}"
