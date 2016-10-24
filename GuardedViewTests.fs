/// <summary>
///     Tests for guarded views.
/// </summary>
///
module Starling.Tests.GuardedView

open Chessie.ErrorHandling

open Starling.Utils
open Starling.Collections

open Starling.Core.GuardedView
open Starling.Core.GuardedView.Traversal
open Starling.Core.TypeSystem
open Starling.Core.Expr
open Starling.Core.Var
open Starling.Core.Traversal
open Starling.Core.View
open Starling.Core.View.Traversal
open Starling.Core.Symbolic
open Starling.Core.Model

module Tests =
    open NUnit.Framework

    open Starling.Utils.Testing
    open Starling.Core.TypeSystem

    /// <summary>
    ///     Test traversal for position-based substitution.
    /// </summary>
    let positionTestSub : Traversal<Expr<Var>, Expr<Var>, unit> =
        let ptsVar ctx tv =
            let exp =
                match (typeOf tv) with
                | Int () ->
                    match ctx with
                    | Positions (Positive::xs) -> IInt 1L
                    | Positions (Negative::xs) -> IInt 0L
                    | _ -> IInt -1L
                    |> Int
                | Array (eltype, length, ()) ->
                    Array
                        (eltype,
                         length,
                         match ctx with
                         | Positions (Positive::xs) -> AVar "pos"
                         | Positions (Negative::xs) -> AVar "neg"
                         | _ -> AVar "?")
                | Bool () ->
                    match ctx with
                    | Positions (x::xs) -> Context.overapprox x
                    | _ -> BVar "?"
                    |> Bool
            ok (ctx, exp)
        tliftToExprSrc ptsVar


    /// <summary>
    ///     Case studies for testing Term traversal.
    /// </summary>
    module TermTraversal =
        open Starling.Utils.Testing
        open Starling.Core.Pretty
        open Starling.Core.Traversal.Pretty

        /// <summary>
        ///     Tests term traversal on positional substitutions.
        /// </summary>
        let check
          (expected : Term<BoolExpr<Var>, GView<Var>, Func<Expr<Var>>>)
          (pos : TraversalContext)
          (gv : Term<BoolExpr<Var>, GView<Var>, Func<Expr<Var>>>) : unit =
            let trav = tliftOverTerm positionTestSub
            let result = trav pos gv

            assertOkAndEqual (pos, expected) result
                (printTraversalError (fun _ -> String "?") >> printUnstyled)

        [<Test>]
        let ``successfully translate a positive Term`` () =
            check
                { Cmd = BAnd [ bEq BFalse BFalse; bEq BFalse (BNot BTrue) ]
                  WPre =
                    Multiset.ofFlatList
                        [ gfunc BTrue "bar"
                            [ Typed.Int (IInt 0L); Typed.Bool BFalse ]
                          gfunc (BGt (IInt 1L, IInt 1L)) "barbaz"
                            [ Typed.Int (IAdd [ IInt 0L; IInt 0L ]) ] ]
                  Goal =
                    (vfunc "bar" [ Typed.Int (IInt 1L); Typed.Bool BTrue ]) }
                Context.positive
                { Cmd =
                    BAnd
                        [ bEq (BVar "foo") (BVar "bar")
                          bEq (BVar "baz") (BNot (BVar "baz")) ]
                  WPre =
                    Multiset.ofFlatList
                        [ gfunc (BVar "foo") "bar"
                            [ Typed.Int (IVar "baz")
                              Typed.Bool (BVar "fizz") ]
                          gfunc (BGt (IVar "foobar", IVar "barbar")) "barbaz"
                            [ Typed.Int
                                (IAdd [ IVar "foobaz"; IVar "bazbaz" ]) ] ]
                  Goal =
                    vfunc "bar"
                        [ Typed.Int (IVar "baz")
                          Typed.Bool (BVar "barbaz") ] }


        [<Test>]
        let ``Successfully translate a negative DTerm`` () =
            check
                { Cmd = BAnd [ bEq BTrue BTrue; bEq BTrue (BNot BFalse) ]
                  WPre =
                    Multiset.ofFlatList
                        [ gfunc BFalse "bar"
                            [ Typed.Int (IInt 1L); Typed.Bool BTrue ]
                          gfunc (BGt (IInt 0L, IInt 0L)) "barbaz"
                            [ Typed.Int (IAdd [ IInt 1L; IInt 1L ]) ] ]
                  Goal =
                    vfunc "bar" [ Typed.Int (IInt 0L); Typed.Bool BFalse ] }
                Context.negative
                { Cmd =
                    BAnd
                        [ bEq (BVar "foo") (BVar "bar")
                          bEq (BVar "baz") (BNot (BVar "baz")) ]
                  WPre =
                    Multiset.ofFlatList
                        [ gfunc (BVar "foo") "bar"
                            [ Typed.Int (IVar "baz"); Typed.Bool (BVar "fizz") ]
                          gfunc (BGt (IVar "foobar", IVar "barbar")) "barbaz"
                            [ Typed.Int
                                (IAdd [ IVar "foobaz"; IVar "bazbaz" ]) ] ]
                  Goal =
                    vfunc "bar"
                       [ Typed.Int (IVar "baz"); Typed.Bool (BVar "barbaz") ] }

    /// <summary>
    ///     Case studies for testing GFunc traversal.
    /// </summary>
    module GFuncTraversal =
        open Starling.Utils.Testing
        open Starling.Core.Pretty
        open Starling.Core.Traversal.Pretty

        /// <summary>
        ///     Tests GFunc traversal on positional substitutions.
        /// </summary>
        let check
          (expected : GFunc<Var>)
          (pos : TraversalContext)
          (gv : GFunc<Var>) : unit =
            let trav = tliftOverGFunc positionTestSub
            let result = trav pos gv

            assertOkAndEqual (pos, expected) result
                (printTraversalError (fun _ -> String "?") >> printUnstyled)

        [<Test>]
        let ``GFunc substitution in +ve case works properly`` () =
            check
                (gfunc BFalse "bar" [ Typed.Int (IInt 1L); Typed.Bool BTrue ] )
                Context.positive
                (gfunc (BVar "foo") "bar"
                    [ Typed.Int (IVar "baz"); Typed.Bool (BVar "fizz") ] )

        [<Test>]
        let ``GFunc substitution in -ve case works properly`` () =
            check
                (gfunc BTrue "bar" [ Typed.Int (IInt 0L); Typed.Bool BFalse ] )
                Context.negative
                (gfunc (BVar "foo") "bar"
                    [ Typed.Int (IVar "baz"); Typed.Bool (BVar "fizz") ])


    /// <summary>
    ///     Case studies for testing GView traversal.
    /// </summary>
    module GViewTraversal =
        open Starling.Utils.Testing
        open Starling.Core.Pretty
        open Starling.Core.Traversal.Pretty

        /// <summary>
        ///     Tests GView traversal on positional substitutions.
        /// </summary>
        let check
          (expected : GView<Var>)
          (pos : TraversalContext)
          (gv : GView<Var>) : unit =
            let trav = tchainM (tliftOverGFunc positionTestSub) id
            let result = trav pos gv

            assertOkAndEqual (pos, expected) result
                (printTraversalError (fun _ -> String "?") >> printUnstyled)

        [<Test>]
        let ``+ve empty GView substitution is a no-op`` () =
            check Multiset.empty Context.positive Multiset.empty

        [<Test>]
        let ``-ve empty GView substitution is a no-op`` () =
            check Multiset.empty Context.negative Multiset.empty

        [<Test>]
        let ``Singleton GView substitution in +ve case works properly`` () =
            check
                (Multiset.singleton
                    (gfunc BFalse "bar"
                        [ Typed.Int (IInt 1L)
                          Typed.Bool BTrue ] ))
                Context.positive
                (Multiset.singleton
                    (gfunc (BVar "foo") "bar"
                        [ Typed.Int (IVar "baz")
                          Typed.Bool (BVar "fizz") ] ))

        [<Test>]
        let ``Singleton GView substitution in -ve case works properly`` () =
            check
                (Multiset.singleton
                    (gfunc BTrue "bar"
                        [ Typed.Int (IInt 0L); Typed.Bool BFalse ] ))
                Context.negative
                (Multiset.singleton
                    (gfunc (BVar "foo") "bar"
                        [ Typed.Int (IVar "baz")
                          Typed.Bool (BVar "fizz") ] ))

        [<Test>]
        let ``Multi GView substitution in +ve case works properly`` () =
            check
                (Multiset.ofFlatList
                    [ gfunc BFalse "bar"
                        [ Typed.Int (IInt 1L)
                          Typed.Bool BTrue ]
                      gfunc (BGt (IInt 0L, IInt 0L)) "barbaz"
                        [ Typed.Int
                              (IAdd [ IInt 1L; IInt 1L ]) ] ] )
                Context.positive
                (Multiset.ofFlatList
                    [ gfunc (BVar "foo") "bar"
                        [ Typed.Int (IVar "baz")
                          Typed.Bool (BVar "fizz") ]
                      gfunc (BGt (IVar "foobar", IVar "barbar")) "barbaz"
                        [ Typed.Int
                              (IAdd [ IVar "foobaz"; IVar "bazbaz" ]) ] ] )

        [<Test>]
        let ``Multi GView substitution in -ve case works properly`` () =
            check
                (Multiset.ofFlatList
                    [ gfunc BTrue "bar"
                        [ Typed.Int (IInt 0L); Typed.Bool BFalse ]
                      gfunc (BGt (IInt 1L, IInt 1L)) "barbaz"
                        [ Typed.Int (IAdd [ IInt 0L; IInt 0L ]) ] ] )
                Context.negative
                (Multiset.ofFlatList
                    [ gfunc (BVar "foo") "bar"
                        [ Typed.Int (IVar "baz"); Typed.Bool (BVar "fizz") ]
                      gfunc (BGt (IVar "foobar", IVar "barbar")) "barbaz"
                        [ Typed.Int
                              (IAdd [ IVar "foobaz"; IVar "bazbaz" ]) ] ] )