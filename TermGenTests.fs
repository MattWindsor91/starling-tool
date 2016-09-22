/// <summary>
///     Collections used in Starling.
/// </summary>
module Starling.Tests.TermGen

open Starling.TermGen

open Starling.Utils
open Starling.Utils.Testing
open Starling.Core.TypeSystem
open Starling.Core.Expr
open Starling.Core.Model
open Starling.Core.View
open Starling.Core.GuardedView
open Starling.Core.Symbolic
open Starling.Core.Var

/// <summary>
///     Tests for <c>TermGen</c>.
/// </summary>
module Tests =
    open NUnit.Framework

    module TestTermGen =
        module TestNormalise =

            [<Test>]
            let ``normalise converts 3 instances of A(x)[4] to A(x)[12]`` () =
                assertEqual
                    (iterated
                        (gfunc BTrue "A" [ Bool (sbBefore "x") ] )
                        (Some (AInt 12L : IntExpr<Sym<MarkedVar>>)))
                    (normalise
                        (iterated
                            (gfunc BTrue "A" [ Bool (sbBefore "x") ] )
                            (Some (AInt 4L : IntExpr<Sym<MarkedVar>>)))
                        3)


            [<Test>]
            let ``normalise converts 6 instances of A(x)[n] to A(x)[6*n]`` () =
                assertEqual
                    (iterated
                        (gfunc BTrue "A" [ Bool (sbBefore "x") ] )
                        (Some (AMul [ siBefore "n"; AInt 6L ])))
                    (normalise
                        (iterated
                            (gfunc BTrue "A" [ Bool (sbBefore "x") ] )
                            (Some (siBefore "n")))
                        6)

            [<Test>]
            let ``normalise converts 1 instances of A(x)[n] to A(x)[n]`` () =
                assertEqual
                    (iterated
                        (gfunc BTrue "A" [ Bool (sbBefore "x") ] )
                        (Some (siBefore "n")))
                    (normalise
                        (iterated
                            (gfunc BTrue "A" [ Bool (sbBefore "x") ] )
                            (Some (siBefore "n")))
                        1)


        module TestWPreCreation =
            open Starling.Collections

            [<Test>]
            let ``Subtracting different funcs yields the original func`` () =
                assertEqual
                    (Multiset.singleton <|
                        iterated
                            (smgfunc BTrue "foo" [ Expr.Bool (sbGoal 0I "bar") ])
                            (Some (AInt 1L)))
                    (termGenFrame
                        [ iterated
                            (smvfunc "foo" [ Expr.Bool (sbGoal 0I "bar") ])
                            None ]
                        (Multiset.singleton
                            (iterated
                                (smgfunc BTrue "blop" [ Expr.Bool (sbGoal 0I "bar") ])
                                (Some (AInt 11L)))))

            [<Test>]
            let ``Subtracting emp from a single func yields the original func`` () =
                assertEqual
                    (Multiset.singleton <|
                        iterated
                            (smgfunc BTrue "foo" [ Expr.Bool (sbGoal 0I "bar") ])
                            None)
                    (termGenFrame
                        [ iterated
                            (smvfunc "foo" [ Expr.Bool (sbGoal 0I "bar") ])
                            None ]
                        Multiset.empty)

            [<Test>]
            let ``Subtracting a single func from emp yields emp`` () =
                assertEqual
                    Multiset.empty
                    (termGenFrame
                        []
                        (Multiset.singleton
                            (iterated
                                (smgfunc BTrue "foo" [ Expr.Bool (sbGoal 0I "bar") ])
                                None)))

            [<Test>]
            let ``Subtracting emp from an iterated func yields the original func`` () =
                assertEqual
                    (Multiset.singleton <|
                        iterated
                            (smgfunc BTrue "foo" [ Expr.Bool (sbGoal 0I "bar") ])
                            (Some (AInt 10L)))
                    (termGenFrame
                        [ iterated
                            (smvfunc "foo" [ Expr.Bool (sbGoal 0I "bar") ])
                            (Some (AInt 10L)) ]
                        Multiset.empty)

            [<Test>]
            let ``Exact-subtracting an iterated func from itself cancels out`` () =
                assertEqual
                    Multiset.empty
                    (termGenFrame
                        [ iterated
                            (smvfunc "foo" [ Expr.Bool (sbGoal 0I "bar") ])
                            (Some (AInt 9L)) ]
                        (Multiset.singleton
                            (iterated
                                (smgfunc BTrue "foo" [ Expr.Bool (sbGoal 0I "bar") ])
                                (Some (AInt 9L)))))

            [<Test>]
            let ``Over-subtracting an iterated func from itself truncates`` () =
                assertEqual
                    Multiset.empty
                    (termGenFrame
                        [ iterated
                            (smvfunc "foo" [ Expr.Bool (sbGoal 0I "bar") ])
                            (Some (AInt 10L)) ]
                        (Multiset.singleton
                            (iterated
                                (smgfunc BTrue "foo" [ Expr.Bool (sbGoal 0I "bar") ])
                                (Some (AInt 11L)))))

            [<Test>]
            let ``Under-subtracting an iterated func from itself leaves a remainder`` () =
                assertEqual
                    (Multiset.singleton <|
                        iterated
                            (smgfunc BTrue "foo" [ Expr.Bool (sbGoal 0I "bar") ])
                            (Some (AInt 4L)))
                    (termGenFrame
                        [ iterated
                            (smvfunc "foo" [ Expr.Bool (sbGoal 0I "bar") ])
                            (Some (AInt 10L)) ]
                        (Multiset.singleton
                            (iterated
                                (smgfunc BTrue "foo" [ Expr.Bool (sbGoal 0I "bar") ])
                                (Some (AInt 6L)))))

            [<Test>]
            let ``Subtraction normalises the subtracted view correctly`` () =
                assertEqual
                    (Multiset.singleton <|
                        iterated
                            (smgfunc BTrue "foo" [ Expr.Bool (sbGoal 0I "bar") ])
                            (Some (AInt 2L)))
                    (termGenFrame
                        [ iterated
                            (smvfunc "foo" [ Expr.Bool (sbGoal 0I "bar") ])
                            (Some (AInt 5L))
                          iterated
                            (smvfunc "foo" [ Expr.Bool (sbGoal 0I "bar") ])
                            (Some (AInt 3L)) ]
                        (Multiset.singleton
                            (iterated
                                (smgfunc BTrue "foo" [ Expr.Bool (sbGoal 0I "bar") ])
                                (Some (AInt 6L)))))

            [<Test>]
            let ``Subtraction normalises the subtracting view correctly`` () =
                assertEqual
                    (Multiset.singleton <|
                        iterated
                            (smgfunc BTrue "foo" [ Expr.Bool (sbGoal 0I "bar") ])
                            (Some (AInt 2L)))
                    (termGenFrame
                        [ iterated
                            (smvfunc "foo" [ Expr.Bool (sbGoal 0I "bar") ])
                            (Some (AInt 10L)) ]
                        (Multiset.ofFlatList
                            [ iterated
                                (smgfunc BTrue "foo" [ Expr.Bool (sbGoal 0I "bar") ])
                                (Some (AInt 5L))
                              iterated
                                (smgfunc BTrue "foo" [ Expr.Bool (sbGoal 0I "bar") ])
                                (Some (AInt 3L)) ] ))


            [<Test>]
            let ``Subtracting a func from one with the same name generates an x!=y guard`` () =
                assertEqual
                    (Multiset.singleton
                        (iterated
                            (smgfunc
                               (BNot (bEq (sbGoal 0I "bar")
                                          (sbAfter "baz")))
                               "foo" [ Expr.Bool (sbGoal 0I "bar") ])
                            (Some (AInt 1L))))
                    (termGenFrame
                        [ iterated
                            (smvfunc "foo" [ Expr.Bool (sbGoal 0I "bar") ])
                            None ]
                        (Multiset.singleton
                            (iterated
                                (smgfunc BTrue "foo" [ Expr.Bool (sbAfter "baz") ])
                                None)))

            [<Test>]
            let ``General-case subtraction on one func produces the correct rewrite`` () =
                assertEqual
                    (Multiset.ofFlatList
                        [ (iterated
                            (smgfunc
                                (mkNot
                                    (mkAnd
                                        [ sbBefore "G"
                                          bEq (sbAfter "x") (sbAfter "y") ]))
                               "A" [ Expr.Bool (sbAfter "x") ])
                            (Some (siAfter "n")))
                          (iterated
                            (smgfunc
                                (BAnd
                                    [ sbBefore "G"
                                      bEq (sbAfter "x") (sbAfter "y")
                                      mkGt (siAfter "n") (siAfter "k") ])
                               "A" [ Expr.Bool (sbAfter "x") ])
                            (Some (mkSub2 (siAfter "n") (siAfter "k")))) ])
                    (termGenFrame
                        [ iterated
                            (smvfunc "A" [ Expr.Bool (sbAfter "x") ])
                            (Some (siAfter "n")) ]
                        (Multiset.singleton
                            (iterated
                                (smgfunc (sbBefore "G") "A" [ Expr.Bool (sbAfter "y") ])
                                (Some (siAfter "k")))))


    module TestIterFlatten =
        open Starling.TermGen.Iter

        module TestLowerGuards =
            let protos =
                FuncDefiner.ofSeq
                    [ (dfunc "f" [Bool "x"], { IsIterated = true; IsAnonymous = false })
                      (dfunc "g" [Bool "x"], { IsIterated = false; IsAnonymous = false }) ]

            [<Test>]
            let ``Drop iterated subview down to non-iterated subview`` ()=
                Assert.That(
                    okOption <|
                    lowerIteratedSubview
                        protos
                        { Cond = BTrue
                          Item =
                            [ iterated
                                (smvfunc "f" [Bool BTrue])
                                (Some (AInt 3L)) ] },
                    Is.EqualTo(
                        Some <|
                        { Cond = (BTrue : BoolExpr<Sym<MarkedVar>>)
                          Item = [ smvfunc "f" [Int (AInt 3L); Bool BTrue] ] }))

            [<Test>]
            let ``Drop iterated SMVFunc down to non-iterated SMVFunc`` ()=
                Assert.That(
                    okOption <|
                    lowerIterSMVFunc
                        protos
                        (iterated
                            (smvfunc "f" [Bool (sbAfter "x")])
                            (Some (mkMul2 (AInt 2L) (siBefore "n")))),
                    Is.EqualTo(
                        Some <|
                        [ smvfunc "f"
                            [ Int <| mkMul2 (AInt 2L) (siBefore "n")
                              Bool (sbAfter "x") ]]))

            [<Test>]
            let ``Drop non-iterated IteratedSMVFunc's down to non-iterated SMVFunc`` ()=
                Assert.That(
                    okOption <|
                    lowerIterSMVFunc
                        protos
                        (iterated
                            (smvfunc "g" [Bool (sbAfter "z")])
                            (Some (AInt 4L))),
                    Is.EqualTo(
                        Some <|
                        [ for x in 1..4 -> smvfunc "g" [ Bool (sbAfter "z") ]]))
(*
 *  old termgen test before iterated views were added
 *
    /// <summary>
    ///     NUnit tests for <c>TermGen</c>.
    /// </summary>
    type NUnit () =
        /// <summary>
        ///     Test cases for <c>termGenFrame</c>.
        /// </summary>

        /// <summary>
        ///     Tests <c>termGenFrame</c>.
        /// </summary>
        [<TestCaseSource("FrameSubtracts")>]
        member x.``termGenFrame performs multiset minus correctly`` r q =
            termGenFrame r q
*)
