﻿/// <summary>
///     Tests for <c>Starling.Lang.Desugar</c>.
/// </summary>
module Starling.Tests.Lang.Desugar

open NUnit.Framework
open Starling.Tests.TestUtils

open Starling.Collections

open Starling.Core.Var
open Starling.Core.View

open CViews.Ast
open CViews.AstNode
open Starling.Lang.Desugar

let normalCtx : BlockContext =
    let svars = [ (TInt, "serving"); (TInt, "ticket") ]
    let tvars = [ (TInt, "s"); (TInt, "t") ]
    let vprotos = Seq.empty
    { DCtx = initialContext svars tvars vprotos
      LocalRewrites = Map.empty }

let dupeCtx : BlockContext =
    let svars =
        [ (TInt, "__ok_0")
          (TBool, "__ok_1")
          (TInt, "serving")
          (TInt, "ticket") ]
    let tvars = [ (TInt, "s"); (TInt, "t") ]
    let vprotos = Seq.empty
    { DCtx = initialContext svars tvars vprotos
      LocalRewrites = Map.empty }

let rewritingCtx : BlockContext =
    { normalCtx with
        LocalRewrites =
            Map.ofList
                [ ("s", "0_32_s")
                  ("t", "10_9_t") ] }

/// <summary>
///     Tests for the block level of the desugaring layer.
///     <para>
///         This module will be quite sparse, because most desugars
///         can be tested more efficiently at lower levels.
///     </para>
/// </summary>
module DesugarBlock =
    let check
      (expectedCtx : BlockContext)
      (expectedBlock : FullBlock<ViewExpr<DesugaredGView>, FullCommand>)
      (ctx : BlockContext)
      (ast : Command list)
      : unit =
        let got = desugarBlock ctx ast
        assertEqual (expectedCtx, expectedBlock) got

    [<Test>]
    let ``desugaring the empty block generates an unknown view`` () : unit =
        let nfunc =
            NoIterator
                (Func =
                    func "__unknown_0"
                        [ { ParamName = "s"; ParamType = TInt }
                          { ParamName = "t"; ParamType = TInt } ]
                        UnknownSynth,
                 IsAnonymous = false)

        check
            { normalCtx with
                DCtx =
                { normalCtx.DCtx with
                    GeneratedProtos = Set.singleton nfunc } }
            { Pre =
                freshNode
                    (Advisory
                        [ (freshNode True,
                           func "__unknown_0"
                            [ freshNode (Identifier "s")
                              freshNode (Identifier "t") ]
                            UnknownSynth ) ] )
              Cmds = [] }
            normalCtx
            []

module DesugarMarkedView =
    let check
      (expectedCtx : BlockContext)
      (expectedView : ViewExpr<DesugaredGView>)
      (ctx : BlockContext)
      (ast : Marked<View>)
      : unit =
        let got = desugarMarkedView ctx ast
        assertEqual (expectedCtx, expectedView) got

    [<Test>]
    let ``desugaring an unknown view creates a fresh view`` () : unit =
        let nfunc =
            NoIterator
                (Func =
                    func "__unknown_0"
                        [ { ParamName = "s"; ParamType = TInt }
                          { ParamName = "t"; ParamType = TInt } ]
                        UnknownSynth,
                 IsAnonymous = false)

        check
            { normalCtx with
                DCtx =
                { normalCtx.DCtx with
                    GeneratedProtos = Set.singleton nfunc } }
            (Advisory
                [ (freshNode True,
                   func "__unknown_0"
                    [ freshNode (Identifier "s")
                      freshNode (Identifier "t") ]
                    UnknownSynth) ] )
            normalCtx
            Unknown


    [<Test>]
    let ``desugaring an unknown view ignores rewrites`` () : unit =
        let nfunc =
            NoIterator
                (Func =
                    func "__unknown_0"
                        [ { ParamName = "s"; ParamType = TInt }
                          { ParamName = "t"; ParamType = TInt } ]
                        UnknownSynth,
                 IsAnonymous = false)

        check
            { rewritingCtx with
                DCtx =
                { normalCtx.DCtx with
                    GeneratedProtos = Set.singleton nfunc } }
            (Advisory
                [ (freshNode True,
                   func "__unknown_0"
                    [ freshNode (Identifier "s")
                      freshNode (Identifier "t") ]
                    UnknownSynth) ] )
            rewritingCtx
            Unknown

/// <summary>Tests for the atomic command desugarer.</summary>
module DesugarAtomic =
    let check
      (expectedCtx : BlockContext)
      (expectedAtom : DesugaredAtomic)
      (ctx : BlockContext)
      (ast : Atomic)
      : unit =
        let got = desugarAtomic ctx ast
        assertEqual (expectedCtx, expectedAtom) got

    [<Test>]
    let ``Desugar assert into an assignment to the okay variable`` () =
        check
            { normalCtx with
                DCtx =
                    { normalCtx.DCtx with
                        OkayBool = Some "__ok_0"
                        SharedVars = (TBool, "__ok_0") :: normalCtx.DCtx.SharedVars } }
            (DAPrim
                (freshNode
                    (Fetch
                        (freshNode (Identifier "__ok_0"),
                         freshNode (Identifier "foobar"),
                         None))))
            normalCtx
            (freshNode
                (AAssert (freshNode (Identifier "foobar"))))

    [<Test>]
    let ``Desugar error into a false assignment to the okay variable`` () =
        check
            { normalCtx with
                DCtx =
                    { normalCtx.DCtx with
                        OkayBool = Some "__ok_0"
                        SharedVars = (TBool, "__ok_0") :: normalCtx.DCtx.SharedVars } }
            (DAPrim
                (freshNode
                    (Fetch
                        (freshNode (Identifier "__ok_0"),
                         freshNode False,
                         None))))
            normalCtx
            (freshNode AError)

    [<Test>]
    let ``Rewrite thread locals in asserts properly`` () =
        check
            { rewritingCtx with
                DCtx =
                    { rewritingCtx.DCtx with
                        OkayBool = Some "__ok_0"
                        SharedVars = (TBool, "__ok_0") :: rewritingCtx.DCtx.SharedVars } }
            (DAPrim
                (freshNode
                    (Fetch
                        (freshNode (Identifier "__ok_0"),
                        (freshNode
                            (BopExpr
                                (Eq,
                                 freshNode (Identifier "0_32_s"),
                                 freshNode (Identifier "10_9_t")))),
                         None))))
            rewritingCtx
            (freshNode
                (AAssert
                    (freshNode
                        (BopExpr
                            (Eq,
                             freshNode (Identifier "s"),
                             freshNode (Identifier "t"))))))

    [<Test>]
    let ``Desugar assert properly when normal okay is taken`` () =
        check
            { dupeCtx with
                DCtx =
                    { dupeCtx.DCtx with
                        OkayBool = Some "__ok_2"
                        SharedVars = (TBool, "__ok_2") :: dupeCtx.DCtx.SharedVars } }
            (DAPrim
                (freshNode
                    (Fetch
                        (freshNode (Identifier "__ok_2"),
                         freshNode (Identifier "foobar"),
                         None))))
            dupeCtx
            (freshNode
                (AAssert (freshNode (Identifier "foobar"))))

    [<Test>]
    let ``Desugar error properly when normal okay is taken`` () =
        check
            { dupeCtx with
                DCtx =
                { dupeCtx.DCtx with
                    OkayBool = Some "__ok_2"
                    SharedVars = (TBool, "__ok_2") :: dupeCtx.DCtx.SharedVars } }
            (DAPrim
                (freshNode
                    (Fetch
                        (freshNode (Identifier "__ok_2"),
                         freshNode False,
                         None))))
            dupeCtx
            (freshNode AError)


/// <summary>Tests for the single-view desugarer.</summary>
module DesugarView =
    let check
      (expectedCtx : BlockContext)
      (expectedView : DesugaredGView)
      (ctx : BlockContext)
      (ast : View)
      : unit =
        let got = desugarView ctx ast
        assertEqual (expectedCtx, expectedView) got

    [<Test>]
    let ``Desugar the empty view into an empty view`` () =
        check normalCtx [] normalCtx Unit

    [<Test>]
    let ``Desugar a local expression`` () =
        check
            { normalCtx with
                DCtx =
                { normalCtx.DCtx with
                    LocalLiftView = Some "__lift_0"
                    GeneratedProtos =
                        Set.ofList
                            [ (NoIterator
                                    (Func = func "__lift_0" [ { ParamName = "x"; ParamType = TBool } ] LocalSynth,
                                    IsAnonymous = false)) ] } }
            [ (freshNode True, func "__lift_0" [ freshNode (Identifier "bar") ] LocalSynth) ]
            normalCtx
            (Local (freshNode (Identifier "bar")))

    [<Test>]
    let ``Desugar a local expression with correct rewriting`` () =
        check
            { rewritingCtx with
                DCtx =
                { rewritingCtx.DCtx with
                    LocalLiftView = Some "__lift_0"
                    GeneratedProtos =
                        Set.ofList
                            [ (NoIterator
                                    (Func = func "__lift_0" [ { ParamName = "x"; ParamType = TBool } ] LocalSynth,
                                    IsAnonymous = false)) ] } }
            [ (freshNode True,
               func "__lift_0"
                   [ freshNode
                        (BopExpr
                            (Eq,
                             freshNode (Identifier "0_32_s"),
                             freshNode (Identifier "10_9_t"))) ]
                   LocalSynth) ]
            rewritingCtx
            (Local
                (freshNode
                    (BopExpr
                        (Eq,
                         freshNode (Identifier "s"),
                         freshNode (Identifier "t")))))


    [<Test>]
    let ``Desugar a falsehood`` () =
        check
            { normalCtx with
                DCtx =
                { normalCtx.DCtx with
                    LocalLiftView = Some "__lift_0"
                    GeneratedProtos =
                        Set.ofList
                            [ (NoIterator
                                    (Func = func "__lift_0" [ { ParamName = "x"; ParamType = TBool } ] LocalSynth,
                                    IsAnonymous = false)) ] } }
            [ (freshNode True, func "__lift_0" [ freshNode (False) ] LocalSynth) ]
            normalCtx
            Falsehood

    [<Test>]
    let ``Desugar a flat join`` () =
        check
            normalCtx
            [ (freshNode True, regFunc "foo" [ freshNode (Identifier "bar") ])
              (freshNode True, regFunc "bar" [ freshNode (Identifier "baz") ]) ]
            normalCtx
            (Join
                (Func (regFunc "foo" [ freshNode (Identifier "bar") ]),
                 Func (regFunc "bar" [ freshNode (Identifier "baz") ])))

    [<Test>]
    let ``Desugar a single conditional`` () =
        check
            normalCtx
            [ (freshNode (Identifier "s"),
               regFunc "foo" [ freshNode (Identifier "bar") ] )
              (freshNode (UopExpr (Neg, freshNode (Identifier "s"))),
               regFunc "bar" [ freshNode (Identifier "baz") ] ) ]
            normalCtx
            (View.If
                (freshNode (Identifier "s"),
                 Func (regFunc "foo" [ freshNode (Identifier "bar") ] ),
                 Some (Func (regFunc "bar" [ freshNode (Identifier "baz") ] ))))

    [<Test>]
    let ``Desugar a single conditional with no else`` () =
        check
            normalCtx
            [ (freshNode (Identifier "s"),
               regFunc "foo" [ freshNode (Identifier "bar") ] ) ]
            normalCtx
            (View.If
                (freshNode (Identifier "s"),
                 Func (regFunc "foo" [ freshNode (Identifier "bar") ] ),
                 None))

    [<Test>]
    let ``Desugars with rewritten conditionals behave properly`` () =
        check
            rewritingCtx
            [ (freshNode (Identifier "0_32_s"),
               regFunc "foo" [ freshNode (Identifier "bar") ] ) ]
            rewritingCtx
            (View.If
                (freshNode (Identifier "s"),
                 Func (regFunc "foo" [ freshNode (Identifier "bar") ] ),
                 None))

    [<Test>]
    let ``Convert a complex-nested CondView-list to a GuarView-list with complex guards`` () =
        check
            normalCtx
            [ (freshNode
                (BopExpr
                    (And,
                     freshNode (Identifier "s"),
                     freshNode (Identifier "t"))),
               regFunc "foo" [ freshNode (Identifier "bar") ] )
              (freshNode
                 (BopExpr
                     (And,
                      freshNode (Identifier "s"),
                      freshNode (Identifier "t"))),
               regFunc "bar" [ freshNode (Identifier "baz") ] )
              (freshNode
                 (BopExpr
                     (And,
                      freshNode (Identifier "s"),
                      freshNode
                         (UopExpr (Neg, (freshNode (Identifier "t")))))),
               regFunc "fizz" [ freshNode (Identifier "buzz") ] )
              (freshNode (Identifier "s"),
               regFunc "in" [ freshNode (Identifier "out") ] )
              (freshNode
                 (UopExpr (Neg, freshNode (Identifier "s"))),
               regFunc "ding" [ freshNode (Identifier "dong") ] ) ]
            normalCtx
            (View.If
                (freshNode (Identifier "s"),
                 Join
                    (View.If
                        (freshNode (Identifier "t"),
                         Join
                            (Func (regFunc "foo" [ freshNode (Identifier "bar") ] ),
                             Func (regFunc "bar" [ freshNode (Identifier "baz") ] )),
                         Some (Func (regFunc "fizz" [ freshNode (Identifier "buzz") ] ))),
                     Func (regFunc "in" [ freshNode (Identifier "out") ])),
                 Some (Func (regFunc "ding" [ freshNode (Identifier "dong") ]))))
