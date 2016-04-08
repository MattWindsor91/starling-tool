/// The part of the Z3 backend that translates terms to Z3.
module Starling.Backends.Z3

open Microsoft
open Chessie.ErrorHandling
open Starling
open Starling.Collections
open Starling.Core.Expr
open Starling.Core.Model
open Starling.Core.GuardedView
open Starling.Core.Instantiate
open Starling.Core.Sub
open Starling.Core.Z3
open Starling.Reifier
open Starling.Optimiser


/// <summary>
///     Types for the Z3 backend, including errors.
/// </summary>
[<AutoOpen>]
module Types =
    // A Z3-reified term.
    type ZTerm = Term<Z3.BoolExpr, Z3.BoolExpr, Z3.BoolExpr>

    /// Type of requests to the Z3 backend.
    type Request =
        /// Only translate the term views; return `Output.Translate`.
        | Translate
        /// Translate and combine term Z3 expressions; return `Output.Combine`.
        | Combine
        /// Translate, combine, and run term Z3 expressions; return `Output.Sat`.
        | Sat

    /// Type of responses from the Z3 backend.
    [<NoComparison>]
    type Response =
        /// Output of the term translation step only.
        | Translate of Model<ZTerm, DFunc>
        /// Output of the final Z3 terms only.
        | Combine of Model<Microsoft.Z3.BoolExpr, DFunc>
        /// Output of satisfiability reports for the Z3 terms.
        | Sat of Map<string, Microsoft.Z3.Status>

    /// A Z3 translation error.
    type Error =
        /// The translator was given an indefinite constraint.
        /// The Z3 backend cannot handle indefinite constraints.
        | IndefiniteConstraint of viewdef: DFunc
        /// Instantiation of a view failed.
        | InstantiationError of view: VFunc
                              * details: Starling.Core.Instantiate.Types.Error


/// <summary>
///     Pretty printers for the Z3 types.
/// </summary>
module Pretty =            
    open Starling.Core.Pretty
    open Starling.Core.Model.Pretty
    open Starling.Core.Instantiate.Pretty
    open Starling.Core.Z3.Pretty

    /// Pretty-prints a response.
    let printResponse mview =
        function
        | Response.Translate m ->
            printModelView
                mview
                (printTerm printZ3Exp printZ3Exp printZ3Exp)
                printDFunc
                m
        | Response.Combine m ->
            printModelView
                mview
                printZ3Exp
                printDFunc
                m
        | Response.Sat s -> printMap Inline String printSat s

    /// Pretty-prints Z3 translation errors.
    let printError =
        function
        | IndefiniteConstraint vd ->
            fmt "constraint of '{0}' is indefinite ('?'), and Z3 cannot use it"
                [ printDFunc vd ]
        | InstantiationError (vfunc, err) ->
            colonSep [ fmt "couldn't instantiate view '{0}'" [ printVFunc vfunc ]
                       printError err ]
    

/// <summary>
///     Functions for translating Starling elements into Z3.
/// </summary>
module Translator =
    open Starling.Core.Z3.Expr

    /// Produces the reification of an unguarded func.
    /// This corresponds to D^ in the theory.
    let interpretVFunc ft func =
        instantiate func ft
        |> lift (withDefault BTrue)  // Undefined views go to True by metatheory
        |> mapMessages (curry InstantiationError func)

    let interpretGFunc ft {Cond = c; Item = i} =
        interpretVFunc ft i
        |> lift (mkImplies c)

    /// Interprets an entire view application over the given functable.
    let interpretGView ft =
        Multiset.toSeq
        >> Seq.map (interpretGFunc ft)
        >> collect
        >> lift Seq.toList
        >> lift mkAnd

    /// Interprets all of the views in a term over the given functable.
    let interpretTerm ft : STerm<GView, VFunc> -> Result<FTerm, Error> =
        tryMapTerm ok (interpretGView ft) (interpretVFunc ft)

    /// <summary>
    ///   Tries to make a <c>FuncTable</c> from <c>model</c>'s view definitions.
    /// </summary>
    /// <parameter name="model">
    ///   The model whose <c>ViewDefs</c> are to be turned into a <c>FuncTable</c>.
    /// </parameter>
    /// <returns>
    ///   A Chessie result, which, when ok, contains a <c>FuncTable</c> mapping
    ///   each defining view in <c>model</c> to its <c>BoolExpr</c> meaning.
    /// </returns>
    /// <remarks>
    ///   <para>
    ///     This stage requires all views in <c>model.ViewDefs</c> to be definite,
    ///     and will fail if any are not.
    ///   </para>
    /// </remarks>
    let makeFuncTable model =
        model.ViewDefs
        |> Seq.map
            (function
             | { View = vs; Def = None } -> IndefiniteConstraint vs |> fail
             | { View = vs; Def = Some s } -> (vs, s) |> ok)
        |> collect
        |> lift Starling.Core.Instantiate.makeFuncTable

    /// <summary>
    ///   Interprets all views in a model, converting them to <c>FTerm</c>s.
    /// </summary>
    /// <parameter name="model">
    ///   The model whose views are to be interpreted.
    /// </parameter>
    /// <returns>
    ///   A Chessie result, which, when ok, contains a <c>Model</c> equivalent to
    ///   <c>model</c> except that each view is replaced with the <c>BoolExpr</c>
    ///   interpretation of it from <c>model</c>'s <c>ViewDefs</c>.
    /// </returns>
    /// <remarks>
    ///   <para>
    ///     This stage requires all views in <c>model.ViewDefs</c> to be definite,
    ///     and will fail if any are not.
    ///   </para>
    /// </remarks>
    let interpret model : Result<Model<FTerm, DFunc>, Error> =
        makeFuncTable model
        |> bind (fun ft -> tryMapAxioms (interpretTerm ft) model)

    /// Combines the components of a reified term.
    let combineTerm (ctx: Z3.Context) {Cmd = c; WPre = w; Goal = g} =
        (* This is effectively asking Z3 to refute (c ^ w => g).
         *
         * This arranges to:
         *   - ¬(c^w => g) premise
         *   - ¬(¬(c^w) v g) def. =>
         *   - ((c^w) ^ ¬g) deMorgan
         *   - (c^w^¬g) associativity.
         *)
        boolToZ3 ctx (mkAnd [c ; w; mkNot g] )

    /// Combines reified terms into a list of Z3 terms.
    let combineTerms ctx = mapAxioms (combineTerm ctx)


/// <summary>
///     Proof execution using Z3.
/// </summary>
module Run =
    /// Runs Z3 on a single term, given the context in `model`.
    let runTerm (ctx: Z3.Context) _ term =
        let solver = ctx.MkSimpleSolver()
        solver.Assert [| term |]
        solver.Check [||]

    /// Runs Z3 on a model.
    let run ctx = axioms >> Map.map (runTerm ctx)


/// Shorthand for the parser stage of the frontend pipeline.
let translate = Translator.interpret
/// Shorthand for the collation stage of the frontend pipeline.
let combine = Translator.combineTerms >> lift
/// Shorthand for the modelling stage of the frontend pipeline.
let sat = Run.run >> lift

/// Runs the Starling Z3 backend.
/// Takes two arguments: the first is the `Response` telling the backend what
/// to output; the second is the reified model to process with Z3.
let run resp =
    use ctx = new Z3.Context()
    match resp with
    | Request.Translate ->
        translate
        >> lift (mapAxioms (mapTerm (Expr.boolToZ3 ctx)
                                    (Expr.boolToZ3 ctx)
                                    (Expr.boolToZ3 ctx)))
        >> lift Response.Translate
    | Request.Combine -> translate >> combine ctx >> lift Response.Combine
    | Request.Sat -> translate >> combine ctx >> sat ctx >> lift Response.Sat
