/// <summary>
///     Module for performing desugaring operations on a collated AST.
/// </summary>
module Starling.Lang.Desugar

open Starling.Collections
open Starling.Utils
open Starling.Core.View
open Starling.Core.Var
open Starling.Lang.AST
open Starling.Lang.Collator

/// <summary>
///     A partly modelled view prototype, whose parameters use Starling's type
///     system rather than the language's.
/// </summary>
type DesugaredViewProto = GeneralViewProto<TypedVar>

/// <summary>
///     A desugared view atom, ready for modelling.
/// </summary>
type DesugaredFunc = Func<AST.Types.Expression>

/// <summary>
///     A desugared guarded func, ready for modelling.
/// </summary>
type DesugaredGFunc = AST.Types.Expression * DesugaredFunc

/// <summary>
///     A desugared guarded view, ready for modelling.
/// </summary>
type DesugaredGView = DesugaredGFunc list


/// <summary>
///     The set of new objects generated by desugaring, which will need to be
///     generated in the modeller, as well as background information needed by
///     the desugaring operation.
/// </summary>
type DesugarContext =
    { /// <summary>The list of shared variables in the program.</summary>
      SharedVars : (TypeLiteral * string) list
      /// <summary>The list of thread variables in the program.</summary>
      ThreadVars : (TypeLiteral * string) list
      /// <summary>The name of the local lift view, if any.</summary>
      LocalLiftView : string option
      /// <summary>The list of fresh views generated.</summary>
      GeneratedProtos : Set<ViewProto>
      /// <summary>The list of views already present in the system.</summary>
      ExistingProtos : Set<ViewProto>
      /// <summary>The name of the 'ok' Boolean, if any.</summary>
      OkayBool : string option
    }

/// <summary>
///     A local desugaring context for blocks.
/// </summary>
type BlockContext =
    { /// <summary>
      ///     The general desugaring context used in this block.
      ///     This can be updated, and should be extracted at the end of the
      ///     block desugar.
      /// </summary>
      DCtx : DesugarContext
      /// <summary>
      ///     The set of variable renames generated by local variable lifting
      ///     on this block.
      /// </summary>
      LocalRewrites : Map<string, string> }

/// <summary>
///     An atomic command with errors and asserts desugared,
///     and missing branches inserted.
/// </summary>
type DesugaredAtomic =
    | DAPrim of Prim
    | DACond of
        cond : Expression
        * trueBranch : DesugaredAtomic list
        * falseBranch : DesugaredAtomic list

/// <summary>
///     A block whose missing views have been filled up.
/// </summary>
type FullBlock<'view, 'cmd> =
    { /// <summary> The precondition of the block.</summary>
      Pre : Node<'view>
      /// <summary>
      ///     The commands in the block, and their subsequent views.
      /// </summary>
      Cmds : ('cmd * Node<'view>) list }

/// <summary>A non-view command with FullBlocks.</summary>
type FullCommand' =
    /// <summary>
    ///     A miracle (atomically establishes its postcondition).
    /// </summary>
    | FMiracle
    /// A set of sequentially composed primitives.
    | FPrim of PrimSet<DesugaredAtomic>
    /// An if-then-else statement, with optional else.
    | FIf of ifCond : Expression option
          * thenBlock : FullBlock<ViewExpr<DesugaredGView>, FullCommand>
          * elseBlock : FullBlock<ViewExpr<DesugaredGView>, FullCommand> option
    /// A while loop.
    | FWhile of Expression * FullBlock<ViewExpr<DesugaredGView>, FullCommand>
    /// A do-while loop.
    | FDoWhile of FullBlock<ViewExpr<DesugaredGView>, FullCommand>
               * Expression // do { b } while (e)
    /// A list of parallel-composed blocks.
    | FBlocks of FullBlock<ViewExpr<DesugaredGView>, FullCommand> list
and FullCommand = Node<FullCommand'>

module Pretty =
    open Starling.Core.Pretty
    open Starling.Core.View.Pretty
    open Starling.Lang.AST.Pretty

    /// <summary>
    ///     Pretty-prints desugared atomic actions.
    /// </summary>
    /// <param name="a">The <see cref="DesugaredAtomic'"/> to print.</param>
    /// <returns>
    ///     A <see cref="Doc"/> representing <paramref name="a"/>.
    /// </returns>
    let rec printDesugaredAtomic (a : DesugaredAtomic) : Doc =
        match a with
        | DAPrim p -> printPrim p
        | DACond (cond = c; trueBranch = t; falseBranch = f) ->
            printITE printExpression printDesugaredAtomic c t (Some f)

    /// <summary>
    ///     Prints a <see cref="FullCommand'"/>.
    /// </summary>
    /// <param name="pView">Pretty-printer for views.</param>
    /// <param name="pCmd">Pretty-printer for commands.</param>
    /// <param name="fb">The <see cref="FullBlock'"/> to print.</param>
    /// <typeparam name="View">Type of views in the block.</typeparam>
    /// <typeparam name="Cmd">Type of commands in the block.</typeparam>
    /// <returns>
    ///     The <see cref="Doc"/> representing <paramref name="fc"/>.
    /// </returns>
    let printFullBlock (pView : 'View -> Doc) (pCmd : 'Cmd -> Doc)
      (fb : FullBlock<'View, 'Cmd>) : Doc =
        let printStep (c, v) = vsep [ Indent (pCmd c); pView v.Node ]
        let indocs = pView fb.Pre.Node :: List.map printStep fb.Cmds
        braced (ivsep indocs)

    /// <summary>
    ///     Prints a <see cref="DesugaredGView"/>.
    /// </summary>
    /// <param name="v">The view to print.</param>
    /// <returns>
    ///     The <see cref="Doc"/> representing <paramref name="v"/>.
    /// </returns>
    let printDesugaredGView (v : DesugaredGView) : Doc =
        let pv (g, b) =
            String "if"
            <+> parened (printExpression g)
            <+> braced (func b.Name (List.map printExpression b.Params))
        hsepStr " * " (List.map pv v)

    /// <summary>
    ///     Prints a <see cref="FullCommand'"/>.
    /// </summary>
    /// <param name="fc">The <see cref="FullCommand'"/> to print.</param>
    /// <returns>
    ///     The <see cref="Doc"/> representing <paramref name="fc"/>.
    /// </returns>
    let rec printFullCommand' (fc : FullCommand') : Doc =
        // TODO(CaptainHayashi): dedupe with PrintCommand'.
        match fc with
        | FMiracle -> syntaxStr "..."
        (* The trick here is to make Prim [] appear as ;, but
           Prim [x; y; z] appear as x; y; z;, and to do the same with
           atomic lists. *)
        | FPrim { PreLocals = ps; Atomics = ts; PostLocals = qs } ->
            seq { yield! Seq.map printPrim ps
                  yield (ts
                         |> Seq.map printDesugaredAtomic
                         |> semiSep |> withSemi |> braced |> angled)
                  yield! Seq.map printPrim qs }
            |> semiSep |> withSemi
        | FIf(c, t, fo) ->
            hsep [ "if" |> String |> syntax
                   c |> printCondition |> parened
                   t |> printFullBlock (printViewExpr printDesugaredGView) printFullCommand
                   (maybe Nop
                        (fun f ->
                            hsep
                                [ "else" |> String |> syntax
                                  printFullBlock (printViewExpr printDesugaredGView) printFullCommand f ])
                        fo) ]
        | FWhile(c, b) ->
            hsep [ "while" |> String |> syntax
                   parened (printExpression c)
                   b |> printFullBlock (printViewExpr printDesugaredGView) printFullCommand ]
        | FDoWhile(b, c) ->
            hsep [ "do" |> String |> syntax
                   printFullBlock (printViewExpr printDesugaredGView) printFullCommand b
                   "while" |> String |> syntax
                   parened (printExpression c) ]
            |> withSemi
        | FBlocks bs ->
            bs
            |> List.map (printFullBlock (printViewExpr printDesugaredGView) printFullCommand)
            |> hsepStr "||"
    /// <summary>
    ///     Prints a <see cref="FullCommand"/>.
    /// </summary>
    /// <param name="fc">The <see cref="FullCommand"/> to print.</param>
    /// <returns>
    ///     The <see cref="Doc"/> representing <paramref name="fc"/>.
    /// </returns>
    and printFullCommand (fc : FullCommand) : Doc = printFullCommand' fc.Node

let protoName (p : GeneralViewProto<'P>) : string =
    // TODO(MattWindsor91): doc comment.
    match p with
    | NoIterator (f, _) -> f.Name
    | WithIterator f -> f.Name


/// <summary>
///    Fresh object generators for desugaring.
/// </summary>
module private Generators =
    /// <summary>
    ///     Generates a fresh name from a source position and suffix.
    ///     <para>
    ///         This is constant time, and relies on the fact that users cannot
    ///         create names starting with numbers.  We assume that the position
    ///         and name given uniquely define the object being named.
    ///     </para>
    /// </summary>
    /// <param name="pos">The position to use when generating the name.</param>
    /// <param name="suffix">The suffix to add to the position.</param>
    /// <returns>
    ///    A name that should be unique, if the combination of position and
    ///    suffix is.
    /// </returns>
    let genNameFromPosition (pos : SourcePosition) (suffix : string) : string =
        sprintf "%d_%d_%s" pos.Line pos.Column suffix

    /// <summary>
    ///     Given a set of existing names and a prefix, generate a fresh name
    ///     not contained in that set.
    ///     <para>
    ///        This has worst-case time O(n), where n is the number of elements
    ///        in <paramref name="existing"/>.
    ///     </para>
    /// </summary>
    /// <param name="existing">The set of existing names.</param>
    /// <param name="prefix">The prefix to use when generating names.</param>
    /// <returns>
    ///     A name containing <paramref name="prefix"/> and not contained in
    ///     <paramref name="existing"/>.
    /// </returns>
    let genName (existing : Set<string>) (prefix : string) : string =
        (* Keep spinning a number up until we get to a fresh name.
           Inefficient, but simple. *)
        let rec tryGenName (k : bigint) : string =
            let name = sprintf "__%s_%A" prefix k
            if existing.Contains name then tryGenName (k + 1I) else name
        tryGenName 0I

    /// <summary>
    ///     Generates a fresh view with a given func type and parameter list.
    ///     Inserts that view into the given context.
    ///     The view's name is derived from its given func type.
    ///     <para>
    ///         The view is guaranteed to have a name that does not clash with
    ///         an generated or existing view.
    ///     </para>
    /// </summary>
    /// <param name="ftype">The type of func to generate.</param>
    /// <param name="pars">The parameters to use for the view prototype.</param>
    /// <param name="ctx">The <see cref="DesugarContext"/> to extend.</param>
    /// <returns>
    ///     A pair of the context updated with the new view, and its name.
    /// </returns>
    let genView (ftype : FuncType) (pars : Param list) (ctx : DesugarContext)
      : DesugarContext * string =
        let vnames =
            // Can't union-map because the proto types are different.
            Set.union
                (Set.map protoName ctx.ExistingProtos)
                (Set.map protoName ctx.GeneratedProtos)

        let newName = genName vnames (ftype.ToString ())
        let newProto =
            NoIterator
                ({ Name = newName; Params = pars; FuncType = ftype }, false)
        let ctx' = { ctx with GeneratedProtos = ctx.GeneratedProtos.Add newProto }
        (ctx', newName)

    /// <summary>
    ///     Generates the lifter view in a context, if it does not exist.
    /// </summary>
    /// <param name="ctx">The current desugaring context.</param>
    /// <returns>
    ///     <paramref name="ctx"/> updated to contain a lifter if it didn't
    ///     already, and the lifter's name.  The lifter always takes one
    ///     parameter, `bool x`.
    /// </returns>
    let genLifter (ctx : DesugarContext) : DesugarContext * string =
        match ctx.LocalLiftView with
        | Some n -> (ctx, n)
        | None ->
            (* We need to generate the view, then set the context to use it as
               the lifter.  The lifter has one parameter: the lifted Boolean. *)
            let ctxR, n =
                genView LocalSynth [ { ParamName = "x"; ParamType = TBool } ] ctx
            ({ ctxR with LocalLiftView = Some n }, n)

    /// <summary>
    ///     Generates the okay variable in a context, if it does not exist.
    ///     <para>
    ///         The okay variable is used when a program contains an assertion
    ///         or error command, and is used to represent the failure of the
    ///         program when an error occurs.
    ///     </para>
    /// </summary>
    /// <param name="ctx">The current desugaring context.</param>
    /// <returns>
    ///     <paramref name="ctx"/> updated to contain an okay variable if it
    ///     didn't already, and the variable's name.  The variable is always of
    ///     type `bool`.
    /// </returns>
    /// <remarks>
    ///     It is currently the modeller's responsibility to constrain on the
    ///     okay variable.  This function does add the okay variable to the
    ///     list of shared variables, though.
    /// </remarks>
    let genOkay (ctx : DesugarContext) : DesugarContext * string =
        match ctx.OkayBool with
        | Some n -> (ctx, n)
        | None ->
            let vars =
                Set.ofSeq
                    (Seq.map snd
                        (Seq.append ctx.SharedVars ctx.ThreadVars))
            let n = genName vars "ok"
            ({ ctx with
                OkayBool = Some n
                SharedVars = (TBool, n) :: ctx.SharedVars }, n)



/// <summary>
///     Rewrite references to method parameters and local variables into
///     references to lifted thread-local variables.
/// </summary>
module private LocalRewriting =
    open Starling.Collections
    open Starling.Core.Symbolic

    // TODO(CaptainHayashi): this is a royal mess...
    let rewriteVar (ctx : BlockContext) n = withDefault n (ctx.LocalRewrites.TryFind n)

    let rec rewriteSymbolic (ctx : BlockContext) s =
        List.map
            (function
             | SymArg a -> SymArg (rewriteExpression ctx a)
             | SymString t -> SymString t)
            s
    and rewriteExpression (ctx : BlockContext) expr =
        let rewriteExpression' =
            function
            | True -> True
            | False -> False
            | Num k -> Num k
            | Identifier n -> Identifier (rewriteVar ctx n)
            | Symbolic s -> Symbolic (rewriteSymbolic ctx s)
            | BopExpr (bop, l, r) -> BopExpr (bop, rewriteExpression ctx l, rewriteExpression ctx r)
            | UopExpr (uop, l) -> UopExpr (uop, rewriteExpression ctx l)
            | ArraySubscript (arr, sub) -> ArraySubscript (rewriteExpression ctx arr, rewriteExpression ctx sub)
        { expr with Node = rewriteExpression' expr.Node }


    let rewriteAFunc (ctx : BlockContext) (func : AFunc) : AFunc =
        Func.updateParams func (List.map (rewriteExpression ctx) func.Params) 
    let rec rewritePrim (ctx : BlockContext) prim =
        let rewritePrim' =
            function
            | CompareAndSwap (src, test, dest) ->
                CompareAndSwap (rewriteExpression ctx src, rewriteExpression ctx test, rewriteExpression ctx dest)
            | Fetch (l, r, fm) ->
                Fetch (rewriteExpression ctx l, rewriteExpression ctx r, fm)
            | Postfix (e, fm) ->
                Postfix (rewriteExpression ctx e, fm)
            | Assume e -> Assume (rewriteExpression ctx e)
            | SymCommand sym ->
                SymCommand (rewriteSymbolic ctx sym)
            | Havoc v -> Havoc (rewriteVar ctx v)
        { prim with Node = rewritePrim' prim.Node }

    /// <summary>
    ///     Converts method parameters to thread-local variables.
    /// </summary>
    /// <param name="ctx">The current desugaring context.</params>
    /// <param name="pars">The params to desugar.</param>
    /// <param name="pos">
    ///     The position of the method.
    ///     This is used to freshen the parameter names.
    /// </param>
    /// <returns>
    ///     A <see cref="BlockContext"/> containing an extension of
    ///     <paramref name="ctx"/> contain the thread-local variable
    ///     equivalent of <paramref name="pars"/>, as well as a substitution map
    ///     to use to rename accesses to the thread-local variable in the method
    ///     itself.
    /// </returns>
    let desugarMethodParams
      (ctx : DesugarContext) (pars : Param list) (pos : SourcePosition)
      : BlockContext =
        let desugarParam (tvs, tmap) par =
            let newName = Generators.genNameFromPosition pos par.ParamName
            ((par.ParamType, newName) :: tvs, Map.add par.ParamName newName tmap)

        let tvars, tsubs =
            List.fold desugarParam (ctx.ThreadVars, Map.empty) pars
        { DCtx = { ctx with ThreadVars = tvars }
          LocalRewrites = tsubs }


/// <summary>
///     Performs desugaring operations on a view, possibly creating new
///     view prototypes.
/// </summary>
/// <param name="ctx">The current desugaring context.</param>
/// <param name="view">The view to be converted.</param>
/// <returns>
///     A pair of the new context and desugared view.
/// </returns>
let desugarView
  (ctx : BlockContext)
  (view : AST.Types.View)
  : BlockContext * DesugaredGView =
    let rec desugarIn suffix c v =
        match v with
        | Unit -> (c, [])
        | Falsehood ->
            // Treat {| false |} as {| local { false } |} for simplicity.
            desugarIn suffix c (Local (freshNode False))
        | Local e ->
            (* Treat {| local { x } |} as {| lift(x) |} for simplicity.
               Generate lift if it doesn't exist. *)
            let (dc', liftName) = Generators.genLifter c.DCtx
            let c' = { c with DCtx = dc' }
            desugarIn suffix c'
                (Func { Name = liftName; Params = [e]; FuncType = LocalSynth })
        | Func v -> (c, [ (suffix, LocalRewriting.rewriteAFunc ctx v) ])
        | Join (x, y) -> desugarJoin c suffix x suffix y
        | View.If (i, t, eo) ->
            // Empty elses are equivalent to the unit.
            let e = withDefault Unit eo

            // Need to make sure we rewrite any local references in the
            // conditional.
            let ir = LocalRewriting.rewriteExpression c i

            let addSuff x =
                match suffix.Node with
                | True -> x
                | _ -> freshNode (BopExpr (And, suffix, x))

            // ITE is just a join with different suffixes.
            desugarJoin c
                (addSuff ir) t
                (addSuff (freshNode (UopExpr (Neg, ir)))) e
    and desugarJoin c suffx x suffy y =
        (* It doesn't really matter in which order we do these, as long as
            they get the right guard and thread the context through. *)
        let cx, xv = desugarIn suffx c  x
        let cy, yv = desugarIn suffy cx y

        (cy, List.append xv yv)

    // TODO(MattWindsor91): woefully inefficient?
    desugarIn (freshNode True) ctx view

/// <summary>
///     Converts a possibly-unknown marked view into one over known views,
///     desugaring the inner view if possible.
/// </summary>
/// <param name="ctx">The current desugaring context.</param>
/// <param name="marked">The view to be converted.</param>
/// <returns>
///     A pair of the desugared view and the view prototypes generated
///     inside it.
/// </returns>
let desugarMarkedView (ctx : BlockContext) (marked : Marked<View>)
  : BlockContext * ViewExpr<DesugaredGView> =
    match marked with
    | Unmarked v -> pairMap id Mandatory (desugarView ctx v)
    | Questioned v -> pairMap id Mandatory (desugarView ctx v)
    | Unknown ->
        (* We assume that the UnknownViewParams are named to correspond to
           thread-local variables. *)
        let tvars = ctx.DCtx.ThreadVars

        (* NOTE: We don't do any thread-local remapping here, because the
           parameters here directly correspond to the 'original' thread-local
           variables in the outer scope.  If a user has shadowed these, then
           that has no effect here, which is slightly counter-intuitive. *)
        let texprs = List.map (snd >> Identifier >> freshNode) tvars
        let tpars =
            List.map (fun (t, n) -> { ParamName = n; ParamType = t })
                tvars

        let dctx, vname = Generators.genView UnknownSynth tpars ctx.DCtx
        ({ ctx with DCtx = dctx },
         Advisory [ (freshNode True, func vname texprs UnknownSynth ) ])

/// <summary>
///     Desugars an atomic command.
/// </summary>
/// <param name="ctx">The block desugaring context.</param>
/// <param name="a">The <see cref="Atomic"/> to desugar.</param>
/// <returns>The resulting <see cref="DesugaredAtomic"/>.</returns>
let rec desugarAtomic (ctx : BlockContext) (a : Atomic)
  : BlockContext * DesugaredAtomic =
    match a.Node with
    | AAssert k ->
        (* assert(x) is lowered into 'ok = x'.
           Generate the variable 'ok' if it doesn't exist yet. *)
        let dctx, ok = Generators.genOkay ctx.DCtx

        let assignOk =
            freshNode
                (Fetch
                    (freshNode (Identifier ok),
                    LocalRewriting.rewriteExpression ctx k,
                    Direct))

        ({ ctx with DCtx = dctx }, DAPrim assignOk)
    | AError ->
        (* error is lowered into 'assert(false)' and then
           re-lowered. *)
        desugarAtomic ctx (freshNode (AAssert (freshNode False)))
    | APrim p ->
        // Just do normal primitive desugaring on an APrim.
        (ctx, DAPrim (LocalRewriting.rewritePrim ctx p))
    | ACond (cond, trueBranch, falseBranchO) ->
        (* Desugaring distributes over ACond.
           We desugar a missing false branch into an empty one. *)
        let falseBranch = withDefault [] falseBranchO

        let cond' = LocalRewriting.rewriteExpression ctx cond

        let ctxT, trueBranch' = mapAccumL desugarAtomic ctx trueBranch
        let ctx', falseBranch' = mapAccumL desugarAtomic ctxT falseBranch

        (ctx', DACond (cond', trueBranch', falseBranch'))


/// <summary>
///     Desugars a primitive set.
/// </summary>
/// <param name="ctx">The current block desugaring context.</param>
/// <param name="ps">The <see cref="PrimSet"/> to desugar.</param>
/// <returns>
///     The resulting block desugaring context, alongside the desugared
///     <see cref="PrimSet"/>.
/// </returns>
let desugarPrimSet (ctx : BlockContext) (ps : PrimSet<Atomic>)
  : BlockContext * PrimSet<DesugaredAtomic> =
    let ctx', ats = mapAccumL desugarAtomic ctx ps.Atomics

    (ctx',
     { PreLocals = List.map (LocalRewriting.rewritePrim ctx) ps.PreLocals
       Atomics = ats
       // TODO(MattWindsor91): Are the uses of ctx and ctx' here correct?
       PostLocals = List.map (LocalRewriting.rewritePrim ctx') ps.PostLocals } )

/// <summary>
///     Performs desugaring on a command.
/// </summary>
/// <param name="ctx">The current block desugaring context.</param>
/// <param name="cmd">The command whose views are to be converted.</param>
/// <returns>
///     A pair of the new block desugar context and optional 
///     desugared command (the desugar could have removed the command
///     entirely).
/// </returns>
let rec desugarCommand (ctx : BlockContext) (cmd : Command)
  : BlockContext * FullCommand option =
    let ctx', cmd' =
        match cmd.Node with
        | VarDecl { VarType = ty; VarNames = ns } ->
            let desugarVar (tvs, tmap) name =
                // TODO(MattWindsor91):
                // duplicate checking, or finer grained positioning.
                let newName = Generators.genNameFromPosition cmd.Position name
                ((ty, newName) :: tvs, Map.add name newName tmap)

            let tvars, tsubs =
                List.fold desugarVar (ctx.DCtx.ThreadVars, ctx.LocalRewrites)
                    ns
            let ctx' =
                { DCtx = { ctx.DCtx with ThreadVars = tvars }
                  LocalRewrites = tsubs }
            
            (ctx', None)
        | ViewExpr v -> failwith "should have been handled at block level"
        | Miracle -> (ctx, Some FMiracle)
        | If (e, t, fo) ->
            let (tc, t') = desugarBlock ctx t
            let (fc, f') =
                match fo with
                | None -> tc, None
                | Some f -> pairMap id Some (desugarBlock tc f)
            let ast = FIf (Option.map (LocalRewriting.rewriteExpression ctx) e, t', f')
            (fc, Some ast)
        | While (e, b) ->
            let (ctx', b') = desugarBlock ctx b
            (ctx', Some (FWhile (LocalRewriting.rewriteExpression ctx e, b')))
        | DoWhile (b, e) ->
            let (ctx', b') = desugarBlock ctx b
            (ctx', Some (FDoWhile (b', LocalRewriting.rewriteExpression ctx e)))
        | Blocks bs ->
            let (ctx', bs') = mapAccumL desugarBlock ctx bs
            (ctx', Some (FBlocks bs'))
        | Prim ps ->
            let ctx', ps' = desugarPrimSet ctx ps
            (ctx', Some (FPrim ps'))
    (ctx', Option.map (fun c -> cmd |=> c) cmd')

/// <summary>
///     Performs desugaring over a command block.
/// </summary>
/// <param name="ctx">The block desugaring context.</param>
/// <param name="tsubs">
///     A map of thread-local variable substitutions from the outer block, used
///     in conjunction with other to lift local variable declarations.
/// </param>
/// <param name="block">
///     The block whose views are to be converted.
/// </param>
/// <returns>
///     A pair of the desugared block and an updated <see cref="BlockContext"/>
///     containing newly generated views and constraints arising from the
///     desugar.  This context will not contain any internal updates to
///     the substitution tables.
/// </returns>
and desugarBlock (ctx : BlockContext) (block : Command list)
  : BlockContext * FullBlock<ViewExpr<DesugaredGView>, FullCommand> =
    (* Block desugaring happens in two stages.
       - First, we fill in every gap where a view should be, but isn't, with
         an unknown view.
       - Next, we desugar the resulting fully specified block. *)

    // Add an Unknown view to the start of a block without one.
    let cap l =
        match l with
        | ({ Node = ViewExpr v } as c) :: _ -> (l, c |=> v)
        | _ -> (freshNode (ViewExpr Unknown) :: l, freshNode Unknown)

    (* If the first item isn't a view, we have to synthesise a block
       precondition. *)
    let (blockP, pre) = cap block

    let skip () =
        freshNode (Prim { PreLocals = []; Atomics = []; PostLocals = [] })

    (* If the last item isn't a view, we have to synthesise a block
       postcondition.
       (TODO(CaptainHayashi): do this efficiently) *)
    let blockPQ = List.rev (fst (cap (List.rev blockP)))

    (* If there is only one item in the block, then by the above it must be
       a view, so we can skip processing commands. *)
    let cmds =
        match blockPQ with
        | [x] -> []
        | _ ->
        (* Next, we have to slide down the entire block pairwise.
           1. If we see ({| view |}, {| view |}), insert a skip between them.
           2. If we see (cmd, {| view |}), add it directly to the full block;
           3. If we see ({| view |}, cmd), ignore it.  Either the view is the
              precondition at the start, which is accounted for, or it was just
              added through rule 1. and can be ignored;
           4. If we see (vardecl, command), add (vardecl, None) to the full
              block. When we come to the next stage of desugaring, we'll resolve
              vardecl to None, and drop the entire viewed command.
           5. If we see (cmd, cmd), add (cmd, {| ? |}) to the full block.
              We'll add the next command on the next pass. *)
        let blockPairs = Seq.windowed 2 blockPQ

        let fillBlock bsf pair =
            match pair with
            | [| { Node = ViewExpr x } as nx; { Node = ViewExpr y } as ny |] -> (skip (), Some (nx |=> x)) :: bsf
            | [| cx                         ; { Node = ViewExpr y } as ny |] -> (cx, Some (ny |=> y)) :: bsf
            | [| { Node = ViewExpr x } as nx; _                           |] -> bsf
            | [| { Node = VarDecl  _ } as cx; _                           |] -> (cx, None) :: bsf
            | [| cx                         ; _                           |] -> (cx, Some (freshNode Unknown)) :: bsf
            | x -> failwith (sprintf "unexpected window in fillBlock: %A" x)

        // The above built the block backwards, so reverse it.
        List.rev (Seq.fold fillBlock [] blockPairs)

    (* Now we can desugar each view and command in the block contents.
       This is where variable rewriting etc. starts to happen. *)
    let desugarViewedCommand c (cmd, post) =
        let cc, cmd' = desugarCommand c cmd
        (* If the commmand was a vardecl, it will have been erased.
           The user may or may not have put a view in after it.
           If they have, we need to generate an id step; if not, we can
           eliminate the entire viewed command. *)
        match cmd', post with
        | None, None -> (cc, None)
        | Some c, Some p ->
            let cp, post' = desugarMarkedView cc p.Node
            (cp, Some (c, p |=> post'))
        | None, Some p ->
            // TODO(MattWindsor91): this is horrible.
            let cp, post' = desugarMarkedView cc p.Node
            let id = FPrim { PreLocals = []; Atomics = []; PostLocals = [] }
            (cp, Some (freshNode id, p |=> post'))
        | Some _, None -> failwith "expected a view for the end of this command"

    let pc, pre' = desugarMarkedView ctx pre.Node
    let ctx', cmds' = mapAccumL desugarViewedCommand pc cmds

    let block' = { Pre = pre |=> pre' ; Cmds = List.choose id cmds' }

    (* Throw away any changes to ctx that weren't to the global context.
       This is to make sure that any changes to substitution tables
       scoped within this block won't leak into external blocks. *)
    ({ ctx with DCtx = ctx'.DCtx }, block')

/// <summary>
///     Creates an initial desugaring context.
/// </summary>
/// <param name="tvars">The list of thread-local variables.</param>
/// <param name="vprotos">The sequence of existing view prototypes.</param>
/// <returns>An initial <see cref="DesugarContext"/>.</returns>
let initialContext
  (svars : (TypeLiteral * string) seq)
  (tvars : (TypeLiteral * string) seq)
  (vprotos : ViewProto seq)
  : DesugarContext =
    { SharedVars = List.ofSeq svars
      ThreadVars = List.ofSeq tvars
      LocalLiftView = None
      OkayBool = None
      GeneratedProtos = Set.empty
      ExistingProtos = Set.ofSeq vprotos }

/// <summary>
///     Converts a sequence of methods whose views can be unknown into
///     a sequence of methods over known views.
///
///     <para>
///         This effectively replaces every view <c>{| ? |}</c> with
///         a view <c>{| n(locals) |}</c>, where <c>n</c> is fresh,
///         and then adds <c>n</c> to the view prototypes considered by
///         the constraint searcher.
///     </para>
///     <para>
///         It also collapses <c>{| false |}</c>,
///         <c>{| locals {...} |}</c>, and <c>{| if v { ... } |}</c>
///         into guarded views.
///     </para>
/// </summary>
/// <param name="tvars">
///     The <c>VarMap</c> of thread-local variables.
/// </param>
/// <param name="methods">
///     The methods to convert, as a map from names to bodies.
/// </param>
/// <returns>
///     A pair of desugared methods and the <see cref="DesugarContext"/>
///     containing newly generated views and constraints arising from the
///     desugar.
/// </returns>
let desugar
  (collated : CollatedScript)
  : (DesugarContext *
     Map<string,
         Node<FullBlock<ViewExpr<DesugaredGView>, FullCommand>>>) =
    let ctx =
        initialContext collated.SharedVars collated.ThreadVars collated.VProtos

    let desugarMethod ctx mnode =
        let { Signature = sigt; Body = body } = mnode.Node
        let pos = mnode.Position
        let ctxP = LocalRewriting.desugarMethodParams ctx sigt.Params pos
        let ctxB, bodyB = desugarBlock ctxP body
        (ctxB.DCtx, (sigt.Name, mnode |=> bodyB))

    let ctxM, methodsM = mapAccumL desugarMethod ctx collated.Methods

    (ctxM, Map.ofSeq methodsM)