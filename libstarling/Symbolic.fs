/// <summary>
///     Symbolic variables, and functions for dealing with them.
///
///     <para>
///         Symbolic variables (<c>Sym</c>) are how Starling encodes
///         arbitrary functions on zero or more variables that involve
///         syntax or concepts Starling can't internally handle.
///     </para>
///     <para>
///         They overload the variable position in expressions with a
///         disjunction between regular variables and uninterpreted,
///         arbitrary strings.  These strings are parameterised by
///         expression variables, as if they were method calls.
///         However, they represent a textual substitution of the
///         given variables into the string.
///     </para>
///     <para>
///         Starling proofs using symbolic variables cannot be proven
///         automatically.  Instead, the symbols must either be removed,
///         or replaced with some other Starling construct.  The typemap
///         <c>tryRemoveSym</c> tries to remove all <c>Sym</c>s from
///         expressions, failing if any exist.  The function
///         <c>approx</c> substitutes <c>true</c> and <c>false</c> for
///         symbols in Boolean positions, depending on whether they arise
///         in a positive or negative position.
///     </para>
/// </summary>
module Starling.Core.Symbolic

open Chessie.ErrorHandling
open Starling.Utils
open Starling.Core.TypeSystem
open Starling.Core.Expr
open Starling.Core.Var
open Starling.Core.Traversal

/// <summary>
///     Types for symbolic and variable maps.
/// </summary>
[<AutoOpen>]
module Types =
    /// <summary>
    ///     A fragment of a symbolic sentence.
    /// </summary>
    /// <typeparam name="Expr">The type of argument expressions.</typeparam>
    type SymbolicWord<'Expr> =
        /// <summary>
        ///     A string part of a symbolic sentence.
        /// </summary>
        | SymString of string
        /// <summary>
        ///     An argument part of a symbolic sentence.
        /// </summary>
        | SymArg of 'Expr

    /// <summary>
    ///     A symbolic, parameterised over arbitrary expressions.
    /// </summary>
    /// <typeparam name="Expr">The type of argument expressions.</typeparam>
    type Symbolic<'Expr> = SymbolicWord<'Expr> list

    /// <summary>
    ///     A variable reference that may be symbolic.
    ///
    ///     <para>
    ///         A symbolic variable is one whose value depends on an
    ///         uninterpreted function of multiple 'real' Starling variables.
    ///         It allows encoding into Starling of patterns of variable usage
    ///         Starling doesn't yet understand natively.
    ///     </para>
    /// </summary>
    /// <typeparam name="var">
    ///     The non-symbolic variable <c>Sym</c> wraps.
    /// </typeparam>
    type Sym<'var> when 'var : equality =
        /// <summary>
        ///     A symbolic variable, predicated over multiple expressions.
        ///     The symbol itself is the name inside the <c>Func</c>.
        /// </summary>
        | Sym of Symbolic<Expr<Sym<'var>>>
        /// <summary>
        ///     A regular, non-symbolic variable.
        | Reg of 'var


/// <summary>
///     Type synonyms for expressions over various forms of symbolic
///     variable.
/// </summary>
[<AutoOpen>]
module SymExprs =
    /// <summary>
    ///     An expression of arbitrary type using symbolic <c>Var</c>s.
    /// </summary>
    type SVExpr = Expr<Sym<Var>>
    /// <summary>
    ///     An expression of Boolean type using symbolic <c>Var</c>s.
    /// </summary>
    type SVBoolExpr = BoolExpr<Sym<Var>>
    /// <summary>
    ///     An expression of integral type using <c>Var</c>s.
    /// </summary>
    type SVIntExpr = IntExpr<Sym<Var>>

    /// <summary>
    ///     An expression of arbitrary type using symbolic <c>MarkedVar</c>s.
    /// </summary>
    type SMExpr = Expr<Sym<MarkedVar>>
    /// <summary>
    ///     An expression of Boolean type using symbolic <c>MarkedVar</c>s.
    /// </summary>
    type SMBoolExpr = BoolExpr<Sym<MarkedVar>>
    /// <summary>
    ///     An expression of integral type using symbolic <c>MarkedVar</c>s.
    /// </summary>
    type SMIntExpr = IntExpr<Sym<MarkedVar>>


/// <summary>
///     Utilities for creating symbolic variables.
/// </summary>
[<AutoOpen>]
module Create =
    /// Creates an integer sym-variable.
    let siVar c = c |> Reg |> IVar

    /// Creates an before-marked integer sym-variable.
    let siBefore c = c |> Before |> Reg |> IVar

    /// Creates an after-marked integer sym-variable.
    let siAfter c = c |> After |> Reg |> IVar

    /// Creates a goal-marked integer sym-variable.
    let siGoal i c = (i, c) |> Goal |> Reg |> IVar

    /// Creates an intermediate-marked integer sym-variable.
    let siInter i c = (i, c) |> Intermediate |> Reg |> IVar

    /// Creates a Boolean sym-variable.
    let sbVar c = c |> Reg |> BVar

    /// Creates an before-marked Boolean sym-variable.
    let sbBefore c = c |> Before |> Reg |> BVar

    /// Creates an before-marked Boolean sym-variable.
    let sbAfter c = c |> After |> Reg |> BVar

    /// Creates a goal-marked Boolean sym-variable.
    let sbGoal i c = (i, c) |> Goal |> Reg |> BVar

    /// Creates an intermediate-marked Boolean sym-variable.
    let sbInter i c = (i, c) |> Intermediate |> Reg |> BVar


/// <summary>
///     Utilities to traverse or eliminate symbolic variables.
/// </summary>
module Traversal =
    /// <summary>
    ///     Lifts a traversal over expressions to one over symbolic words.
    /// </summary>
    let rec tliftOverSymbolicWord
      (sub : Traversal<'SrcExpr, 'DstExpr, 'Error, 'Var>)
      : Traversal<SymbolicWord<'SrcExpr>,
                  SymbolicWord<'DstExpr>, 'Error, 'Var> =
        fun ctx ->
            function
            | SymString s -> ok (ctx, SymString s)
            | SymArg a -> tchain sub SymArg ctx a

    /// <summary>
    ///     Lifts a traversal from typed variables to symbolic expressions
    ///     such that it now takes typed symbolic variables as input.
    ///     <para>
    ///         This is needed because <see cref="tliftToExprSrc"/>
    ///         and other such traversals don't play well with symbolics.
    ///     </para>
    /// </summary>
    /// <param name="traversal">The <see cref="Traversal"/> to lift.</param>
    /// <typeparam name="SrcVar">Type of variables entering traversal.</param>
    /// <typeparam name="DstVar">Type of variables leaving traversal.</param>
    /// <typeparam name="Var">The type of variables inside the context.</typeparam>
    /// <returns>The lifted <see cref="Traversal"/>.</returns>
    let rec tliftToTypedSymVarSrc
      (traversal : Traversal<CTyped<'SrcVar>, Expr<Sym<'DstVar>>, 'Error, 'Var>)
      : Traversal<CTyped<Sym<'SrcVar>>, Expr<Sym<'DstVar>>, 'Error, 'Var> =
        let rec subInTypedSym ctx sym =
            match (valueOf sym) with
            | Reg r -> traversal ctx (withType (typeOf sym) r)
            | Sym s ->
                tchainL (tliftOverSymbolicWord sub)
                    (fun s' -> mkVarExp (withType (typeOf sym) (Sym s')))
                    ctx s
        and sub = tliftToExprSrc subInTypedSym
        subInTypedSym

    /// <summary>
    ///     Lifts a Traversal from variables to symbolic variables to accept
    ///     symbolic variables.
    /// </summary>
    and tliftToSymSrc
      (sub : Traversal<'SrcVar, Sym<'DstVar>, 'Error, 'Var>)
      : Traversal<Sym<'SrcVar>, Sym<'DstVar>, 'Error, 'Var> =
        fun ctx ->
            function
            | Reg r -> sub ctx r
            | Sym s ->
                let wsub =
                    tliftOverSymbolicWord
                        (tliftToExprSrc 
                            (tliftToTypedSymVarSrc
                                (tliftToExprDest (tliftOverCTyped sub))))
                tchainL wsub Sym ctx s

    /// <summary>
    ///     Lifts a Traversal from variables to variables to return
    ///     symbolic variables.
    /// </summary>
    let tliftToSymDest
      (sub : Traversal<'SrcVar, 'DstVar, 'Error, 'Var>)
      : Traversal<'SrcVar, Sym<'DstVar>, 'Error, 'Var> =
        fun ctx -> sub ctx >> lift (pairMap id Reg)

    /// <summary>
    ///     Lifts a Traversal from variables to variables to one from
    ///     symbolic variables to symbolic variables.
    /// </summary>
    let tliftOverSym
      (sub : Traversal<'SrcVar, 'DstVar, 'Error, 'Var>)
      : Traversal<Sym<'SrcVar>, Sym<'DstVar>, 'Error, 'Var> =
        sub |> tliftToSymDest |> tliftToSymSrc

    /// <summary>
    ///     A traversal for removing symbols from variables.
    /// </summary>
    /// <param name="err">
    ///     Function mapping a symbol's contents to an error to throw when
    ///     detecting one.
    /// </param>
    /// <typeparam name="Error">
    ///     The type of <paramref name="err"/>.
    /// </typeparam>
    /// <typeparam name="Var">
    ///     The type of regular (non-symbolic) variables.
    /// </typeparam>
    /// <typeparam name="VarB">
    ///     The type of variables inside the context.
    /// </typeparam>
    /// <returns>
    ///     A <see cref="Traversal"/> trying to remove symbols from
    ///     variables.
    /// </returns>
    let removeSymFromVar (err : Symbolic<Expr<Sym<'Var>>> -> 'Error)
      : Traversal<Sym<'Var>, 'Var, 'Error, 'VarB> =
        ignoreContext
            (function
             | Sym s -> s |> err |> Inner |> fail
             | Reg f -> ok f)

    /// <summary>
    ///     A traversal for removing symbols from expressions.
    /// </summary>
    /// <param name="err">
    ///     Function mapping a symbol's contents to an error to throw when
    ///     detecting one.
    /// </param>
    /// <typeparam name="Error">The type of <paramref name="err"/>.</typeparam>
    /// <typeparam name="Var">
    ///     The type of regular (non-symbolic) variables.
    /// </typeparam>
    /// <typeparam name="VarB">
    ///     The type of variables in the context.
    /// </typeparam>
    /// <returns>
    ///     A <see cref="Traversal"/> trying to remove symbols from
    ///     expressions.
    /// </returns>
    let removeSymFromExpr (err : Symbolic<Expr<Sym<'Var>>> -> 'Error)
      : Traversal<Expr<Sym<'Var>>, Expr<'Var>, 'Error, 'VarB> =
        (tliftOverExpr (tliftOverCTyped (removeSymFromVar err)))

    /// <summary>
    ///     A traversal for removing symbols from Boolean expressions.
    /// </summary>
    /// <param name="err">
    ///     Function mapping a symbol's contents to an error to throw when
    ///     detecting one.
    /// </param>
    /// <typeparam name="Error">The type of <paramref name="err"/>.</typeparam>
    /// <typeparam name="Var">
    ///     The type of regular (non-symbolic) variables.
    /// </typeparam>
    /// <typeparam name="VarB">
    ///     The type of variables in the context.
    /// </typeparam>
    /// <returns>
    ///     A <see cref="Traversal"/> trying to remove symbols from
    ///     Boolean expressions.
    /// </returns>
    let removeSymFromBoolExpr (err : Symbolic<Expr<Sym<'Var>>> -> 'Error)
      : Traversal<TypedBoolExpr<Sym<'Var>>, BoolExpr<'Var>, 'Error, 'VarB> =
        tliftToBoolSrc
            (tliftToExprDest
                (tliftOverCTyped (removeSymFromVar err)))

    /// <summary>
    ///     Traversal for converting symbolic expressions with a marker.
    /// </summary>
    /// <typeparam name="MVar">The type of marked variables.</typeparam>
    let traverseSymWithMarker
      (marker : Var -> 'MVar)
      : Traversal<Sym<Var>, Sym<'MVar>, 'Error, 'Var> =
        tliftOverSym (ignoreContext (marker >> ok))

    /// <summary>
    ///     Traversal for converting type-annotated symbolic variables with a
    ///     marker.
    /// </summary>
    /// <param name="marker">The marker to lift into a traversal.</param>
    /// <typeparam name="MVar">The type of marked variables.</typeparam>
    /// <returns>
    ///     The marker function <paramref name="marker"/>, lifted into a
    ///     <see cref="Traversal"/> over symbolic <see cref="Var"/>s annotated
    ///     using <see cref="CTyped"/>.
    /// </returns>
    let traverseTypedSymWithMarker
      (marker : Var -> 'MVar)
      : Traversal<CTyped<Sym<Var>>, CTyped<Sym<'MVar>>, 'Error, 'Var> =
        tliftOverCTyped (traverseSymWithMarker marker)

    /// <summary>
    ///     Traversal for converting symbolic expressions with a marker.
    /// </summary>
    /// <param name="marker">The marker to lift into a traversal.</param>
    /// <typeparam name="MVar">The type of marked variables.</typeparam>
    /// <returns>
    ///     The marker function <paramref name="marker"/>, lifted into a
    ///     <see cref="Traversal"/> over symbolic <see cref="Var"/>s annotated
    ///     using <see cref="CTyped"/>.
    /// </returns>
    let traverseSymExprWithMarker
      (marker : Var -> 'MVar)
      : Traversal<Expr<Sym<Var>>, Expr<Sym<'MVar>>, 'Error, 'Var> =
        tliftOverExpr (traverseTypedSymWithMarker marker)

    /// <summary>
    ///     Converts a symbolic Boolean to its pre-state.
    /// </summary>
    let beforeBool (expr : TypedBoolExpr<Sym<Var>>)
      : Result<BoolExpr<Sym<MarkedVar>>, TraversalError<'Error>> =
        mapTraversal
            (tliftToBoolSrc
                (tliftToExprDest 
                    (traverseTypedSymWithMarker Before))) expr

    /// <summary>
    ///     Converts a symbolic expression to its pre-state.
    /// </summary>
    let before (expr : Expr<Sym<Var>>)
      : Result<Expr<Sym<MarkedVar>>, TraversalError<'Error>> =
        mapTraversal (traverseSymExprWithMarker Before) expr

    /// <summary>
    ///     Converts a symbolic expression to its post-state.
    /// </summary>
    let after (expr : Expr<Sym<Var>>)
      : Result<Expr<Sym<MarkedVar>>, TraversalError<'Error>> =
        mapTraversal (traverseSymExprWithMarker After) expr

    /// <summary>
    ///     Replaces symbols in a Boolean position with their
    ///     under-approximation.
    /// </summary>
    let approx
      : Traversal<CTyped<Sym<MarkedVar>>, Expr<Sym<MarkedVar>>, unit, unit> =
        let rec sub ctx =
            // Only symbolic Booleans are handled specially.
            function
            | Bool (t, Sym x) ->
                match ctx with
                | Positions (position::_) ->
                    ok (ctx, Bool (t, Context.underapprox position))
                | c -> fail (ContextMismatch ("position context", c))
            (* Everything else just has approximation bubbled through
               in a type-generic way. *)
            | WithType (Sym s, t) ->
                tchainL (tliftOverSymbolicWord rmap) (Sym >> withType t >> mkVarExp) ctx s
            | WithType (Reg x, t) ->
                ok (ctx, mkVarExp (withType t (Reg x)))
        and sf = tliftToExprSrc sub
        and rmap ctx = sf (Context.push id ctx)

        sub

/// <summary>
///     Traversal for accumulating symbolic variables.
/// <summary>
let rec collectSymVars
  : Traversal<CTyped<Sym<'Var>>, CTyped<Sym<'Var>>, 'Error, 'Var> =
    // TODO(CaptainHayashi): de-duplicate this.
    fun ctx ->
        function
        | WithType (Reg v, tc) ->
            lift
                (fun ctx -> (ctx, withType tc (Reg v)))
                (pushVar ctx (withType tc v))
        | WithType (Sym s, tc) ->
            tchainL
                (Traversal.tliftOverSymbolicWord (tliftOverExpr collectSymVars))
                (Sym >> withType tc)
                ctx s

/// <summary>
///     Maps a Chessie function over all arguments in a symbol.
/// </summary>
/// <param name="f">The function to map.</param>
/// <param name="sym">The symbol to map over.</param>
/// <typeparam name="Src">The type of arguments before the map.</param>
/// <typeparam name="Dst">The type of arguments after the map.</param>
/// <typeparam name="Error">The type of Chessie errors.</param>
/// <returns>The resulting symbol, if all maps succeeded.</returns>
let tryMapSym (f : 'Src -> Result<'Dst, 'Error>) (sym : Symbolic<'Src>)
  : Result<Symbolic<'Dst>, 'Error> =
    collect
        (List.map
            (function
             | SymString s -> ok (SymString s)
             | SymArg a -> lift SymArg (f a))
            sym)

/// <summary>
///     Pretty printers for symbolics.
/// </summary>
module Pretty =
    open Starling.Core.Pretty
    open Starling.Core.Expr.Pretty
    open Starling.Core.Var.Pretty

    /// <summary>
    ///     Pretty-prints a symbolic sentence.
    /// </summary>
    /// <param name="pArg">Pretty-printer for arguments.</param>
    /// <param name="s">The symbolic sentence to print.</param>
    /// <returns>
    ///     The <see cref="Doc"/> resulting from printing <paramref name="s"/>.
    /// </returns>
    let printSymbolic (pArg : 'Arg -> Doc) (s : Symbolic<'Arg>) : Doc =
        let printSymbolicWord =
            function
            | SymString s -> String s
            | SymArg i -> ssurround "[|" "|]" (pArg i)

        hjoin (List.map printSymbolicWord s)

    /// <summary>
    ///     Pretty-prints a <c>Sym</c>, with interpolation.
    /// </summary>
    /// <param name="pReg">
    ///     Pretty printer to use for regular variables.
    /// </param>
    /// <param name="sym">The symbolic to print.</sym>
    /// <typeparam name="Reg">
    ///     The type of regular variables in expressions.
    /// </typeparam>
    /// <returns>
    ///     A <see cref="Doc"/> representing <paramref name="sym"/>.
    /// </returns>
    let rec printSym (pReg : 'Reg -> Doc) (sym : Sym<'Reg>) : Doc =
        match sym with
        | Reg r -> pReg r
        | Sym s -> 
            let pArg = printExpr (printSym pReg)
            parened (String "sym" <+> quoted (printSymbolic pArg s))

    /// Pretty-prints a SVExpr.
    let printSVExpr = printExpr (printSym String)
    /// Pretty-prints a SMExpr.
    let printSMExpr = printExpr (printSym printMarkedVar)
    /// Pretty-prints a SVBoolExpr.
    let printSVBoolExpr = printBoolExpr (printSym String)
    /// Pretty-prints a SMBoolExpr.
    let printSMBoolExpr = printBoolExpr (printSym printMarkedVar)

/// Strip the marked part of the annotation
/// and return just the internal 'var
let unmarkMarkedVar =
    function
    | Before s            -> s
    | After s             -> s
    | Goal(_, s)          -> s
    | Intermediate(_, s)  -> s

/// Takes a type annotated MarkedVar and strips away the Marked part of the Var
/// i.e. (Int (Before s)) => (Int s)
let unmark : CTyped<MarkedVar> -> TypedVar = mapCTyped unmarkMarkedVar

/// Returns the set of all variables annotated with their types
/// contained within the symbolic Expr.
let symExprVars (expr : Expr<Sym<'Var>>) : Result<Set<CTyped<'Var>>, TraversalError<'Error>> =
    findVars (tliftOverExpr collectSymVars) expr
