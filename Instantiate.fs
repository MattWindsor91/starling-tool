/// <summary>
///     Func instantiation.
///
///     <para>
///         Starling has multiple stages during which we need to look up a
///         func in a list mapping funcs to Boolean expressions, and
///         substitute func's arguments for the parameters in that Boolean
///         expression.
///     </para>
///     <para>
///         This is the resposibility of <c>Starling.Core.Instantiate</c>,
///         which contains the function <c>instantiate</c> for this
///         purpose.
///      </para>
///     <para>
///         In addition, it contains more generic functions for looking
///         up funcs in tables, which are useful throughout Starling.
///     </para>
/// </summary>
module Starling.Core.Instantiate

open Chessie.ErrorHandling
open Starling.Collections
open Starling.Utils
open Starling.Core.Expr
open Starling.Core.Model
open Starling.Core.Var
open Starling.Core.Sub


/// <summary>
///     Types used in func instantiation.
/// </summary>
[<AutoOpen>]
module Types =
    /// <summary>
    ///     Type of func instantiation tables.
    /// </summary>
    /// <typeparam name="defn">
    ///     Type of definitions of <c>Func</c>s stored in the table.
    ///     May be <c>unit</c>.
    /// </typeparam>
    type FuncTable<'defn> =
        // TODO(CaptainHayashi): this should probably be a map,
        // but translating it to one seems non-trivial.
        // Would need to define equality on funcs very loosely.
        (DFunc * 'defn) list

    /// <summary>
    ///     Type of Chessie errors arising from Instantiate.
    /// </summary>
    type Error =
        /// <summary>
        ///     The func looked up has a parameter <c>param</c>, which
        ///     has been assigned to an argument of the incorrect type
        ///     <c>atype</c>.
        /// </summary>
        | TypeMismatch of param: (Type * string) * atype: Type
        /// <summary>
        ///     The func looked up has <c>fn</c> arguments, but its
        ///     definition has <c>dn</c> parameters.
        /// </summary>
        | CountMismatch of fn: int * dn: int


/// <summary>
///     Pretty printers used in func instantiation.
/// </summary>
module Pretty =
    open Starling.Core.Pretty
    open Starling.Core.Model.Pretty
    open Starling.Core.Var.Pretty

    /// Pretty-prints instantiation errors.
    let printError =
        function
        | TypeMismatch (par, atype) ->
            fmt "parameter '{0}' conflicts with argument of type '{1}'"
                [ printParam par; printType atype ]
        | CountMismatch (fn, dn) ->
            fmt "view usage has {0} parameter(s), but its definition has {1}"
                [ fn |> sprintf "%d" |> String; dn |> sprintf "%d" |> String ]


(*
 * Building FuncTables
 *)

/// <summary>
///     Builds a <c>FuncTable</c> from a sequence of pairs of <c>Func</c>
///     and definition.
/// </summary>
/// <param name="fseq">
///     The sequence of (<c>Func</c>, <c>'defn</c>) pairs.
/// </param>
/// <typeparam name="defn">
///     The type of <c>Func</c> definitions.  May be <c>unit</c>.
/// </typeparam>
/// <returns>
///     A <c>FuncTable</c> allowing the <c>'defn</c>s of the given <c>Func</c>s
///     to be looked up.
/// </returns>
let makeFuncTable fseq : FuncTable<'defn> =
    // This function exists to smooth over any changes in FuncTable
    // representation we make later (eg. to maps).
    Seq.toList fseq

/// <summary>
///     Returns the <c>Func</c>s contained in a <c>FuncTable</c>.
/// </summary>
/// <param name="ftab">
///     The <c>FuncTable</c> to break apart.
/// </param>
/// <typeparam name="defn">
///     The type of <c>Func</c> definitions.  May be <c>unit</c>.
/// </typeparam>
/// <returns>
///     A sequence of <c>Func</c>s contained in <paramref name="ftab" />.
/// </returns>
let funcsInTable (ftab : FuncTable<'defn>) =
    Seq.map fst ftab


(*
 * Func lookup
 *)

/// <summary>
///     Checks whether <c>func</c> and <c>_arg1</c> agree on parameter
///     count.
/// </summary>
/// <parameter name="func">
///     The func being looked up, the process of which this check is part.
/// </parameter>
/// <parameter name="_arg1">
///     An <c>Option</c>al pair of <c>DFunc</c> and its defining
///     <c>BoolExpr</c>.
///     The value <c>None</c> suggests that <c>func</c> has no definition,
///     which can be ok (eg. if the <c>func</c> is a non-defining view).
/// </parameter>
/// <returns>
///     A Chessie result, where the <c>ok</c> value is the optional pair of
///     prototype func and definition, and the failure value is a
///     <c>Starling.Instantiate.Error</c>.
/// </returns>
let checkParamCount func =
    function
    | None -> ok None
    | Some def ->
        let fn = List.length func.Params
        let dn = List.length (fst def).Params
        if fn = dn then ok (Some def) else CountMismatch (fn, dn) |> fail

/// <summary>
///     Look up <c>func</c> in <c>_arg1</c>.
///
///     <param>
///         This checks that the use of <c>func</c> agrees on the number of
///         parameters, but not necessarily types.  You will need to add
///         type checking if needed.
///     </param>
/// </summary>
/// <parameter name="func">
///     The func to look up in <c>_arg1</c>.
/// </parameter>
/// <parameter name="_arg1">
///     An associative sequence mapping <c>Func</c>s to some definition.
/// </parameter>
/// <returns>
///     A Chessie result, where the <c>ok</c> value is an <c>Option</c>
///     containing the pair of
///     prototype func and definition, and the failure value is a
///     <c>Starling.Instantiate.Error</c>.  If the <c>ok</c> value is
///     <c>None</c>, it means no (valid or otherwise) definition exists.
/// </returns>
let lookup func =
    // First, try to find a func whose name agrees with ours.
    Seq.tryFind (fun (dfunc, _) -> dfunc.Name = func.Name)
    >> checkParamCount func

/// <summary>
///     Checks whether <c>func</c> and <c>_arg1</c> agree on parameter
///     types.
/// </summary>
/// <parameter name="func">
///     The func being looked up, the process of which this check is part.
/// </parameter>
/// <parameter name="def">
///     The <c>DFunc</c> that <paramref name="func" /> has matched.
/// </parameter>
/// <returns>
///     A Chessie result, where the <c>ok</c> value is
///     <paramref name="func" />, and the failure value is a
///     <c>Starling.Instantiate.Error</c>.
/// </returns>
let checkParamTypes func def =
    List.map2
        (curry
             (function
              | (AExpr _, ((Bool, _) as param)) -> TypeMismatch (param, Int) |> fail
              | (BExpr _, ((Int, _) as param)) -> TypeMismatch (param, Bool) |> fail
              | _ -> ok ()))
        func.Params
        def.Params
    |> collect
    |> lift (fun _ -> func)

/// <summary>
///     Produces a <c>VSubFun</c> that substitutes the arguments of
///     <c>_arg1</c> for their parameters in <c>_arg2</c>.
/// </summary>
/// <param name="_arg1">
///     The func providing the arguments to substitute.
/// </param>
/// <param name="_arg2_">
///     The <c>DFunc</c> into which we are substituting.
/// </param>
/// <returns>
///     A <c>VSubFun</c> performing the above substitutions.
/// </returns>
let paramSubFun {Params = fpars} {Params = dpars} =
    let pmap =
        Seq.map2 (fun (_, name) up -> name, up) dpars fpars
        |> Map.ofSeq

    // TODO(CaptainHayashi): make this type-safe.
    // TODO(CaptainHayashi): maybe have a separate Const leg for params.
    {AVSub =
        function
        | Unmarked p as up ->
            match (Map.tryFind p pmap) with
            | Some (AExpr e) -> e
            | Some _ -> failwith "param substitution type error"
            | None -> AConst up
        | q -> AConst q
     BVSub =
        function
        | Unmarked p as up ->
            match (Map.tryFind p pmap) with
            | Some (BExpr e) -> e
            | Some _ -> failwith "param substitution type error"
            | None -> BConst up
        | q -> BConst q
    }

/// <summary>
///     Given a func <c>func</c>, its prototype <c>dfunc</c>, and that
///     prototype's Boolean interpretation <c>expr</c>, calculate the
///     value of <c>expr</c> with the arguments of <c>func</c>
///     substituted for the parameters of <c>dfunc</c>.
/// </summary>
/// <parameter name="func">
///     The <c>VFunc</c> whose arguments are to be substituted into
///     <c>expr</c>.
/// </parameter>
/// <parameter name="dfunc">
///     The <c>VFunc</c> whose parameters in <c>expr</c> are to be
///     replaced.
/// </parameter>
/// <parameter name="expr">
///     The <c>BoolExpr</c> in which each instance of a parameter from
///     <c>dfunc</c> is to be replaced with its argument in
///     <c>func</c>.
/// </parameter>
/// <returns>
///     <c>expr</c> with the substitutions above made.
/// </returns>
let substitute func dfunc expr =
    paramSubFun func dfunc |> flip boolSubVars expr

/// <summary>
///     Look up <c>func</c> in <c>_arg1</c>, and instantiate the
///     resulting Boolean expression, substituting <c>func.Params</c>
///     for the parameters in the expression.
/// </summary>
/// <parameter name="func">
///     The <c>VFunc</c> whose arguments are to be substituted into
///     its definition in <c>_arg1</c>.
/// </parameter>
/// <parameter name="_arg1">
///     The <c>FuncTable</c> whose definition for <c>func</c> is to be
///     instantiated.
/// </parameter>
/// <returns>
///     The instantiation of <c>func</c> as an <c>Option</c>al
///     <c>BoolExpr</c>.
/// </returns>
let instantiate func =
    lookup func
    >> bind (function
             | None -> ok None
             | Some (def, snd) -> lift (fun _ -> Some (def, snd))
                                       (checkParamTypes func def))
    >> lift (Option.map (uncurry (substitute func)))