/// <summary>
///     The part of the Starling process that performs the
///     backend-agnostic (in theory) part of reification.
/// </summary>
module Starling.Reifier

open Chessie.ErrorHandling
open Starling.Collections
open Starling.Utils
open Starling.Core.Expr
open Starling.Core.View
open Starling.Core.Var
open Starling.Core.Model
open Starling.Core.Command
open Starling.Core.GuardedView
open Starling.Core.Symbolic
open Starling.Core.TypeSystem
open Starling.TermGen.Iter

[<AutoOpen>]
module Types =
    /// <summary>
    ///     Errors that can be returned by the reifier.
    /// </summary>
    type Error =
        /// <summary>
        ///     An iterated view definition failed the inductive downclosure
        ///     property.
        ///
        ///     <para>
        ///         This is the property that, if a view definition holds for
        ///         a given iterator <c>n + 1</c>, it holds for the iterator
        ///         <c>n</c>.  These two instances of the definition are called
        ///         <c>sdef</> and <c>bdef</c> below.
        ///     </para>
        /// </summary>
        | InductiveDownclosureError of view : DView * sdef : BoolExpr<Sym<Var>>
                                     * bdef : BoolExpr<Sym<Var>>
        /// <summary>
        ///     An iterated view definition failed the base downclosure
        ///     property with regards to the given definition of 'emp'.
        ///
        ///     <para>
        ///         This is the property that a view definition, when
        ///         instantiated with iterator <c>0</c>, is no stronger than
        ///         the definition for <c>emp</c> if any.
        ///     </para>
        /// </summary>
        | BaseDownclosureError of view : DView * def : BoolExpr<Sym<Var>>
                                * emp : BoolExpr<Sym<Var>>
        /// <summary>
        ///     An iterated view definition contains more than one iterated
        ///     func.
        ///
        ///     <para>
        ///         This restriction is very conservative, and will probably
        ///         be relaxed in the future.
        ///     </para>
        /// </summary>
        | TooManyIteratedFuncs of view : DView * amount : int
        /// <summary>
        ///     An non-iterated func is being used in an iterated manner.
        /// </summary>
        | IteratorOnNonIterated of func : IteratedDFunc
        /// <summary>
        ///     A definition contains both iterated and non-iterated funcs.
        ///
        ///     <para>
        ///         This restriction is very conservative, and will probably
        ///         be relaxed in the future.
        ///     </para>
        /// </summary>
        | MixedFuncType of view : DView
        /// <summary>
        ///     A view occurred in a constraint that does not exist.
        ///
        ///     <para>
        ///         Usually this will be caught earlier on, but this is here
        ///         to make sure.
        ///     </para>
        /// </summary>
        | NoSuchView of name : string
        /// <summary>
        ///     A view lookup failed during downclosure checking.
        ///
        ///     <para>
        ///         Usually this will be caught earlier on, but this is here
        ///         to make sure.
        ///     </para>
        /// </summary>
        | LookupError of name : string * err : Core.Instantiate.Types.Error
        /// <summary>
        ///     An iterator had the wrong type.
        /// </summary>
        | BadIteratorType of view : DView * ty : Type
        /// <summary>
        ///     An iterator was missing where there should have been one.
        /// </summary>
        | MissingIterator of view : DView
        /// <summary>
        ///     An iterated view constraint contained a symbol where we didn't
        ///     expect one.
        ///
        ///     <para>
        ///         This is a fairly restrictive error and may be relaxed
        ///         later on.
        ///     </para>
        /// </summary>
        | SymInIteratedConstraint of sym : string


/// <summary>
///     Downclosure checking.
///
///     <para>
///         The presence of this in the reifier is a marriage of convenience.
///         Later, we might separate it.
///     </para>
/// </summary>
module Downclosure =
    open Starling.Core.ExprEquiv
    open Starling.Core.Sub

    /// Adapts Instantiate.lookup to the downclosure checker's needs.
    let lookupFunc (protos : FuncDefiner<ProtoInfo>) (func : Func<_>)
      : Result<ProtoInfo option, Error> =
        // TODO(CaptainHayashi): proper doc comment
        // TODO(CaptainHayashi): merge with Modeller.lookupFunc?
        protos
        |> Core.Instantiate.lookup func
        |> mapMessages (curry LookupError func.Name)
        |> lift (Option.map snd)

    let downclosureMap (f : Var -> IntExpr<Sym<Var>>)
      : BoolExpr<Sym<Var>> -> BoolExpr<Sym<Var>> =
        let m = Mapper.make f (Reg >> BVar) |> liftVToSym |> onVars
        Mapper.mapBoolCtx m NoCtx >> snd

    let checkBaseDownclosure
      (empDefn : BoolExpr<Sym<Var>>)
      (iterator : Var)
      (func : IteratedDFunc)
      (defn : BoolExpr<Sym<Var>>)
      : Result<IteratedDFunc * BoolExpr<Sym<Var>>, Error> =
        (* To do the base downclosure, we need to replace all instances of the
           iterator in the definition with 0. *)
        let subZero v = if v = iterator then AInt 0L else AVar (Reg v)

        let baseDefn = downclosureMap subZero defn

        (* Base downclosure for a view V[n](x):
             D(emp) => D(V[0](x))
           That is, the definition of V when the iterator is 0 can be no
           stricter than the definition of emp.

           The definition of emp can only mention global variables, so it does
           not need to be freshened. *)
        let check = mkImplies empDefn baseDefn

        // Expression equivalence cannot handle symbols, so try remove them.
        // TODO(CaptainHayashi): is it sound to approximate here?
        bind
            (fun chk ->
                // Pose that the check is a tautology, and use Z3 to check it.
                let closed = equivHolds id (equiv chk BTrue)

                if closed
                then ok (func, defn)
                else fail (BaseDownclosureError ([func], defn, empDefn)))
            (check
             |> Mapper.mapBoolCtx (tsfRemoveSym SymInIteratedConstraint) NoCtx
             |> snd)

    let checkInductiveDownclosure (iterator : Var)
      (func : IteratedDFunc)
      (defn : BoolExpr<Sym<Var>>)
      : Result<IteratedDFunc * BoolExpr<Sym<Var>>, Error> =
        (* To do the inductive downclosure, we need to replace all instances of
           the iterator in the definition with (iterator + 1) in one version. *)
        let increment v =
            if v = iterator
            then mkAdd2 (AVar (Reg v)) (AInt 1L)
            else AVar (Reg v)

        let succDefn = downclosureMap increment defn

        (* Base downclosure for a view V[n](x):
             (0 <= n) => (D(V[n+1](x)) => D(V[n](x)))
           That is, the definition of V when the iterator is n+1 implies the
           definition of V when the iterator is n, for all positive n. *)
        let check =
            mkImplies
                (mkLe (AInt 0L) (AVar (Reg iterator)))
                (mkImplies succDefn defn)

        // Expression equivalence cannot handle symbols, so try remove them.
        // TODO(CaptainHayashi): is it sound to approximate here?
        bind
            (fun chk ->
                // Pose that the check is a tautology, and use Z3 to check it.
                let closed = equivHolds id (equiv chk BTrue)

                if closed
                then ok (func, defn)
                else fail (InductiveDownclosureError ([func], succDefn, defn)))
            (check
             |> Mapper.mapBoolCtx (tsfRemoveSym SymInIteratedConstraint) NoCtx
             |> snd)

    /// <summary>
    ///     Checks the base and inductive downclosure properties on a given
    ///     arity-1 view definition.
    /// </summary>
    /// <param name="func">The view definition's lone defined func.</param>
    /// <param name="empDefn">The definition of 'emp', if one exists.</param>
    /// <param name="defn">The definition of <paramref name="func"/>.</param>
    /// <returns>
    ///     The view definition itself, if it obeys downclosure; an error
    ///     stating which property failed, otherwise.
    /// </returns>
    let checkDownclosure (func : IteratedDFunc)
        (empDefn : BoolExpr<Sym<Var>> option)
        (defn : BoolExpr<Sym<Var>> option)
        : Result<IteratedDFunc * BoolExpr<Sym<Var>> option, Error> =
        let checkIterator =
            function
            | None -> fail (MissingIterator [func])
            | Some (Int v) -> ok v
            | Some v -> fail (BadIteratorType ([func], typeOf v))

        // If empDefn is undefined, it becomes true by the theory.
        let empDefnT = withDefault BTrue empDefn

        (* No point checking downclosure of an indefinite viewdef.
           This job is delegated to the indefinite-proof backends, like HSF and
           MuZ3, which must make sure their synthesised definitions are
           downclosed. *)
        match defn with
        | None -> ok (func, None)
        | Some d ->
            func.Iterator
            |> checkIterator
            |> bind
                (fun i ->
                    checkBaseDownclosure empDefnT i func d
                    >>= uncurry (checkInductiveDownclosure i))
            |> lift (fun (f, d) -> f, Some d)

    /// <summary>
    ///     Performs iterated view well-formedness checking on the left of a
    ///     view definition.
    /// </summary>
    /// <param name="empDefn">The definition, if any, of 'emp'.</param>
    /// <param name="vprotos">The view prototypes in use.</param>
    /// <param name="def">The definition being checked.</param>
    /// <returns>
    ///     The view definition if all checks passed; errors otherwise.
    /// </returns>
    let checkDef
      (empDefn : BoolExpr<Sym<Var>> option)
      (vprotos : FuncDefiner<ProtoInfo>)
      (def : DView * BoolExpr<Sym<Var>> option)
      : Result<DView * BoolExpr<Sym<Var>> option, Error> =
        let (lhs, rhs) = def

        trial {
            (* First, we check the prototypes of the views in the lhs to see which
               are iterated. *)

            let iterprotos, normprotos =
                List.partition (fun func -> func.Iterator <> None) lhs

            match (iterprotos, normprotos) with
            (* Correct non-iterated view definition.
               No more checking necessary. *)
            | [], _ -> return def
            (* Correct iterated view definition, as long as i is actually an
               iterated func.
               Need to check inductive and base downclosure. *)
            | [i], [] ->
                let! iInfo = lookupFunc vprotos i.Func
                return!
                    (match iInfo with
                     | Some { IsIterated = true } ->
                        (lift
                            (fun (v, r) -> ([v], r))
                            (checkDownclosure i empDefn rhs))
                     | Some { IsIterated = _ } -> fail (IteratorOnNonIterated i)
                     | None -> fail (NoSuchView (i.Func.Name)))
            // Over-large iterated view definition (for now, anyway).
            | xs, [] -> return! fail (TooManyIteratedFuncs (lhs, List.length xs))
            // Mixed view definition (currently not allowed).
            | _, _ -> return! fail (MixedFuncType lhs)
        }

    /// <summary>
    ///     Performs all downclosure and well-formedness checking on iterated
    ///     constraints.
    /// </summary>
    /// <param name="definer">
    ///     The definer whose constraints are being checked.
    /// </param>
    /// <returns>
    ///     The definer if all checks passed; errors otherwise.
    /// </returns>
    let check
      (vprotos : FuncDefiner<ProtoInfo>)
      (definer : ViewDefiner<BoolExpr<Sym<Var>> option>)
        : Result<ViewDefiner<BoolExpr<Sym<Var>> option>, Error> =

        (* Currently, we don't actually modify the definer, but this may
           change in future. *)

        (* This is needed for base downclosure checking.
           If it doesn't exist, then we don't do those checks. *)
        let empDefn =
            definer
            |> ViewDefiner.toSeq
            |> Seq.tryFind (fst >> List.isEmpty)
            |> Option.bind snd

        let checkEach def definer =
            ok def >>= checkDef empDefn vprotos >>= (fun _ -> ok definer)

        definer |> ViewDefiner.toSeq |> seqBind checkEach definer

/// Splits an iterated GFunc into a pair of guard and iterated func.
let iterGFuncTuple
  ({ Iterator = i; Func = { Cond = c; Item = f }} : IteratedGFunc<'Var>)
  : BoolExpr<'Var> * IteratedContainer<Func<Expr<'Var>>, IntExpr<'Var>> =
    (c, iterated f i)

/// <summary>
///     Generates the powerset of a set expressed as a list.
/// </summary>
/// <param name="xs">The set whose powerset is sought.</param>
/// <typeparam name="A">The type of set elements.</typeparam>
/// <returns>
///     The powerset of <paramref name="xs"/>, as a lazy sequence.
/// </returns>
let rec powerset (xs : 'A list) : 'A list seq =
    // originally by Tomas Petricek: see http://stackoverflow.com/a/16826554
    seq {
        match xs with
        | [] -> yield []
        | h::t -> for x in powerset t do yield! [x; h::x] }

/// <summary>
///     Given a series of iterated funcs
///     <c>A(a)[i] * A(b)[j] * A(c)[k] * ...</c>, generate the equalities
///     <c>i==j, i==k, ...</c>.
/// </summary>
/// <param name="flist">The NON-EMPTY list of funcs to generate over.</param>
/// <returns>The series of parameter equalities described above.</returns>
let paramEqualities (flist : IteratedFunc<Sym<MarkedVar>> list)
  : BoolExpr<Sym<MarkedVar>> list =
    let x = List.head flist
    concatMap
        (fun y -> List.map2 mkEq x.Func.Params y.Func.Params)
        (List.tail flist)

/// <summary>
///     Preprocesses a view for reification.
///
///     <para>
///         This function converts a view multiset into a list, and expands out
///         any case splits over potentially-equal iterated funcs:
///         for example, <c>(G1 -> A(x)[i]) * (G2 -> A(y)[j])</c> will also
///         generate the func <c>(G1 ^ G2 ^ x=y -> A(x)[i+j])</c> when
///         A(x) is iterated.
///     </para>
/// </summary>
/// <param name="protos">The prototypes used to find iterated funcs.</param>
/// <param name="view">The view to preprocess.</param>
/// <returns>The preprocessed view, as a func list.</returns>
let preprocessView
  (protos : FuncDefiner<ProtoInfo>)
  (view : IteratedGView<Sym<MarkedVar>>)
  : IteratedGFunc<Sym<MarkedVar>> list =
    (* First of all, find out which view prototypes are marked (i)terated,
       and which are (n)ot.  We assume func names are unique and all func
       references are well-formed (correct set of parameters), which should have
       been checked earlier. *)
    let ifuncs, nfuncs =
        protos
        |> FuncDefiner.toSeq
        |> Seq.fold
            (fun (is, ns) (func, pinfo) ->
                if pinfo.IsIterated
                then (Set.add func.Name is, ns)
                else (is, Set.add func.Name ns))
            (Set.empty, Set.empty)

    (* Now, go through the multiset's funcs.  Since the multiset may contain
       n copies of a k-iterated func, we normalise to one copy of a k*n-iterated
       func to get rid of the outer iterator.  What we then do depends on
       whether the func is iterated or not: if it isn't, we just emit it;
       otherwise, we add it to an equivalence class based on the func name so
       we can do the case-split expansion. *)
    let expandFuncToReify
      (ns, ic : Map<string, IteratedGFunc<Sym<MarkedVar>> list>) func n =
        let norm = TermGen.normalise func n
        let nname = norm.Func.Item.Name
        if ifuncs.Contains nname
        then (ns, Map.add nname (norm::ic.[nname]) ic)
        else (norm::ns, ic)

    // Have to make sure each class exists first; else exceptions happen.
    let icempty = ifuncs |> Seq.map (fun name -> (name, [])) |> Map.ofSeq
    let nlist, iclasses = Multiset.fold expandFuncToReify ([], icempty) view

    (* Now, go through the equivalence classes to calculate their case-split
       expansion.  We do this by working out every single possible set of
       equalities between the funcs in the class: say, for

       G1->A(a)[i] * G2->A(b)[j] * G3->A(c)[k]

       we have the cases (), (a=b), (a=c), (b=c), (a=b && a=c).  This turns out
       to be the powerset of the class, less the empty set, where each element
       of the powerset denotes equality between the members' parameters.
       We don't need the map keys at this stage. *)
    let ipsets : (IteratedGFunc<Sym<MarkedVar>> list) seq =
        iclasses |> Map.toSeq |> Seq.map (snd >> powerset) |> Seq.concat

    (* Finally, we need to convert those equivalence powersets into a list of
       funcs. *)
    let mergeEqualitySet xs =
        function
        // Trivial cases first.
        | [] -> xs
        | [func] -> func::xs
        (* Now we have a set of func G1->A(a)[i], G2->A(b)[j], G3->A(c)[k]...
           first, calculate the new guard G1^G2^G3^a=b^a=c^... *)
        | gfuncs ->
            let guards, funcs =
                gfuncs
                |> List.map iterGFuncTuple
                |> List.unzip
            let equalities = paramEqualities funcs
            let nguard = mkAnd (guards @ equalities)

            // And the new iterator i+j+k+...
            let iter =
                funcs
                |> List.map (fun f -> f.Iterator)
                |> List.fold mkAdd2 (AInt 0L)

            { Iterator = iter
              Func = { Cond = nguard; Item = (List.head funcs).Func }}
            :: xs

    Seq.fold mergeEqualitySet nlist ipsets

/// Calculate the multiset of ways that this View matches the pattern in dv and add to the assumulator.
let reifySingleDef
  (protos : FuncDefiner<ProtoInfo>)
  (view : IteratedGFunc<Sym<MarkedVar>> list)
  (accumulator : Set<GuardedIteratedSubview>)
  (dv : DView, _)
  : Set<GuardedIteratedSubview> =
    (* When we finish, we need to pull all of the guards out of the funcs
       we've matched, conjoin them, and use them to guard the iterated view
       the funcs now form.  These are then added to the accumulator. *)
    let mergeResults results accumulator =
        let guars, views = results |> List.map iterGFuncTuple |> List.unzip
        let cond = mkAnd guars

        if (isFalse cond)
        then accumulator
        else Set.add { Cond = cond; Item = List.rev views } accumulator

    (* First, we define what it means to match a view against a single pattern
       func p, given the rest of the pattern is in 'pattern'. *)
    let rec matchSingleView
      (p: IteratedDFunc)
      pattern
      (view : IteratedGFunc<Sym<MarkedVar>> list)
      rview
      accumulator
      result =
        match view with
        | [] -> accumulator
        | v :: view ->
            (* This pattern matches if, and only if, the funcs match. *)
            let pMatchesV =
                p.Func.Name = v.Func.Item.Name
                && p.Func.Params.Length = v.Func.Item.Params.Length

            (* Reification works by building up an accumulator of results from
               performing multiple possible pattern matches.  Because each
               single view match case-splits based on whether or not we take
               the match, we have to push lots of recursive match results into
               our own accumulator. *)
            let accumulator =
                match p.Iterator, pMatchesV with
                | (_, false) ->
                    // The view doesn't match, so this match is dead.
                    accumulator
                | (None, true) ->
                    (* How many times does a non-iterated func A(x) match an
                       iterated func (G1->A(y)[n])?  Once, becoming
                       (G1 && n>0 -> A(y)[1]).  We then have to put
                       (G1 && n>0 -> A(y)[n-1]) back onto the view to match.

                       But what if n is always 0?  Then this pattern match
                       gets a false guard and, because we conjoin all the
                       pattern match guards above, it short-circuits to
                       false and kills off the entire view. *)

                    let nIsPos = mkGt v.Iterator (AInt 0L)
                    let func = { v.Func with Cond = mkAnd2 v.Func.Cond nIsPos }

                    let result =
                        { Func = func; Iterator = AInt 1L } :: result

                    let view =
                        { Func = func; Iterator = mkSub2 v.Iterator (AInt 1L) }
                        :: view

                    matchMultipleViews pattern (rview @ view) accumulator result
                | (Some n, true) ->
                    (* How many times does an iterated func A(x)[i]
                       iterated func A(y)[n]?  As above, all of them.
                       Thus, no remnant is put onto the view, and the entire
                       func is put onto the result. *)
                    let result = v :: result

                    (* We also now match against all of the funcs we
                       refused earlier (rview). *)
                    matchMultipleViews pattern (rview @ view) accumulator result
            (* Now consider the case where we didn't choose the match.
               This function now goes onto the set of refused funcs that
               are placed back into any match we do choose. *)
            matchSingleView p pattern view (v :: rview) accumulator result
    (* We can now specify what it means to reify a view against an entire
       view pattern. *)
    and matchMultipleViews
      (pattern : DView)
      (view : IteratedGFunc<Sym<MarkedVar>> list) accumulator result =
        // TODO(CaptainHayashi): pattern vetting.
        match pattern with
        | [] -> mergeResults result accumulator
        (* Because we preprocessed the view, in both iterated and non-iterated
           cases we can simply traverse the view from left to right, and,
           every time we find something matching the pattern, split on whether
           we accept it.
           Run the case where we do and feed its results into the accumulator,
           then run the case where we don't with that accumulator. *)
        | p :: pattern -> matchSingleView p pattern view [] accumulator result

    matchMultipleViews dv view accumulator []

/// Reifies an dvs entire view application.
let reifyView
  (protos : FuncDefiner<ProtoInfo>)
  (definer : ViewDefiner<SVBoolExpr option>)
  (vap : IteratedGView<Sym<MarkedVar>>)
  : Set<GuardedIteratedSubview> =
    let goal = preprocessView protos vap
    definer
    |> ViewDefiner.toSeq
    |> Seq.fold (reifySingleDef protos goal) Set.empty

/// Reifies all of the terms in a model's axiom list.
let reify
  (model : Model<Term<'a, IteratedGView<Sym<MarkedVar>>, IteratedOView>,
                 ViewDefiner<SVBoolExpr option>>)
  : Result<Model<Term<'a, Set<GuardedIteratedSubview>, IteratedOView>,
                 ViewDefiner<SVBoolExpr option>>, Error> =
    model
    |> mapAxioms (mapTerm id (reifyView model.ViewProtos model.ViewDefs) id)
    |> tryMapViewDefs (Downclosure.check model.ViewProtos)


/// <summary>
///     Pretty printers for the reifier types.
/// </summary>
module Pretty =
    open Starling.Core.Pretty
    open Starling.Core.Expr.Pretty
    open Starling.Core.Symbolic.Pretty
    open Starling.Core.TypeSystem.Pretty
    open Starling.Core.Var.Pretty
    open Starling.Core.View.Pretty

    /// <summary>
    ///     Pretty-prints an <see cref="Error"/>.
    /// </summary>
    let printError : Error -> Doc =
        function
        | InductiveDownclosureError (view, sdef, bdef) ->
            headed "Iterated view does not satisfy inductive downclosure property"
                [ errorInfo <|
                    headed "View being constrained"
                        [ printDView view ]
                  errorInfo <|
                    headed "Constraint failing inductive downclosure"
                        [ printBoolExpr (printSym printVar) bdef ]
                  headed "Constraint must be implied by the following"
                    [ errorInfo <| printBoolExpr (printSym printVar) sdef ] ]
        | BaseDownclosureError (view, def, emp) ->
            headed "Iterated view does not satisfy base downclosure property"
                [ errorInfo <|
                    headed "View being constrained"
                        [ printDView view ]
                  errorInfo <|
                    headed "Constraint failing base downclosure"
                        [ printBoolExpr (printSym printVar) def ]
                  headed "Constraint must be no stronger than 'emp' when \
                          iterator is zero"
                    [ errorInfo <| printBoolExpr (printSym printVar) emp ] ]
        | TooManyIteratedFuncs (view, count) ->
            fmt "constraint '{0}' contains {1} iterated funcs, but iterated \
                 definitions can only contain at most one"
                [ printDView view
                  String (sprintf "%i" count) ]
        | MixedFuncType view ->
            fmt "constraint '{0}' mixes iterated and non-iterated views"
                [ printDView view ]
        | NoSuchView name -> fmt "no view prototype for '{0}'" [ String name ]
        | LookupError(name, err) -> wrapped "lookup for view" (name |> String) (err |> Core.Instantiate.Pretty.printError)
        | IteratorOnNonIterated func ->
            fmt "view '{0}' is not iterated, but used in an iterated constraint"
                [ printIteratedContainer
                    printDFunc
                    (Option.map printTypedVar >> withDefault Nop)
                    func ]
        | BadIteratorType (view, ty) ->
            fmt "iterator on constraint '{0}' is of type {1}, should be int"
                [ printDView view
                  printType ty ]
        | MissingIterator view ->
            fmt "constraint '{0}' should have an iterator, but does not"
                [ printDView view ]
        | SymInIteratedConstraint sym ->
            fmt "symbol '{0}' not allowed in an iterated constraint"
                [ String sym ]
        >> error
