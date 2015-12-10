module Starling.Expander

open Starling.Collections
open Starling.Z3
open Starling.Model
open Starling.Utils
open Microsoft.Z3

/// Calculates the powerset of a set.
let powerset set =
    // TODO(CaptainHayashi): relocate to where this is used, or delete if not.
    Set.fold (fun ps s -> ps + Set.map (fun p -> s + p) ps) (new Set<Set<BoolExpr>> ( [Set.empty] )) set

/// Converts a view from conditional to guarded form.
/// This takes the Z3 context, and the set of all conditions forming the
/// suffix of any guards generated from this view.
let rec resolveCondViewIn (suffix: Set<BoolExpr>) (ctx: Context) cv =
    match cv with
    | CSetView v -> [ {GCond = suffix |> Set.toArray |> mkAnd ctx
                       GItem = v} ]
    | CITEView (expr, tviews, fviews) ->
        List.concat [ resolveCondViewsIn (suffix.Add expr) ctx (Multiset.toList tviews)
                      resolveCondViewsIn (suffix.Add (ctx.MkNot expr)) ctx (Multiset.toList fviews) ]
/// Resolves a list of views, given a set of conditions held true.
and resolveCondViewsIn suffix ctx =
    concatMap (resolveCondViewIn suffix ctx)

/// Resolves a full condition-view multiset into a guarded-view multiset.
let resolveCondViews ctx =
    // TODO(CaptainHayashi): woefully inefficient.
    Multiset.toList
    >> resolveCondViewsIn Set.empty ctx
    >> Multiset.ofList

/// Expands a condition pair.
let expandCondPair ctx cpair =
    { Pre = resolveCondViews ctx cpair.Pre
      Post = resolveCondViews ctx cpair.Post }

/// Expands an axiom.
let expandAxiom ctx axiom =
    { Conditions = expandCondPair ctx axiom.Conditions
      Inner = axiom.Inner }

/// Expands a list of axioms.
let expandAxioms ctx axioms = List.map (expandAxiom ctx) axioms

/// Expands an entire model.
let expand (model: FlatModel) = withAxioms (expandAxioms model.Context model.Axioms) model