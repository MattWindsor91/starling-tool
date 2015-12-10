module Starling.Model

open Microsoft
open Starling.Collections

/// A 'generic' view, parameterised over its parameter type.
type GenView<'a> =
    // TODO(CaptainHayashi): rename to ViewDef.
    { VName: string
      VParams: 'a list }

/// A view definition, whose parameters are type-string pairs.
type ViewDef = GenView<Var.Type * string>

/// A view, whose parameters are expressions.
type View = GenView<Z3.Expr>

/// A conditional (flat or if-then-else) view.
type CondView =
    // TODO(CaptainHayashi): rename to View.
    | CITEView of Z3.BoolExpr * Multiset<CondView> * Multiset<CondView>
    // TODO(CaptainHayashi): expand to all expressions.
    | CSetView of View

/// A guarded item.
type Guarded<'a> =
    {GCond: Z3.BoolExpr
     GItem: 'a}

/// A guarded view.
type GuarView = Guarded<View>

/// A reified view.
type ReView = Guarded<Multiset<View>>

/// A constraint, containing a multiset of views and a Z3 predicate.
type GenConstraint<'a> =
    {CViews: Multiset<GenView<'a>>
     CZ3: Z3.BoolExpr}

/// A model constraint, set over ViewDefs with type-string parameters.
type Constraint = GenConstraint<Var.Type * string>

/// A constraint as used in framed axioms, set over ViewDefs with Z3 expression
/// parameters.
type Z3Constraint = GenConstraint<Z3.Expr>

/// A pair of conditions.
type ConditionPair<'v> =
    { Pre: 'v 
      Post: 'v }

/// A modelled primitive command.
type Prim =
    | IntLoad of dest: Var.LValue option * src: Var.LValue * mode: Var.FetchMode
    | BoolLoad of dest: Var.LValue * src: Var.LValue
    | IntStore of dest: Var.LValue * src: Z3.ArithExpr
    | BoolStore of dest: Var.LValue * src: Z3.BoolExpr
    | IntCAS of dest: Var.LValue * test: Var.LValue * set: Z3.ArithExpr
    | BoolCAS of dest: Var.LValue * test: Var.LValue * set: Z3.BoolExpr
    | IntLocalSet of dest: Var.LValue * src: Z3.ArithExpr
    | BoolLocalSet of dest: Var.LValue * src: Z3.BoolExpr
    | PrimId
    | PrimAssume of Z3.BoolExpr

/// A general Hoare triple, consisting of precondition, inner item, and
/// postcondition.
type Hoare<'c, 'i> =
    { Conditions: ConditionPair<'c>
      Inner: 'i }

type PartConditionPair = ConditionPair<Multiset<CondView>>
type PartHoare<'i> = Hoare<Multiset<CondView>, 'i>

/// A flat axiom, containing a possibly-conditional Hoare triple on an
/// atomic action.
type FlatAxiom = PartHoare<Prim>

/// A fully resolved axiom, containing a guarded Hoare triple on an
/// atomic action.
type FullAxiom = Hoare<Multiset<GuarView>, Prim>

/// A semantically translated axiom, carrying a Z3 Boolean expression as
/// a command.
type SemAxiom = Hoare<Multiset<GuarView>, Z3.BoolExpr>

/// An axiom combined with a frame.
type FramedAxiom =
    {Axiom: SemAxiom
     Frame: Multiset<GuarView>}

/// An unreified term.
type Term = Hoare<Multiset<GuarView>, Z3.BoolExpr>
    
/// A reified term.
type ReTerm = Hoare<Multiset<ReView>, Z3.BoolExpr>

/// A Z3-reified term.
type ZTerm = Hoare<Z3.BoolExpr, Z3.BoolExpr>

/// A partially resolved axiom.
type PartAxiom =
    | PAAxiom of FlatAxiom
    | PAWhile of isDo: bool * expr: Z3.BoolExpr * outer: PartConditionPair * inner: PartHoare<PartAxiom list>
    | PAITE of expr: Z3.BoolExpr * outer: PartConditionPair * inTrue: PartHoare<PartAxiom list> * inFalse: PartHoare<PartAxiom list>

/// Extracts the outer condition pair of a part-axiom.
let cpairOfPartAxiom pa =
    match pa with
    | PAAxiom a -> a.Conditions
    | PAWhile (outer = o) -> o
    | PAITE (outer = o) -> o

/// A parameterised model of a Starling program.
[<NoComparison>]
type Model<'a, 'c> =
    { Context: Z3.Context

      Globals: Var.VarMap
      Locals: Var.VarMap
      Axioms: 'a
      VProtos: Map<string, (Var.Type * string) list>

      // This corresponds to the function D.
      DefViews: 'c }

/// A partly-resolved-axiom model of a Starling program.
type PartModel = Model<PartAxiom list, Constraint list>

/// A flattened model of a Starling program.
type FlatModel = Model<FlatAxiom list, Constraint list>

/// A full model of a Starling program.
type FullModel = Model<FullAxiom list, Constraint list>

/// A semantically translated model of a Starling program.
type SemModel = Model<SemAxiom list, Constraint list>

/// Disposes the Z3 context inside a Model.
let disposeZ3 model = model.Context.Dispose ()

/// Creates a new model that is the input model with a different axiom set.
/// The axiom set may be of a different type.
let withAxioms (axioms: 'y) (model: Model<'x, 'c>): Model<'y, 'c> =
    {Context = model.Context
     Globals = model.Globals
     Locals = model.Locals
     VProtos = model.VProtos
     DefViews = model.DefViews
     Axioms = axioms}
