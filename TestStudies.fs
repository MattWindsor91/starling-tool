/// <summary>
///     Pre-processed case studies for unit testing.
/// </summary>
module Starling.Tests.Studies

open Starling
open Starling.Collections
open Starling.Core.TypeSystem
open Starling.Core.Definer
open Starling.Core.Expr
open Starling.Core.Symbolic
open Starling.Core.Var
open Starling.Core.Command
open Starling.Core.Command.Create
open Starling.Core.Model
open Starling.Core.View
open Starling.Core.GuardedView
open Starling.Lang.AST
open Starling.Lang.Collator
open Starling.Lang.Modeller
open Starling.Lang.Desugar


/// The raw form of the ticket lock.
let ticketLock = """
view holdTick(int t);
view holdLock();

constraint emp                         -> ticket >= serving;
constraint holdTick(t)                 -> ticket > t;
constraint holdLock()                  -> ticket > serving;
constraint holdLock()   * holdTick(t)  -> serving != t;
constraint holdTick(ta) * holdTick(tb) -> ta != tb;
constraint holdLock()   * holdLock()   -> false;

shared int ticket;
shared int serving;
thread int t;
thread int s;

method lock() {
  {| emp |}
    <t = ticket++>;
  {| holdTick(t) |}
    do {
      {| holdTick(t) |}
        <s = serving>;
      {| if (s == t) { holdLock() } else { holdTick(t) } |}
    } while (s != t);
  {| holdLock() |}
}

method unlock() {
  {| holdLock() |}
    <serving++>;
  {| emp |}
}
"""

let pos l c node =
    { Position = { StreamName = "Examples/Pass/ticketLock.cvf"
                   Line = l
                   Column = c }
      Node = node }

/// The correct parsing of the ticket lock's lock method.
let ticketLockLockMethodAST =
    { Signature = func "lock" []
      Body =
        [ pos 14L 3L (ViewExpr (Unmarked Unit))
          pos 15L 5L <| Command'.Prim
               { PreLocals = []
                 Atomics =
                     [ pos 15L 6L <| APrim
                        (pos 15L 6L <| Fetch
                            (pos 15L 6L (Identifier "t"),
                             pos 15L 10L (Identifier "ticket"),
                             Increment)) ]
                 PostLocals = [] }
          pos 16L 3L <| ViewExpr
            (Unmarked
                (View.Func
                    ( func "holdTick"
                          [ pos 16L 15L <| Identifier "t" ] )))
          pos 17L 5L <| DoWhile
            ([ pos 18L 7L <| ViewExpr
                (Unmarked
                    (View.Func
                        ( func "holdTick"
                             [ pos 18L 19L <| Identifier "t" ] )))
               pos 19L 9L <| Command'.Prim
                { PreLocals = []
                  Atomics =
                   [ pos 19L 10L <| APrim
                       (pos 19L 10L
                           (Fetch
                               (pos 19L 10L <| Identifier "s",
                                pos 19L 14L <| Identifier "serving",
                                Direct))) ]
                  PostLocals = [] }
               pos 20L 7L <| ViewExpr
                (Unmarked
                    (View.If
                        (pos 20L 15L <| BopExpr
                            (Eq,
                             pos 20L 13L (Identifier "s"),
                             pos 20L 18L (Identifier "t")),
                         View.Func (func "holdLock" []),
                         Some <| View.Func
                             (func "holdTick"
                                  [ pos 20L 50L (Identifier "t") ] )))) ],
             pos 21L 16L <| BopExpr
                (Neq,
                 pos 21L 14L (Identifier "s"),
                 pos 21L 19L (Identifier "t")))
          pos 22L 3L <| ViewExpr
            (Unmarked (View.Func (func "holdLock" []))) ] }

/// The correct parsing of the ticket lock's unlock method.
let ticketLockUnlockMethodAST =
    { Signature = func "unlock" []
      Body =
        [ pos 29L 3L <| ViewExpr
            (Unmarked (View.Func (func "holdLock" [])))
          pos 30L 5L <| Command'.Prim
            { PreLocals = []
              Atomics =
                [ pos 30L 6L <| APrim
                    (pos 30L 6L <| Postfix
                        (pos 30L 6L (Identifier "serving"),
                         Increment)) ]
              PostLocals = [] }
          pos 31L 3L <| ViewExpr (Unmarked Unit) ] }

let ticketLockConstraint01 =
    (ViewSignature.Unit,
     Some
      {Position = {StreamName = "Examples/Pass/ticketLock.cvf";
                   Line = 38L;
                   Column = 50L;};
       Node =
        BopExpr
          (Ge,{Position = {StreamName = "Examples/Pass/ticketLock.cvf";
                           Line = 38L;
                           Column = 43L;};
               Node = Identifier "ticket";},
           {Position = {StreamName = "Examples/Pass/ticketLock.cvf";
                        Line = 38L;
                        Column = 53L;};
            Node = Identifier "serving";});})

let ticketLockConstraint02 =
    (ViewSignature.Func (func "holdTick" ["t"]),
     Some
      {Position = {StreamName = "Examples/Pass/ticketLock.cvf";
                   Line = 41L;
                   Column = 50L;};
       Node =
        BopExpr
          (Gt,{Position = {StreamName = "Examples/Pass/ticketLock.cvf";
                           Line = 41L;
                           Column = 43L;};
               Node = Identifier "ticket";},
           {Position = {StreamName = "Examples/Pass/ticketLock.cvf";
                        Line = 41L;
                        Column = 52L;};
            Node = Identifier "t";});})

let ticketLockConstraint03 =
    (ViewSignature.Func (func "holdLock" []),
     Some
      {Position = {StreamName = "Examples/Pass/ticketLock.cvf";
                   Line = 42L;
                   Column = 50L;};
       Node =
        BopExpr
          (Neq,{Position = {StreamName = "Examples/Pass/ticketLock.cvf";
                            Line = 42L;
                            Column = 43L;};
                Node = Identifier "ticket";},
           {Position = {StreamName = "Examples/Pass/ticketLock.cvf";
                        Line = 42L;
                        Column = 53L;};
            Node = Identifier "serving";});})

let ticketLockConstraint04 =
    (ViewSignature.Join
        (ViewSignature.Func (func "holdLock" []),
         ViewSignature.Func (func "holdTick" ["t"])),
     Some
      {Position = {StreamName = "Examples/Pass/ticketLock.cvf";
                   Line = 45L;
                   Column = 51L;};
       Node =
        BopExpr
          (Neq,{Position = {StreamName = "Examples/Pass/ticketLock.cvf";
                            Line = 45L;
                            Column = 43L;};
                Node = Identifier "serving";},
           {Position = {StreamName = "Examples/Pass/ticketLock.cvf";
                        Line = 45L;
                        Column = 54L;};
            Node = Identifier "t";});})

let ticketLockConstraint05 =
    (ViewSignature.Join
        (ViewSignature.Func (func "holdTick" ["ta"]),
         ViewSignature.Func (func "holdTick" ["tb"])),
     Some
      {Position = {StreamName = "Examples/Pass/ticketLock.cvf";
                   Line = 46L;
                   Column = 46L;};
       Node =
        BopExpr
          (Neq,{Position = {StreamName = "Examples/Pass/ticketLock.cvf";
                            Line = 46L;
                            Column = 43L;};
                Node = Identifier "ta";},
           {Position = {StreamName = "Examples/Pass/ticketLock.cvf";
                        Line = 46L;
                        Column = 49L;};
            Node = Identifier "tb";});})

let ticketLockConstraint06 =
    (ViewSignature.Join
        (ViewSignature.Func (func "holdLock" []),
         ViewSignature.Func (func "holdLock" [])),
     Some
      {Position = {StreamName = "Examples/Pass/ticketLock.cvf";
                   Line = 47L;
                   Column = 43L;};
       Node = False;})

/// The parsed form of the ticket lock.
let ticketLockParsed =
    [ { Position =
            { StreamName = "Examples/Pass/ticketLock.cvf"
              Line = 34L; Column = 1L }
        Node =
            ViewProtos
                [ NoIterator
                    (func "holdTick" [ { ParamType = TInt; ParamName = "t" } ],
                     false) ] }
      { Position =
            { StreamName = "Examples/Pass/ticketLock.cvf"
              Line = 35L; Column = 1L }
        Node =
            ViewProtos
                [ NoIterator (func "holdLock" [], false) ] }
      { Position = {StreamName = "Examples/Pass/ticketLock.cvf";
                   Line = 38L;
                   Column = 1L;};
       Node =
        Constraint ticketLockConstraint01}

      {Position = {StreamName = "Examples/Pass/ticketLock.cvf";
                   Line = 41L;
                   Column = 1L;};
       Node =
        Constraint ticketLockConstraint02}

      {Position = {StreamName = "Examples/Pass/ticketLock.cvf";
                   Line = 42L;
                   Column = 1L;};
       Node =
        Constraint ticketLockConstraint03}

      {Position = {StreamName = "Examples/Pass/ticketLock.cvf";
                   Line = 45L;
                   Column = 1L;};
       Node =
        Constraint ticketLockConstraint04}

      {Position = {StreamName = "Examples/Pass/ticketLock.cvf";
                   Line = 46L;
                   Column = 1L;};
       Node =
        Constraint ticketLockConstraint05}

      {Position = {StreamName = "Examples/Pass/ticketLock.cvf";
                   Line = 47L;
                   Column = 1L;};
       Node =
        Constraint ticketLockConstraint06}

      {Position = {StreamName = "Examples/Pass/ticketLock.cvf";
                   Line = 5L
                   Column = 1L}
       Node = SharedVars { VarType = TInt; VarNames = ["ticket"] }}
      {Position = {StreamName = "Examples/Pass/ticketLock.cvf"
                   Line = 6L
                   Column = 1L}
       Node = SharedVars { VarType = TInt; VarNames = ["serving"] }}
      {Position = {StreamName = "Examples/Pass/ticketLock.cvf"
                   Line = 7L
                   Column = 1L}
       Node = ThreadVars { VarType = TInt; VarNames = ["t"] }}
      {Position = {StreamName = "Examples/Pass/ticketLock.cvf"
                   Line = 8L
                   Column = 1L}
       Node = ThreadVars { VarType = TInt; VarNames = ["s"] }}

      {Position = {StreamName = "Examples/Pass/ticketLock.cvf"
                   Line = 13L
                   Column = 1L}
       Node = Method ticketLockLockMethodAST}

      {Position = {StreamName = "Examples/Pass/ticketLock.cvf"
                   Line = 28L
                   Column = 1L}
       Node = Method ticketLockUnlockMethodAST}
       ]

/// The collated form of the ticket lock.
let ticketLockCollated =
    { Pragmata = []
      Typedefs = []
      CollatedScript.SharedVars =
          [ (TInt, "ticket")
            (TInt, "serving") ]
      ThreadVars =
          [ (TInt, "t")
            (TInt, "s") ]
      Search = None
      VProtos =
          [ NoIterator
              (func "holdTick" [ { ParamType = TInt; ParamName = "t" } ], false)
            NoIterator
              (func "holdLock" [], false) ]
      Constraints =
          [ // constraint emp -> ticket >= serving;
            ticketLockConstraint01
            // constraint holdTick(t) -> ticket > t;
            ticketLockConstraint02
            // constraint holdLock() -> ticket > serving;
            ticketLockConstraint03
            // constraint holdLock() * holdTick(t) -> serving != t;
            ticketLockConstraint04
            // constraint holdTick(ta) * holdTick(tb) -> ta != tb;
            ticketLockConstraint05
            // constraint holdLock() * holdLock() -> false;
            ticketLockConstraint06 ]
      Methods =
          [ {Position = emptyPosition; Node = ticketLockLockMethodAST}
            {Position = emptyPosition; Node = ticketLockUnlockMethodAST} ] }

/// Shorthand for Multiset.singleton.
let sing = Multiset.singleton

let oneGFunc (cnd : BoolExpr<Sym<Var>>) (name : string)
  (ps : Expr<Sym<Var>> list)
  : IteratedGFunc<Sym<Var>> =
    iterated (svgfunc cnd name ps) (IInt 1L)

/// The guarded holdLock view.
let gHoldLock cnd : IteratedGFunc<Sym<Var>> =
    oneGFunc cnd "holdLock" []

/// The guarded holdTick view.
let gHoldTick cnd : IteratedGFunc<Sym<Var>> =
    oneGFunc cnd "holdTick" [normalIntExpr (siVar "t")]

/// Produces the expression 's!before == t!before'.
let sIsT = iEq (siVar "s") (siVar "t")

/// The ticket lock's lock method, in guarded form.
let ticketLockLock : ModellerBlock =
      { Pre = freshNode (Mandatory <| Multiset.empty)
        Cmds =
            [ ( command "!ILoad++"
                     [ normalIntExpr (siVar "t"); normalIntExpr (siVar "ticket") ]
                     [ normalIntExpr (siVar "t");
                       normalIntExpr (siVar "ticket"); ]
                |> List.singleton |> Prim,
                freshNode (Mandatory <| sing (gHoldTick BTrue)) )
              ( While (isDo = true,
                       expr = BNot sIsT,
                       inner =
                           { Pre = freshNode (Mandatory <| sing (gHoldTick BTrue))
                             Cmds =
                                 [ ( command "!ILoad"
                                          [ normalIntExpr (siVar "s") ]
                                          [ normalIntExpr (siVar "serving"); ]
                                     |> List.singleton |> Prim,
                                     freshNode (
                                       Mandatory <|
                                       Multiset.ofFlatList
                                           [ gHoldLock sIsT
                                             gHoldTick (BNot sIsT) ] )) ] } ),
                freshNode (Mandatory <| sing (gHoldLock BTrue)) ) ] } 

/// The ticket lock's unlock method, in guarded form.
let ticketLockUnlock : ModellerBlock =
      { Pre = freshNode (Mandatory <| sing (gHoldLock BTrue))
        Cmds =
            [ ( command "!I++" [ normalIntExpr (siVar "serving") ] [ normalIntExpr (siVar "serving") ]
                |> List.singleton |> Prim,
                freshNode (Mandatory <| Multiset.empty )) ] }

/// The methods of the ticket lock.
let ticketLockMethods =
    [ ("lock", ticketLockLock)
      ("unlock", ticketLockUnlock) ] |> Map.ofList

/// The view definitions of the ticket lock model.
let ticketLockViewDefs =
    [([],
      Some <| BGe(normalInt (siVar "ticket"), normalInt (siVar "serving")))
     ([ { Func = func "holdTick" [ TypedVar.Int (normalRec, "t") ]
          Iterator = None } ],
      Some <| BGt(normalInt (siVar "ticket"), normalInt (siVar "t")))
     ([ { Func = func "holdLock" []
          Iterator = None } ],
      Some <| BNot (iEq (siVar "ticket") (siVar "serving")))
     ([ { Func = func "holdLock" []
          Iterator = None }
        { Func = func "holdTick" [ TypedVar.Int (normalRec, "t") ]
          Iterator = None } ],
      Some <| BNot(iEq (siVar "serving") (siVar "t")))
     ([ { Func = func "holdTick" [ TypedVar.Int (normalRec, "ta") ]
          Iterator = None }
        { Func = func "holdTick" [ TypedVar.Int (normalRec, "tb") ]
          Iterator = None } ],
      Some <| BNot(iEq (siVar "ta") (siVar "tb")))
     ([ { Func = func "holdLock" []
          Iterator = None }
        { Func = func "holdLock" []
          Iterator = None } ],
      Some BFalse) ]

let ticketLockViewProtos : FuncDefiner<ProtoInfo> =
    FuncDefiner.ofSeq
        [ (func "holdTick" [ TypedVar.Int (normalRec, "t") ],
           { IsIterated = false; IsAnonymous = false })
          (func "holdLock" [],
           { IsIterated = false; IsAnonymous = false }) ]

/// The model of the ticket lock.
let ticketLockModel : Model<ModellerBlock, ViewDefiner<BoolExpr<Sym<Var>> option>> =
    { Pragmata = []
      LocalLiftView = None
      SharedVars =
          Map.ofList [ ("serving", Type.Int (normalRec, ()))
                       ("ticket", Type.Int (normalRec, ())) ]
      ThreadVars =
          Map.ofList [ ("s", Type.Int (normalRec, ()))
                       ("t", Type.Int (normalRec, ())) ]
      Axioms = ticketLockMethods
      ViewDefs = ticketLockViewDefs
      ViewProtos = ticketLockViewProtos
      Semantics = Starling.Lang.Modeller.coreSemantics
      DeferredChecks = [] }
