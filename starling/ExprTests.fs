/// <summary>
///     Utilities and types for working with expressions.
/// </summary>
module Starling.Tests.Core.Expr

open Starling.Utils
open Starling.Core.TypeSystem
open NUnit.Framework

open Starling.Core.Expr 

module ExprSimp =

  [<Test>] 
  let ``Expression simplification on trivial conjunctions.`` () = 
    Assert.AreEqual (
      (simp (BAnd [BTrue; BTrue])), 
      BTrue
    )

  [<Test>] 
  let ``Expression de-duplication, conjuction removal.`` () = 
    Assert.AreEqual (
     simp (BAnd [ BEq 
                   (normalIntExpr (IVar "foo"), 
                    normalIntExpr (IVar "bar"));
                  BEq 
                   (normalIntExpr (IVar "bar"), 
                    normalIntExpr (IVar "foo"))
               ]), 
     BEq (normalIntExpr (IVar "foo"), 
          normalIntExpr (IVar "bar"))
   ) 

  [<Test>] 
  let ``Expression de-duplication, disjunction removal.`` () = 
    Assert.AreEqual (
     simp (BOr [ BEq 
                   (normalIntExpr (IVar "foo"), 
                    normalIntExpr (IVar "bar"));
                  BEq 
                   (normalIntExpr (IVar "bar"), 
                    normalIntExpr (IVar "foo"))
               ]), 
     BEq (normalIntExpr (IVar "foo"), 
          normalIntExpr (IVar "bar"))
   ) 
