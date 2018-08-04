/// Old AST stuff not yet ported properly to libcviews.
module Starling.Lang.AST

open CViews.Ast
open CViews.AstNode

open Starling
open Starling.Collections
open Starling.Core.Var.Types

/// <summary>
///     Pretty printers for the AST.
/// </summary>
module Pretty =
    open Starling.Collections.Func.Pretty
    open Starling.Core.Pretty
    open Starling.Core.Var.Pretty

    /// <summary>
    ///     Hidden building-blocks for AST pretty-printers.
    /// </summary>
    module private Helpers =
        /// Pretty-prints blocks with the given indent level (in spaces).
        /// This does not include the curly braces.
        let printBlock (pCmd : 'Cmd -> Doc) (c : 'Cmd list) : Doc =
            ivsep (List.map (pCmd >> Indent) c)
               
    /// Prints a source position.
    let printPosition (pos : SourcePosition) : Doc =
        String pos.StreamName
        <-> String ":" <-> String (sprintf "%d" pos.Line)
        <-> String ":" <-> String (sprintf "%d" pos.Column)

    /// Pretty-prints Boolean operations.
    let printBinOp : BinOp -> Doc =
        function
        | Mul -> "*"
        | Div -> "/"
        | Mod -> "%"
        | Add -> "+"
        | Sub -> "-"
        | Gt -> ">"
        | Ge -> ">="
        | Le -> "<"
        | Lt -> "<="
        | Imp -> "=>"
        | Eq -> "=="
        | Neq -> "!="
        | And -> "&&"
        | Or -> "||"
        >> String >> syntax

    let printPreOp : PreOp -> Doc =
        function
        | Neg -> "!"
        >> String >> syntax

    /// Pretty-prints expressions.
    /// This is not guaranteed to produce an optimal expression.
    let rec printExpression' (expr : Expression') : Doc =
        match expr with
        | True -> String "true" |> syntaxLiteral
        | False -> String "false" |> syntaxLiteral
        | Num i -> i.ToString() |> String |> syntaxLiteral
        | Identifier x -> syntaxIdent (String x)
        | Symbolic sym -> printSolverExpression sym
        | BopExpr(op, a, b) ->
            hsep [ printExpression a
                   printBinOp op
                   printExpression b ]
            |> parened
        | UopExpr(op, a) ->
            hsep [ printPreOp op
                   printExpression a ]
        | ArraySubscript (array, subscript) ->
            printExpression array <-> squared (printExpression subscript)
    and printExpression (x : Expression) : Doc = printExpression' x.Node
    /// <summary>
    ///     Pretty-prints a symbolic literal.
    /// </summary>
    /// <param name="s">The symbolic to print.</param>
    /// <returns>
    ///     The <see cref="Doc"/> resulting from printing <paramref name="s"/>.
    /// </returns>
    and printSolverExpression (s : SolverExpression) : Doc =
        let printSymWord =
            function
            | SEString s -> String s
            | SEArg a -> a |> printExpression |> ssurround "[|" "|]"       
        let body = s |> List.map (stripNode >> printSymWord) |> hjoin |> braced
        String "%" <-> body

    /// <summary>
    ///     Pretty-prints an if-then-else-like construct.
    /// </summary>
    /// <param name="pLeg">Pretty-printer for 'then'/'else' legs.</param>
    /// <param name="cond">The conditional to print.</param>
    /// <param name="thenLeg">The 'then' leg to print.</param>
    /// <param name="elseLeg">The optional 'else' leg to print.</param>
    /// <typeparam name="Leg">Type of 'then'/'else' leg items.</typeparam>
    /// <returns>
    ///     A <see cref="Doc"/> capturing the if-then-else form.
    /// </returns>
    let printITELike
        (pLeg : 'Leg -> Doc)
        (cond : Expression)
        (thenLeg : 'Leg)
        (elseLeg : 'Leg option)
        : Doc =
        syntaxStr "if"
        <+> parened (printExpression cond)
        <+> braced (pLeg thenLeg)
        <+> (maybe
                Nop
                (fun e -> syntaxStr "else" <+> braced (pLeg e))
                    elseLeg)

    /// <summary>
    ///     Pretty-prints an if-then-else.
    /// </summary>
    /// <param name="pCmd">Pretty-printer for commands.</param>
    /// <param name="cond">The conditional to print.</param>
    /// <param name="thenCmds">The 'then' leg to print.</param>
    /// <param name="elseCmds">The optional 'else' leg to print.</param>
    /// <typeparam name="Cmd">Type of commands</typeparam>
    /// <returns>
    ///     A <see cref="Doc"/> capturing the if-then-else form.
    /// </returns>
    let printITE
        (pCmd : 'Cmd -> Doc)
        (cond : Expression)
        (thenCmds : 'Cmd list)
        (elseCmds : ('Cmd list) option)
        : Doc =
            printITELike (Helpers.printBlock pCmd) cond thenCmds elseCmds

    /// <summary>
    ///     Pretty-prints a type literal.
    /// </summary>
    /// <param name="lit">The <see cref="TypeLiteral"/> to print.</param>
    /// <returns>
    ///     A <see cref="Doc"/> representing the given type literal.
    /// </returns>
    let printTypeLiteral (lit : TypeLiteral) : Doc =
        let rec pl lit suffix =
            match lit with
            | TInt -> syntaxIdent (String ("int")) <-> suffix
            | TBool -> syntaxIdent (String ("bool")) <-> suffix
            | TUser s -> syntaxLiteral (String s) <-> suffix
            | TArray (len, contents) ->
                let lenSuffix = squared (String (sprintf "%d" len))
                pl contents (suffix <-> lenSuffix)
        pl lit Nop

    /// Pretty-prints parameters.
    let printParam (par : Param) : Doc =
        hsep
            [ printTypeLiteral par.ParamType
              syntaxLiteral (String par.ParamName) ]

    /// Pretty-prints assertion view atoms.
    let printAssertAtom (a: AssertAtom): Doc =
        func a.AAName (List.map printExpression a.AAArgs)

    /// Pretty-prints prototype view atoms.
    let printProtoAtom (a: ProAtom): Doc =
        let ap = func a.PAName (List.map printParam a.PAParams)
        if a.PAIterated
        then String "iter" <+> ap
        else ap

    /// Pretty-prints a list of prototype view atoms.
    let printProtoAtomList (vps : ProAtom list) : Doc =
        hsep [ syntax (String "view")
               commaSep (List.map printProtoAtom vps) ]
        |> withSemi

    /// Pretty-prints signature view atoms.
    let printSigAtom (a: SigAtom): Doc =
        func a.SAName (List.map String a.SAParams)

    /// Pretty-prints views.
    let rec printView : View -> Doc =
        function
        | View.Func f -> printAssertAtom f
        | View.Unit -> String "emp" |> syntaxView
        | View.Falsehood -> String "false" |> syntaxView
        | View.Join(l, r) -> binop "*" (printView l) (printView r)
        | View.If(e, l, r) -> printITELike printView e l r
        | View.Local l -> syntaxView (String "local") <+> braced (printExpression l)

    /// Pretty-prints marked view lines.
    let rec printMarkedView (pView : 'view -> Doc) : Marked<'view> -> Doc =
        function
        | Unmarked v -> pView v
        | Questioned v -> hjoin [ pView v ; String "?" |> syntaxView ]
        | Unknown -> String "?" |> syntaxView
        >> ssurround "{| " " |}"
 
    /// Pretty-prints view definitions.
    let rec printViewSignature : ViewSignature -> Doc =
        function
        | ViewSignature.Func f -> printSigAtom f
        | ViewSignature.Unit -> String "emp" |> syntaxView
        | ViewSignature.Join(l, r) -> binop "*" (printViewSignature l) (printViewSignature r)
        | ViewSignature.Iterated(f, e) -> hsep [String "iter" |> syntaxView; hjoin [String "[" |> syntaxView; String e; String "]" |> syntaxView]; printSigAtom f]

    /// Pretty-prints constraints.
    let printConstraint (view : ViewSignature) (def : Expression option) : Doc =
        hsep [ String "constraint" |> syntax
               printViewSignature view
               String "->" |> syntax
               (match def with
                | Some d -> printExpression d
                | None _ -> String "?" |> syntax) ]
        |> withSemi

    /// Pretty-prints exclusivity constraints.
    let printExclusive (xs : SigAtom list) : Doc =
        hsep ((String "exclusive") ::
              (List.map printSigAtom xs))
        |> withSemi

    /// Pretty-prints exclusivity constraints.
    let printDisjoint (xs : SigAtom list) : Doc =
        hsep ((String "disjoint") ::
              (List.map printSigAtom xs))
        |> withSemi

    /// Pretty-prints postfix operators.
    let printPostOp (o: PostOp): Doc =
        match o with
        | PlusPlus -> String "++"
        | MinusMinus -> String "--"

    /// Pretty-prints local assignments.
    let printAssign (dest : Expression) (src : Expression) : Doc =
        equality (printExpression dest) (printExpression src)

    /// <summary>
    ///     Pretty-prints primitive actions.
    /// </summary>
    /// <param name="p">The <see cref="Prim'"/> to print.</param>
    /// <returns>
    ///     A <see cref="Doc"/> representing <paramref name="p"/>.
    /// </returns>
    let rec printPrim' (p : Prim') : Doc =
        match p with
        | CompareAndSwap(l, f, t) ->
            func "CAS" [ printExpression l
                         printExpression f
                         printExpression t ]
        | Fetch(l, r, mo) ->
            let rp = printExpression r
            equality
                (printExpression l)
                (maybe rp (fun m -> rp <-> printPostOp m) mo)
        | Postfix(l, m) -> printExpression l <-> printPostOp m
        | Assume e -> func "assume" [ printExpression e ]
        | SymCommand sym -> printSolverExpression sym
        | Havoc var -> String "havoc" <+> String var
    and printPrim (x : Prim) : Doc = printPrim' x.Node

    /// <summary>
    ///     Pretty-prints atomic actions.
    /// </summary>
    /// <param name="a">The <see cref="Atomic'"/> to print.</param>
    /// <returns>
    ///     A <see cref="Doc"/> representing <paramref name="a"/>.
    /// </returns>
    let rec printAtomic' (a : Atomic') : Doc =
        match a with
        | APrim p -> printPrim p
        | AError -> syntaxStr "error"
        | AAssert e -> syntaxStr "assert" <+> parened (printExpression e)
        | ACond (cond = c; trueBranch = t; falseBranch = f) ->
            printITE printAtomic c t f
    and printAtomic (x : Atomic) : Doc = printAtomic' x.Node

    /// Pretty-prints a variable declaration, without semicolon.
    let printVarDecl (vs : VarDecl) : Doc =
        let vsp = commaSep (List.map printVar vs.VarNames)
        hsep [ printTypeLiteral vs.VarType; vsp ]

    /// Pretty-prints commands.
    let rec printCommand' (cmd : Command') : Doc =
        match cmd with
        (* The trick here is to make Prim [] appear as ;, but
           Prim [x; y; z] appear as x; y; z;, and to do the same with
           atomic lists. *)
        | Command'.Prim { PreLocals = ps; Atomics = ts; PostLocals = qs } ->
            seq { yield! Seq.map printPrim ps
                  yield (ts
                         |> Seq.map printAtomic
                         |> semiSep |> withSemi |> braced |> angled)
                  yield! Seq.map printPrim qs }
            |> semiSep |> withSemi
        | Command'.Miracle -> syntaxStr "..."
        | Command'.If(c, t, f) ->
            printITE printCommand c t f
        | Command'.While(c, b) ->
            hsep [ "while" |> String |> syntax
                   c |> printExpression |> parened
                   b |> Helpers.printBlock printCommand ]
        | Command'.DoWhile(b, c) ->
            hsep [ "do" |> String |> syntax
                   b |> Helpers.printBlock printCommand
                   "while" |> String |> syntax
                   c |> printExpression |> parened ]
            |> withSemi
        | Command'.Blocks bs ->
            bs
            |> List.map (Helpers.printBlock printCommand)
            |> hsepStr "||"
        | Command'.ViewExpr v -> printMarkedView printView v
        | Command'.VarDecl vs ->
            withSemi (syntaxStr "thread" <+> printVarDecl vs)
    and printCommand (x : Command) : Doc = printCommand' x.Node

    /// <summary>
    ///     Prints a command block.
    /// </summary>
    /// <param name="block">The block to print.</param>
    /// <returns>A <see cref="Doc"/> capturing <paramref name="block"/>.
    let printCommandBlock (block : Command list) : Doc =
        Helpers.printBlock printCommand block

    /// Pretty-prints methods.
    let printMethod (pCmd : 'cmd -> Doc) (m : Method<'cmd>)
                    : Doc =
        hsep [ "method" |> String |> syntax
               func m.MName (List.map printParam m.MParams)
               Helpers.printBlock pCmd m.MBody ]

    /// Pretty-prints a search directive.
    let printSearch (i : int) : Doc =
        hsep [ String "search" |> syntax
               sprintf "%d" i |> String ]

    /// Pretty-prints a script variable list of the given class.
    let printScriptVars (cls : string) (vs : VarDecl) : Doc =
        withSemi (hsep [ String cls |> syntax; printVarDecl vs ])

    /// <summary>Prints a pragma.</summary>
    /// <param name="pragma">The pragma to print.</summary>
    /// <returns>
    ///     A <see cref="Doc"/> for printing <paramref name="pragma"/>.
    /// </returns>
    let printPragma (pragma : Pragma) : Doc =
        String pragma.Key <+> braced (String pragma.Value)

    /// Pretty-prints script lines.
    let printScriptItem' (item : ScriptItem') : Doc =
        match item with
        | Pragma p -> withSemi (printPragma p)
        | Typedef (ty, name) ->
            withSemi (syntaxIdent (String "typedef") <+> printTypeLiteral ty <+> String name)
        | SharedVars vs -> printScriptVars "shared" vs
        | ThreadVars vs -> printScriptVars "thread" vs
        | Method m ->
            fun mdoc -> vsep [Nop; mdoc; Nop]
            <| printMethod printCommand m
        | ViewProtos v -> printProtoAtomList v
        | Search i -> printSearch i
        | Constraint (view, def) -> printConstraint view def
        | Exclusive xs -> printExclusive xs
        | Disjoint xs -> printDisjoint xs
    let printScriptItem (x : ScriptItem) : Doc = printScriptItem' x.Node

    /// Pretty-prints scripts.
    /// each line on its own line
    let printScript (xs : ScriptItem list) : Doc =
        VSep (List.map printScriptItem xs, Nop)


(*
 * Expression classification
 *)

/// Active pattern classifying bops as to whether they create
/// arithmetic or Boolean expressions.
let (|ArithOp|BoolOp|) : BinOp -> Choice<unit, unit> =
    function
    | Mul | Div | Add | Sub | Mod -> ArithOp
    | Gt | Ge | Le | Lt | Imp -> BoolOp
    | Eq | Neq -> BoolOp
    | And | Or -> BoolOp

/// Active pattern classifying bops as to whether they take in
/// arithmetic, Boolean, or indeterminate operands.
let (|ArithIn|BoolIn|AnyIn|) : BinOp -> Choice<unit, unit, unit> =
    function
    | Mul | Div | Add | Sub | Mod -> ArithIn
    | Gt | Ge | Le | Lt -> ArithIn
    | Eq | Neq -> AnyIn
    | And | Or | Imp -> BoolIn

/// Active pattern classifying inner expressions as to whether they are
/// arithmetic, Boolean, or indeterminate.
let (|BoolExp'|ArithExp'|AnyExp'|) (e : Expression')
  : Choice<Expression', Expression', Expression'> =
    match e with
    | Identifier _ -> AnyExp' e
    | Symbolic _ -> AnyExp' e
    | ArraySubscript _ -> AnyExp' e
    | Num _ -> ArithExp' e
    | True | False -> BoolExp' e
    | BopExpr(BoolOp, _, _) | UopExpr(_) -> BoolExp' e
    | BopExpr(ArithOp, _, _) -> ArithExp' e

/// Active pattern classifying expressions as to whether they are
/// arithmetic, Boolean, or indeterminate.
let (|BoolExp|ArithExp|AnyExp|) (e : Expression)
  : Choice<Expression, Expression, Expression> =
    match e.Node with
    | BoolExp' _ -> BoolExp e
    | ArithExp' _ -> ArithExp e
    | AnyExp' _ -> AnyExp e

/// <summary>
///     Active pattern classifying expressions as lvalues or rvalues.
/// </summary>
let (|LValue|RValue|) (e : Expression) : Choice<Expression, Expression> =
    match e.Node with
    (* TODO(CaptainHayashi): symbolic lvalues?
       These, however, need a lot of thought as to what the framing semantics
       are. *)
    | Identifier _ | ArraySubscript _ -> LValue e
    | _ -> RValue e

(*
 * Misc
 *)
let emptyPosition : SourcePosition =
    { StreamName = ""; Line = 0L; Column = 0L; }
let freshNode (a : 'a) : Node<'a> =
  { Position = emptyPosition; Node = a }
let node (streamname : string)
         (line : int64)
         (column : int64)
         (a : 'a)
         : Node<'a> =
    { Position = { StreamName = streamname; Line = line; Column = column }; Node = a }
