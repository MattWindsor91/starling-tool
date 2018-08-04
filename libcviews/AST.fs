/// <summary>
///    The abstract syntax tree for the CViews language.
/// </summary>
module CViews.Ast

open CViews.AstNode

/// A Boolean operator.
type BinOp =
    | Mul // a * b
    | Div // a / b
    | Mod // a % b
    | Add // a + b
    | Sub // a - b
    | Gt // a > b
    | Ge // a >= b
    | Le // a <= b
    | Lt // a < b
    | Imp // a => b
    | Eq // a == b
    | Neq // a != b
    | And // a && b
    | Or // a || b

/// A unary pre-operator.
type PreOp =
    | Neg // ! a

/// A unary post-operator.
type PostOp =
    | PlusPlus // ++
    | MinusMinus // --


/// An untyped, raw expression.
/// These currently cover all languages, but this may change later.
type Expression' =
    | True // true
    | False // false
    | Num of int64 // 42
    | Identifier of string // foobaz
    | Symbolic of SolverExpression // %{foo}(exprs)
    | BopExpr of BinOp * Expression * Expression // a BOP b
    | UopExpr of PreOp * Expression // UOP a
    | ArraySubscript of array : Expression * subscript : Expression
/// <summary>
///     A fragment of a solver expression AST.
/// </summary>
and SolverExpressionWord =
    /// <summary>
    ///     A string part of a symbolic sentence.
    /// </summary>
    | SEString of string
    /// <summary>
    ///     An argument part of a symbolic sentence.
    /// </summary>
    | SEArg of Node<Expression'>
and SolverExpression = Node<SolverExpressionWord> list
and Expression = Node<Expression'>

/// <summary>
///     A primitive command.
/// </summary>
type Prim' =
    | CompareAndSwap of
        src : Expression
        * test : Expression
        * dest : Expression // <CAS(a, b, c)>
    | Fetch of Expression * Expression * PostOp option // <a = b??>
    | Postfix of Expression * PostOp // <a++> or <a-->
    | Assume of Expression // <assume(e)>
    | SymCommand of symbol : SolverExpression // %{xyz}(x, y)
    | Havoc of var : string // havoc var
and Prim = Node<Prim'>

/// <summary>
///     An atomic action.
/// </summary>
type Atomic' =
    /// <summary>An atomic primitive.</summary>
    | APrim of Prim
    /// <summary>
    ///     A failure command.
    ///     This is semantically equivalent to <c>AAssert False</c>.
    /// </summary>
    | AError
    /// <summary>An assertion.</summary>
    | AAssert of cond : Expression
    /// <summary>An atomic conditional.</summary>
    | ACond of
        cond : Expression
        * trueBranch : Atomic list
        * falseBranch : (Atomic list) option
and Atomic = Node<Atomic'>

/// <summary>
///     A view, annotated with additional syntax.
///
///     <para>
///         This is modelled as Starling's <c>ViewExpr</c>, which
///         cannot be <c>Unknown</c>.
///     </para>
/// </summary>
/// <typeparam name="view">
///     The type of view wrapped inside this expression.
/// </typeparam>
type Marked<'view> =
    /// <summary>
    ///     An unannotated view.
    /// </summary>
    | Unmarked of 'view
    /// <summary>
    ///     A ?-annotated view.
    /// </summary>
    | Questioned of 'view
    /// <summary>
    ///     An unknown view.
    /// </summary>
    | Unknown

/// A view atom appearing in a signature.
type SigAtom =
    { SAName: string
      SAParams: string list }
    
/// Constructs a SigAtom.
let sigAtom (name: string) (pars: string list): SigAtom =
    { SAName = name; SAParams = pars }

/// A view atom appearing in an assertion.
type AssertAtom =
    { AAName: string
      // TODO(@MattWindsor91): make it so these needn't be generic.
      AAArgs: Expression list }
    
/// Constructs an AssertAtom.
let assertAtom (name: string) (args: Expression list): AssertAtom =
    { AAName = name; AAArgs = args }

/// <summary>
///     An AST type literal.
///     <para>
///         This is kept separate from the Starling type system to allow
///         it to become more expressive later on (eg typedefs).
///     </para>
/// </summary>
type TypeLiteral =
    /// <summary>An integer type.</summary>
    | TInt
    /// <summary>A Boolean type.</summary>
    | TBool
    /// <summary>An unknown, and probably user-defined, type.</summary>
    | TUser of name : string
    /// <summary>An array type.</summary>
    | TArray of length : int * contentT : TypeLiteral

/// <summary>
///     An AST formal parameter declaration.
/// </summary>
type Param =
    { /// <summary>The type of the parameters.</summary>
        ParamType : TypeLiteral
        /// <summary>The names of the parameters.</summary>
        ParamName : string
    }

/// A view atom appearing in a prototype.
type ProAtom =
    { PAName: string
      PAParams: Param list
      PAIterated: bool }
    
/// Constructs a ProAtom.
let proAtom (name: string) (pars: Param list) (iterated: bool): ProAtom =
    { PAName = name; PAParams = pars; PAIterated = iterated }

/// Gets the name of a ProAtom.
let protoName (p: ProAtom): string = p.PAName;

/// A view as seen on the LHS of a ViewDef.
type ViewSignature =
    | Unit
    | Join of ViewSignature * ViewSignature
    | Func of SigAtom
    | Iterated of SigAtom * string

/// <summary>
///     An AST variable declaration.
/// </summary>
type VarDecl =
    { /// <summary>The type of the variables.</summary>
        VarType : TypeLiteral
        /// <summary>The names of the variables.</summary>
        VarNames : string list
    }

/// A view.
type View =
        /// <summary>The unit view, `emp`.</summary>
    | Unit
        /// <summary>The always-false view, `false`.</summary>
    | Falsehood
        /// <summary>A `*`-conjunction of two views.</summary>
    | Join of View * View
        /// <summary>An abstract-predicate view.</summary>
    | Func of AssertAtom
        /// <summary>A local view, `local { P }`.</summary>
    | Local of Expression
        /// <summary>A conditional view, `if P { V1 } [else { V2 }]`.</summary>
    | If of Expression * View * View option

/// A set of primitives.
type PrimSet<'Atomic> =
    { PreLocals: Prim list
      Atomics: 'Atomic list
      PostLocals: Prim list }

/// A statement in the command language.
type Command' =
    /// A view expression.
    | ViewExpr of Marked<View>
    /// <summary>A variable declaration.</summary>
    | VarDecl of VarDecl
    /// <summary>
    ///     A miracle command.
    ///     Miracles atomically establish their postcondition.
    /// </summary>
    | Miracle
    /// A set of sequentially composed primitives.
    | Prim of PrimSet<Atomic>
    /// An if-then-else statement, with optional else.
    | If of ifCond : Expression
            * thenBlock : Command list
            * elseBlock : Command list option
    /// A while loop.
    | While of Expression * Command list
    /// A do-while loop.
    | DoWhile of Command list
                * Expression // do { b } while (e)
    /// A list of parallel-composed blocks.
    | Blocks of Command list list
and Command = Node<Command'>

/// A method.
type Method<'cmd> =
    { MName : string // main
      MParams: Param list // (argv, argc) ...
      MBody : 'cmd list } // ... { ... }

/// <summary>
///     A directive for adding backend-specific information.
/// </summary>
type Pragma =
    { ///<summary>The key of the pragma.</summary>
        Key : string
        ///<summary>The value of the pragma.</summary>
        Value : string }

/// A top-level item in a Starling script.
type ScriptItem' =
    | Pragma of Pragma // pragma ...;
    | Typedef of TypeLiteral * string // typedef int Node;
    | SharedVars of VarDecl // shared int name1, name2, name3;
    | ThreadVars of VarDecl // thread int name1, name2, name3;
    | Method of Method<Command> // method main(argv, argc) { ... }
    | Search of int // search 0;
    | ViewProtos of ProAtom list // view name(int arg);
    | Constraint of ViewSignature * Expression option // constraint emp => true
    | Exclusive of SigAtom list // exclusive p(x), q(x), r(x)
    | Disjoint of SigAtom list // disjoint p(x), q(x), r(x)
    override this.ToString() = sprintf "%A" this
and ScriptItem = Node<ScriptItem'>

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
