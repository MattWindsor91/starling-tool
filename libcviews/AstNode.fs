module CViews.AstNode

/// A position in a CViews source file.
type SourcePosition =
    { StreamName: string; Line: int64; Column: int64; }
    override this.ToString() = sprintf "SourcePosition { StreamName = \"%s\"; Line = %d; Column = %d; };" this.StreamName this.Line this.Column

/// An AST node.
///
/// Nodes wrap some item in the AST with information about position.
type Node<'a> =
    { Position: SourcePosition; Node: 'a }
    static member (|>>) (n, f) = { Position = n.Position; Node = f n.Node }
    static member (|=>) (n, b) = { Position = n.Position; Node = b }
    override this.ToString() = sprintf "<%A: %A>" this.Position this.Node

let node (streamname : string)
         (line : int64)
         (column : int64)
         (a : 'a)
         : Node<'a> =
    { Position = { StreamName = streamname; Line = line; Column = column }; Node = a }

/// Gets the underlying syntax content of a Node.
let stripNode (a: Node<'a>): 'a = a.Node

let emptyPosition : SourcePosition =
    { StreamName = ""; Line = 0L; Column = 0L; }

let freshNode (a : 'a) : Node<'a> =
  { Position = emptyPosition; Node = a }