module MiniC.Compiler.Ast

type Program = Declaration list

and Declaration =
    | StaticVariableDeclaration of VariableDeclaration
    | FunctionDeclaration of FunctionDeclaration

and TypeSpec = 
    | Void
    | Bool
    | Int
    | Float
    override x.ToString() =
        match x with
        | Void  -> "void"
        | Bool  -> "bool"
        | Int   -> "int"
        | Float -> "float"

and VariableDeclaration =
    | ScalarVariableDeclaration of TypeSpec * Identifier
    | ArrayVariableDeclaration of TypeSpec * Identifier

and FunctionDeclaration = TypeSpec * Identifier * Parameter * CompoundStatement

and Identifier = string

and Parameter = VariableDeclaration list

and IdentifierRef = { Identifier : string }

and Statement = 
    | ExpressionStatement of ExpressionStatement
    | CompoundStatement of CompoundStatement
    | IfStatement of IfStatement
    | WhileStatement of WhileStatement
    | ReturnStatement of Expression option
    | BreakStatement

and ExpressionStatement = 
    | Expression of Expression
    | Nop

and CompoundStatement = LocalVariableDeclarations * Statement list

and LocalVariableDeclarations = VariableDeclaration list

and IfStatement = Expression * Statement * Statement option

and WhileStatement = Expression * Statement

and Expression =
    | ScalarAssignmentExpression of IdentifierRef * Expression
    | ArrayAssignmentExpression of IdentifierRef * Expression * Expression
    | BinaryExpression of Expression * BinaryOperator * Expression
    | UnaryExpression of UnaryOperator * Expression
    | IdentifierExpression of IdentifierRef
    | ArrayIdentifierExpression of Identifier * Expression
    | FunctionCallExpression of Identifier * Arguments
    | ArraySizeExpression of IdentifierRef
    | LiteralExpression of Literal
    | ArrayAllocationExpression of TypeSpec * Expression

and Arguments = Expression list

and BinaryOperator =
    | ConditionalOr
    | Equal
    | NotEqual
    | LessEqual
    | Less
    | GreatEqual
    | Great
    | ConditionalAnd
    | Add
    | Subtract
    | Multiply
    | Divide
    | Modulus
    override x.ToString() =
        match x with
        | ConditionalOr -> "||"
        | Equal -> "=="
        | NotEqual -> "!="
        | LessEqual -> "<="
        | Less -> "<"
        | GreatEqual -> ">="
        | Great -> ">"
        | ConditionalAnd -> "&&"
        | Add -> "+"
        | Subtract -> "-"
        | Multiply -> "*"
        | Divide -> "/"
        | Modulus -> "%"

and UnaryOperator =
    | LogicalNegate
    | Negate
    | Identity

and Literal =
    | BoolLiteral of bool
    | IntLiteral of int
    | FloatLiteral of float
    override x.ToString() =
        match x with
        | BoolLiteral(b) -> b.ToString()
        | IntLiteral(i) -> i.ToString()
        | FloatLiteral(f) -> f.ToString()