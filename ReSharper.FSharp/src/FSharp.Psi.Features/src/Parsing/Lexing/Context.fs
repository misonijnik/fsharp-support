namespace JetBrains.ReSharper.Plugins.FSharp.Psi.Features.Parsing.Lexing

type AddBlockEnd = AddBlockEnd | NoAddBlockEnd | AddOneSidedBlockEnd
type FirstInSequence = FirstInSeqBlock | NotFirstInSeqBlock

[<Struct>]
type Position =
    val Line: int
    val StartOfLineAbsoluteOffset: int
    val AbsoluteOffset: int
    member x.Column = x.AbsoluteOffset - x.StartOfLineAbsoluteOffset

    new (line: int, startOfLineAbsoluteOffset: int, absoluteOffset: int) =
        { Line = line
          AbsoluteOffset = absoluteOffset
          StartOfLineAbsoluteOffset = startOfLineAbsoluteOffset }

    member x.NextLine = new Position (x.Line + 1, x.AbsoluteOffset, x.AbsoluteOffset)
    member x.EndOfToken n = new Position (x.Line, x.StartOfLineAbsoluteOffset, x.AbsoluteOffset + n)
    member x.ShiftColumnBy by = new Position (x.Line, x.StartOfLineAbsoluteOffset, x.AbsoluteOffset + by)
    member x.ColumnMinusOne = new Position (x.Line, x.StartOfLineAbsoluteOffset, x.StartOfLineAbsoluteOffset - 1)
    static member Empty = new Position ()
    static member FirstLine fileIdx = new Position (1, 0, 0)

type Context =
    | CtxtLetDecl of bool * Position
    | CtxtIf of Position
    | CtxtTry of Position
    | CtxtFun of Position
    | CtxtFunction of Position
    | CtxtWithAsLet of Position
    | CtxtWithAsAugment of Position
    | CtxtMatch of Position
    | CtxtFor of Position
    | CtxtWhile of Position
    | CtxtWhen of Position
    | CtxtVanilla of Position * bool
    | CtxtThen of Position
    | CtxtElse of Position
    | CtxtDo of Position
    | CtxtInterfaceHead of Position
    | CtxtTypeDefns of Position
    | CtxtNamespaceHead of Position * Token
    | CtxtModuleHead of Position * Token
    | CtxtMemberHead of Position
    | CtxtMemberBody of Position
    | CtxtModuleBody of Position * bool
    | CtxtNamespaceBody of Position
    | CtxtException of Position
    | CtxtParen of Token * Position 
    | CtxtSeqBlock of FirstInSequence * Position * AddBlockEnd
    | CtxtMatchClauses of bool * Position
