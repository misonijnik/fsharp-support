namespace JetBrains.ReSharper.Plugins.FSharp.Psi.Features.Parsing.Lexing
open JetBrains.ReSharper.Psi.Parsing
open JetBrains.ReSharper.Psi.ExtensionsAPI.Tree
open JetBrains.ReSharper.Plugins.FSharp.Common.Util
open JetBrains.ReSharper.Plugins.FSharp.Psi.Parsing
open System.Collections.Generic
type Token = FSharpTokenType

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
    member x.NextLine = Position (x.Line + 1, x.AbsoluteOffset, x.AbsoluteOffset)
    member x.EndOfToken n = Position (x.Line, x.StartOfLineAbsoluteOffset, x.AbsoluteOffset + n)
    member x.ShiftColumnBy by = Position (x.Line, x.StartOfLineAbsoluteOffset, x.AbsoluteOffset + by)
    member x.ColumnMinusOne = Position (x.Line, x.StartOfLineAbsoluteOffset, x.StartOfLineAbsoluteOffset - 1)
    static member Empty = Position ()
    static member FirstLine = Position (1, 0, 0)

[<Struct>]
type PositionWithColumn =
    val Position: Position
    val Column: int
    new (position: Position, column: int) = { Position = position; Column = column }

[<Struct>]
type TokenTup =
    val CurrentTokenType : TokenNodeType
    val StartPosition : Position
    val EndPosition : Position
    val LexicalState : int
    new (currTokenType, bufferStart, bufferEnd, lexicalState) =
        { CurrentTokenType = currTokenType;
          StartPosition = bufferStart;
          EndPosition = bufferEnd;
          LexicalState = lexicalState; }

    member x.StartPosOfTokenTup () =
        if Token.EOF == x.CurrentTokenType
            then x.StartPosition.ColumnMinusOne
            else x.StartPosition

    member x.UseLocation(tok) =
        TokenTup (tok, x.StartPosition, x.EndPosition, x.LexicalState)

    member x.UseStartLocation(tok) =
        TokenTup (tok, x.StartPosition, x.StartPosition, x.LexicalState)

    member x.UseEndLocation(tok) =
        TokenTup (tok, x.EndPosition, x.EndPosition, x.LexicalState)

    member x.UseShiftedLocation(tok, shiftLeft, shiftRight) =
        TokenTup (tok, x.StartPosition.ShiftColumnBy shiftLeft, x.EndPosition.ShiftColumnBy shiftRight, x.LexicalState)

module Postprocessing =
    open JetBrains.ReSharper.Plugins.FSharp.Psi.Features.Util.ImmutableStack

    let dummyTokenTup = TokenTup ()
    let rec handleDummy (token : TokenNodeType) =
        match token with
        | :? Token.DummyNodeType as token -> handleDummy token.Token
        | _ -> token

    let rec suffixExists p l = if isEmpty l then false else p (pop l) || suffixExists p (pop l)


    let odeclend = NodeTypeSet (Token.ORIGHT_BLOCK_END, Token.OBLOCKEND, Token.ODECLEND)
    let infix =
        NodeTypeSet
           (Token.COMMA,
            Token.BAR_BAR,
            Token.AMP_AMP,
            Token.AMP,
            Token.OR,
            Token.INFIX_BAR_OP,
            Token.BAD_INFIX_BAR_OP,
            Token.INFIX_AMP_OP,
            Token.BAD_INFIX_AMP_OP,
            Token.INFIX_COMPARE_OP,
            Token.BAD_INFIX_COMPARE_OP,
            Token.INFIX_AT_HAT_OP,
            Token.BAD_INFIX_AT_HAT_OP,
            Token.PLUS_MINUS_OP,
            Token.BAD_PLUS_MINUS_OP,
            Token.INFIX_STAR_DIV_MOD_OP,
            Token.BAD_INFIX_STAR_DIV_MOD_OP,
            Token.INFIX_STAR_STAR_OP,
            Token.BAD_INFIX_STAR_STAR_OP,
            Token.COLON_COLON,
            Token.COLON_GREATER,
            Token.COLON_QMARK,
            Token.COLON_EQUALS,
            Token.MINUS,
            Token.PLUS,
            Token.STAR,
            Token.QMARK_QMARK,
            Token.DOLLAR)

    let ifBlockContinuator =
        NodeTypeSet (Token.THEN, Token.ELSE, Token.ELIF, Token.END, Token.RPAREN)
     |> odeclend.Union

    let tryBlockContinuator =
        NodeTypeSet(Token.FINALLY, Token.WITH)
     |> odeclend.Union

    let private thenBlockContinuator = odeclend
    let private doneDeclEnd = NodeTypeSet (Token.DONE) |> odeclend.Union
    let private andDeclEnd = NodeTypeSet (Token.AND) |> odeclend.Union
    let private interfaceContinuator = NodeTypeSet (Token.END) |> odeclend.Union

    //todo: override locateToken to add EOF token [misonijnik]
    let private notNamespaceContinuator = NodeTypeSet (Token.EOF, Token.NAMESPACE)
    let private typeContinuator =
        NodeTypeSet (Token.RBRACE, Token.WITH, Token.BAR, Token.AND, Token.END)
     |> odeclend.Union

    let private typeSeqBlockElementContinuator = NodeTypeSet (Token.BAR, Token.OBLOCKBEGIN) |> odeclend.Union
    let private seqBlockElementContinuator =
        NodeTypeSet
           (Token.END,
            Token.AND,
            Token.WITH,
            Token.THEN,
            Token.RPAREN,
            Token.RBRACE,
            Token.RBRACK,
            Token.BAR_RBRACK,
            Token.RQUOTE_TYPED,
            Token.RQUOTE_UNTYPED)
     |> odeclend.Union

    let private longIdentifier = NodeTypeSet (Token.IDENTIFIER, Token.DOT)
    let private longIdentifierOrGlobal = NodeTypeSet (Token.GLOBAL) |> longIdentifier.Union
    let private atomicExprEndToken =
        NodeTypeSet
           (Token.IDENTIFIER,
            Token.RPAREN,
            Token.RBRACK,
            Token.RBRACE,
            Token.BAR_RBRACK,
            Token.END,
            Token.NULL,
            Token.FALSE,
            Token.TRUE,
            Token.UNDERSCORE)
     |> Token.Literals.Union
     |> Token.Strings.Union

    let private parenTokensBalance =
        NodeTypeDictionary<TokenNodeType>
            ([|
                new KeyValuePair<NodeType, TokenNodeType>(Token.LPAREN, Token.RPAREN)
                new KeyValuePair<NodeType, TokenNodeType>(Token.LBRACE, Token.RPAREN)
                new KeyValuePair<NodeType, TokenNodeType>(Token.LBRACK, Token.RBRACK)
                new KeyValuePair<NodeType, TokenNodeType>(Token.INTERFACE, Token.END)
                new KeyValuePair<NodeType, TokenNodeType>(Token.CLASS, Token.END)
                new KeyValuePair<NodeType, TokenNodeType>(Token.SIG, Token.END)
                new KeyValuePair<NodeType, TokenNodeType>(Token.STRUCT, Token.END)
                new KeyValuePair<NodeType, TokenNodeType>(Token.LBRACK_BAR, Token.BAR_RBRACK)
                //todo: LESS and GREATER may be in type application [misonijnik]
                new KeyValuePair<NodeType, TokenNodeType>(Token.BEGIN_TYPE_APP, Token.END_TYPE_APP)
                new KeyValuePair<NodeType, TokenNodeType>(Token.BEGIN, Token.END)
                new KeyValuePair<NodeType, TokenNodeType>(Token.LQUOTE_TYPED, Token.RQUOTE_TYPED)
                new KeyValuePair<NodeType, TokenNodeType>(Token.LQUOTE_UNTYPED, Token.RQUOTE_UNTYPED)
             |])

    let leftBraceBrack = NodeTypeSet (Token.LBRACE, Token.LBRACK, Token.LBRACK_BAR)
    let leftParenBrackAndBegin = NodeTypeSet (Token.LPAREN, Token.LBRACK, Token.LBRACK_BAR, Token.BEGIN)
    let leftParenBegin = NodeTypeSet (Token.BEGIN, Token.LPAREN) |> leftBraceBrack.Union
    let leftParen = NodeTypeSet Token.LPAREN |> leftBraceBrack.Union
    let sigStructBegin = NodeTypeSet (Token.SIG, Token.STRUCT, Token.BEGIN)
    let leftBraceBrackAndBeginTypeApp = NodeTypeSet (Token.BEGIN, Token.BEGIN_TYPE_APP) |> leftBraceBrack.Union
    let classStructInterface = NodeTypeSet (Token.CLASS, Token.STRUCT, Token.INTERFACE)
    let leftParenBrackLess = NodeTypeSet (Token.LPAREN, Token.LESS, Token.LBRACK_LESS)
    let grammarInTypes =
        NodeTypeSet
           (Token.DEFAULT,
            Token.COLON,
            Token.COLON_GREATER,
            Token.STRUCT,
            Token.NULL,
            Token.DELEGATE,
            Token.AND,
            Token.WHEN,
            Token.DOT_DOT,
            Token.MINUS,
            Token.GLOBAL,
            Token.CONST,
            Token.KEYWORD_STRING_SOURCE_DIRECTORY,
            Token.KEYWORD_STRING_SOURCE_FILE,
            Token.KEYWORD_STRING_LINE,
            Token.NULL,
            Token.SBYTE,
            Token.INT16,
            Token.INT32,
            Token.INT64,
            Token.NATIVEINT,
            Token.BYTE,
            Token.UINT16,
            Token.UINT32,
            Token.UINT64,
            Token.UNATIVEINT,
            Token.DECIMAL,
            Token.BIGNUM,
            Token.STRING,
            Token.BYTEARRAY,
            Token.CHARACTER_LITERAL,
            Token.TRUE,
            Token.FALSE,
            Token.IEEE32,
            Token.IEEE64,
            Token.DOT,
            Token.UNDERSCORE,
            Token.EQUALS,
            Token.IDENTIFIER,
            Token.COMMA,
            Token.RARROW,
            Token.HASH,
            Token.STAR,
            Token.QUOTE)

    let beforeTypeApplication =
        NodeTypeSet
           (Token.DELEGATE,
            Token.IDENTIFIER,
            Token.IEEE64,
            Token.IEEE32,
            Token.DECIMAL,
            Token.SBYTE,
            Token.INT16,
            Token.INT32,
            Token.INT64,
            Token.NATIVEINT,
            Token.BYTE,
            Token.UINT16,
            Token.UINT32,
            Token.UINT64,
            Token.BIGNUM)

    let adjacentPrefixTokens =
        NodeTypeSet
           (Token.MINUS,
            Token.PLUS,
            Token.PERCENT,
            Token.PERCENT_PERCENT,
            Token.AMP,
            Token.AMP_AMP)

    let signedDigits =
        NodeTypeSet
           (Token.SBYTE,
            Token.INT16,
            Token.INT32,
            Token.INT64,
            Token.NATIVEINT,
            Token.IEEE32,
            Token.IEEE64,
            Token.DECIMAL,
            Token.BIGNUM)

    let controlFlow =
        NodeTypeSet
           (Token.TRY,
            Token.MATCH,
            Token.MATCH_BANG,
            Token.IF,
            Token.LET,
            Token.LET_BANG,
            Token.FOR,
            Token.WHILE)

    let forcesHeadContextClosure =
        NodeTypeSet
           (Token.END,
            Token.ELSE,
            Token.ELIF,
            Token.DONE,
            Token.IN,
            Token.RPAREN,
            Token.BEGIN_TYPE_APP,
            Token.RBRACE,
            Token.RBRACK,
            Token.BAR_RBRACK,
            Token.WITH,
            Token.FINALLY,
            Token.RQUOTE_TYPED,
            Token.RQUOTE_UNTYPED)

    let isInfix token = infix.[token]
    let isNonAssocInfixToken token = token == Token.EQUALS
    let isIfBlockContinuator token = ifBlockContinuator.[handleDummy token]
    let isTryBlockContinuator token = tryBlockContinuator.[handleDummy token]
    let isThenBlockContinuator token = thenBlockContinuator.[handleDummy token]
    let isDoContinuator token = doneDeclEnd.[handleDummy token]
    let isNamespaceContinuator token = not notNamespaceContinuator.[handleDummy token]
    let isTypeContinuator token = typeContinuator.[handleDummy token]
    let isForLoopContinuator token = doneDeclEnd.[handleDummy token]
    let isWhileBlockContinuator token = doneDeclEnd.[handleDummy token]
    let isLetContinuator token = andDeclEnd.[handleDummy token]
    let isTypeSeqBlockElementContinuator token = typeSeqBlockElementContinuator.[handleDummy token]
    let isSeqBlockElementContinuator token =
        isInfix token || seqBlockElementContinuator.[handleDummy token]
    let isWithAugmentBlockContinuator token = handleDummy token == Token.END
    let isLongIdentifier token = longIdentifier.[token]
    let isLongIdentifierOrGlobal token = longIdentifierOrGlobal.[token]
    let isAtomicExprEndToken token = atomicExprEndToken.[token]
    let isParenTokensBalance leftToken rightToken =
        leftToken <> null && rightToken <> null && parenTokensBalance.[leftToken] == rightToken
    let isLeftBraceBrack token = leftBraceBrack.[token]
    let isLeftParenBegin token = leftParenBegin.[token]
    let isLeftParen token = leftParen.[token]
    let isSigStructBegin token = sigStructBegin.[token]
    let isLeftBraceBrackAndBeginTypeApp token = leftBraceBrackAndBeginTypeApp.[token]
    let isClassStructInterface token = classStructInterface.[token]
    let isLeftParenBrackAndBegin token = leftParenBrackAndBegin.[token]
    let isLeftParenBrackLess token = leftParenBrackLess.[token]
    let isGrammarInTypes token = grammarInTypes.[token]
    let isBeforeTypeApplication token = beforeTypeApplication.[token]
    let isAdjacentPrefixTokens token = adjacentPrefixTokens.[token]
    let isSignedDigits token = signedDigits.[token]
    let isControlFlow token = controlFlow.[token]
    let isSemiSemi token = token == Token.SEMICOLON_SEMICOLON
    let isForcesHeadContextClosure token = forcesHeadContextClosure.[token]
