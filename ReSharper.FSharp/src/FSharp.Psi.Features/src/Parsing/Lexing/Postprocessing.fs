namespace JetBrains.ReSharper.Plugins.FSharp.Psi.Features.Parsing.Lexing
open JetBrains.ReSharper.Psi.Parsing
open JetBrains.ReSharper.Psi.ExtensionsAPI.Tree
open JetBrains.ReSharper.Plugins.FSharp.Common.Util
open JetBrains.ReSharper.Plugins.FSharp.Psi.Parsing
open JetBrains.ReSharper.Plugins.FSharp.Psi.Features.Util.FSharpTokenUtil
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
    let leftToken =
        NodeTypeSet (Token.SIG, Token.LQUOTE_TYPED, Token.LQUOTE_UNTYPED, Token.LPAREN)
     |> leftBraceBrackAndBeginTypeApp.Union
    let classStructInterface = NodeTypeSet (Token.CLASS, Token.STRUCT, Token.INTERFACE)
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

    let balancingRule =
        NodeTypeSet
           (Token.END,
            Token.RPAREN,
            Token.RBRACE,
            Token.RBRACK,
            Token.BAR_RBRACK,
            Token.RQUOTE_TYPED,
            Token.RQUOTE_UNTYPED,
            Token.BEGIN_TYPE_APP)

    let namespaceDotRecGlobal = NodeTypeSet (Token.NAMESPACE, Token.DOT, Token.REC, Token.GLOBAL)
    let recIdentifier = NodeTypeSet (Token.REC, Token.IDENTIFIER)
    let recIdentifierGlobal = NodeTypeSet (Token.GLOBAL) |> recIdentifier.Union

    let forcesHeadContextClosure =
        NodeTypeSet
           (Token.ELSE,
            Token.ELIF,
            Token.DONE,
            Token.IN,
            Token.WITH,
            Token.FINALLY)
     |> balancingRule.Union

    let accessModifier = NodeTypeSet (Token.PUBLIC, Token.PRIVATE, Token.INTERNAL)
    let moduleDotRec = NodeTypeSet (Token.MODULE, Token.DOT, Token.REC)

    let valStaticAbstractMemberOverrideDefault =
        NodeTypeSet (Token.VAL, Token.STATIC, Token.ABSTRACT, Token.MEMBER, Token.OVERRIDE, Token.DEFAULT)
    let afterWith =
        NodeTypeSet (Token.RBRACE, Token.IDENTIFIER, Token.PUBLIC, Token.PRIVATE, Token.INTERNAL, Token.INLINE)
    let afterInterface =
        NodeTypeSet
           (Token.DEFAULT,
            Token.OVERRIDE,
            Token.INTERFACE,
            Token.NEW,
            Token.TYPE,
            Token.STATIC,
            Token.END,
            Token.MEMBER,
            Token.ABSTRACT,
            Token.INHERIT,
            Token.LBRACK_LESS)

    let (|Infix|) token = infix.[token]
    let isNonAssocInfixToken token = (|EQUALS|) token
    let isIfBlockContinuator token = ifBlockContinuator.[handleDummy token]
    let isTryBlockContinuator token = tryBlockContinuator.[handleDummy token]
    let isThenBlockContinuator token = thenBlockContinuator.[handleDummy token]
    let isDoContinuator token = doneDeclEnd.[handleDummy token]
    let isInterfaceContinuator token = interfaceContinuator.[handleDummy token]
    let isNamespaceContinuator token = not notNamespaceContinuator.[handleDummy token]
    let isTypeContinuator token = typeContinuator.[handleDummy token]
    let isForLoopContinuator token = doneDeclEnd.[handleDummy token]
    let isWhileBlockContinuator token = doneDeclEnd.[handleDummy token]
    let isLetContinuator token = andDeclEnd.[handleDummy token]
    let isTypeSeqBlockElementContinuator token = typeSeqBlockElementContinuator.[handleDummy token]
    let isSeqBlockElementContinuator token = (|Infix|) token || seqBlockElementContinuator.[handleDummy token]
    let isWithAugmentBlockContinuator token = (|END|) <| handleDummy token
    let isLongIdentifier token = longIdentifier.[token]
    let isLongIdentifierOrGlobal token = longIdentifierOrGlobal.[token]
    let isAtomicExprEndToken token = atomicExprEndToken.[token]
    let isParenTokensBalance leftToken rightToken =
        isNotNull leftToken && isNotNull rightToken && parenTokensBalance.[leftToken] == rightToken
    let isLeftBraceBrack token = leftBraceBrack.[token]
    let isLeftParenBegin token = leftParenBegin.[token]
    let (|LeftParen|) token = leftParen.[token]
    let isSigStructBegin token = sigStructBegin.[token]
    let isLeftBraceBrackAndBeginTypeApp token = leftBraceBrackAndBeginTypeApp.[token]
    let isClassStructInterface token = classStructInterface.[token]
    let isLeftParenBrackAndBegin token = leftParenBrackAndBegin.[token]
    let (|GrammarInTypes|) token = grammarInTypes.[token]
    let (|BeforeTypeApplication|) token = beforeTypeApplication.[token]
    let (|AdjacentPrefixTokens|) token = adjacentPrefixTokens.[token]
    let isSignedDigits token = signedDigits.[token]
    let isControlFlow token = controlFlow.[token]
    let isSemiSemi token = (|SEMICOLON_SEMICOLON|) token
    let (|ForcesHeadContextClosure|) token = forcesHeadContextClosure.[token]
    let (|BalancingRule|) token = balancingRule.[token]
    let (|NamespaceDotRecGlobal|) token = namespaceDotRecGlobal.[token]
    let (|RecIdentifierGlobal|) token = recIdentifierGlobal.[token]
    let (|AccessModifier|) token = accessModifier.[token]
    let (|ModuleDotRec|) token = moduleDotRec.[token]
    let (|RecIdentifier|) token = recIdentifier.[token]
    let isValStaticAbstractMemberOverrideDefault token = valStaticAbstractMemberOverrideDefault.[token]
    let (|LeftToken|) token = leftToken.[token]
    let (|AfterWith|) token = afterWith.[token]
    let (|AfterInterface|) token = afterInterface.[token]
