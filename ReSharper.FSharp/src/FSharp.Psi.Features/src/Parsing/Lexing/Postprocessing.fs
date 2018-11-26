namespace JetBrains.ReSharper.Plugins.FSharp.Psi.Features.Parsing.Lexing
open JetBrains.ReSharper.Psi.Parsing
open JetBrains.ReSharper.Psi.ExtensionsAPI.Tree
open JetBrains.ReSharper.Plugins.FSharp.Common.Util
open JetBrains.ReSharper.Plugins.FSharp.Psi.Parsing
open System.Collections.Generic
type Token = FSharpTokenType

module Postprocessing =
    let rec handleDummy (token : TokenNodeType) =
        match token with
        | :? Token.DummyNodeType as token -> handleDummy token.Token
        | _ -> token

    let odeclend = new NodeTypeSet(Token.ORIGHT_BLOCK_END, Token.OBLOCKEND, Token.ODECLEND)
    let infix =
        NodeTypeSet(
            Token.COMMA,
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
        NodeTypeSet(Token.THEN, Token.ELSE, Token.ELIF, Token.END, Token.RPAREN)
     |> odeclend.Union

    let tryBlockContinuator =
        NodeTypeSet(Token.FINALLY, Token.WITH)
     |> odeclend.Union

    let private thenBlockContinuator = odeclend
    let private doneDeclEnd = new NodeTypeSet(Token.DONE) |> odeclend.Union
    let private andDeclEnd = new NodeTypeSet(Token.AND) |> odeclend.Union
    let private interfaceContinuator = new NodeTypeSet(Token.END) |> odeclend.Union

    //todo: override locateToken to add EOF token [misonijnik]
    let private notNamespaceContinuator = new NodeTypeSet(Token.EOF, Token.NAMESPACE)
    let private typeContinuator =
        NodeTypeSet(Token.RBRACE, Token.WITH, Token.BAR, Token.AND, Token.END)
     |> odeclend.Union

    let private typeSeqBlockElementContinuator = new NodeTypeSet(Token.BAR, Token.OBLOCKBEGIN) |> odeclend.Union
    let private seqBlockElementContinuator =
        NodeTypeSet(
            Token.END, Token.AND, Token.WITH, Token.THEN, Token.RPAREN, Token.RBRACE,
            Token.RBRACK, Token.BAR_RBRACK, Token.RQUOTE_TYPED, Token.RQUOTE_UNTYPED)
     |> odeclend.Union

    let private longIdentifier = new NodeTypeSet(Token.IDENTIFIER, Token.DOT)
    let private longIdentifierOrGlobal = new NodeTypeSet(Token.GLOBAL) |> longIdentifier.Union
    let private atomicExprEndToken =
        NodeTypeSet(
            Token.IDENTIFIER, Token.RPAREN, Token.RBRACK, Token.RBRACE, Token.BAR_RBRACK, Token.END,
            Token.NULL, Token.FALSE, Token.TRUE, Token.UNDERSCORE)
     |> Token.Literals.Union
     |> Token.Strings.Union

    let private parenTokensBalance =
        NodeTypeDictionary<TokenNodeType>(
            [|
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

    let leftBraceBrack = new NodeTypeSet(Token.LBRACE, Token.LBRACK, Token.LBRACK_BAR)
    let leftParenBrackAndBegin = new NodeTypeSet(Token.LPAREN, Token.LBRACK, Token.LBRACK_BAR, Token.BEGIN)
    let leftParen = new NodeTypeSet(Token.BEGIN, Token.LPAREN) |> leftBraceBrack.Union
    let sigStructBegin = new NodeTypeSet(Token.SIG, Token.STRUCT, Token.BEGIN)
    let leftBraceBrackAndBeginTypeApp = new NodeTypeSet(Token.BEGIN, Token.BEGIN_TYPE_APP) |> leftBraceBrack.Union
    let classStructInterface = new NodeTypeSet(Token.CLASS, Token.STRUCT, Token.INTERFACE)

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
    let isLeftParen token = leftParen.[token]
    let isSigStructBegin token = sigStructBegin.[token]
    let isLeftBraceBrackAndBeginTypeApp token = leftBraceBrackAndBeginTypeApp.[token]
    let isClassStructInterface token = classStructInterface.[token]
    let isLeftParenBrackAndBegin token = leftParenBrackAndBegin.[token]
