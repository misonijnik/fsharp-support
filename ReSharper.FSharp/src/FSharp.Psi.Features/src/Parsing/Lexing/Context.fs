namespace JetBrains.ReSharper.Plugins.FSharp.Psi.Features.Parsing.Lexing
open JetBrains.ReSharper.Plugins.FSharp.Psi.Features.Util
open JetBrains.ReSharper.Plugins.FSharp.Common.Util
open JetBrains.ReSharper.Psi.Parsing
open JetBrains.ReSharper.Plugins.FSharp.Psi.Parsing
open JetBrains.Util.DataStructures

type AddBlockEnd = AddBlockEnd | NoAddBlockEnd | AddOneSidedBlockEnd
type FirstInSequence = FirstInSeqBlock | NotFirstInSeqBlock

type Context =
    | CtxtDo of Position
    | CtxtElse of Position
    | CtxtException of Position
    | CtxtFor of Position
    | CtxtFun of Position
    | CtxtFunction of Position
    | CtxtIf of Position
    | CtxtInterfaceHead of Position
    | CtxtLetDecl of bool * Position
    | CtxtMatch of Position
    | CtxtMatchClauses of bool * Position
    | CtxtMemberBody of Position
    | CtxtMemberHead of Position
    | CtxtModuleBody of Position * bool
    | CtxtModuleHead of Position * TokenNodeType
    | CtxtNamespaceBody of Position
    | CtxtNamespaceHead of Position * TokenNodeType
    | CtxtParen of TokenNodeType * Position
    | CtxtSeqBlock of FirstInSequence * Position * AddBlockEnd
    | CtxtThen of Position
    | CtxtTry of Position
    | CtxtTypeDefns of Position
    | CtxtVanilla of Position * bool
    | CtxtWhen of Position
    | CtxtWhile of Position
    | CtxtWithAsAugment of Position
    | CtxtWithAsLet of Position
    
    member x.StartPos = 
        match x with 
        | CtxtDo p
        | CtxtElse p
        | CtxtException p
        | CtxtFor p
        | CtxtFun p
        | CtxtFunction p
        | CtxtIf p
        | CtxtInterfaceHead p
        | CtxtLetDecl (_, p)
        | CtxtMatch p
        | CtxtMatchClauses (_, p)
        | CtxtMemberBody p
        | CtxtMemberHead p
        | CtxtModuleBody (p, _)
        | CtxtModuleHead (p, _)
        | CtxtNamespaceBody p
        | CtxtNamespaceHead (p, _)
        | CtxtParen (_, p)
        | CtxtSeqBlock (_, p, _)
        | CtxtThen p
        | CtxtTry p
        | CtxtTypeDefns p
        | CtxtVanilla (p, _)
        | CtxtWhen p
        | CtxtWhile p
        | CtxtWithAsAugment p
        | CtxtWithAsLet p -> p

    member c.StartCol = c.StartPos.Column

module Context =
    open ImmutableStack
    let dummyCtxt = CtxtDo (Position ())
    let getCtxt stackLength stack n = if stackLength >= n then peekN n stack else dummyCtxt

    let isVanilla = function
        | CtxtVanilla _ -> true
        | _ -> false

    let isMemberBody = function
        | CtxtMemberBody _ -> true
        | _ -> false

    let isParenWithLBrace = function
        | CtxtParen (token, _) -> token == Token.LBRACE
        | _ -> false

    let isParenWithLParen = function
        | CtxtParen (token, _) -> token == Token.LPAREN
        | _ -> false

    let detectJoinInContext stack =
        let rec check stack =
            match peek stack with
            | CtxtParen(token, _) -> FSharpTokenType.LBRACE == token
            | CtxtSeqBlock _
            | CtxtDo _
            | CtxtFor _ -> check (pop stack)
            | _ -> false

        isVanilla (peek stack) && check (pop stack)

    let rec unindentationLimit (newCtxt : Context) strict (stack : ImmutableStack<Context>) =
        let stackLength = length stack
        let getCtxt n = getCtxt stackLength stack n
        if stackLength = 0 then PositionWithColumn(newCtxt.StartPos, -1) else
            match newCtxt, peek stack, getCtxt 2, getCtxt 3, getCtxt 4 with
            | _, CtxtVanilla _, _, _, _ -> unindentationLimit newCtxt strict (pop stack)
            | _, CtxtSeqBlock _, _, _, _
            | _, CtxtParen _, _, _, _ when not strict -> unindentationLimit newCtxt strict (pop stack)
            | _,  (CtxtMatch _ as ctxt1), CtxtSeqBlock _, (CtxtParen(token, _) as ctxt2), _ when
                stackLength >= 3 && (Token.BEGIN == token || Token.LPAREN == token) ->
                    if ctxt1.StartCol <= ctxt2.StartCol 
                         then PositionWithColumn(ctxt1.StartPos, ctxt2.StartCol) 
                         else PositionWithColumn(ctxt1.StartPos, ctxt2.StartCol)
            | CtxtMatchClauses _, CtxtFunction _, CtxtSeqBlock _, (CtxtLetDecl _ as limitCtxt), _ when
                stackLength >= 3 -> PositionWithColumn(limitCtxt.StartPos, limitCtxt.StartCol)
            | CtxtMatchClauses _, CtxtFunction _, _, _, _ -> unindentationLimit newCtxt strict (pop stack)
            | _, CtxtMatchClauses _, (CtxtTry _ as limitCtxt), _, _ when stackLength >= 2 ->
                PositionWithColumn(limitCtxt.StartPos, limitCtxt.StartCol)
            | _, CtxtFun _, _, _, _ -> unindentationLimit newCtxt false (pop stack)
            | _, CtxtParen (token,_), CtxtSeqBlock _, _, _ when
                stackLength >= 2 && Postprocessing.isLeftBraceBrack token ->
                    unindentationLimit newCtxt false (pop stack)
            | _, CtxtParen (token,_), CtxtVanilla _, CtxtSeqBlock _, _ when
                stackLength >= 3 && Postprocessing.isLeftBraceBrack token ->
                    unindentationLimit newCtxt false (pop stack)
            | _, CtxtSeqBlock _, CtxtParen (token,_), CtxtVanilla _, CtxtSeqBlock _ when
                stackLength >= 4 && Postprocessing.isLeftBraceBrack token ->
                    unindentationLimit newCtxt false (pop stack)
            | CtxtSeqBlock _, CtxtElse _, (CtxtIf _ as limitCtxt), _, _ when
                stackLength >= 2 -> PositionWithColumn(limitCtxt.StartPos, limitCtxt.StartCol)
            | CtxtWithAsAugment _, ((CtxtInterfaceHead _ | CtxtMemberHead _ | CtxtException _ | CtxtTypeDefns _) as limitCtxt), _, _, _ ->
                PositionWithColumn(limitCtxt.StartPos, limitCtxt.StartCol)
            | _, (CtxtWithAsAugment _ | CtxtThen _ | CtxtElse _ | CtxtDo _ ), _, _, _
            | _, CtxtFunction _, _, _, _ -> unindentationLimit newCtxt false (pop stack)
            | _, CtxtParen (token, _), CtxtSeqBlock _, (CtxtModuleBody (_, false) as limitCtxt), _ when
                stackLength >= 3 && Postprocessing.isSigStructBegin token ->
                    PositionWithColumn(limitCtxt.StartPos, limitCtxt.StartCol + 1)
            | _, CtxtParen (token ,_), CtxtSeqBlock _, CtxtThen _, (CtxtIf _ as limitCtxt)
            | _, CtxtParen (token ,_), CtxtSeqBlock _, CtxtElse _, (CtxtIf _ as limitCtxt) when
                stackLength >= 4 && Postprocessing.isLeftParenBegin token ->
                    PositionWithColumn(limitCtxt.StartPos, limitCtxt.StartCol + 1)
            | _, CtxtParen (token, _), CtxtVanilla _, (CtxtSeqBlock _ as limitCtxt), _ when
                stackLength >= 3 && Postprocessing.isLeftBraceBrackAndBeginTypeApp token ->
                    PositionWithColumn(limitCtxt.StartPos, limitCtxt.StartCol + 1)
            | _, CtxtParen (token, _), CtxtSeqBlock _, (CtxtTypeDefns _ as limitCtxt), _ when
                stackLength >= 3 && Postprocessing.isClassStructInterface token ->
                    PositionWithColumn(limitCtxt.StartPos, limitCtxt.StartCol + 1)
            | _, CtxtSeqBlock _, CtxtParen (token, _), CtxtVanilla _, (CtxtSeqBlock _ as limitCtxt) when
                stackLength >= 4 && Postprocessing.isLeftParenBrackAndBegin token ->
                    PositionWithColumn(limitCtxt.StartPos, limitCtxt.StartCol + 1)
            | CtxtSeqBlock _, CtxtParen (token ,_), CtxtSeqBlock _, ((CtxtTypeDefns _ | CtxtLetDecl _ | CtxtMemberBody _ | CtxtWithAsLet _) as limitCtxt), _ when
                stackLength >= 3 && Postprocessing.isLeftParenBrackAndBegin token ->
                    PositionWithColumn(limitCtxt.StartPos, limitCtxt.StartCol + 1)
            | (CtxtIf   _ | CtxtElse _ | CtxtThen _), (CtxtIf _ as limitCtxt), _, _, _
            | CtxtDo _, ((CtxtFor  _ | CtxtWhile _) as limitCtxt), _, _, _ ->
                PositionWithColumn(limitCtxt.StartPos, limitCtxt.StartCol)
            | _, (CtxtInterfaceHead _ as limitCtxt), _, _, _
            | _, (CtxtNamespaceHead _ as limitCtxt), _, _, _
            | _, (CtxtModuleHead _ as limitCtxt), _, _, _
            | _, (CtxtException _ as limitCtxt), _, _, _
            | _, (CtxtModuleBody (_, false) as limitCtxt), _, _, _
            | _, (CtxtIf _ as limitCtxt), _, _, _
            | _, (CtxtWithAsLet _ as limitCtxt), _, _, _
            | _, (CtxtLetDecl _ as limitCtxt), _, _, _
            | _, (CtxtMemberHead _ as limitCtxt), _, _, _
            | _, (CtxtMemberBody _ as limitCtxt), _, _, _ -> PositionWithColumn(limitCtxt.StartPos, limitCtxt.StartCol + 1)
            | _, (CtxtParen _ as limitCtxt), _, _, _
            | _, (CtxtFor _ as limitCtxt), _, _, _
            | _, (CtxtWhen _ as limitCtxt), _, _, _
            | _, (CtxtWhile _ as limitCtxt), _, _, _
            | _, (CtxtTypeDefns _ as limitCtxt), _, _, _
            | _, (CtxtMatch _ as limitCtxt), _, _, _
            | _, (CtxtModuleBody (_, true) as limitCtxt), _, _, _
            | _, (CtxtNamespaceBody _ as limitCtxt), _, _, _
            | _, (CtxtTry _ as limitCtxt), _, _, _
            | _, (CtxtMatchClauses _ as limitCtxt), _, _, _
            | _, (CtxtSeqBlock _ as limitCtxt), _, _, _ -> PositionWithColumn(limitCtxt.StartPos, limitCtxt.StartCol)

    let pushCtxt tokenTup newCtxt (stack : byref<_>) =
        match newCtxt with
        | CtxtVanilla _ -> ()
        | _ ->
            let p1 = unindentationLimit newCtxt true stack
            let c2 = newCtxt.StartCol
            if c2 < p1.Column then () //todo: give good warnings, context is undented [misonijnik]
        stack <- push stack newCtxt

    let popCtxt (stack : byref<_>) = if isEmpty stack then () else mutablePop &stack |> ignore

    let replaceCtxt p ctxt (stack : byref<_>) =
        popCtxt &stack
        pushCtxt p ctxt &stack

    let tokenBalancesHeadContext token stack =
        let stackLength = length stack
        let getCtxt n = getCtxt stackLength stack n
        if stackLength = 0 then false else
            match token, peek stack, getCtxt 2 with
            | endToken, CtxtWithAsAugment _, _ when endToken == Token.END -> true
            | elseIfToken, CtxtIf _, _ when elseIfToken == Token.ELSE || elseIfToken == Token.ELIF -> true
            | doneToken, CtxtDo _, _ when doneToken == Token.DONE -> true
            | withToken, CtxtMatch _, _
            | withToken, CtxtException _, _
            | withToken, CtxtMemberHead _, _
            | withToken, CtxtInterfaceHead _, _
            | withToken, CtxtTry _, _
            | withToken, CtxtTypeDefns _, _
            | withToken, CtxtMemberBody _, _ when withToken == Token.WITH -> true
            | withToken, CtxtSeqBlock _, CtxtParen (lbraceToken, _)
                when stackLength >= 2 && withToken == Token.WITH && lbraceToken == Token.LBRACE -> true
            | finallyToken, CtxtTry _, _ when finallyToken == Token.FINALLY -> true
            | inToken, (CtxtFor _ | CtxtLetDecl _), _ when inToken = Token.IN -> true
            | inToken, _, _ when inToken = Token.IN && detectJoinInContext stack -> true
            | semicolonSemicolonToken, CtxtSeqBlock _, CtxtNamespaceBody _
            | semicolonSemicolonToken, CtxtSeqBlock _, CtxtModuleBody (_, true)
                when stackLength >= 2 && semicolonSemicolonToken == Token.SEMICOLON_SEMICOLON -> true
            | t2, CtxtParen (t1, _), _ -> Postprocessing.isParenTokensBalance t1 t2
            | _ -> false

    let endTokenForACtxt ctxt (tokenOut : byref<TokenNodeType>) =
        match ctxt with 
        | CtxtFun _
        | CtxtMatchClauses _
        | CtxtWithAsLet _ ->
            tokenOut <- Token.OEND
            true
        | CtxtWithAsAugment _  ->
            tokenOut <- Token.ODECLEND
            true
        | CtxtDo _
        | CtxtLetDecl (true, _) ->
            tokenOut <- Token.ODECLEND
            true
        | CtxtSeqBlock(_,_,AddBlockEnd) ->
            tokenOut <- Token.OBLOCKEND
            true
        | CtxtSeqBlock(_,_,AddOneSidedBlockEnd) ->  
            tokenOut <- Token.ORIGHT_BLOCK_END
            true
        | _ -> false

    let tokenForcesHeadContextClosure token stack =
        not (isEmpty stack) && (token == Token.EOF
            || (token == Token.SEMICOLON_SEMICOLON && not (tokenBalancesHeadContext token stack))
            || (Postprocessing.isForcesHeadContextClosure token
                && not (tokenBalancesHeadContext token stack)
                && Postprocessing.suffixExists (tokenBalancesHeadContext token) stack))

    let grace (lexer : ILexer) (offsidePos : Position) token stack =
        let otherResult () =
            if Postprocessing.isInfix token then lexer.GetTokenLength () + 1 else 0
        if length stack = 0 then otherResult ()
        else
            match token, peek stack with
            | barToken, CtxtTypeDefns _ when barToken == Token.BAR -> 2
            | _, CtxtTypeDefns posType when
                offsidePos.Column = posType.Column
             && not (Postprocessing.isTypeSeqBlockElementContinuator token) -> -1
            | namespaceToken, CtxtNamespaceBody posNamespace when
                offsidePos.Column = posNamespace.Column
             && namespaceToken == Token.NAMESPACE -> -1
            | _ -> otherResult ()

    let isNamespaceOrModuleBodyHead stack =
        length stack >= 1
     && match peek stack with
        | CtxtNamespaceBody _
        | CtxtModuleBody (_, true) -> true
        | _ -> false

    let ruleForSeqBlockIsExecuted token stack =
        let res =
            match getCtxt (length stack) stack 1 with
            | CtxtNamespaceBody _ -> token == Token.NAMESPACE
            | CtxtTypeDefns _ -> Postprocessing.isTypeSeqBlockElementContinuator token
            | _ -> Postprocessing.isSeqBlockElementContinuator token
        not res
