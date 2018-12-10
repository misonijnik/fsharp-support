namespace JetBrains.ReSharper.Plugins.FSharp.Psi.Features.Parsing.Lexing
open JetBrains.ReSharper.Plugins.FSharp.Psi.Parsing
open JetBrains.ReSharper.Plugins.FSharp.Psi.Parsing.Lexing
open JetBrains.ReSharper.Psi.Parsing
open JetBrains.DocumentModel.Impl
open JetBrains.Util.DataStructures
open JetBrains.ReSharper.Plugins.FSharp.Common.Util
open JetBrains.ReSharper.Plugins.FSharp.Psi.Features.Util

[<Struct>]
type FSharpLexicalFilterState =
    val CurrentTokenTup : TokenTup
    val PreviousTokenTup : TokenTup
    val DelayedTokens : ImmutableStack<TokenTup>
    val ContextStack : ImmutableStack<Context>
    new (currTokenTup, previousTokenTup, delayedTokens, contextStack) =
        { CurrentTokenTup = currTokenTup;
          PreviousTokenTup = previousTokenTup;
          DelayedTokens = delayedTokens;
          ContextStack = contextStack; }

open ImmutableStack
open Context
open Postprocessing
type LexicalFilter(buffer, lineIndex : LineIndex, compilingFsLib) =
    let myLexer = FSharpLexer buffer
    let tokenTupToState (token : TokenTup) =
        FSharpLexerState
            (token.CurrentTokenType,
             token.EndPosition.AbsoluteOffset,
             token.StartPosition.AbsoluteOffset,
             token.EndPosition.AbsoluteOffset,
             token.LexicalState)

    let compareTokenText text = LexerUtil.CompareTokenText (myLexer, text)
    let mutable previousTokenTup = TokenTup ()
    let mutable delayedTokenTups = ImmutableStack<TokenTup>.Empty
    let delayTokenTup token = mutablePush &delayedTokenTups token
    let mutable tokensThatNeedNoProcessingCount = 0
    let delayTokenTupNoProcessing state =
        delayTokenTup state
        tokensThatNeedNoProcessingCount <- tokensThatNeedNoProcessingCount + 1

    let mutable contextStack = ImmutableStack<Context>.Empty
    let popCtxt () = popCtxt &contextStack
    let pushCtxt tokenTup newCtxt = pushCtxt tokenTup newCtxt &contextStack
    let replaceCtxt p ctxt = replaceCtxt p ctxt &contextStack

    let getCurrentToken () =
        let internalPosition = myLexer.CurrentPosition
        let startLine = (lineIndex.GetLineColByOffset internalPosition.yy_buffer_start).Line
        let endLine = (lineIndex.GetLineColByOffset internalPosition.yy_buffer_end).Line
        TokenTup
            (internalPosition.currTokenType,
             Position (startLine.GetHashCode (),
                       lineIndex.GetLineStartOffset startLine,
                       internalPosition.yy_buffer_start),
             Position (endLine.GetHashCode (),
                       lineIndex.GetLineStartOffset endLine,
                       internalPosition.yy_buffer_end),
             internalPosition.yy_lexical_state)

    let getCurrentPosition () =
        FSharpLexicalFilterState (getCurrentToken (), previousTokenTup, delayedTokenTups, contextStack)

    let popNextTokenTup () =
        if not<| isEmpty delayedTokenTups
            then
                let tokenTup = mutablePop &delayedTokenTups
                myLexer.CurrentPosition <- tokenTupToState tokenTup
                tokenTup
            else
                myLexer.Advance ()
                getCurrentToken ()

    let peekInitial () =
        let initialLookaheadTokenTup  = popNextTokenTup ()
        delayTokenTup initialLookaheadTokenTup
        let ctxt = CtxtSeqBlock (FirstInSeqBlock, initialLookaheadTokenTup.StartPosOfTokenTup (), NoAddBlockEnd)
        mutablePush &contextStack ctxt
        initialLookaheadTokenTup

    let peekNextTokenTup () = 
        let tokenTup = popNextTokenTup ()
        delayTokenTup tokenTup
        tokenTup

    let peekNextToken () = (peekNextTokenTup ()).CurrentTokenType

     //----------------------------------------------------------------------------
     // Adjacency precedence rule
     //----------------------------------------------------------------------------

    let isAdjacent (leftTokenTup : TokenTup) (rightTokenTup : TokenTup) =
        let lparenStartPos = rightTokenTup.StartPosOfTokenTup ()
        let tokenEndPos = leftTokenTup.EndPosition
        (tokenEndPos = lparenStartPos)

    let nextTokenIsAdjacentLParenOrLBrack (tokenTup : TokenTup) (tokenTupOut : byref<_>)=
        let lookaheadTokenTup = peekNextTokenTup ()
        let token = lookaheadTokenTup.CurrentTokenType
        let res = (Token.LPAREN == token || Token.LBRACK == token) && isAdjacent tokenTup lookaheadTokenTup
        if res then tokenTupOut <- lookaheadTokenTup
        res

    let nextTokenIsAdjacent firstTokenTup =
        let lookaheadTokenTup = peekNextTokenTup ()
        isAdjacent firstTokenTup lookaheadTokenTup

    let peekAdjacentTypars indentation (tokenTup : TokenTup) =
        let lookaheadTokenTup = peekNextTokenTup ()
        let token = lookaheadTokenTup.CurrentTokenType
        if
            not (token == Token.INFIX_COMPARE_OP && compareTokenText "</"
         || token == Token.LESS || token == Token.BEGIN_TYPE_APP)
        then false
        else
            let tokenEndPos = tokenTup.EndPosition
            if isAdjacent tokenTup lookaheadTokenTup
            then
                let mutable stack = ImmutableStack<TokenTup>.Empty
                let rec scanAhead nParen = 
                    let lookaheadTokenTup = popNextTokenTup ()
                    let lookaheadToken = lookaheadTokenTup.CurrentTokenType
                    mutablePush &stack lookaheadTokenTup
                    let lookaheadTokenStartPos = lookaheadTokenTup.StartPosOfTokenTup ()
                    match lookaheadToken with
                    | _ when lookaheadToken == Token.EOF || lookaheadToken == Token.SEMICOLON_SEMICOLON -> false
                    | _ when indentation && lookaheadTokenStartPos < tokenEndPos -> false
                    | _ when lookaheadToken == Token.RPAREN || lookaheadToken == Token. RBRACK ->
                        let nParen = nParen - 1
                        if nParen > 0
                        then scanAhead nParen
                        else false
                    | _ when
                        lookaheadToken == Token.GREATER
                     || lookaheadToken == Token.GREATER_RBRACK
                     || lookaheadToken == Token.GREATER_BAR_RBRACK ->
                            let nParen = nParen - 1
                            let hasAfterOp = not (lookaheadToken == Token.GREATER)
                            if nParen > 0
                            then
                                mutablePush &stack lookaheadTokenTup
                                scanAhead nParen 
                            else
                                let mutable nextTokenTup = dummyTokenTup
                                if
                                    not hasAfterOp
                                 && nextTokenIsAdjacentLParenOrLBrack lookaheadTokenTup &nextTokenTup
                                 && nextTokenTup.CurrentTokenType == Token.LPAREN
                                then
                                    let dotTokenTup = peekNextTokenTup()
                                    mutablePush &stack (dotTokenTup.UseStartLocation(Token.HIGH_PRECEDENCE_PAREN_APP))
                                true
                    | _ when
                        isLeftParenBrackLess lookaheadToken
                     || (token == Token.INFIX_COMPARE_OP && compareTokenText "</") ->
                            scanAhead (nParen + 1)
                    | _ when
                        isGrammarInTypes lookaheadToken
                     || (token == Token.INFIX_AT_HAT_OP && (compareTokenText "^" || compareTokenText "^-"))
                     || (token == Token.INFIX_STAR_DIV_MOD_OP && compareTokenText "/") ->
                            scanAhead nParen
                    | _ ->
                        if nParen > 1
                        then scanAhead nParen 
                        else false

                let res = scanAhead 0
                for tokenTup in stack do
                    if res && tokenTup.CurrentTokenType == Token.LESS
                    then
                        delayTokenTup (tokenTup.UseLocation Token.BEGIN_TYPE_APP)
                    elif res && tokenTup.CurrentTokenType == Token.GREATER
                    then
                        delayTokenTup (tokenTup.UseLocation Token.END_TYPE_APP)
                    else delayTokenTup tokenTup
                res
            else false

    let returnToken (tokenTup : TokenTup) =
        previousTokenTup <- getCurrentToken ()
        let token = tokenTup.CurrentTokenType
        delayTokenTup tokenTup
        (popNextTokenTup ()).CurrentTokenType

    let rulesForBothSoftWhiteAndHardWhite (tokenTup : TokenTup) =
        let mutable lookaheadTokenTup = dummyTokenTup
        match tokenTup.CurrentTokenType with
        | token when token == Token.IDENTIFIER && (nextTokenIsAdjacentLParenOrLBrack tokenTup &lookaheadTokenTup) ->
            let hpa =
                match lookaheadTokenTup.CurrentTokenType with 
                | token when token == Token.LPAREN -> Token.HIGH_PRECEDENCE_PAREN_APP
                |  token when token == Token.LBRACK -> Token.HIGH_PRECEDENCE_BRACK_APP
                | _ -> failwith "unreachable"
            delayTokenTup (lookaheadTokenTup.UseStartLocation(hpa))
            delayTokenTup tokenTup
            true
        | token when isBeforeTypeApplication token && peekAdjacentTypars false tokenTup ->
            let lessTokenTup = peekNextTokenTup()
            delayTokenTup (lessTokenTup.UseStartLocation(Token.HIGH_PRECEDENCE_TYAPP))
            delayTokenTup tokenTup
            true
        | token when
                (isAdjacentPrefixTokens token
             || (token == Token.PLUS_MINUS_OP && (compareTokenText "+." || compareTokenText "-."))
             && nextTokenIsAdjacent tokenTup
             && not
                    (isAtomicExprEndToken (previousTokenTup.CurrentTokenType)
                 && (previousTokenTup.EndPosition = tokenTup.StartPosOfTokenTup ()))) ->

            let plusOrMinus =
                token == Token.PLUS || token == Token.MINUS
            let nextTokenTup = popNextTokenTup ()
            let delayMergedToken tok =
                delayTokenTup (TokenTup
                     (tok, tokenTup.StartPosition, nextTokenTup.EndPosition, tokenTup.LexicalState))
            let noMerge () =
                delayTokenTup nextTokenTup
                delayTokenTup (tokenTup.UseLocation (Token.ADJACENT_PREFIX_OP))

            if plusOrMinus && isSignedDigits nextTokenTup.CurrentTokenType
            then delayMergedToken (nextTokenTup.CurrentTokenType)
            else noMerge ()
            true
        | _ -> false

    let pushCtxtSeqBlockAt (p : TokenTup) addBlockBegin addBlockEnd = 
         if addBlockBegin then delayTokenTup(p.UseLocation(Token.OBLOCKBEGIN))
         pushCtxt p (CtxtSeqBlock (FirstInSeqBlock, p.StartPosOfTokenTup (), addBlockEnd))

    let pushCtxtSeqBlock addBlockBegin addBlockEnd =
        pushCtxtSeqBlockAt (peekNextTokenTup ()) addBlockBegin addBlockEnd

    let rec swTokenFetch () =
        let tokenTup = popNextTokenTup()
        let tokenReplaced = rulesForBothSoftWhiteAndHardWhite tokenTup
        if tokenReplaced then swTokenFetch() else returnToken tokenTup

    let insertComingSoonTokens (stack : byref<_>) (tokenTup : TokenTup) comingSoom isHere =
        delayTokenTupNoProcessing (tokenTup.UseLocation isHere)
        for i in 1..6 do
            delayTokenTupNoProcessing (tokenTup.UseEndLocation comingSoom)
        if not compilingFsLib then 
            let rec nextOuterMostInterestingContextIsNamespaceOrModule stack =
                let stackLength = length stack
                let getCtxt n = getCtxt stackLength contextStack n
                match getCtxt 1, getCtxt 2, getCtxt 3 with
                | _, (CtxtNamespaceBody _ | CtxtModuleBody _), _
                    when stackLength >= 2 -> true
                | _, (CtxtParen (token, _)), CtxtSeqBlock _
                    when stackLength >= 3 && (token == Token.BEGIN || token == Token.STRUCT) ->
                        nextOuterMostInterestingContextIsNamespaceOrModule (popN 2 stack)
                | _ when stackLength = 1 -> true
                | _ -> false

            while
                not (isEmpty stack) 
             && (not (nextOuterMostInterestingContextIsNamespaceOrModule stack))
             && (match peek stack with
                | CtxtParen (token, _) when isLeftParen token -> true
                | CtxtSeqBlock _ -> true
                | CtxtVanilla _ -> true
                | _ -> false) do

                match peek stack with
                | CtxtParen _
                | CtxtVanilla _
                | CtxtSeqBlock (_, _, NoAddBlockEnd) -> popCtxt ()
                | CtxtSeqBlock (_, _, AddBlockEnd) ->
                    popCtxt ()
                    delayTokenTupNoProcessing (tokenTup.UseEndLocation Token.OBLOCKEND)
                | CtxtSeqBlock (_, _, AddOneSidedBlockEnd) ->
                    popCtxt ()
                    delayTokenTupNoProcessing (tokenTup.UseEndLocation Token.ORIGHT_BLOCK_END)
                | _ -> failwith "impossible, the while loop guard just above prevents this"

    let insertTokenFromPrevPosToCurrentPos (previousTokenTup : TokenTup) tokenTup token =
        delayTokenTup tokenTup
        let lastTokenPos =
            let pos = previousTokenTup.EndPosition
            pos.ShiftColumnBy 1
        delayTokenTup (TokenTup (token, lastTokenPos, tokenTup.EndPosition, tokenTup.LexicalState))
        returnToken previousTokenTup

    let rec hwTokenFetch useBlockRule =
        let tokenTup = popNextTokenTup()
        let tokenReplaced = rulesForBothSoftWhiteAndHardWhite tokenTup
        if tokenReplaced then hwTokenFetch useBlockRule else

        let tokenStartPos = tokenTup.StartPosOfTokenTup ()
        let token = tokenTup.CurrentTokenType
        let tokenStartCol = tokenStartPos.Column

        let isSameLine () =
            if token == Token.EOF then false
            else ((peekNextTokenTup()).StartPosOfTokenTup ()).Line = tokenStartPos.Line

        let isControlFlowOrNotSameLine () =
            if token == Token.EOF then false
            else not (isSameLine ()) || (isControlFlow (peekNextToken ()))

        let isLongIdentEquals token =
            let rec isLongIdentEquals' () =
                let tokenTup = popNextTokenTup()
                let token = tokenTup.CurrentTokenType
                let res =
                    if token == Token.EQUALS || not (token == Token.DOT)
                    then token == Token.EQUALS
                    else
                        let tokenTup = popNextTokenTup()
                        let token = tokenTup.CurrentTokenType
                        let res = if token == Token.IDENTIFIER then isLongIdentEquals' () else false
                        delayTokenTup tokenTup
                        res
                delayTokenTup tokenTup
                res
            if not (token == Token.GLOBAL) && not (token == Token.IDENTIFIER) then false 
            else isLongIdentEquals' ()

        let reprocess () =
            delayTokenTup tokenTup
            hwTokenFetch useBlockRule

        let reprocessWithoutBlockRule () =
            delayTokenTup tokenTup
            hwTokenFetch false

        let insertToken token =
            delayTokenTup tokenTup
            returnToken (tokenTup.UseStartLocation token)

        let thereIsACtxtMemberBodyOnTheStackAndWeShouldPopStackForUpcomingMember contextStack =
            let mutable res = true
            let mutable existsMemberBody = false
            let mutable currentStack = contextStack
            let mutable countCtxtParenWithLParen = 0
            while ((not existsMemberBody) || res) && not (isEmpty currentStack) do
                let ctxt = peek currentStack
                if Context.isMemberBody ctxt then existsMemberBody <- true
                if Context.isParenWithLBrace ctxt then res <- false
                if Context.isParenWithLParen ctxt
                then
                    countCtxtParenWithLParen <- countCtxtParenWithLParen + 1
                    if countCtxtParenWithLParen >= 2 then res <- false
                currentStack <- pop currentStack
            existsMemberBody && not res
        ()

        let isSemiSemi = isSemiSemi token
        let stackLength = length contextStack
        let getCtxt n = getCtxt stackLength contextStack n

        match token, getCtxt 1, getCtxt 2, getCtxt 3 with
        | _ when tokensThatNeedNoProcessingCount > 0 ->
            tokensThatNeedNoProcessingCount <- tokensThatNeedNoProcessingCount - 1
            returnToken tokenTup
        | _, ctxt, _, _ when Context.tokenForcesHeadContextClosure token contextStack ->
            popCtxt ()
            let mutable token = null
            if endTokenForACtxt ctxt &token then insertToken token else reprocess ()
        | semiSemiToken, _, _, _ when semiSemiToken == Token.SEMICOLON_SEMICOLON && stackLength = 0 ->
            delayTokenTup (tokenTup.UseEndLocation Token.ORESET)
            returnToken tokenTup
        | oResetToken, _, _, _ when oResetToken == Token.ORESET && stackLength = 0 ->
            peekInitial () |> ignore
            hwTokenFetch true
        | inToken, _, _, _ when inToken == Token.IN && detectJoinInContext contextStack ->
            returnToken (tokenTup.UseLocation Token.JOIN_IN)
        | inToken, CtxtLetDecl (blockLet,offsidePos), _, _ when stackLength >= 1 ->
            if tokenStartCol < offsidePos.Column then () //todo: give good warnings, context is undented [misonijnik]
            popCtxt ()
            delayTokenTup (tokenTup.UseEndLocation (Token.ODUMMY.Create inToken))
            returnToken (if blockLet then (tokenTup.UseLocation Token.ODECLEND) else tokenTup)
        | doneToken, CtxtDo offsidePos, _, _ when doneToken == Token.DONE && stackLength >= 1 ->
            popCtxt ()
            delayTokenTup (tokenTup.UseLocation Token.ODECLEND)
            hwTokenFetch useBlockRule
        | t2, CtxtParen (t1, _), _, _ when stackLength >= 1 && isBalancingRule t2 && isParenTokensBalance t1 t2 ->
            popCtxt ()
            delayTokenTup (tokenTup.UseEndLocation (Token.ODUMMY.Create t2))
            returnToken tokenTup
        | endToken, CtxtWithAsAugment offsidePos, _, _ when
                stackLength >= 1 && not (tokenStartCol + 1 <= offsidePos.Column) ->

            popCtxt ()
            delayTokenTup (tokenTup.UseEndLocation (Token.ODUMMY.Create endToken))
            returnToken (tokenTup.UseLocation Token.OEND)
        | token, CtxtNamespaceHead (namespaceTokenPos, prevToken), _, _ when stackLength >= 1 ->
            if isTransitionRule prevToken token
                && namespaceTokenPos.Column < tokenStartPos.Column
                || prevToken == Token.IDENTIFIER && token == Token.DOT
            then
                replaceCtxt tokenTup (CtxtNamespaceHead (namespaceTokenPos, token))
                returnToken tokenTup
            elif token == Token.EOF
            then
                popCtxt ()
                returnToken tokenTup
            else
                delayTokenTup tokenTup
                pushCtxt tokenTup (CtxtNamespaceBody namespaceTokenPos)
                pushCtxtSeqBlockAt tokenTup true AddBlockEnd
                hwTokenFetch true
        | token, CtxtModuleHead (moduleTokenPos, prevToken), _, _ when stackLength >= 1 ->
            match prevToken, token with
            | _ when
                prevToken == Token.MODULE
             && token == Token.GLOBAL
             && moduleTokenPos.Column < tokenStartPos.Column ->
                    replaceCtxt tokenTup (CtxtModuleHead (moduleTokenPos, token))
                    returnToken tokenTup
            | _ when
                prevToken == Token.MODULE
             && isAccessModifier token
             && moduleTokenPos.Column < tokenStartPos.Column -> returnToken tokenTup
            | _ when
                isModuleDotRec prevToken
             && isRecIdentifier token
             && moduleTokenPos.Column < tokenStartPos.Column ->
                    replaceCtxt tokenTup (CtxtModuleHead (moduleTokenPos, token))
                    returnToken tokenTup
            | _ when token == Token.EQUALS && token == Token.COLON ->
                popCtxt ()
                pushCtxt tokenTup (CtxtModuleBody (moduleTokenPos, false))
                pushCtxtSeqBlock true AddBlockEnd 
                returnToken tokenTup
            | _ ->
                popCtxt ()
                if token == Token.EOF then returnToken tokenTup
                else
                    delayTokenTup tokenTup
                    pushCtxt tokenTup (CtxtModuleBody (moduleTokenPos,true))
                    pushCtxtSeqBlockAt tokenTup true AddBlockEnd
                    hwTokenFetch false
        | token, CtxtSeqBlock (_, offsidePos, addBlockEnd), _, _ when
               (let ruleIsExecuted () =
                    isSemiSemi && isNamespaceOrModuleBodyHead (pop contextStack)
                 || tokenStartCol + (grace myLexer offsidePos token (pop contextStack)) < offsidePos.Column
                stackLength >= 1 && ruleIsExecuted ()) ->

            popCtxt ()
            match addBlockEnd with 
            | AddBlockEnd -> insertToken Token.OBLOCKEND
            | AddOneSidedBlockEnd -> insertToken Token.ORIGHT_BLOCK_END
            | NoAddBlockEnd -> reprocess ()
        | _, CtxtVanilla (offsidePos, _), _, _ when
                stackLength >= 1
             && (isSemiSemi || tokenStartCol <= offsidePos.Column) ->

            popCtxt ()
            reprocess ()
        | greaterRBrackToken, CtxtSeqBlock (NotFirstInSeqBlock, offsidePos, addBlockEnd), _, _ when
                stackLength >= 1 && greaterRBrackToken == Token.GREATER_RBRACK ->

            replaceCtxt tokenTup (CtxtSeqBlock (FirstInSeqBlock, offsidePos, addBlockEnd))
            reprocessWithoutBlockRule ()
        | _, CtxtSeqBlock (FirstInSeqBlock, offsidePos, addBlockEnd), _, _ when stackLength >= 1 && useBlockRule ->
            replaceCtxt tokenTup (CtxtSeqBlock (NotFirstInSeqBlock, offsidePos, addBlockEnd))
            reprocessWithoutBlockRule ()
        | token, CtxtSeqBlock (NotFirstInSeqBlock, offsidePos, addBlockEnd), _, _ when
                stackLength >= 1 && useBlockRule && (ruleForSeqBlockIsExecuted token (pop contextStack))
             && tokenStartCol = offsidePos.Column
             && tokenStartPos.Line <> offsidePos.Line ->

            replaceCtxt tokenTup (CtxtSeqBlock (FirstInSeqBlock, offsidePos, addBlockEnd))
            insertTokenFromPrevPosToCurrentPos previousTokenTup tokenTup Token.OBLOCKSEP
        | _, CtxtLetDecl (true, offsidePos), _, _ when
                stackLength >= 1
             && (isSemiSemi
                 || (if isLetContinuator token then tokenStartCol + 1 else tokenStartCol) <= offsidePos.Column) ->

            popCtxt ()
            insertToken Token.ODECLEND
        | _, CtxtDo offsidePos, _, _ when
                stackLength >= 1
             && (isSemiSemi
                 || (if isDoContinuator token then tokenStartCol + 1 else tokenStartCol) <= offsidePos.Column) ->

            popCtxt ()
            insertToken Token.ODECLEND
        | _, CtxtInterfaceHead offsidePos, _, _ when
                stackLength >= 1
             && (isSemiSemi
                 || (if isInterfaceContinuator token then tokenStartCol + 1 else tokenStartCol) <= offsidePos.Column) ->

            popCtxt ()
            reprocess ()
        | _, CtxtTypeDefns offsidePos, _, _ when
                stackLength >= 1
             && (isSemiSemi
                 || (if isTypeContinuator token then tokenStartCol + 1 else tokenStartCol) <= offsidePos.Column) ->

            popCtxt ()
            reprocess ()
        | _, CtxtModuleBody (offsidePos, wholeFile), _, _ when
                stackLength >= 1 && ((isSemiSemi && not wholeFile) ||  tokenStartCol <= offsidePos.Column) ->

            popCtxt ()
            reprocess ()
        | _, CtxtNamespaceBody offsidePos, _, _ when
                stackLength >= 1
             && (if isNamespaceContinuator token then tokenStartCol + 1 else tokenStartCol) <= offsidePos.Column ->

            popCtxt ()
            reprocess ()
        | _, CtxtException offsidePos, _, _ when
                stackLength >= 1 && (isSemiSemi || tokenStartCol <= offsidePos.Column) ->

            popCtxt ()
            reprocess ()
        | _, CtxtMemberBody offsidePos, _, _ when
                stackLength >= 1 && (isSemiSemi || tokenStartCol <= offsidePos.Column) ->

            popCtxt()
            insertToken Token.ODECLEND
        | _, CtxtMemberHead offsidePos, _, _ when
                stackLength >= 1 && (isSemiSemi || tokenStartCol <= offsidePos.Column) ->

            popCtxt ()
            reprocess ()
        | _, CtxtIf offsidePos, _, _ when
                stackLength >= 1
             && (isSemiSemi
                 || (if isIfBlockContinuator token then tokenStartCol + 1 else tokenStartCol) <= offsidePos.Column) ->

            popCtxt ()
            reprocess ()
        | _, CtxtWithAsLet offsidePos, _, _ when
                stackLength >= 1
             && (isSemiSemi
                 || (if isLetContinuator token then tokenStartCol + 1 else tokenStartCol) <= offsidePos.Column) ->

            popCtxt ()
            insertToken Token.OEND
        | _, CtxtWithAsAugment offsidePos, _, _ when
                stackLength >= 1
             && (isSemiSemi
                 || (if isWithAugmentBlockContinuator token then tokenStartCol + 1 else tokenStartCol) <= offsidePos.Column) ->

            popCtxt ()
            insertToken Token.ODECLEND
        | _, CtxtMatch offsidePos, _, _ when
                stackLength >= 1 && (isSemiSemi || tokenStartCol <= offsidePos.Column) ->

            popCtxt ()
            reprocess ()
        | _, CtxtFor offsidePos, _, _ when
                stackLength >= 1
             && (isSemiSemi
                 || (if isForLoopContinuator token then tokenStartCol + 1 else tokenStartCol) <= offsidePos.Column) ->

            popCtxt ()
            reprocess ()
        | _, CtxtWhile offsidePos, _, _ when
                stackLength >= 1
             && (isSemiSemi
                 || (if isWhileBlockContinuator token then tokenStartCol + 1 else tokenStartCol) <= offsidePos.Column) ->

            popCtxt ()
            reprocess ()
        | _, CtxtWhen offsidePos, _, _ when
               stackLength >= 1 && (isSemiSemi || tokenStartCol <= offsidePos.Column) ->

            popCtxt ()
            reprocess ()
        | _, CtxtFun offsidePos, _, _ when
               stackLength >= 1 && (isSemiSemi || tokenStartCol <= offsidePos.Column) ->

            popCtxt ()
            insertToken Token.OEND
        | _, CtxtFunction offsidePos, _, _ when
               stackLength >= 1 && (isSemiSemi || tokenStartCol <= offsidePos.Column) ->

            popCtxt ()
            reprocess ()
        | _, CtxtTry offsidePos, _, _ when
                stackLength >= 1
             && (isSemiSemi
                 || (if isTryBlockContinuator token then tokenStartCol + 1 else tokenStartCol) <= offsidePos.Column) ->

            popCtxt ()
            reprocess ()
        | _, CtxtThen offsidePos, _, _ when
                stackLength >= 1
             && (isSemiSemi
                 || (if isThenBlockContinuator token then tokenStartCol + 1 else tokenStartCol) <= offsidePos.Column) ->

            popCtxt ()
            reprocess ()
        | _, CtxtElse offsidePos, _, _ when
                stackLength >= 1 && (isSemiSemi || tokenStartCol <= offsidePos.Column) ->

            popCtxt ()
            reprocess ()
        | _, CtxtMatchClauses (leadingBar, offsidePos), _, _ when
                stackLength >= 1
             && (isSemiSemi
                 || if token == Token.BAR
                    then
                        let cond1 = tokenStartCol + (if leadingBar then 0 else 2)  < offsidePos.Column
                        let cond2 = tokenStartCol + (if leadingBar then 1 else 2)  < offsidePos.Column
                        if (cond1 <> cond2) then () //todo: give good warnings, context is undented [misonijnik]
                        cond1
                    elif token == Token.END then tokenStartCol + (if leadingBar then -1 else 1) < offsidePos.Column
                    else tokenStartCol + (if leadingBar then -1 else 1) < offsidePos.Column) ->

            popCtxt ()
            insertToken Token.OEND
        | namespaceToken, _, _, _  when stackLength >= 1 && namespaceToken == Token.NAMESPACE ->
            pushCtxt tokenTup (CtxtNamespaceHead (tokenStartPos, token))
            returnToken tokenTup
        | moduleToken, _, _, _ when stackLength >= 1 && moduleToken == Token.MODULE ->
            insertComingSoonTokens &contextStack tokenTup Token.MODULE_COMING_SOON Token.MODULE_IS_HERE
            pushCtxt tokenTup (CtxtModuleHead (tokenStartPos, token))
            hwTokenFetch useBlockRule
        | exceptionToken, _, _, _ when stackLength >= 1 && exceptionToken == Token.EXCEPTION ->
            pushCtxt tokenTup (CtxtException tokenStartPos)
            returnToken tokenTup
        | letToken, (CtxtMemberHead _ as ctxt), _, _ when
                stackLength >= 1 && (letToken == Token.LET || letToken == Token.USE) ->

            let oTokent = if letToken == Token.LET then Token.OLET else Token.OUSE
            let startPos = match ctxt with CtxtMemberHead startPos -> startPos | _ -> tokenStartPos
            popCtxt ()
            pushCtxt tokenTup (CtxtLetDecl (true, startPos))
            returnToken (tokenTup.UseLocation oTokent)
        | letToken, ctxt, _, _ when
                stackLength >= 1 && (letToken == Token.LET || letToken == Token.USE) ->
 
            let oTokent = if letToken == Token.LET then Token.OLET else Token.OUSE
            let blockLet = match ctxt with | CtxtSeqBlock _ | CtxtMatchClauses _ -> true | _ -> false
            pushCtxt tokenTup (CtxtLetDecl (blockLet, tokenStartPos))
            returnToken (if blockLet then (tokenTup.UseLocation oTokent) else tokenTup)
        | letBangToken, ctxt, _, _ when
                stackLength >= 1 && (letBangToken == Token.LET_BANG || letBangToken == Token.USE_BANG) ->
 
            let oTokent = if letBangToken == Token.LET_BANG then Token.OLET_BANG else Token.OUSE_BANG
            let blockLet = match ctxt with | CtxtSeqBlock _ -> true | _ -> false
            pushCtxt tokenTup (CtxtLetDecl (blockLet, tokenStartPos))
            returnToken (if blockLet then (tokenTup.UseLocation oTokent) else tokenTup)
        | token, _, _, _ when
                isValStaticAbstractMemberOverrideDefault token
             && thereIsACtxtMemberBodyOnTheStackAndWeShouldPopStackForUpcomingMember contextStack ->

            delayTokenTupNoProcessing tokenTup
            while (match peek contextStack with CtxtMemberBody _ -> false | _ -> true) do
                let mutable tokenOut = null
                if endTokenForACtxt (peek contextStack) &tokenOut
                then  
                    popCtxt ()
                    delayTokenTupNoProcessing (tokenTup.UseLocation tokenOut)
                else popCtxt ()
            popCtxt ()
            hwTokenFetch useBlockRule
        | token, ctxt, _, _ when
                stackLength >= 1
             && isValStaticAbstractMemberOverrideDefault token
             && (match ctxt with CtxtMemberHead _ -> false | _ -> true) ->

            pushCtxt tokenTup (CtxtMemberHead tokenStartPos)
            returnToken tokenTup
        | token, ctxt, _, _ when stackLength >= 1 && isAccessModifier token && peekNextToken() == Token.NEW ->
            pushCtxt tokenTup (CtxtMemberHead tokenStartPos)
            returnToken tokenTup
        | newToken, ctxt, _, _ when
                stackLength >= 1
             && peekNextToken() == Token.LPAREN
             && (match ctxt with CtxtMemberHead _ -> false | _ -> true) ->
            pushCtxt tokenTup (CtxtMemberHead(tokenStartPos))
            returnToken tokenTup
        | equalsToken, CtxtLetDecl _, _, _ when stackLength >= 1 && equalsToken == Token.EQUALS ->
            pushCtxtSeqBlock true AddBlockEnd
            returnToken tokenTup
        | equalsToken, CtxtTypeDefns _, _, _ when stackLength >= 1 && equalsToken == Token.EQUALS ->
            pushCtxtSeqBlock true AddBlockEnd
            returnToken tokenTup
        | lazyAsserToken, _, _, _ when lazyAsserToken == Token.LAZY || lazyAsserToken == Token.ASSERT ->
            if isControlFlowOrNotSameLine ()
            then
                let token = if lazyAsserToken == Token.LAZY then Token.OLAZY else Token.OASSERT
                pushCtxtSeqBlock true AddBlockEnd
                returnToken (tokenTup.UseLocation token)
            else returnToken tokenTup

    interface ILexer with
        member x.Start () = myLexer.Start ()
        member x.Advance () = myLexer.Advance ()
        member x.CurrentPosition
            with get () = (x :> ILexer<FSharpLexicalFilterState>).CurrentPosition :> obj
            and set value =
                (x :> ILexer<FSharpLexicalFilterState>).CurrentPosition <- (value :?> FSharpLexicalFilterState)
        member x.TokenType = myLexer.TokenType
        member x.TokenStart = myLexer.TokenStart
        member x.TokenEnd = myLexer.TokenEnd
        member x.Buffer = myLexer.Buffer

    interface ILexer<FSharpLexicalFilterState> with
        member x.CurrentPosition
            with get () = getCurrentPosition ()
            and set (value : FSharpLexicalFilterState) =
                let token = value.CurrentTokenTup
                let internalPosition = tokenTupToState token
                myLexer.CurrentPosition <- internalPosition
                previousTokenTup <- value.PreviousTokenTup
                delayedTokenTups <- value.DelayedTokens
                contextStack <- value.ContextStack
