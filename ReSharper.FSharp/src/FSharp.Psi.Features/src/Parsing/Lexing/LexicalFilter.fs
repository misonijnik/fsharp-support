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
    val TokensThatNeedNoProcessingCount : int
    new (currTokenTup, previousTokenTup, delayedTokens, contextStack, tokensThatNeedNoProcessingCount) =
        { CurrentTokenTup = currTokenTup;
          PreviousTokenTup = previousTokenTup;
          DelayedTokens = delayedTokens;
          ContextStack = contextStack;
          TokensThatNeedNoProcessingCount = tokensThatNeedNoProcessingCount; }

open ImmutableStack
open FSharpTokenUtil
open Context
open Postprocessing
type LexicalFilter(buffer, compilingFsLib, lightSyntaxStatus) =
    let myLexer = FSharpLexerWithLineIndexing buffer
    let tokenTupToState (token : TokenTup) =
        FSharpLexerLineIndexingState
            (token.CurrentTokenType,
             token.EndPosition.AbsoluteOffset,
             token.StartPosition.AbsoluteOffset,
             token.EndPosition.AbsoluteOffset,
             token.StartPosition.StartOfLineAbsoluteOffset,
             token.EndPosition.StartOfLineAbsoluteOffset,
             token.StartPosition.Line,
             token.EndPosition.Line,
             token.TypeParenLevel,
             token.LexicalState)

    let mutable currentTokenType : TokenNodeType = null
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
        TokenTup
            (internalPosition.CurrentTokenType,
             Position (internalPosition.LineTokenStart,
                       internalPosition.BufferLineTokenStart,
                       internalPosition.BufferStart),
             Position (internalPosition.LineTokenEnd,
                       internalPosition.BufferLineTokenEnd,
                       internalPosition.BufferEnd),
             myLexer.TypeParenLevel,
             internalPosition.LexicalState)
 
    let makeStep () =
        myLexer.Advance ()
        myLexer.AdvanceWhile skippedTokens |> ignore

    let getCurrentPosition () =
        FSharpLexicalFilterState
            (getCurrentToken (),
             previousTokenTup,
             delayedTokenTups,
             contextStack,
             tokensThatNeedNoProcessingCount)

    let popNextTokenTup' takeStep =
        if not <| isEmpty delayedTokenTups
            then
                let tokenTup = mutablePop &delayedTokenTups
                myLexer.CurrentPosition <- tokenTupToState tokenTup
                tokenTup
            else
                if takeStep then myLexer.Advance ()
                myLexer.AdvanceWhile skippedTokens |> ignore
                getCurrentToken ()

    let popNextTokenTup () = popNextTokenTup' true

    let peekInitial takeStep =
        let initialLookaheadTokenTup = popNextTokenTup' takeStep
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

    let (|INFIX_COMPARE_OP_WITH|) text token =
        (|INFIX_COMPARE_OP|) token && compareTokenText text

    let (|INFIX_AT_HAT_OP_WITH|) text token =
        (|INFIX_AT_HAT_OP|) token && compareTokenText text

    let (|INFIX_STAR_DIV_MOD_OP_WITH|) text token =
        (|INFIX_STAR_DIV_MOD_OP|) token && compareTokenText text

    let isAdjacent (leftTokenTup : TokenTup) (rightTokenTup : TokenTup) =
        let lparenStartPos = rightTokenTup.StartPosOfTokenTup ()
        let tokenEndPos = leftTokenTup.EndPosition
        (tokenEndPos = lparenStartPos)

    let nextTokenIsAdjacentLParenOrLBrack (tokenTup : TokenTup) (tokenTupOut : byref<_>)=
        let lookaheadTokenTup = peekNextTokenTup ()
        let token = lookaheadTokenTup.CurrentTokenType
        match token with
        | LPAREN true
        | LBRACK true when isAdjacent tokenTup lookaheadTokenTup ->
            tokenTupOut <- lookaheadTokenTup
            true
        | _  -> false

    let nextTokenIsAdjacent firstTokenTup =
        let lookaheadTokenTup = peekNextTokenTup ()
        isAdjacent firstTokenTup lookaheadTokenTup

    let peekAdjacentTypars indentation (tokenTup : TokenTup) =
        let lookaheadTokenTup = peekNextTokenTup ()
        let token = lookaheadTokenTup.CurrentTokenType
        match token with
        | INFIX_COMPARE_OP_WITH "</" true
        | LESS true
        | BEGIN_TYPE_APP true ->
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
                    | EOF true
                    | SEMICOLON_SEMICOLON true -> false
                    | _ when indentation && lookaheadTokenStartPos < tokenEndPos -> false
                    | RPAREN true
                    | RBRACK true ->
                        let nParen = nParen - 1
                        if nParen > 0
                        then scanAhead nParen
                        else false
                    | GREATER true
                    | END_TYPE_APP true
                    | GREATER_RBRACK true
                    | GREATER_BAR_RBRACK true ->
                            let nParen = nParen - 1
                            let hasAfterOp = not ((|GREATER|) lookaheadToken)
                            if nParen > 0
                            then
                                scanAhead nParen
                            else
                                let mutable nextTokenTup = dummyTokenTup
                                if
                                    not hasAfterOp
                                 && nextTokenIsAdjacentLParenOrLBrack lookaheadTokenTup &nextTokenTup
                                 && (|LPAREN|) nextTokenTup.CurrentTokenType
                                then
                                    let dotTokenTup = peekNextTokenTup()
                                    mutablePush &stack (dotTokenTup.UseStartLocation(Token.HIGH_PRECEDENCE_PAREN_APP))
                                true
                    | INFIX_COMPARE_OP true ->
                        let mutable nParen = nParen
                        let mutable hasAfterOp = false
                        using (LexerStateCookie.Create (myLexer :> ILexer<FSharpLexerLineIndexingState>)) (fun _ ->
                        let state =
                            FSharpLexerLineIndexingState.Create
                                (lookaheadTokenTup.StartPosition.AbsoluteOffset,
                                 lookaheadTokenTup.StartPosition.StartOfLineAbsoluteOffset,
                                 lookaheadTokenTup.StartPosition.Line,
                                 nParen,
                                 if nParen > 1
                                 then FSharpLexerWithLineIndexing.AdjacentTyApp
                                 else FSharpLexerWithLineIndexing.InitAdjacentTyAppState)
                        myLexer.Start (state, lookaheadTokenTup.EndPosition.AbsoluteOffset)
                        mutablePop &stack |> ignore
                        while not <| (|EOF|) myLexer.TokenType do
                            let tokenTup = getCurrentToken ()
                            if not <| (|GREATER|) tokenTup.CurrentTokenType then hasAfterOp <- true
                            mutablePush &stack tokenTup
                            makeStep ()
                        nParen <- myLexer.TypeParenLevel
                        myLexer.Start (state, myLexer.Buffer.Length))
                        if nParen > 0
                            then scanAhead nParen
                            else
                                let mutable nextTokenTup = dummyTokenTup
                                if
                                    not hasAfterOp
                                 && nextTokenIsAdjacentLParenOrLBrack lookaheadTokenTup &nextTokenTup
                                 && (|LPAREN|) nextTokenTup.CurrentTokenType
                                then
                                    let dotTokenTup = peekNextTokenTup()
                                    mutablePush &stack (dotTokenTup.UseStartLocation(Token.HIGH_PRECEDENCE_PAREN_APP))
                                true
                    | LPAREN true
                    | LESS true
                    | BEGIN_TYPE_APP true
                    | LBRACK true
                    | LBRACK_LESS true
                    | INFIX_COMPARE_OP_WITH "</" true -> scanAhead (nParen + 1)
                    | GrammarInTypes true
                    | INFIX_AT_HAT_OP_WITH "^" true
                    | INFIX_AT_HAT_OP_WITH "^-" true
                    | INFIX_STAR_DIV_MOD_OP_WITH "/" true -> scanAhead nParen
                    | _ ->
                        if nParen > 1
                        then scanAhead nParen
                        else false

                let res = scanAhead 0
                while not <| isEmpty stack do
                    let tokenTup = mutablePop &stack
                    let token = tokenTup.CurrentTokenType
                    match token with
                    | LESS true when res -> delayTokenTup (tokenTup.UseLocation Token.BEGIN_TYPE_APP)
                    | GREATER true when res -> delayTokenTup (tokenTup.UseLocation Token.END_TYPE_APP)
                    | _ -> delayTokenTup tokenTup
                res
            else false
        | _ -> false

    let returnToken (tokenTup : TokenTup) =
        previousTokenTup <- getCurrentToken ()
        let token = tokenTup.CurrentTokenType
        delayTokenTup tokenTup
        popNextTokenTup ()

    let rulesForBothSoftWhiteAndHardWhite (tokenTup : TokenTup) =
        let (|PLUS_MINUS_OP_WITH|) text token = (|PLUS_MINUS_OP|) token && compareTokenText text
        let mutable lookaheadTokenTup = dummyTokenTup
        let token = tokenTup.CurrentTokenType
        match token with
        | IDENTIFIER true when nextTokenIsAdjacentLParenOrLBrack tokenTup &lookaheadTokenTup ->
            let hpa =
                match lookaheadTokenTup.CurrentTokenType with 
                | LPAREN true -> Token.HIGH_PRECEDENCE_PAREN_APP
                | LBRACK true -> Token.HIGH_PRECEDENCE_BRACK_APP
                | _ -> failwith "unreachable"
            delayTokenTup (lookaheadTokenTup.UseStartLocation (hpa))
            delayTokenTup tokenTup
            true
        | BeforeTypeApplication true when peekAdjacentTypars false tokenTup ->
            let lessTokenTup = peekNextTokenTup ()
            delayTokenTup (lessTokenTup.UseStartLocation(Token.HIGH_PRECEDENCE_TYAPP))
            delayTokenTup tokenTup
            true
        | AdjacentPrefixTokens true
        | PLUS_MINUS_OP_WITH "+." true
        | PLUS_MINUS_OP_WITH "-." true when
                nextTokenIsAdjacent tokenTup
             && not
                    (isAtomicExprEndToken (previousTokenTup.CurrentTokenType)
                 && (previousTokenTup.EndPosition = tokenTup.StartPosOfTokenTup ())) ->

            let plusOrMinus = (|PLUS|) token || (|MINUS|) token
            let nextTokenTup = popNextTokenTup ()
            let delayMergedToken tok =
                delayTokenTup (tokenTup.UseLocation tok)
            let noMerge () =
                delayTokenTup nextTokenTup
                delayTokenTup (tokenTup.UseLocation (Token.ADJACENT_PREFIX_OP))

            if plusOrMinus && isSignedDigits nextTokenTup.CurrentTokenType
            then delayMergedToken (nextTokenTup.CurrentTokenType)
            else noMerge ()
            true
        | _ -> false

    let pushCtxtSeqBlockAt (p : TokenTup) addBlockBegin addBlockEnd = 
         if addBlockBegin then delayTokenTup(p.UseStartLocation(Token.OBLOCKBEGIN))
         pushCtxt p (CtxtSeqBlock (FirstInSeqBlock, p.StartPosOfTokenTup (), addBlockEnd))

    let pushCtxtSeqBlock addBlockBegin addBlockEnd =
        pushCtxtSeqBlockAt (peekNextTokenTup ()) addBlockBegin addBlockEnd

    let rec swTokenFetch () =
        let tokenTup = popNextTokenTup()
        if tokenTup.CurrentTokenType == null then tokenTup else
        let tokenReplaced = rulesForBothSoftWhiteAndHardWhite tokenTup
        if tokenReplaced then swTokenFetch() else returnToken tokenTup

    let insertComingSoonTokens' (tokenTup : TokenTup) comingSoon isHere =
        delayTokenTupNoProcessing (tokenTup.UseLocation isHere)
        for i in 1..6 do
            delayTokenTupNoProcessing (tokenTup.UseEndLocation comingSoon)

    let insertComingSoonTokens (stack : byref<_>) (tokenTup : TokenTup) comingSoon isHere =
        insertComingSoonTokens' tokenTup comingSoon isHere
        if not compilingFsLib then 
            let rec nextOuterMostInterestingContextIsNamespaceOrModule stack =
                let stackLength = length stack
                let getCtxt n = getCtxt stackLength contextStack n
                match getCtxt 1, getCtxt 2, getCtxt 3 with
                | _, (CtxtNamespaceBody _ | CtxtModuleBody _), _ -> true
                | _, (CtxtParen ((BEGIN true | STRUCT true), _)), CtxtSeqBlock _ ->
                    nextOuterMostInterestingContextIsNamespaceOrModule (popN 2 stack)
                | _ when stackLength = 1 -> true
                | _ -> false

            while
                not (isEmpty stack) 
             && (not (nextOuterMostInterestingContextIsNamespaceOrModule stack))
             && (match peek stack with
                | CtxtParen (LeftParen true, _) -> true
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
        returnToken (tokenTup.UseStartLocation token)

    let rec hwTokenFetch useBlockRule =
        let tokenTup = popNextTokenTup()
        if tokenTup.CurrentTokenType == null then tokenTup else
        let tokenReplaced = rulesForBothSoftWhiteAndHardWhite tokenTup
        if tokenReplaced then hwTokenFetch useBlockRule else

        let tokenStartPos = tokenTup.StartPosOfTokenTup ()
        let token = tokenTup.CurrentTokenType
        let tokenStartCol = tokenStartPos.Column

        let isSameLine () =
            match token with
            | EOF true -> false
            | _  -> ((peekNextTokenTup()).StartPosOfTokenTup ()).Line = tokenStartPos.Line

        let isControlFlowOrNotSameLine () =
            match token with
            | EOF true -> false
            | _ -> not (isSameLine ()) || (isControlFlow (peekNextToken ()))

        let isLongIdentEquals token =
            let rec isLongIdentEquals' () =
                let tokenTup = popNextTokenTup()
                let token = tokenTup.CurrentTokenType
                let res =
                    match token with
                    | DOT true ->
                        let tokenTup = popNextTokenTup()
                        let token = tokenTup.CurrentTokenType
                        let res = if token == Token.IDENTIFIER then isLongIdentEquals' () else false
                        delayTokenTup tokenTup
                        res
                    | EQUALS true -> true
                    | _ -> false
                delayTokenTup tokenTup
                res
            match token with
            | GLOBAL true
            | IDENTIFIER true -> isLongIdentEquals' ()
            | _ -> false

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

        let isSemiSemi = isSemiSemi token
        let startCol b = if b then tokenStartCol + 1 else tokenStartCol
        let stackLength = length contextStack
        let getCtxt n = getCtxt stackLength contextStack n
        let ctxt1 = getCtxt 1
        let ctxt2 = getCtxt 2
        let ctxt3 = getCtxt 3

        let rec firstChapter () =
            match token, ctxt1, ctxt2, ctxt3 with
            | _ when tokensThatNeedNoProcessingCount > 0 ->
                tokensThatNeedNoProcessingCount <- tokensThatNeedNoProcessingCount - 1
                returnToken tokenTup
            | _, ctxt, _, _ when Context.tokenForcesHeadContextClosure token contextStack ->
                popCtxt ()
                let mutable token = null
                if endTokenForACtxt ctxt &token then insertToken token else reprocess ()
            | SEMICOLON_SEMICOLON true, _, _, _ when stackLength = 0 ->
                delayTokenTup (tokenTup.UseEndLocation Token.ORESET)
                returnToken tokenTup
            | ORESET true, _, _, _ when stackLength = 0 ->
                peekInitial true |> ignore
                hwTokenFetch true
            | IN true, _, _, _ when detectJoinInContext contextStack ->
                returnToken (tokenTup.UseLocation Token.JOIN_IN)
            | IN true , CtxtLetDecl (blockLet,offsidePos), _, _ ->
                if tokenStartCol < offsidePos.Column then () //todo: give good warnings, context is undented [misonijnik]
                popCtxt ()
                delayTokenTup (tokenTup.UseEndLocation (Token.ODUMMY.Create token))
                returnToken (if blockLet then (tokenTup.UseLocation Token.ODECLEND) else tokenTup)
            | DONE true, CtxtDo offsidePos, _, _ ->
                popCtxt ()
                delayTokenTup (tokenTup.UseLocation Token.ODECLEND)
                hwTokenFetch useBlockRule
            |  (BalancingRule true as t2), CtxtParen (t1, _), _, _ when isParenTokensBalance t1 t2 ->
                popCtxt ()
                delayTokenTup (tokenTup.UseEndLocation (Token.ODUMMY.Create t2))
                returnToken tokenTup
            | END true, CtxtWithAsAugment offsidePos, _, _ when not (tokenStartCol + 1 <= offsidePos.Column) ->
                popCtxt ()
                delayTokenTup (tokenTup.UseEndLocation (Token.ODUMMY.Create token))
                returnToken (tokenTup.UseLocation Token.OEND)
            | _, CtxtNamespaceHead (namespaceTokenPos, prevToken), _, _ ->
                match prevToken, token with
                | NamespaceDotRecGlobal true, RecIdentifierGlobal true
                | IDENTIFIER true, DOT true when
                    namespaceTokenPos.Column < tokenStartPos.Column ->
                    replaceCtxt tokenTup (CtxtNamespaceHead (namespaceTokenPos, token))
                    returnToken tokenTup
                | _ ->
                    popCtxt ()
                    match token with
                    | EOF true -> returnToken tokenTup
                    | _ ->
                        delayTokenTup tokenTup
                        pushCtxt tokenTup (CtxtNamespaceBody namespaceTokenPos)
                        pushCtxtSeqBlockAt tokenTup true AddBlockEnd
                        hwTokenFetch true
            | _, CtxtModuleHead (moduleTokenPos, prevToken), _, _ ->
                match prevToken, token with
                | MODULE true, GLOBAL true when moduleTokenPos.Column < tokenStartPos.Column ->
                    replaceCtxt tokenTup (CtxtModuleHead (moduleTokenPos, token))
                    returnToken tokenTup
                | MODULE true, AccessModifier true when moduleTokenPos.Column < tokenStartPos.Column ->
                    returnToken tokenTup
                | ModuleDotRec true, RecIdentifier true when moduleTokenPos.Column < tokenStartPos.Column ->
                    replaceCtxt tokenTup (CtxtModuleHead (moduleTokenPos, token))
                    returnToken tokenTup
                | _, (EQUALS true | COLON true) ->
                    popCtxt ()
                    pushCtxt tokenTup (CtxtModuleBody (moduleTokenPos, false))
                    pushCtxtSeqBlock true AddBlockEnd 
                    returnToken tokenTup
                | _ ->
                    popCtxt ()
                    match token with
                    | EOF true -> returnToken tokenTup
                    | _ ->
                        delayTokenTup tokenTup
                        pushCtxt tokenTup (CtxtModuleBody (moduleTokenPos,true))
                        pushCtxtSeqBlockAt tokenTup true AddBlockEnd
                        hwTokenFetch false
            | token, CtxtSeqBlock (_, offsidePos, addBlockEnd), _, _ when
                    (isSemiSemi && isNamespaceOrModuleBodyHead (pop contextStack)
                  || tokenStartCol + (grace myLexer offsidePos token (pop contextStack)) < offsidePos.Column) ->
    
                popCtxt ()
                match addBlockEnd with 
                | AddBlockEnd -> insertToken Token.OBLOCKEND
                | AddOneSidedBlockEnd -> insertToken Token.ORIGHT_BLOCK_END
                | NoAddBlockEnd -> reprocess ()
            | _ -> secondChapter ()

        and secondChapter () =
            match token, ctxt1, ctxt2, ctxt3 with
            | _, CtxtVanilla (offsidePos, _), _, _ when isSemiSemi || tokenStartCol <= offsidePos.Column ->
                popCtxt ()
                reprocess ()
            | GREATER_RBRACK true, CtxtSeqBlock (NotFirstInSeqBlock, offsidePos, addBlockEnd), _, _ ->
                replaceCtxt tokenTup (CtxtSeqBlock (FirstInSeqBlock, offsidePos, addBlockEnd))
                reprocessWithoutBlockRule ()
            | _, CtxtSeqBlock (FirstInSeqBlock, offsidePos, addBlockEnd), _, _ when useBlockRule ->
                replaceCtxt tokenTup (CtxtSeqBlock (NotFirstInSeqBlock, offsidePos, addBlockEnd))
                reprocessWithoutBlockRule ()
            | token, CtxtSeqBlock (NotFirstInSeqBlock, offsidePos, addBlockEnd), _, _ when
                    useBlockRule && ruleForSeqBlockIsExecuted token (pop contextStack)
                 && tokenStartCol = offsidePos.Column
                 && tokenStartPos.Line <> offsidePos.Line ->
    
                replaceCtxt tokenTup (CtxtSeqBlock (FirstInSeqBlock, offsidePos, addBlockEnd))
                insertTokenFromPrevPosToCurrentPos previousTokenTup tokenTup Token.OBLOCKSEP
            | _, CtxtLetDecl (true, offsidePos), _, _ when
                    isSemiSemi || startCol (isLetContinuator token) <= offsidePos.Column ->
                popCtxt ()
                insertToken Token.ODECLEND
            | _, CtxtDo offsidePos, _, _ when isSemiSemi || startCol (isDoContinuator token) <= offsidePos.Column ->
                popCtxt ()
                insertToken Token.ODECLEND
            | _, CtxtInterfaceHead offsidePos, _, _ when 
                    isSemiSemi || startCol (isInterfaceContinuator token) <= offsidePos.Column ->
                popCtxt ()
                reprocess ()
            | _, CtxtTypeDefns offsidePos, _, _ when
                    isSemiSemi || startCol (isTypeContinuator token) <= offsidePos.Column ->
                popCtxt ()
                reprocess ()
            | _, CtxtModuleBody (offsidePos, wholeFile), _, _ when
                    isSemiSemi && not wholeFile || tokenStartCol <= offsidePos.Column ->
                popCtxt ()
                reprocess ()
            | _, CtxtNamespaceBody offsidePos, _, _ when startCol (isNamespaceContinuator token) <= offsidePos.Column ->
                popCtxt ()
                reprocess ()
            | _, CtxtException offsidePos, _, _ when isSemiSemi || tokenStartCol <= offsidePos.Column ->
                popCtxt ()
                reprocess ()
            | _, CtxtMemberBody offsidePos, _, _ when isSemiSemi || tokenStartCol <= offsidePos.Column ->
                popCtxt()
                insertToken Token.ODECLEND
            | _, CtxtMemberHead offsidePos, _, _ when isSemiSemi || tokenStartCol <= offsidePos.Column ->
                popCtxt ()
                reprocess ()
            | _, CtxtIf offsidePos, _, _ when isSemiSemi || startCol (isIfBlockContinuator token) <= offsidePos.Column ->
                popCtxt ()
                reprocess ()
            | _, CtxtWithAsLet offsidePos, _, _ when isSemiSemi || startCol (isLetContinuator token) <= offsidePos.Column ->
                popCtxt ()
                insertToken Token.OEND
            | _, CtxtWithAsAugment offsidePos, _, _ when 
                 isSemiSemi || startCol (isWithAugmentBlockContinuator token) <= offsidePos.Column ->
                popCtxt ()
                insertToken Token.ODECLEND
            | _, CtxtMatch offsidePos, _, _ when isSemiSemi || tokenStartCol <= offsidePos.Column ->
                popCtxt ()
                reprocess ()
            | _, CtxtFor offsidePos, _, _ when isSemiSemi || (startCol (isForLoopContinuator token) <= offsidePos.Column) ->
                popCtxt ()
                reprocess ()
            | _ -> thirdChapter ()

        and thirdChapter () =
            match token, ctxt1, ctxt2, ctxt3 with
            | _, CtxtWhile offsidePos, _, _ when  
                    isSemiSemi || startCol (isWhileBlockContinuator token) <= offsidePos.Column ->
                popCtxt ()
                reprocess ()
            | _, CtxtWhen offsidePos, _, _ when isSemiSemi || tokenStartCol <= offsidePos.Column ->
                popCtxt ()
                reprocess ()
            | _, CtxtFun offsidePos, _, _ when isSemiSemi || tokenStartCol <= offsidePos.Column ->
                popCtxt ()
                insertToken Token.OEND
            | _, CtxtFunction offsidePos, _, _ when isSemiSemi || tokenStartCol <= offsidePos.Column ->
                popCtxt ()
                reprocess ()
            | _, CtxtTry offsidePos, _, _ when isSemiSemi || startCol (isTryBlockContinuator token) <= offsidePos.Column ->
                popCtxt ()
                reprocess ()
            | _, CtxtThen offsidePos, _, _ when  
                    isSemiSemi || startCol (isThenBlockContinuator token) <= offsidePos.Column ->
                popCtxt ()
                reprocess ()
            | _, CtxtElse offsidePos, _, _ when isSemiSemi || tokenStartCol <= offsidePos.Column ->
                popCtxt ()
                reprocess ()
            | _, CtxtMatchClauses (leadingBar, offsidePos), _, _ when
                    (isSemiSemi
                 || match token with
                    | BAR true ->
                         let cond1 = tokenStartCol + (if leadingBar then 0 else 2) < offsidePos.Column
                         let cond2 = tokenStartCol + (if leadingBar then 1 else 2) < offsidePos.Column
                         if (cond1 <> cond2) then () //todo: give good warnings, context is undented [misonijnik]
                         cond1
                    | END true -> tokenStartCol + (if leadingBar then -1 else 1) < offsidePos.Column
                    | _ -> tokenStartCol + (if leadingBar then -1 else 1) < offsidePos.Column) ->
    
                popCtxt ()
                insertToken Token.OEND
            | NAMESPACE true, _, _, _  when stackLength >= 1 ->
                pushCtxt tokenTup (CtxtNamespaceHead (tokenStartPos, token))
                returnToken tokenTup
            | MODULE true, _, _, _ when stackLength >= 1 ->
                insertComingSoonTokens &contextStack tokenTup Token.MODULE_COMING_SOON Token.MODULE_IS_HERE
                pushCtxt tokenTup (CtxtModuleHead (tokenStartPos, token))
                hwTokenFetch useBlockRule
            | EXCEPTION true, _, _, _ when stackLength >= 1 ->
                pushCtxt tokenTup (CtxtException tokenStartPos)
                returnToken tokenTup
            | (LET true | USE true), (CtxtMemberHead _ as ctxt), _, _ ->
                let oTokent = if (|LET|) token then Token.OLET else Token.OUSE
                let startPos = match ctxt with CtxtMemberHead startPos -> startPos | _ -> tokenStartPos
                popCtxt ()
                pushCtxt tokenTup (CtxtLetDecl (true, startPos))
                returnToken (tokenTup.UseLocation oTokent)
            | (LET true | USE true), ctxt, _, _ when stackLength >= 1 ->
                let oTokent = if (|LET|) token then Token.OLET else Token.OUSE
                let blockLet = match ctxt with | CtxtSeqBlock _ | CtxtMatchClauses _ -> true | _ -> false
                pushCtxt tokenTup (CtxtLetDecl (blockLet, tokenStartPos))
                returnToken (if blockLet then (tokenTup.UseLocation oTokent) else tokenTup)
            | (LET_BANG true | USE_BANG true), ctxt, _, _ when stackLength >= 1 ->
                let oTokent = if (|LET_BANG|) token then Token.OLET_BANG else Token.OUSE_BANG
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
            | _ -> fourthChapter ()

        and fourthChapter () =
            match token, ctxt1, ctxt2, ctxt3 with
            | token, ctxt, _, _ when
                    stackLength >= 1
                 && isValStaticAbstractMemberOverrideDefault token
                 && (match ctxt with CtxtMemberHead _ -> false | _ -> true) ->

                pushCtxt tokenTup (CtxtMemberHead tokenStartPos)
                returnToken tokenTup
            | AccessModifier true, ctxt, _, _ when stackLength >= 1 && (|NEW|) (peekNextToken ()) ->
                pushCtxt tokenTup (CtxtMemberHead tokenStartPos)
                returnToken tokenTup
            | NEW true, ctxt, _, _ when
                    stackLength >= 1 
                 && (|LPAREN|) (peekNextToken ())
                 && (match ctxt with CtxtMemberHead _ -> false | _ -> true) ->
    
                pushCtxt tokenTup (CtxtMemberHead(tokenStartPos))
                returnToken tokenTup
            | EQUALS true, (CtxtLetDecl _ | CtxtTypeDefns _), _, _ ->
                pushCtxtSeqBlock true AddBlockEnd
                returnToken tokenTup
            | (LAZY true | ASSERT true), _, _, _ ->
                if isControlFlowOrNotSameLine ()
                then
                    let token = if (|LAZY|) token then Token.OLAZY else Token.OASSERT
                    pushCtxtSeqBlock true AddBlockEnd
                    returnToken (tokenTup.UseLocation token)
                else returnToken tokenTup
            | EQUALS true, CtxtWithAsLet _, _, _
            | EQUALS true, CtxtVanilla (_, true), CtxtSeqBlock _, (CtxtWithAsLet _ | CtxtParen (LBRACE true, _)) ->
                if isControlFlowOrNotSameLine()
                then pushCtxtSeqBlock true AddBlockEnd
                else pushCtxtSeqBlock false NoAddBlockEnd
                returnToken tokenTup
            | EQUALS true, CtxtMemberHead offsidePos, _, _ ->
                replaceCtxt tokenTup (CtxtMemberBody offsidePos)
                pushCtxtSeqBlock true AddBlockEnd
                returnToken tokenTup
            | LeftToken true, _, _, _ ->
                pushCtxt tokenTup (CtxtParen (token, tokenStartPos))
                pushCtxtSeqBlock false NoAddBlockEnd
                returnToken tokenTup
            | STRUCT true, CtxtSeqBlock _, (CtxtModuleBody _ | CtxtTypeDefns _), _ ->
                pushCtxt tokenTup (CtxtParen (token, tokenStartPos))
                pushCtxtSeqBlock false NoAddBlockEnd
                returnToken tokenTup
            | RARROW true, (CtxtWhile _ | CtxtFor _ | CtxtWhen _ | CtxtMatchClauses _ | CtxtFun _), _, _
            | RARROW true, CtxtSeqBlock _, (CtxtParen ((LBRACK true | LBRACE true | LBRACK_BAR true), _)
                                          | CtxtDo _ 
                                          | CtxtWhile _ 
                                          | CtxtFor _ 
                                          | CtxtWhen _ 
                                          | CtxtMatchClauses _ 
                                          | CtxtTry _ 
                                          | CtxtThen _ 
                                          | CtxtElse _), _ ->
                pushCtxtSeqBlock false AddOneSidedBlockEnd
                returnToken tokenTup
            | LARROW true, _, _, _ when isControlFlowOrNotSameLine () ->
                pushCtxtSeqBlock true AddBlockEnd
                returnToken tokenTup
            | (DO true | DO_BANG true), _, _, _ ->
                let odoToken = if (|DO|) token then Token.ODO else Token.ODO_BANG
                pushCtxt tokenTup (CtxtDo tokenStartPos)
                pushCtxtSeqBlock true AddBlockEnd
                returnToken (tokenTup.UseLocation odoToken)
            | Infix true, ctxt, _, _ when not (isSameLine ()) && (match ctxt with CtxtMatchClauses _ -> false | _ -> true) ->
                pushCtxtSeqBlock false NoAddBlockEnd
                returnToken tokenTup
            | WITH true, (CtxtTry _ | CtxtMatch _), _, _ ->
                let lookaheadTokenTup = peekNextTokenTup ()
                let lookaheadTokenStartPos = lookaheadTokenTup.StartPosOfTokenTup () 
                let leadingBar = (|BAR|) (peekNextToken ())
                pushCtxt lookaheadTokenTup (CtxtMatchClauses (leadingBar, lookaheadTokenStartPos))
                returnToken (tokenTup.UseLocation Token.OWITH)
            | FINALLY true, CtxtTry _, _, _ ->
                pushCtxtSeqBlock true AddBlockEnd
                returnToken tokenTup
            | WITH true, ((CtxtException _ | CtxtTypeDefns _ | CtxtMemberHead _ | CtxtInterfaceHead _ | CtxtMemberBody _) as limCtxt), _, _
            | WITH true, (CtxtSeqBlock _ as limCtxt), CtxtParen(LBRACE true, _), _ ->
                let lookaheadTokenTup = peekNextTokenTup ()
                let lookaheadTokenStartPos = lookaheadTokenTup.StartPosOfTokenTup ()
                let lookaheadToken = lookaheadTokenTup.CurrentTokenType 
                match lookaheadToken with
                | AfterWith true ->
                    let offsidePos = 
                        if lookaheadTokenStartPos.Column > tokenTup.EndPosition.Column
                        then tokenStartPos
                        else limCtxt.StartPos
                    pushCtxt tokenTup (CtxtWithAsLet offsidePos)
                    let isFollowedByLongIdentEquals = 
                        let tokenTup = popNextTokenTup ()
                        let res = isLongIdentEquals (tokenTup.CurrentTokenType)
                        delayTokenTup tokenTup
                        res
    
                    if isFollowedByLongIdentEquals then pushCtxtSeqBlock false NoAddBlockEnd                      
                    returnToken (tokenTup.UseLocation Token.OWITH)
                | _ ->
                    if ((|LBRACK_LESS|) lookaheadToken) && (lookaheadTokenStartPos.Line = tokenTup.StartPosition.Line)
                    then
                        let offsidePos = tokenStartPos
                        pushCtxt tokenTup (CtxtWithAsLet offsidePos)
                        returnToken (tokenTup.UseLocation Token.OWITH)
                    else
                        let offsidePos = limCtxt.StartPos
                        pushCtxt tokenTup (CtxtWithAsAugment offsidePos)
                        pushCtxtSeqBlock true AddBlockEnd
                        returnToken tokenTup
            | _ -> fifthChapter ()

        and fifthChapter () =
            match token, ctxt1, ctxt2, ctxt3 with
            | WITH true, _, _, _ ->
                pushCtxt tokenTup (CtxtWithAsAugment tokenStartPos)
                pushCtxtSeqBlock true AddBlockEnd
                returnToken tokenTup
            | FUNCTION true, _, _, _ ->
                let lookaheadTokenTup = peekNextTokenTup ()
                let lookaheadTokenStartPos = lookaheadTokenTup.StartPosOfTokenTup ()
                let leadingBar = (|BAR|) (peekNextToken ())
                pushCtxt tokenTup (CtxtFunction tokenStartPos)
                pushCtxt lookaheadTokenTup (CtxtMatchClauses (leadingBar, lookaheadTokenStartPos))
                returnToken (tokenTup.UseLocation Token.OFUNCTION)
            | THEN true, _, _, _ ->
                pushCtxt tokenTup (CtxtThen tokenStartPos)
                pushCtxtSeqBlock true AddBlockEnd
                returnToken (tokenTup.UseLocation Token.OTHEN)
            | ELSE true, _, _ ,_ ->
                let lookaheadTokenTup = peekNextTokenTup ()
                let lookaheadTokenStartPos = lookaheadTokenTup.StartPosOfTokenTup ()
                match peekNextToken () with 
                | IF true when isSameLine() ->
                    popNextTokenTup () |> ignore
                    pushCtxt tokenTup (CtxtIf tokenStartPos)
                    returnToken (tokenTup.UseLocation Token.ELIF)
                | _ ->
                    pushCtxt tokenTup (CtxtElse tokenStartPos)
                    pushCtxtSeqBlock true AddBlockEnd
                    returnToken (tokenTup.UseLocation Token.OELSE)
            | (ELIF true | IF true), _, _, _ ->
                pushCtxt tokenTup (CtxtIf tokenStartPos)
                returnToken tokenTup
            | (MATCH true| MATCH_BANG true), _, _,_ ->
                pushCtxt tokenTup (CtxtMatch tokenStartPos)
                returnToken tokenTup
            | FOR true, _, _, _ ->
                pushCtxt tokenTup (CtxtFor tokenStartPos)
                returnToken tokenTup
            | WHILE true, _, _, _ ->
                pushCtxt tokenTup (CtxtWhile tokenStartPos)
                returnToken tokenTup
            | WHEN true, CtxtSeqBlock _, _, _ ->
                pushCtxt tokenTup (CtxtWhen tokenStartPos)
                returnToken tokenTup
            | FUN true, _, _, _ ->
                pushCtxt tokenTup (CtxtFun tokenStartPos)
                returnToken (tokenTup.UseLocation Token.OFUN)
            | INTERFACE true, _, _, _ ->
                let lookaheadTokenTup = peekNextTokenTup()
                let lookaheadTokenStartPos = lookaheadTokenTup.StartPosOfTokenTup ()
                let lookaheadToken = lookaheadTokenTup.CurrentTokenType
                match lookaheadToken with
                | AfterInterface true ->
                    pushCtxt tokenTup (CtxtParen (token, tokenStartPos))
                    pushCtxtSeqBlock true AddBlockEnd
                    returnToken tokenTup
                | _ ->
                    pushCtxt tokenTup (CtxtInterfaceHead tokenStartPos)
                    returnToken (tokenTup.UseLocation Token.OINTERFACE_MEMBER)
            | CLASS true, _, _, _ ->
                pushCtxt tokenTup (CtxtParen (token, tokenStartPos))
                pushCtxtSeqBlock true AddBlockEnd
                returnToken tokenTup
            | TYPE true, _, _, _ ->
                insertComingSoonTokens &contextStack tokenTup Token.TYPE_COMING_SOON Token.TYPE_IS_HERE
                pushCtxt tokenTup (CtxtTypeDefns tokenStartPos)
                hwTokenFetch useBlockRule
            | TRY true, _, _, _ ->
                pushCtxt tokenTup (CtxtTry tokenStartPos)
                pushCtxtSeqBlock false AddOneSidedBlockEnd
                returnToken tokenTup
            | OBLOCKBEGIN true, _, _, _ -> returnToken tokenTup
            | ODUMMY true, _, _, _ -> hwTokenFetch useBlockRule
            | _, CtxtSeqBlock _, _, _ ->
                pushCtxt tokenTup (CtxtVanilla(tokenStartPos, isLongIdentEquals token))
                returnToken tokenTup
            | _ -> returnToken tokenTup

        firstChapter ()

    let rec nextToken () =
        let tokenTup =
            if lightSyntaxStatus
            then hwTokenFetch true
            else swTokenFetch ()
        if tokenTup.CurrentTokenType == null then null else
        match tokenTup.CurrentTokenType with
        | RBRACE true ->
            insertComingSoonTokens' tokenTup Token.RBRACE_COMING_SOON Token.RBRACE_IS_HERE
            nextToken ()
        | RPAREN true ->
            insertComingSoonTokens' tokenTup Token.RPAREN_COMING_SOON Token.RPAREN_IS_HERE
            nextToken ()
        | OBLOCKEND true ->
            insertComingSoonTokens' tokenTup Token.OBLOCKEND_COMING_SOON Token.OBLOCKEND_IS_HERE
            nextToken ()
        | token -> token

    let locateToken () =
        if currentTokenType = null
        then currentTokenType <- nextToken ()

    interface ILexer with
        member x.Start () =
            myLexer.Start ()
            peekInitial false |> ignore

        member x.Advance () =
            locateToken ()
            currentTokenType <- null

        member x.CurrentPosition
            with get () = (x :> ILexer<FSharpLexicalFilterState>).CurrentPosition :> obj
            and set value =
                (x :> ILexer<FSharpLexicalFilterState>).CurrentPosition <- (value :?> FSharpLexicalFilterState)

        member x.TokenType =
            locateToken ()
            currentTokenType

        member x.TokenStart =
            locateToken ()
            myLexer.TokenStart

        member x.TokenEnd =
            locateToken ()
            myLexer.TokenEnd

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
