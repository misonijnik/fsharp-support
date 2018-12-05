namespace JetBrains.ReSharper.Plugins.FSharp.Psi.Features.Parsing.Lexing
open JetBrains.ReSharper.Plugins.FSharp.Psi.Parsing
open JetBrains.ReSharper.Plugins.FSharp.Psi.Parsing.Lexing
open JetBrains.ReSharper.Psi.Parsing
open JetBrains.DocumentModel.Impl
open JetBrains.Util.DataStructures
open JetBrains.ReSharper.Plugins.FSharp.Common.Util
open JetBrains.ReSharper.Plugins.FSharp.Psi.Features.Util

type FSharpLexicalFilterState =
    val CurrentTokenTup : TokenTup
    val DelayedTokens : ImmutableStack<TokenTup>
    val ContextStack : ImmutableStack<Context>
    new (currTokenTup, delayedTokens, contextStack) =
        { CurrentTokenTup = currTokenTup;
          DelayedTokens = delayedTokens;
          ContextStack = contextStack; }

open ImmutableStack
type LexicalFilter(buffer, lineIndex : LineIndex) =
    let myLexer = FSharpLexer buffer
    let tokenTupToState (token : TokenTup) =
        FSharpLexerState
            (token.CurrentTokenType,
             token.EndPosition.AbsoluteOffset,
             token.StartPosition.AbsoluteOffset,
             token.EndPosition.AbsoluteOffset,
             token.LexicalState)

    let compareTokenText text = LexerUtil.CompareTokenText (myLexer, text)
    let mutable delayedTokenTups = ImmutableStack<TokenTup>.Empty
    let delayTokenTup token = mutablePush &delayedTokenTups token
    let mutable tokensThatNeedNoProcessingCount = 0
    let delayTokenTupNoProcessing state =
        delayTokenTup state
        tokensThatNeedNoProcessingCount <- tokensThatNeedNoProcessingCount + 1

    let mutable contextStack = ImmutableStack<Context>.Empty
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
        FSharpLexicalFilterState (getCurrentToken (), delayedTokenTups, contextStack)

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
                    | _ when lookaheadToken == Token.GREATER
                     || lookaheadToken == Token.GREATER_RBRACK
                     || lookaheadToken == Token.GREATER_BAR_RBRACK ->
                            let nParen = nParen - 1
                            let hasAfterOp = not (lookaheadToken == Token.GREATER)
                            if nParen > 0
                            then
                                mutablePush &stack lookaheadTokenTup
                                scanAhead nParen 
                            else
                                let mutable nextTokenTup = Postprocessing.dummyTokenTup
                                if
                                    not hasAfterOp
                                 && (nextTokenIsAdjacentLParenOrLBrack lookaheadTokenTup &nextTokenTup
                                     && nextTokenTup.CurrentTokenType == Token.LPAREN)
                                then
                                    let dotTokenTup = peekNextTokenTup()
                                    mutablePush &stack (dotTokenTup.UseStartLocation(Token.HIGH_PRECEDENCE_PAREN_APP))
                                true
                    | _ when Postprocessing.isLeftParenBrackLess lookaheadToken
                     || (token == Token.INFIX_COMPARE_OP && compareTokenText "</") -> scanAhead (nParen + 1)
                    | _ when Postprocessing.isGrammarInTypes lookaheadToken
                     || (token == Token.INFIX_AT_HAT_OP && (compareTokenText "^" || compareTokenText "^-"))
                     || (token == Token.INFIX_STAR_DIV_MOD_OP && compareTokenText "/") ->
                         scanAhead nParen
                    | _ ->
                        if nParen > 1
                        then scanAhead nParen 
                        else false

                let res = scanAhead 0
                for tokenTup in stack do
                    if res && tokenTup.CurrentTokenType == Token.LESS then
                        delayTokenTup (tokenTup.UseLocation Token.BEGIN_TYPE_APP)
                    elif res && tokenTup.CurrentTokenType == Token.GREATER then
                        delayTokenTup (tokenTup.UseLocation Token.END_TYPE_APP)
                    else delayTokenTup tokenTup
                res
            else false

    let returnToken (tokenTup : TokenTup) =
        let token = tokenTup.CurrentTokenType
        delayTokenTup tokenTup
        (popNextTokenTup ()).CurrentTokenType

    let rulesForBothSoftWhiteAndHardWhite (tokenTup : TokenTup) =
        let mutable lookaheadTokenTup = Postprocessing.dummyTokenTup
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
        | token when Postprocessing.isBeforeTypeApplication token && peekAdjacentTypars false tokenTup ->
            let lessTokenTup = peekNextTokenTup()
            delayTokenTup (lessTokenTup.UseStartLocation(Token.HIGH_PRECEDENCE_TYAPP))
            delayTokenTup tokenTup
            true
        | token
            when
               (let nextTokenTup = popNextTokenTup ()
                let nextToken = nextTokenTup.CurrentTokenType
                let nextTokenIsAdjacent = nextTokenIsAdjacent nextTokenTup
                delayTokenTup nextTokenTup
                Postprocessing.isAdjacentPrefixTokens nextToken
             || (nextToken == Token.PLUS_MINUS_OP && (compareTokenText "+." || compareTokenText "-."))
             && nextTokenIsAdjacent
             && not (Postprocessing.isAtomicExprEndToken (tokenTup.CurrentTokenType)
                 && (tokenTup.EndPosition = nextTokenTup.StartPosOfTokenTup ()))) ->
                    let opTokenTup = popNextTokenTup ()
                    let plusOrMinus =
                        opTokenTup.CurrentTokenType == Token.PLUS || opTokenTup.CurrentTokenType == Token.MINUS
                    let nextTokenTup = popNextTokenTup ()
                    let delayMergedToken tok =
                        delayTokenTup (TokenTup
                             (tok, opTokenTup.StartPosition, nextTokenTup.EndPosition, opTokenTup.LexicalState))
                    let noMerge () =
                        delayTokenTup nextTokenTup
                        delayTokenTup (opTokenTup.UseLocation (Token.ADJACENT_PREFIX_OP))
                        delayTokenTup tokenTup

                    if plusOrMinus && Postprocessing.isSignedDigits nextTokenTup.CurrentTokenType
                    then delayMergedToken (nextTokenTup.CurrentTokenType)
                    else noMerge ()
                    true
        | _ -> false

    let pushCtxtSeqBlockAt (p : TokenTup) addBlockBegin addBlockEnd = 
         if addBlockBegin then
             delayTokenTup(p.UseLocation(Token.OBLOCKBEGIN))
         Context.pushCtxt p (CtxtSeqBlock (FirstInSeqBlock, p.StartPosOfTokenTup (), addBlockEnd)) &contextStack

    let pushCtxtSeqBlock addBlockBegin addBlockEnd =
        pushCtxtSeqBlockAt (peekNextTokenTup ()) addBlockBegin addBlockEnd

    let rec swTokenFetch () =
        let tokenTup = popNextTokenTup()
        let tokenReplaced = rulesForBothSoftWhiteAndHardWhite tokenTup
        if tokenReplaced then swTokenFetch() 
        else returnToken tokenTup

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

        let isControlFlowOrNotSaneLine () =
            if token == Token.EOF then false
            else not (isSameLine ()) || (Postprocessing.isControlFlow (peekNextToken ()))

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

        let insertTokenFromPrevPosToCurrentPos (previousTokenTup : TokenTup) tokenTup token =
            delayTokenTup tokenTup
            let lastTokenPos =
                let pos = previousTokenTup.EndPosition
                pos.ShiftColumnBy 1
            delayTokenTup (TokenTup (token, lastTokenPos, tokenTup.EndPosition, tokenTup.LexicalState))
            returnToken previousTokenTup

        let insertToken token =
            delayTokenTup tokenTup
            returnToken (TokenTup (token, tokenTup.StartPosOfTokenTup (), tokenTup.EndPosition, tokenTup.LexicalState))

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

        let insertComingSoonTokens (stack : byref<_>) comingSoom isHere compilingFsLib =
            delayTokenTupNoProcessing (tokenTup.UseLocation isHere)
            for i in 1..6 do
                delayTokenTupNoProcessing (tokenTup.UseEndLocation comingSoom)
            if not compilingFsLib then 
                let rec nextOuterMostInterestingContextIsNamespaceOrModule stack =
                    let stackLength = length stack
                    let getCtxt n = if stackLength >= n then peekN n stack else Context.dummyCtxt
                    match getCtxt 1, getCtxt 2, getCtxt 3 with
                    | _, (CtxtNamespaceBody _ | CtxtModuleBody _), _
                        when stackLength >= 2 -> true
                    | _, (CtxtParen (token, _)), CtxtSeqBlock _
                        when stackLength >= 3 && (token == Token.BEGIN || token == Token.STRUCT) ->
                            nextOuterMostInterestingContextIsNamespaceOrModule (popN 2 stack)
                    | _ when stackLength == 1 -> true
                    | _ -> false

                while
                    not (isEmpty stack) 
                 && (not (nextOuterMostInterestingContextIsNamespaceOrModule stack))
                 && (match peek stack with
                    | CtxtParen (token, _) when Postprocessing.isLeftParen token -> true
                    | CtxtSeqBlock _ -> true
                    | CtxtVanilla _ -> true
                    | _ -> false) do

                    match peek stack with
                    | CtxtParen _
                    | CtxtVanilla _
                    | CtxtSeqBlock (_, _, NoAddBlockEnd) -> Context.popCtxt &stack
                    | CtxtSeqBlock (_, _, AddBlockEnd) ->
                        Context.popCtxt &stack
                        delayTokenNoProcessing (tokenTup.UseEndLocation Token.OBLOCKEND)
                    | CtxtSeqBlock (_, _, AddOneSidedBlockEnd) ->
                        Context.popCtxt &stack
                        delayTokenNoProcessing (tokenTup.UseEndLocation Token.ORIGHT_BLOCK_END)
                    | _ -> failwith "impossible, the while loop guard just above prevents this"

    interface ILexer with
        member x.Start () = myLexer.Start()
        member x.Advance () = myLexer.Advance()
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
                delayedTokenTups <- value.DelayedTokens
                contextStack <- value.ContextStack
