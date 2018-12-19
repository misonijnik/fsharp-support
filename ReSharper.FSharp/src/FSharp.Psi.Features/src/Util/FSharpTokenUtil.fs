namespace JetBrains.ReSharper.Plugins.FSharp.Psi.Features.Util
open JetBrains.ReSharper.Plugins.FSharp.Psi.Parsing
open JetBrains.ReSharper.Psi.Parsing
type Token = FSharpTokenType

module FSharpTokenUtil =
    let private (==) (leftToken : TokenNodeType) (rightToken : TokenNodeType) =
        if leftToken = null || rightToken = null
        then leftToken = rightToken 
        else leftToken.Index = rightToken.Index
    let (|ABSTRACT|) token = token == Token.ABSTRACT
    let (|AND|) token = token == Token.AND
    let (|AS|) token = token == Token.AS
    let (|ASR|) token = token == Token.ASR
    let (|ASSERT|) token = token == Token.ASSERT
    let (|BASE|) token = token == Token.BASE
    let (|BEGIN|) token = token == Token.BEGIN
    let (|CLASS|) token = token == Token.CLASS
    let (|CONST|) token = token == Token.CONST
    let (|DEFAULT|) token = token == Token.DEFAULT
    let (|DELEGATE|) token = token == Token.DELEGATE
    let (|DO|) token = token == Token.DO
    let (|DO_BANG|) token = token == Token.DO_BANG
    let (|DONE|) token = token == Token.DONE
    let (|DOWNCAST|) token = token == Token.DOWNCAST
    let (|DOWNTO|) token = token == Token.DOWNTO
    let (|ELIF|) token = token == Token.ELIF
    let (|ELSE|) token = token == Token.ELSE
    let (|END|) token = token == Token.END
    let (|EXCEPTION|) token = token == Token.EXCEPTION
    let (|EXTERN|) token = token == Token.EXTERN
    let (|FALSE|) token = token == Token.FALSE
    let (|FINALLY|) token = token == Token.FINALLY
    let (|FIXED|) token = token == Token.FIXED
    let (|FOR|) token = token == Token.FOR
    let (|FUN|) token = token == Token.FUN
    let (|FUNCTION|) token = token == Token.FUNCTION
    let (|GLOBAL|) token = token == Token.GLOBAL
    let (|IF|) token = token == Token.IF
    let (|IN|) token = token == Token.IN
    let (|JOIN_IN|) token = token == Token.JOIN_IN
    let (|INHERIT|) token = token == Token.INHERIT
    let (|INLINE|) token = token == Token.INLINE
    let (|INTERFACE|) token = token == Token.INTERFACE
    let (|INTERNAL|) token = token == Token.INTERNAL
    let (|LAZY|) token = token == Token.LAZY
    let (|LET|) token = token == Token.LET
    let (|LET_BANG|) token = token == Token.LET_BANG
    let (|MATCH|) token = token == Token.MATCH
    let (|MATCH_BANG|) token = token == Token.MATCH_BANG
    let (|MEMBER|) token = token == Token.MEMBER
    let (|MODULE|) token = token == Token.MODULE
    let (|MUTABLE|) token = token == Token.MUTABLE
    let (|NAMESPACE|) token = token == Token.NAMESPACE
    let (|NEW|) token = token == Token.NEW
    let (|NULL|) token = token == Token.NULL
    let (|OF|) token = token == Token.OF
    let (|OPEN|) token = token == Token.OPEN
    let (|OR|) token = token == Token.OR
    let (|OVERRIDE|) token = token == Token.OVERRIDE
    let (|PRIVATE|) token = token == Token.PRIVATE
    let (|PUBLIC|) token = token == Token.PUBLIC
    let (|REC|) token = token == Token.REC
    let (|RETURN|) token = token == Token.RETURN
    let (|RETURN_BANG|) token = token == Token.RETURN_BANG
    let (|SIG|) token = token == Token.SIG
    let (|STATIC|) token = token == Token.STATIC
    let (|STRUCT|) token = token == Token.STRUCT
    let (|THEN|) token = token == Token.THEN
    let (|TO|) token = token == Token.TO
    let (|TRUE|) token = token == Token.TRUE
    let (|TRY|) token = token == Token.TRY
    let (|TYPE|) token = token == Token.TYPE
    let (|UPCAST|) token = token == Token.UPCAST
    let (|USE|) token = token == Token.USE
    let (|USE_BANG|) token = token == Token.USE_BANG
    let (|VAL|) token = token == Token.VAL
    let (|VOID|) token = token == Token.VOID
    let (|WHEN|) token = token == Token.WHEN
    let (|WHILE|) token = token == Token.WHILE
    let (|WITH|) token = token == Token.WITH
    let (|YIELD|) token = token == Token.YIELD
    let (|YIELD_BANG|) token = token == Token.YIELD_BANG
    let (|UNDERSCORE|) token = token == Token.UNDERSCORE

    let (|GREATER|) token = token == Token.GREATER
    let (|LESS|) token = token == Token.LESS
    let (|BEGIN_TYPE_APP|) token = token == Token.BEGIN_TYPE_APP
    let (|END_TYPE_APP|) token = token == Token.END_TYPE_APP

    let (|LPAREN|) token = token == Token.LPAREN
    let (|RPAREN|) token = token == Token.RPAREN
    let (|LBRACE|) token = token == Token.LBRACE
    let (|RBRACE|) token = token == Token.RBRACE
    let (|LBRACK|) token = token == Token.LBRACK
    let (|RBRACK|) token = token == Token.RBRACK

    let (|LBRACK_LESS|) token = token == Token.LBRACK_LESS
    let (|GREATER_BAR_RBRACK|) token = token == Token.GREATER_BAR_RBRACK
    let (|GREATER_RBRACK|) token = token == Token.GREATER_RBRACK
    let (|LBRACK_BAR|) token = token == Token.LBRACK_BAR
    let (|BAR_RBRACK|) token = token == Token.BAR_RBRACK

    let (|LQUOTE_UNTYPED|) token = token == Token.LQUOTE_UNTYPED
    let (|RQUOTE_UNTYPED|) token = token == Token.RQUOTE_UNTYPED
    let (|LQUOTE_TYPED|) token = token == Token.LQUOTE_TYPED
    let (|RQUOTE_TYPED|) token = token == Token.RQUOTE_TYPED

    let (|AMP|) token = token == Token.AMP
    let (|AMP_AMP|) token = token == Token.AMP_AMP
    let (|BAR|) token = token == Token.BAR
    let (|BAR_BAR|) token = token == Token.BAR_BAR
    let (|STAR|) token = token == Token.STAR
    let (|COMMA|) token = token == Token.COMMA
    let (|RARROW|) token = token == Token.RARROW
    let (|DOT_DOT|) token = token == Token.DOT_DOT
    let (|DOT|) token = token == Token.DOT
    let (|COLON|) token = token == Token.COLON
    let (|COLON_COLON|) token = token == Token.COLON_COLON
    let (|COLON_GREATER|) token = token == Token.COLON_GREATER
    let (|COLON_QMARK_GREATER|) token = token == Token.COLON_QMARK_GREATER
    let (|COLON_QMARK|) token = token == Token.COLON_QMARK
    let (|COLON_EQUALS|) token = token == Token.COLON_EQUALS
    let (|LPAREN_STAR_RPAREN|) token = token == Token.LPAREN_STAR_RPAREN
    let (|PERCENT|) token = token == Token.PERCENT
    let (|PERCENT_PERCENT|) token = token == Token.PERCENT_PERCENT
    let (|SEMICOLON|) token = token == Token.SEMICOLON
    let (|SEMICOLON_SEMICOLON|) token = token == Token.SEMICOLON_SEMICOLON
    let (|LARROW|) token = token == Token.LARROW
    let (|EQUALS|) token = token == Token.EQUALS
    let (|DOLLAR|) token = token == Token.DOLLAR
    let (|MINUS|) token = token == Token.MINUS
    let (|QMARK|) token = token == Token.QMARK
    let (|QMARK_QMARK|) token = token == Token.QMARK_QMARK
    let (|QUOTE|) token = token == Token.QUOTE

    let (|PLUS|) token = token == Token.PLUS

    let (|KEYWORD_STRING_SOURCE_DIRECTORY|) token = token == Token.KEYWORD_STRING_SOURCE_DIRECTORY
    let (|KEYWORD_STRING_SOURCE_FILE|) token = token == Token.KEYWORD_STRING_SOURCE_FILE
    let (|KEYWORD_STRING_LINE|) token = token == Token.KEYWORD_STRING_LINE

    let (|PP_HELP|) token = token == Token.PP_HELP
    let (|PP_QUIT|) token = token == Token.PP_QUIT
    let (|PP_TIME|) token = token == Token.PP_TIME
    let (|PP_I|) token = token == Token.PP_I
    let (|PP_IF_SECTION|) token = token == Token.PP_IF_SECTION
    let (|PP_ELSE_SECTION|) token = token == Token.PP_ELSE_SECTION
    let (|PP_ENDIF|) token = token == Token.PP_ENDIF
    let (|PP_NOWARN|) token = token == Token.PP_NOWARN

    let (|PP_OR|) token = token == Token.PP_OR
    let (|PP_AND|) token = token == Token.PP_AND
    let (|PP_NOT|) token = token == Token.PP_NOT
    let (|PP_LPAR|) token = token == Token.PP_LPAR
    let (|PP_RPAR|) token = token == Token.PP_RPAR
    let (|EOF|) token = token == Token.EOF

    let (|IDENTIFIER|) token = token == Token.IDENTIFIER
    let (|SYMBOLIC_OP|) token = token == Token.SYMBOLIC_OP
    let (|BAD_SYMBOLIC_OP|) token = token == Token.BAD_SYMBOLIC_OP
    let (|ADJACENT_PREFIX_OP|) token = token == Token.ADJACENT_PREFIX_OP
    let (|INFIX_STAR_STAR_OP|) token = token == Token.INFIX_STAR_STAR_OP
    let (|BAD_INFIX_STAR_STAR_OP|) token = token == Token.BAD_INFIX_STAR_STAR_OP
    let (|INFIX_STAR_DIV_MOD_OP|) token = token == Token.INFIX_STAR_DIV_MOD_OP
    let (|BAD_INFIX_STAR_DIV_MOD_OP|) token = token == Token.BAD_INFIX_STAR_DIV_MOD_OP
    let (|PLUS_MINUS_OP|) token = token == Token.PLUS_MINUS_OP
    let (|BAD_PLUS_MINUS_OP|) token = token == Token.BAD_PLUS_MINUS_OP
    let (|INFIX_AT_HAT_OP|) token = token == Token.INFIX_AT_HAT_OP
    let (|BAD_INFIX_AT_HAT_OP|) token = token == Token.BAD_INFIX_AT_HAT_OP
    let (|INFIX_COMPARE_OP|) token = token == Token.INFIX_COMPARE_OP
    let (|BAD_INFIX_COMPARE_OP|) token = token == Token.BAD_INFIX_COMPARE_OP
    let (|INFIX_AMP_OP|) token = token == Token.INFIX_AMP_OP
    let (|BAD_INFIX_AMP_OP|) token = token == Token.BAD_INFIX_AMP_OP
    let (|INFIX_BAR_OP|) token = token == Token.INFIX_BAR_OP
    let (|BAD_INFIX_BAR_OP|) token = token == Token.BAD_INFIX_BAR_OP
    let (|PREFIX_OP|) token = token == Token.PREFIX_OP
    let (|BAD_PREFIX_OP|) token = token == Token.BAD_PREFIX_OP
    let (|FUNKY_OPERATOR_NAME|) token = token == Token.FUNKY_OPERATOR_NAME
    let (|BLOCK_COMMENT|) token = token == Token.BLOCK_COMMENT
    let (|SHEBANG|) token = token == Token.SHEBANG
    let (|LINE_COMMENT|) token = token == Token.LINE_COMMENT
    let (|HASH|) token = token == Token.HASH
    let (|TEXT|) token = token == Token.TEXT
    let (|DEAD_CODE|) token = token == Token.DEAD_CODE

    let (|TYPE_COMING_SOON|) token = token == Token.TYPE_COMING_SOON
    let (|TYPE_IS_HERE|) token = token == Token.TYPE_IS_HERE
    let (|MODULE_COMING_SOON|) token = token == Token.MODULE_COMING_SOON
    let (|MODULE_IS_HERE|) token = token == Token.MODULE_IS_HERE
    let (|HIGH_PRECEDENCE_BRACK_APP|) token = token == Token.HIGH_PRECEDENCE_BRACK_APP
    let (|HIGH_PRECEDENCE_PAREN_APP|) token = token == Token.HIGH_PRECEDENCE_PAREN_APP
    let (|HIGH_PRECEDENCE_TYAPP|) token = token == Token.HIGH_PRECEDENCE_TYAPP
    let (|OLET|) token = token == Token.OLET
    let (|OLET_BANG|) token = token == Token.OLET_BANG
    let (|OUSE|) token = token == Token.OUSE
    let (|OUSE_BANG|) token = token == Token.OUSE_BANG
    let (|ODO|) token = token == Token.ODO
    let (|ODO_BANG|) token = token == Token.ODO_BANG
    let (|OTHEN|) token = token == Token.OTHEN
    let (|OELSE|) token = token == Token.OELSE
    let (|OFUNCTION|) token = token == Token.OFUNCTION
    let (|OFUN|) token = token == Token.OFUN
    let (|ORESET|) token = token == Token.ORESET
    let (|OBLOCKBEGIN|) token = token == Token.OBLOCKBEGIN
    let (|OBLOCKSEP|) token = token == Token.OBLOCKSEP
    let (|OEND|) token = token == Token.OEND
    let (|ODECLEND|) token = token == Token.ODECLEND
    let (|ORIGHT_BLOCK_END|) token = token == Token.ORIGHT_BLOCK_END
    let (|OBLOCKEND|) token = token == Token.OBLOCKEND
    let (|OBLOCKEND_COMING_SOON|) token = token == Token.OBLOCKEND_COMING_SOON
    let (|OBLOCKEND_IS_HERE|) token = token == Token.OBLOCKEND_IS_HERE
    let (|OINTERFACE_MEMBER|) token = token == Token.OINTERFACE_MEMBER
    let (|OLAZY|) token = token == Token.OLAZY
    let (|OASSERT|) token = token == Token.OASSERT

    let (|IEEE32|) token = token == Token.IEEE32
    let (|IEEE64|) token = token == Token.IEEE64
    let (|DECIMAL|) token = token == Token.DECIMAL
    let (|BYTE|) token = token == Token.BYTE
    let (|INT16|) token = token == Token.INT16
    let (|INT32|) token = token == Token.INT32
    let (|INT64|) token = token == Token.INT64
    let (|SBYTE|) token = token == Token.SBYTE
    let (|UINT16|) token = token == Token.UINT16
    let (|UINT32|) token = token == Token.UINT32
    let (|UINT64|) token = token == Token.UINT64
    let (|BIGNUM|) token = token == Token.BIGNUM
    let (|NATIVEINT|) token = token == Token.NATIVEINT
    let (|UNATIVEINT|) token = token == Token.UNATIVEINT

    let (|UNFINISHED_BLOCK_COMMENT|) token = token == Token.UNFINISHED_BLOCK_COMMENT
    let (|UNFINISHED_STRING_IN_COMMENT|) token = token == Token.UNFINISHED_STRING_IN_COMMENT
    let (|UNFINISHED_VERBATIM_STRING_IN_COMMENT|) token = token == Token.UNFINISHED_VERBATIM_STRING_IN_COMMENT
    let (|UNFINISHED_TRIPLE_QUOTED_STRING_IN_COMMENT|) token = token == Token.UNFINISHED_TRIPLE_QUOTED_STRING_IN_COMMENT

    let (|CHARACTER_LITERAL|) token = token == Token.CHARACTER_LITERAL
    let (|STRING|) token = token == Token.STRING
    let (|VERBATIM_STRING|) token = token == Token.VERBATIM_STRING
    let (|TRIPLE_QUOTED_STRING|) token = token == Token.TRIPLE_QUOTED_STRING
    let (|BYTEARRAY|) token = token == Token.BYTEARRAY
    let (|VERBATIM_BYTEARRAY|) token = token == Token.VERBATIM_BYTEARRAY
    let (|BYTECHAR|) token = token == Token.BYTECHAR

    let (|UNFINISHED_STRING|) token = token == Token.UNFINISHED_STRING
    let (|UNFINISHED_VERBATIM_STRING|) token = token == Token.UNFINISHED_VERBATIM_STRING
    let (|UNFINISHED_TRIPLE_QUOTED_STRING|) token = token == Token.UNFINISHED_TRIPLE_QUOTED_STRING

    let (|RESERVED_LITERAL_FORMATS|) token = token == Token.RESERVED_LITERAL_FORMATS
    let (|RESERVED_SYMBOLIC_SEQUENCE|) token = token == Token.RESERVED_SYMBOLIC_SEQUENCE
    let (|RESERVED_IDENT_FORMATS|) token = token == Token.RESERVED_IDENT_FORMATS

    let (|BAD_TAB|) token = token == Token.BAD_TAB
    let (|BAD_CHARACTER|) token = token == Token.BAD_CHARACTER

    let (|PP_LOAD|) token = token == Token.PP_LOAD
    let (|PP_REFERENCE|) token = token == Token.PP_REFERENCE
    let (|PP_LINE|) token = token == Token.PP_LINE
    let (|PP_LIGHT|) token = token == Token.PP_LIGHT
    let (|PP_CONDITIONAL_SYMBOL|) token = token == Token.PP_CONDITIONAL_SYMBOL
    let (|PP_DIRECTIVE|) token = token == Token.PP_DIRECTIVE
    let (|PP_BAD_CHARACTER|) token = token == Token.PP_BAD_CHARACTER

    let (|ODUMMY|) token = token == Token.ODUMMY
