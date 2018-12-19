﻿using System;
using JetBrains.ReSharper.Plugins.FSharp.Psi.Impl.Tree;
using JetBrains.ReSharper.Psi;
using JetBrains.ReSharper.Psi.ExtensionsAPI.Tree;
using JetBrains.ReSharper.Psi.Parsing;
using JetBrains.Text;

namespace JetBrains.ReSharper.Plugins.FSharp.Psi.Parsing
{
  public partial class FSharpTokenType
  {
    public class FSharpTokenNodeType : TokenNodeType
    {
      public FSharpTokenNodeType(string name, int index) : base(name, index)
      {
        FSharpNodeTypeIndexer.Instance.Add(this, index);
        TokenRepresentation = name;
      }

      public override LeafElementBase Create(string text)
      {
        if (Identifiers[this])
          return new FSharpIdentifierToken(this, text);

        if (IsComment)
          return new FSharpComment(this, text);

        if (IsStringLiteral)
          return new FSharpString(this, text);

        return this != DEAD_CODE
          ? new FSharpToken(this, text)
          : new FSharpDeadCodeToken(this, text);
      }

      public override bool IsWhitespace => this == WHITESPACE || this == NEW_LINE;
      public override bool IsComment => this == LINE_COMMENT || this == BLOCK_COMMENT;
      public override bool IsStringLiteral => Strings[this];
      public override bool IsConstantLiteral => Literals[this];
      public override bool IsIdentifier => Identifiers[this];
      public override bool IsKeyword => Keywords[this];
      public bool IsDummy => Index == ODUMMY_NODE_TYPE_INDEX;

      public override string TokenRepresentation { get; }

      public override LeafElementBase Create(IBuffer buffer, TreeOffset startOffset, TreeOffset endOffset) =>
        throw new NotImplementedException();
    }

    private abstract class FixedTokenNodeElement : FSharpTokenBase { }
    
    private class FixedTokenNodeType : FSharpTokenNodeType, IFixedTokenNodeType
    {
      protected FixedTokenNodeType(string name, int index, string representation) : base(name, index) =>
        TokenRepresentation = representation;

      public override string TokenRepresentation { get; }

      public override LeafElementBase Create(string token) =>
        Create(null, TreeOffset.Zero, TreeOffset.Zero);

      public LeafElementBase Create() =>
        throw new NotImplementedException();
    }

    private sealed class WhitespaceNodeType : FSharpTokenNodeType
    {
      public WhitespaceNodeType(int nodeTypeIndex) : base("WHITE_SPACE", nodeTypeIndex)
      {
      }

      public override LeafElementBase Create(string text) => new Whitespace(text);
    }

    private sealed class NewLineNodeType : FSharpTokenNodeType
    {
      public NewLineNodeType(int nodeTypeIndex) : base("NEW_LINE", nodeTypeIndex)
      {
      }

      public override LeafElementBase Create(string text) => new NewLine(text);
    }

    public sealed class DummyNodeType : FSharpTokenNodeType
    {
      public readonly TokenNodeType Token;

      public DummyNodeType(int nodeTypeIndex) : base("ODUMMY", nodeTypeIndex)
      {
      }

      private DummyNodeType(int nodeTypeIndex, TokenNodeType token) : base("ODUMMY", nodeTypeIndex) =>
        Token = token;

      public TokenNodeType Create(TokenNodeType token) => new DummyNodeType(Index, token);
    }

    public const int WHITESPACE_NODE_TYPE_INDEX = LAST_GENERATED_TOKEN_TYPE_INDEX + 1;
    public static readonly TokenNodeType WHITESPACE = new WhitespaceNodeType(WHITESPACE_NODE_TYPE_INDEX);

    public const int NEW_LINE_NODE_TYPE_INDEX = LAST_GENERATED_TOKEN_TYPE_INDEX + 2;
    public static readonly TokenNodeType NEW_LINE = new NewLineNodeType(NEW_LINE_NODE_TYPE_INDEX);

    public const int ODUMMY_NODE_TYPE_INDEX = LAST_GENERATED_TOKEN_TYPE_INDEX + 3;
    public static readonly DummyNodeType ODUMMY = new DummyNodeType(ODUMMY_NODE_TYPE_INDEX);

    public static readonly NodeTypeSet RightBraces;
    public static readonly NodeTypeSet LeftBraces;
    public static readonly NodeTypeSet AccessModifiersKeywords;
    public static readonly NodeTypeSet Keywords;
    public static readonly NodeTypeSet Identifiers;
    public static readonly NodeTypeSet Strings;
    public static readonly NodeTypeSet Literals;
    public static readonly NodeTypeSet Comments;

    static FSharpTokenType()
    {
      AccessModifiersKeywords = new NodeTypeSet(PUBLIC, PRIVATE, INTERNAL);

      LeftBraces = new NodeTypeSet(
        LPAREN,
        LBRACE,
        LBRACK,
        LQUOTE_UNTYPED,
        LBRACK_BAR,
        LBRACK_LESS,
        LQUOTE_TYPED);

      RightBraces = new NodeTypeSet(
        RPAREN,
        RBRACE,
        RBRACK,
        RQUOTE_UNTYPED,
        BAR_RBRACK,
        RQUOTE_TYPED,
        GREATER_RBRACK);

      Keywords = new NodeTypeSet(
        ABSTRACT,
        AND,
        AS,
        ASSERT,
        BASE,
        BEGIN,
        CLASS,
        DEFAULT,
        DELEGATE,
        DO,
        DO_BANG,
        DONE,
        DOWNCAST,
        DOWNTO,
        ELIF,
        ELSE,
        END,
        EXCEPTION,
        EXTERN,
        FALSE,
        FINALLY,
        FIXED,
        FOR,
        FUN,
        FUNCTION,
        GLOBAL,
        IF,
        IN,
        INHERIT,
        INLINE,
        INTERFACE,
        INTERNAL,
        LAZY,
        LET,
        MATCH,
        MATCH_BANG,
        MEMBER,
        MODULE,
        MUTABLE,
        NAMESPACE,
        NEW,
        NULL,
        OF,
        OPEN,
        OR,
        OVERRIDE,
        PRIVATE,
        PUBLIC,
        REC,
        RETURN,
        STATIC,
        STRUCT,
        THEN,
        TO,
        TRUE,
        TRY,
        TYPE,
        UPCAST,
        USE,
        VAL,
        VOID,
        WHEN,
        WHILE,
        WITH,
        YIELD,

        HASH,
        RARROW);

      Identifiers = new NodeTypeSet(
        IDENTIFIER,
        SYMBOLIC_OP,
        GREATER,
        LESS);

      Strings = new NodeTypeSet(
        CHARACTER_LITERAL,
        STRING,
        UNFINISHED_STRING,
        VERBATIM_STRING,
        UNFINISHED_VERBATIM_STRING,
        TRIPLE_QUOTED_STRING,
        UNFINISHED_TRIPLE_QUOTED_STRING,
        BYTEARRAY);

      Literals = new NodeTypeSet(
        IEEE32,
        IEEE64,
        DECIMAL,
        BYTE,
        INT16,
        INT32,
        INT64,
        SBYTE,
        UINT16,
        UINT32,
        UINT64,
        BIGNUM,
        NATIVEINT,
        UNATIVEINT);

      Comments = new NodeTypeSet(
        BLOCK_COMMENT,
        UNFINISHED_BLOCK_COMMENT,
        UNFINISHED_STRING_IN_COMMENT,
        UNFINISHED_VERBATIM_STRING_IN_COMMENT,
        UNFINISHED_TRIPLE_QUOTED_STRING_IN_COMMENT);
    }
  }
}
