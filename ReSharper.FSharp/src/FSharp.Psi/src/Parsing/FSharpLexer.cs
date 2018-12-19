using System.Text.RegularExpressions;
using JetBrains.Text;
using JetBrains.ReSharper.Plugins.FSharp.Psi.Parsing.Lexing;
using JetBrains.ReSharper.Psi.Parsing;

namespace JetBrains.ReSharper.Plugins.FSharp.Psi.Parsing
{
  public class FSharpLexer : FSharpLexerGenerated
    {
      public FSharpLexer(IBuffer buffer) : base(buffer) {}

      public FSharpLexer(IBuffer buffer, int startOffset, int endOffset) : base(buffer, startOffset, endOffset) {}
    }

  public struct FSharpLexerLineIndexingState
  {
    public TokenNodeType CurrentTokenType;
    public int BufferIndex;
    public int BufferStart;
    public int BufferEnd;
    public int BufferLineTokenStart;
    public int BufferLineTokenEnd;
    public int LineTokenStart;
    public int LineTokenEnd;
    public int TypeParenLevel;
    public int LexicalState;

    public FSharpLexerLineIndexingState(
      TokenNodeType currentTokenType,
      int bufferIndex,
      int bufferStart,
      int bufferEnd,
      int bufferLineTokenStart,
      int bufferLineTokenEnd,
      int lineTokenStart,
      int lineTokenEnd,
      int typeParenLevel,
      int lexicalState)
    {
      CurrentTokenType = currentTokenType;
      BufferIndex = bufferIndex;
      BufferStart = bufferStart;
      BufferEnd = bufferEnd;
      BufferLineTokenStart = bufferLineTokenStart;
      BufferLineTokenEnd = bufferLineTokenEnd;
      LineTokenStart = lineTokenStart;
      LineTokenEnd = lineTokenEnd;
      TypeParenLevel = typeParenLevel;
      LexicalState = lexicalState;
    }

    public static FSharpLexerLineIndexingState Create(int start, int startOfLine, int line, int nParen, int lexicalState) =>
      new FSharpLexerLineIndexingState(
        null,
        start,
        start,
        start,
        startOfLine,
        startOfLine,
        line,
        line,
        nParen,
        lexicalState);
  }

  public class FSharpLexerWithLineIndexing : FSharpLexer, ILexer, ILexer<FSharpLexerLineIndexingState>
  {
    private int bufferLineStart;
    private int bufferLineEnd;
    private int lineStart;
    private int lineEnd;
    private bool EOFHere = false;
    public FSharpLexerWithLineIndexing(IBuffer buffer) : base(buffer) {}

    public FSharpLexerWithLineIndexing(IBuffer buffer, int startOffset, int endOffset)
      : base(buffer, startOffset, endOffset) {}

    object ILexer.CurrentPosition
    {
      get { return CurrentPosition; }
      set { CurrentPosition = (FSharpLexerLineIndexingState) value; }
    }

    public new FSharpLexerLineIndexingState CurrentPosition
    {
      get
      {
        FSharpLexerLineIndexingState tokenPosition;
        tokenPosition.CurrentTokenType = CurrentToken;
        tokenPosition.BufferIndex = BufferIndex;
        tokenPosition.BufferStart = BufferStart;
        tokenPosition.BufferEnd = BufferEnd;
        tokenPosition.BufferLineTokenStart = bufferLineStart;
        tokenPosition.BufferLineTokenEnd = bufferLineEnd;
        tokenPosition.LineTokenStart = lineStart;
        tokenPosition.LineTokenEnd = lineEnd;
        tokenPosition.LexicalState = LexicalState;
        tokenPosition.TypeParenLevel = TypeParenLevel;
        return tokenPosition;
      }
      set
      {
        CurrentToken = value.CurrentTokenType;
        BufferIndex = value.BufferIndex;
        BufferStart = value.BufferStart;
        BufferEnd = value.BufferEnd;
        bufferLineStart = value.BufferLineTokenStart;
        bufferLineEnd = value.BufferLineTokenEnd;
        lineStart = value.LineTokenStart;
        lineEnd = value.LineTokenEnd;
        TypeParenLevel = value.TypeParenLevel;
        LexicalState = value.LexicalState;
        if (CurrentToken == FSharpTokenType.EOF ||
            CurrentToken == null && BufferEnd == EOFPos)
          EOFHere = true;
        else
          EOFHere = false;
      }
    }

    public void Start(FSharpLexerLineIndexingState state, int endOffset)
    {
      CurrentPosition = state;
      EOFPos = endOffset;
    }
    public override TokenNodeType _locateToken()
    {
      lineStart = lineEnd;
      bufferLineStart = bufferLineEnd;
      var token = base._locateToken();
      if (token == FSharpTokenType.NEW_LINE)
      {
        lineEnd++;
        bufferLineEnd = BufferEnd;
      }
      else if (FSharpTokenType.Comments[token] || FSharpTokenType.Strings[token])
      {
        lineEnd += Regex.Matches(this.GetCurrTokenText(),"\n|\r\n").Count;
        bufferLineEnd = BufferEnd;
      }
      else if (!EOFHere && token == null)
      {
        EOFHere = true;
        return FSharpTokenType.EOF;
      }

      return token;
    }

    public static int InitAdjacentTyAppState => INIT_ADJACENT_TYAPP;
    public static int AdjacentTyApp => ADJACENT_TYAPP;
  }
}