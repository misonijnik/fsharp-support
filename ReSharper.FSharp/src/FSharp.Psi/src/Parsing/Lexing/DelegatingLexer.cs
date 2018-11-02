using JetBrains.ReSharper.Psi.Parsing;
using JetBrains.Text;

namespace JetBrains.ReSharper.Plugins.FSharp.Psi.Parsing.Lexing
{
    public class DelegatingLexer<T> : ILexer<T>
    {
        protected readonly ILexer<T> myLexer;
        public DelegatingLexer(ILexer<T> lexer) => myLexer = lexer;
        public void Start()
        {
            myLexer.Start();
        }

        public void Advance()
        {
            myLexer.Advance();
        }

        public T CurrentPosition
        {
            get => myLexer.CurrentPosition;
            set => myLexer.CurrentPosition = value;
        }

        object ILexer.CurrentPosition
        {
            get => CurrentPosition;
            set => CurrentPosition = (T) value;
        }

        public TokenNodeType TokenType => myLexer.TokenType;
        public int TokenStart => myLexer.TokenStart;
        public int TokenEnd => myLexer.TokenEnd;
        public IBuffer Buffer => myLexer.Buffer;
    }
}