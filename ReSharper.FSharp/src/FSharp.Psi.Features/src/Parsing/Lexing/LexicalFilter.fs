namespace JetBrains.ReSharper.Plugins.FSharp.Psi.Features.Parsing.Lexing
open JetBrains.ReSharper.Plugins.FSharp.Psi.Parsing.Lexing
type LexicalFilter(buffer, lineIndex) =
    inherit DelegatingLexer<FSharpLexerState>(buffer)