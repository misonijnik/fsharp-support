﻿using JetBrains.ReSharper.Plugins.FSharp.Psi.Impl.DeclaredElement;
using JetBrains.ReSharper.Psi;
using Microsoft.FSharp.Compiler.SourceCodeServices;

namespace JetBrains.ReSharper.Plugins.FSharp.Psi.Impl.Tree
{
  internal partial class RecordFieldDeclaration
  {
    protected override string DeclaredElementName => Identifier.GetSourceName();
    public override TreeTextRange GetNameRange() => Identifier.GetNameRange();

    protected override IDeclaredElement CreateDeclaredElement() =>
      GetFSharpSymbol() is FSharpField field ? new FSharpRecordField(this, field) : null;
  }
}
