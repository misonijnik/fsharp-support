namespace JetBrains.ReSharper.Plugins.FSharp.Tests.Features.Daemon

open JetBrains.ReSharper.Daemon.VisualElements
open JetBrains.ReSharper.Plugins.FSharp.Tests.Features.Daemon
open JetBrains.ReSharper.TestFramework
open NUnit.Framework

[<TestReferences("System.Drawing")>]
type FSharpColorHighlightingTest() =
    inherit FSharpHighlightingTestBase()

    override x.RelativeTestDataPath = "features/daemon/colorHighlighting"

    override x.HighlightingPredicate(highlighting, _, _) =
        highlighting :? ColorHighlighting

    [<Test>] member x.``Properties 01``() = x.DoNamedTest()