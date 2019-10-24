namespace JetBrains.ReSharper.Plugins.FSharp.Psi.Features.LanguageService

open JetBrains.ReSharper.Host.Features.QuickDefinition
open JetBrains.ReSharper.Plugins.FSharp.Psi
open JetBrains.ReSharper.Plugins.FSharp.Psi.Tree
open JetBrains.ReSharper.Plugins.FSharp.Psi.Util
open JetBrains.ReSharper.Psi

[<Language(typeof<FSharpLanguage>)>]
type FSharpQuickDefinitionService() =
    inherit DefaultQuickDefinitionLanguageService()

    override x.GetPresentableNode(node) =
        match node with
        | :? ISynPat as pat ->
            let pat = skipIntermediatePatParents pat
            let binding = BindingNavigator.GetByHeadPattern(pat)
            LetNavigator.GetByBinding(binding) :> _

        | _ -> base.GetPresentableNode(node)