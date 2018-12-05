namespace JetBrains.ReSharper.Plugins.FSharp.Psi.Features.Util
open JetBrains.Util.DataStructures

module ImmutableStack =
    open System

    let isEmpty (stack : ImmutableStack<_>) = stack.IsEmpty
    let length (stack : ImmutableStack<_>) = stack.GetCount ()

    let mutablePush (stack : outref<ImmutableStack<_>>) value =
        stack <- stack.Push value

    let mutablePop (stack : outref<ImmutableStack<_>>) =
        let value = stack.Peek ()
        stack <- stack.Pop ()
        value

    let peek (stack : ImmutableStack<_>) = stack.Peek ()

    let push (stack : ImmutableStack<_>) value = stack.Push value

    let pop (stack : ImmutableStack<_>) = stack.Pop ()

    let rec popN n (stack : ImmutableStack<_>) =
        if n < 1 then raise (ArgumentException "'n' must be greater then 0!")
        elif n = 1 then pop stack else popN (n - 1) (pop stack)
    let peekN n stack = stack |> popN n |> peek
