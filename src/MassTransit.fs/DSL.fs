namespace MassTransit.FSharp

open System
open System.Threading.Tasks
open MassTransit


[<AutoOpen>]
module Event =
    let discard<'a> = Unchecked.defaultof<'a>

type ActivityBuilder<'state> = 
    member __.Yield _ = []
    [<CustomOperation "ifElse">]
    member _.Conditional(state:_, onTrue, onFalse) : ActivityBuilder<'state> =
        Unchecked.defaultof<_>
    [<CustomOperation "transition">]
    member _.Transition(state:_, _) : ActivityBuilder<'state> =
        Unchecked.defaultof<_>
    [<CustomOperation "activity">]
    member _.Activity(state:_, _) : ActivityBuilder<'state> =
        Unchecked.defaultof<_>

type EventBuilder<'state> = 
    member __.Yield _ = []
    [<CustomOperation "correlatedBy">]
    member _.CorrelatedBy(state:_, _) : EventBuilder<'state> =
        Unchecked.defaultof<_>
    [<CustomOperation "onMissing">]
    member _.OnMissing(state:_, _) : EventBuilder<'state> =
        Unchecked.defaultof<_>

type TransitionBuilder<'state> = 
    member __.Yield _ = []
    [<CustomOperation "to">]
    member _.To(state:_, _) : TransitionBuilder<'state> =
        Unchecked.defaultof<_>
    [<CustomOperation "toFinal">]
    member _.ToFinal(state:_) : TransitionBuilder<'state> =
        Unchecked.defaultof<_>

type StateBuilder<'state> = 
    member __.Yield _ = []
    [<CustomOperation "on">]
    member _.On(state:_, _) : StateBuilder<'state> =
        Unchecked.defaultof<_>

type MachineBuilder<'state> = 
    member __.Yield _ = []
    [<CustomOperation "events">]
    member _.Events(state:_, events) : MachineBuilder<'state> =
        Unchecked.defaultof<_>
    [<CustomOperation "configure">]
    member _.Configure(state:_, states) : MachineBuilder<'state> =
        Unchecked.defaultof<_>
