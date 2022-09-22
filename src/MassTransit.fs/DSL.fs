namespace MassTransit.FSharp

open System
open System.Threading.Tasks
open MassTransit

[<RequireQualifiedAccess>]
type Event<'event when 'event : not struct> =
    static member Of (smm: IStateMachineModifier<'saga>) = 
        let mutable event: MassTransit.Event<'event> = null
        smm.Event(typeof<'event>.Name, &event) |> ignore
        event
    static member Discard _ = Unchecked.defaultof<'a> //?

[<RequireQualifiedAccess>]
type State = 
    static member Of (e:#Enum) = 
        fun (smm: IStateMachineModifier<'saga>) ->
            let mutable state: MassTransit.State<'saga> = null
            smm.State(string e, &state) |> ignore
            state

    static member Final (smm: IStateMachineModifier<'saga>) =
        smm.Final


type ActivityBuilder<'saga,'event 
                when 'saga: not struct
                and 'saga :> ISaga
                and 'saga :> SagaStateMachineInstance
                and 'event: not struct>() =
    let bind cfg state (builder:IStateMachineEventActivitiesBuilder<'saga>, smm:IStateMachineModifier<'saga>) =
        state (builder,smm)
        builder.When(Event<'event>.Of smm, tee (tuple smm >> cfg)) |> ignore

    member _.Yield _ = ignore

    [<CustomOperation "ifElse">]
    member _.IfElse(state,cond,onTrue,onFalse) =
        state |> bind (fun (binder,_) -> binder.IfElse(cond, onTrue, onFalse) |> ignore)

    [<CustomOperation "transition">]
    member _.Transition(state, mkState) =
        state |> bind (fun (binder,smm) -> binder.TransitionTo(mkState smm) |> ignore)

    [<CustomOperation "ofType">]
    member _.OfType<'activity when 'activity: not struct
                              and 'activity :> IStateMachineActivity<'saga,'event>>
            (state) =
        state |> bind (fun (binder,_) -> binder.Activity(fun x -> x.OfType<'activity>()) |> ignore)

    [<CustomOperation "ofInstanceType">]
    member _.OfInstanceType<'activity when 'activity: not struct
                                      and 'activity :> IStateMachineActivity<'saga>
                                      and 'activity :> IStateMachineActivity<'saga,'event>>
            (state) =
        state |> bind (fun (binder,_) -> binder.Activity(fun x -> x.OfInstanceType<'activity>()) |> ignore)


// type ActivityBuilder<'saga
//         when 'saga: not struct
//         and 'saga :> ISaga
//         and 'saga :> SagaStateMachineInstance>() =


[<AutoOpen>]
module internal Internals =
    [<RequireQualifiedAccess>]
    type State<'enum when 'enum :> Enum and 'enum: struct and 'enum: (new: unit -> 'enum)> =
        static member Declare (smm: IStateMachineModifier<'saga>) = 
            for e in Enum.GetValues<'enum>() do
                let mutable state: MassTransit.State<'saga> = null
                smm.State(string e, &state) |> ignore

        static member Initially (smm: IStateMachineModifier<'saga>) = 
            smm.Initially(), smm

        static member During (mkState: _ -> MassTransit.State<'saga>) (smm: IStateMachineModifier<'saga>) = 
            smm.During(mkState smm), smm

    [<RequireQualifiedAccess>]
    type Activity =
        static member Apply (activities:#seq<IStateMachineEventActivitiesBuilder<'saga>*IStateMachineModifier<'saga> -> unit>) args =
            for a in activities do a args

type EventBuilder() = 
    member _.Yield _ = List.empty<IStateMachineModifier<'saga> -> unit>
    [<CustomOperation "correlatedBy">]
    member _.CorrelatedBy(state:_, _) : list<IStateMachineModifier<'saga> -> unit> =
        state
    [<CustomOperation "onMissing">]
    member _.OnMissing(state:_, _) : list<IStateMachineModifier<'saga> -> unit>=
        state

// type ActivityBuilder<'event when 'event: not struct>() = 
//     member _.Yield _ = List.empty<EventActivityBinder<'saga,'event> -> unit>
//     [<CustomOperation "ifElse">]
//     member _.Conditional(state:_, onTrue, onFalse) : list<EventActivityBinder<'saga,'event> -> unit> =
//         state
//     [<CustomOperation "transition">]
//     member _.Transition(state:_, _) : list<EventActivityBinder<'saga,'event> -> unit> =
//         state
//     [<CustomOperation "ofType">]
//     member _.OfType<'activity>(state:_, _) : list<EventActivityBinder<'saga,'event> -> unit> =
//         state
//     [<CustomOperation "ofInstanceType">]
//     member _.OfInstanceType<'activity>(state:_, _) : list<EventActivityBinder<'saga,'event> -> unit> =
//         state


type StateMachineBuilder<'enum, 'saga
        when 'saga :> SagaStateMachineInstance
        and 'saga : not struct 
        and 'enum :> Enum and 'enum: struct and 'enum: (new: unit -> 'enum)>() = 
    member _.Yield _ = [State<'enum>.Declare]
    
    [<CustomOperation "events">]
    member _.Events(state: list<IStateMachineModifier<'saga> -> unit>, events: list<list<IStateMachineModifier<'saga> -> unit>>) =
        state @ List.collect id events
    
    [<CustomOperation "instanceState">]
    member _.InstanceState(state: list<IStateMachineModifier<'saga> -> unit>, property:Linq.Expressions.Expression<Func<'saga,MassTransit.State>>) =
        (fun (smm:IStateMachineModifier<'saga>) -> smm.InstanceState property |> ignore) :: state
    
    [<CustomOperation "initially">]
    member _.Initially(
            state: list<IStateMachineModifier<'saga> -> unit>, 
            activities: list<IStateMachineEventActivitiesBuilder<'saga>*IStateMachineModifier<'saga> -> unit>) =
        [State<'enum>.Initially >> Activity.Apply activities] @ state
    
    [<CustomOperation "during">]
    member _.During(
            state: list<IStateMachineModifier<'saga> -> unit>,
            stateEnum: 'enum,
            activities: list<IStateMachineEventActivitiesBuilder<'saga>*IStateMachineModifier<'saga> -> unit>) : list<IStateMachineModifier<'saga> -> unit> =
        [State<'enum>.During (State.Of stateEnum) >> Activity.Apply activities] @ state
    
[<AutoOpen>]
module Builders =
    let stateMachine<'enum,'saga
            when 'saga :> SagaStateMachineInstance
            and 'saga : not struct 
            and 'enum :> Enum and 'enum: struct and 'enum: (new: unit -> 'enum)> = StateMachineBuilder<'enum, 'saga>()
    let on<'saga,'event
            when 'saga: not struct
            and 'saga :> ISaga
            and 'saga :> SagaStateMachineInstance
            and 'event: not struct> = ActivityBuilder<'saga, 'event>()
    // let activity<'saga,'event
    //         when 'saga: not struct
    //         and 'saga :> ISaga
    //         and 'saga :> SagaStateMachineInstance> = ActivityBuilder<'saga>()
        