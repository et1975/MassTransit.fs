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

    static member Final (smm: IStateMachineModifier<'saga>) = smm.Final
    static member Initial (smm: IStateMachineModifier<'saga>) = smm.Initial


type ActivityBuilder<'saga
                    when 'saga: not struct
                    and 'saga :> ISaga
                    and 'saga :> SagaStateMachineInstance>
        (apply: (EventActivityBinder<'saga> * IStateMachineModifier<'saga> -> unit) ->
                EventActivityBinder<'saga> * IStateMachineModifier<'saga> -> 
                unit) =
    let bind (cfg: EventActivityBinder<'saga> * IStateMachineModifier<'saga> -> unit) 
             (state: EventActivityBinder<'saga> * IStateMachineModifier<'saga> -> unit) =
        tee state >> apply cfg

    member _.Yield _ = ignore

    [<CustomOperation "ifElse">]
    member _.IfElse(state,cond,onTrue,onFalse) =
        state |> bind (fun (binder,_) -> binder.IfElse(cond, onTrue, onFalse) |> ignore)

    [<CustomOperation "transition">]
    member _.Transition(state, mkState) =
        state |> bind (fun (binder,smm) -> binder.TransitionTo(mkState smm) |> ignore)

    [<CustomOperation "handle">]
    member _.Handle(state, handler) =
        state |> bind (fun (binder,smm) -> binder.Then(handler) |> ignore)

    [<CustomOperation "ofType">]
    member _.OfType<'activity when 'activity: not struct
                              and 'activity :> IStateMachineActivity<'saga>>
            (state) =
        state |> bind (fun (binder,_) -> binder.Activity(fun x -> x.OfType<'activity>()) |> ignore)

type ActivityBuilder<'saga,'event 
                    when 'saga: not struct
                    and 'saga :> ISaga
                    and 'saga :> SagaStateMachineInstance
                    and 'event: not struct>
        (apply: (EventActivityBinder<'saga,'event> * IStateMachineModifier<'saga> -> unit) ->
                IStateMachineEventActivitiesBuilder<'saga> * IStateMachineModifier<'saga> -> 
                unit) =
    let bind (cfg: EventActivityBinder<'saga,'event> * IStateMachineModifier<'saga> -> unit) 
             (state: IStateMachineEventActivitiesBuilder<'saga> * IStateMachineModifier<'saga> -> unit) =
        tee state >> apply cfg

    member _.Yield _ = ignore

    [<CustomOperation "ifElse">]
    member _.IfElse(state,cond,onTrue,onFalse) =
        state |> bind (fun (binder,_) -> binder.IfElse(cond, onTrue, onFalse) |> ignore)

    [<CustomOperation "transition">]
    member _.Transition(state, mkState) =
        state |> bind (fun (binder,smm) -> binder.TransitionTo(mkState smm) |> ignore)

    [<CustomOperation "handle">]
    member _.Handle(state, handler) =
        state |> bind (fun (binder,smm) -> binder.Then(handler) |> ignore)

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

[<AutoOpen>]
module internal Adapters =
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

        static member DuringAny (smm: IStateMachineModifier<'saga>) = 
            smm.DuringAny(), smm
        static member Tuple  (mkState: _ -> MassTransit.State<'saga>) (smm: IStateMachineModifier<'saga>) = 
            mkState smm, smm

    [<RequireQualifiedAccess>]
    type Activity =
        static member BuildAll (activities:#seq<IStateMachineEventActivitiesBuilder<'saga>*IStateMachineModifier<'saga> -> unit>) args =
            for a in activities do a args
        static member WhenEnter (activities:#seq<EventActivityBinder<'saga>*IStateMachineModifier<'saga> -> unit>) (state, smm:IStateMachineModifier<'saga>) =
            smm.WhenEnter(state, tee (fun binder -> for a in activities do a (binder,smm)))
            |> ignore

type EventBuilder() = 
    member _.Yield _ = List.empty<IStateMachineModifier<'saga> -> unit>
    [<CustomOperation "correlatedBy">]
    member _.CorrelatedBy(state:_, _) : list<IStateMachineModifier<'saga> -> unit> =
        state
    [<CustomOperation "onMissing">]
    member _.OnMissing(state:_, _) : list<IStateMachineModifier<'saga> -> unit>=
        state

type StateMachineBuilder<'saga, 'enum
        when 'saga :> SagaStateMachineInstance
        and 'saga : not struct 
        and 'enum :> Enum and 'enum: struct and 'enum: (new: unit -> 'enum)>() = 
    member _.Yield _ = [State<'enum>.Declare]
    
    [<CustomOperation "events">]
    member _.Events(state: list<IStateMachineModifier<'saga> -> unit>, events: list<list<IStateMachineModifier<'saga> -> unit>>) =
        state @ List.collect id events
    
    [<CustomOperation "instanceState">]
    member _.InstanceState(state: list<IStateMachineModifier<'saga> -> unit>,
                           property:Linq.Expressions.Expression<Func<'saga,MassTransit.State>>) =
        (fun (smm:IStateMachineModifier<'saga>) -> smm.InstanceState property |> ignore) :: state
    
    [<CustomOperation "initially">]
    member _.Initially(state: list<IStateMachineModifier<'saga> -> unit>, 
                       activities: list<IStateMachineEventActivitiesBuilder<'saga>*IStateMachineModifier<'saga> -> unit>) =
        [State<'enum>.Initially >> Activity.BuildAll activities] @ state
    
    [<CustomOperation "duringAny">]
    member _.DuringAny(state: list<IStateMachineModifier<'saga> -> unit>,
                       activities: list<IStateMachineEventActivitiesBuilder<'saga>*IStateMachineModifier<'saga> -> unit>) =
        [State<'enum>.DuringAny >> Activity.BuildAll activities] @ state
    
    [<CustomOperation "during">]
    member _.During(state: list<IStateMachineModifier<'saga> -> unit>,
                    mkState: IStateMachineModifier<'saga> -> MassTransit.State<'saga>,
                    activities: list<IStateMachineEventActivitiesBuilder<'saga>*IStateMachineModifier<'saga> -> unit>) =
        [State<'enum>.During mkState >> Activity.BuildAll activities] @ state

    [<CustomOperation "whenEnter">]
    member _.WhenEnter(state: list<IStateMachineModifier<'saga> -> unit>,
                       mkState: IStateMachineModifier<'saga> -> MassTransit.State<'saga>,
                       activities: list<EventActivityBinder<'saga>*IStateMachineModifier<'saga> -> unit>) =
        [State<'enum>.Tuple mkState >> Activity.WhenEnter activities] @ state
    
[<AutoOpen>]
module Builders =
    let stateMachine<'saga, 'enum
            when 'saga :> SagaStateMachineInstance
            and 'saga : not struct 
            and 'enum :> Enum and 'enum: struct and 'enum: (new: unit -> 'enum)> = StateMachineBuilder<'saga, 'enum>()
    let on<'saga,'event
            when 'saga: not struct
            and 'saga :> ISaga
            and 'saga :> SagaStateMachineInstance
            and 'event: not struct> =
        ActivityBuilder<'saga, 'event>(fun cfg (builder:IStateMachineEventActivitiesBuilder<'saga>,smm:IStateMachineModifier<'saga>) -> builder.When(Event<'event>.Of smm, tee (tuple smm >> cfg)) |> ignore)
    let activity<'saga
            when 'saga: not struct
            and 'saga :> ISaga
            and 'saga :> SagaStateMachineInstance> =
        ActivityBuilder<'saga>(fun cfg args -> cfg args |> ignore)
        