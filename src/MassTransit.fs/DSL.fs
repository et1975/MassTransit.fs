namespace MassTransit.FSharp

open System
open System.Threading.Tasks
open MassTransit

type MTState = MassTransit.State
type MTState<'saga when 'saga: not struct and 'saga :> ISaga> = MassTransit.State<'saga>
type MTEvent = MassTransit.Event
type MTEvent<'e when 'e : not struct> = MassTransit.Event<'e>

[<RequireQualifiedAccess>]
type Event<'event when 'event : not struct> =
    static member Discard (cfg:IMissingInstanceConfigurator<'saga,'event>) = cfg.Discard()
    static member Execute (callback) (cfg:IMissingInstanceConfigurator<'saga,'event>) = cfg.Execute(Action<ConsumeContext<'event>> callback)
    static member ExecuteAsync (callback) (cfg:IMissingInstanceConfigurator<'saga,'event>) = cfg.ExecuteAsync(Func<ConsumeContext<'event>,Task> callback)

[<RequireQualifiedAccess>]
type State = 
    static member Of (e:#Enum) = 
        fun (smm: IStateMachineModifier<'saga>) ->
            let mutable state: MTState<'saga> = null
            smm.State(string e, &state) |> ignore
            state

    static member Final (smm: IStateMachineModifier<'saga>) = smm.Final
    static member Initial (smm: IStateMachineModifier<'saga>) = smm.Initial

[<RequireQualifiedAccess>]
type Activity<'saga,'event
                when 'saga: not struct
                and 'saga :> ISaga
                and 'saga :> SagaStateMachineInstance
                and 'event: not struct> = 
    static member OfType<'activity
                          when 'activity: not struct
                          and 'activity :> IStateMachineActivity<'saga,'event>>
            (binder:EventActivityBinder<'saga,'event>) = 
        binder.Activity(fun x -> x.OfType<'activity>())

    static member OfInstanceType<'activity
                                   when 'activity: not struct
                                   and 'activity :> IStateMachineActivity<'saga>>
            (binder:EventActivityBinder<'saga,'event>) = 
        binder.Activity(fun x -> x.OfInstanceType<'activity>())

    static member OfInstanceType<'activity
                                   when 'activity: not struct
                                   and 'activity :> IStateMachineActivity<'saga>>
            (binder:EventActivityBinder<'saga>) = 
        binder.Activity(fun x -> x.OfType<'activity>())

[<AutoOpen>]
module private Adapters =
    [<RequireQualifiedAccess>]
    type Event<'event when 'event : not struct> =
        static member Of (smm: IStateMachineModifier<'saga>) = 
            let mutable event: MTEvent<'event> = null
            smm.Event(typeof<'event>.Name, &event) |> ignore
            event
        static member Configure cfg (smm: IStateMachineModifier<'saga>) = 
            let mutable event: MTEvent<'event> = null
            smm.Event(typeof<'event>.Name, Action<IEventCorrelationConfigurator<'saga,'event>> cfg, &event) |> ignore


    [<RequireQualifiedAccess>]
    type State<'enum when 'enum :> Enum and 'enum: struct and 'enum: (new: unit -> 'enum)> =
        static member Declare (smm: IStateMachineModifier<'saga>) = 
            for e in Enum.GetValues<'enum>() do
                let mutable state: MTState<'saga> = null
                smm.State(string e, &state) |> ignore

        static member Initially (smm: IStateMachineModifier<'saga>) = smm.Initially(), smm

        static member During (mkState: _ -> MTState<'saga>) (smm: IStateMachineModifier<'saga>) = smm.During(mkState smm), smm

        static member DuringAny (smm: IStateMachineModifier<'saga>) = smm.DuringAny(), smm

        static member Tuple  (mkState: _ -> #MTState) (smm: IStateMachineModifier<'saga>) = mkState smm, smm

    [<RequireQualifiedAccess>]
    type Activity =
        static member BuildAll (activities:#seq<IStateMachineEventActivitiesBuilder<'saga>*IStateMachineModifier<'saga> -> unit>) args =
            for a in activities do a args

        static member WhenEnter (activities:#seq<EventActivityBinder<'saga>*IStateMachineModifier<'saga> -> unit>) (state, smm:IStateMachineModifier<'saga>) =
            smm.WhenEnter(state, tee (fun binder -> for a in activities do a (binder,smm))) |> ignore
        
        static member WhenEnterAny (activities:#seq<EventActivityBinder<'saga>*IStateMachineModifier<'saga> -> unit>) (smm:IStateMachineModifier<'saga>) =
            smm.WhenEnterAny(tee (fun binder -> for a in activities do a (binder,smm))) |> ignore
        
        static member WhenLeave (activities:#seq<EventActivityBinder<'saga>*IStateMachineModifier<'saga> -> unit>) (state, smm:IStateMachineModifier<'saga>) =
            smm.WhenLeave(state, tee (fun binder -> for a in activities do a (binder,smm))) |> ignore
        
        static member WhenLeaveAny (activities:#seq<EventActivityBinder<'saga>*IStateMachineModifier<'saga> -> unit>) (smm:IStateMachineModifier<'saga>) =
            smm.WhenLeaveAny(tee (fun binder -> for a in activities do a (binder,smm))) |> ignore
        
        static member Finally (activities:#seq<EventActivityBinder<'saga>*IStateMachineModifier<'saga> -> unit>) (smm:IStateMachineModifier<'saga>) =
            smm.Finally(tee (fun binder -> for a in activities do a (binder,smm))) |> ignore
        
        static member AfterLeave (activities:#seq<EventActivityBinder<'saga,MTState>*IStateMachineModifier<'saga> -> unit>) (state, smm:IStateMachineModifier<'saga>) =
            smm.AfterLeave(state, tee (fun binder -> for a in activities do a (binder,smm))) |> ignore
        
        static member AfterLeaveAny (activities:#seq<EventActivityBinder<'saga,MTState>*IStateMachineModifier<'saga> -> unit>) (smm:IStateMachineModifier<'saga>) =
            smm.AfterLeaveAny(tee (fun binder -> for a in activities do a (binder,smm))) |> ignore
        
        static member BeforeEnter (activities:#seq<EventActivityBinder<'saga,MTState>*IStateMachineModifier<'saga> -> unit>) (state, smm:IStateMachineModifier<'saga>) =
            smm.BeforeEnter(state, tee (fun binder -> for a in activities do a (binder,smm))) |> ignore
        
        static member BeforeEnterAny (activities:#seq<EventActivityBinder<'saga,MTState>*IStateMachineModifier<'saga> -> unit>) (smm:IStateMachineModifier<'saga>) =
            smm.BeforeEnterAny(tee (fun binder -> for a in activities do a (binder,smm))) |> ignore


type EventBuilder<'event when 'event : not struct>() = 
    member _.Yield _ = fun (_:IStateMachineModifier<'saga>) -> ()
    [<CustomOperation "correlatedBy">]
    member _.CorrelatedBy(state, conf) =
        tee state
        >> Event<'event>.Configure (fun x -> x.CorrelateById (Func<ConsumeContext<'event>,Guid> conf) |> ignore)
        >> ignore
    
    [<CustomOperation "onMissing">]
    member _.OnMissing(state, conf) =
        tee state 
        >> Event<'event>.Configure (fun x -> x.OnMissingInstance(Func<IMissingInstanceConfigurator<'b,'event>,IPipe<ConsumeContext<'event>>> conf) |> ignore) 
        >> ignore


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
    
    [<CustomOperation "conditionally">]
    member _.If(state,cond,onTrue) =
        state |> bind (fun (binder,smm) -> binder.If(cond, tee (tuple smm >> onTrue)) |> ignore)

    [<CustomOperation "conditionally">]
    member _.If(state,cond,onTrue) =
        state |> bind (fun (binder,smm) -> binder.IfAsync(cond, tee (tuple smm >> onTrue)) |> ignore)

    [<CustomOperation "ifElse">]
    member _.IfElse(state,cond,onTrue,onFalse) =
        state |> bind (fun (binder,smm) -> binder.IfElse(cond, tee (tuple smm >> onTrue), tee (tuple smm >> onFalse)) |> ignore)

    [<CustomOperation "ifElse">]
    member _.IfElse(state,cond,onTrue,onFalse) =
        state |> bind (fun (binder,smm) -> binder.IfElseAsync(cond, tee (tuple smm >> onTrue), tee (tuple smm >> onFalse)) |> ignore)

    [<CustomOperation "transition">]
    member _.Transition(state, mkState) =
        state |> bind (fun (binder,smm) -> binder.TransitionTo(mkState smm) |> ignore)

    [<CustomOperation "handle">]
    member _.Handle(state, handler) =
        state |> bind (fun (binder,smm) -> binder.Then(handler) |> ignore)

    [<CustomOperation "bind">]
    member _.Bind(state,binding) =
        state |> bind (fun (binder,_) -> binding binder |> ignore)


type EventActivityBuilder<'saga, 'event
                    when 'saga: not struct
                    and 'saga :> ISaga
                    and 'saga :> SagaStateMachineInstance
                    and 'event: not struct>
        (apply: (EventActivityBinder<'saga,'event> * IStateMachineModifier<'saga> -> unit) ->
                EventActivityBinder<'saga,'event> * IStateMachineModifier<'saga> -> 
                unit) =
    let bind (cfg: EventActivityBinder<'saga,'event> * IStateMachineModifier<'saga> -> unit) 
             (state: EventActivityBinder<'saga,'event> * IStateMachineModifier<'saga> -> unit) =
        tee state >> apply cfg

    member _.Yield _ = ignore
    
    [<CustomOperation "conditionally">]
    member _.If(state, cond, onTrue) =
        state |> bind (fun (binder,smm) -> binder.If(StateMachineCondition<'saga,'event> cond, tee (tuple smm >> onTrue)) |> ignore)

    [<CustomOperation "conditionally">]
    member _.If(state,cond,onTrue) =
        state |> bind (fun (binder,smm) -> binder.IfAsync(StateMachineAsyncCondition<'saga,'event> cond, tee (tuple smm >> onTrue)) |> ignore)

    [<CustomOperation "ifElse">]
    member _.IfElse(state,cond,onTrue,onFalse) =
        state |> bind (fun (binder,smm) -> binder.IfElse(StateMachineCondition<'saga,'event> cond, tee (tuple smm >> onTrue), tee (tuple smm >> onFalse)) |> ignore)

    [<CustomOperation "ifElse">]
    member _.IfElse(state,cond,onTrue,onFalse) =
        state |> bind (fun (binder,smm) -> binder.IfElseAsync(StateMachineAsyncCondition<'saga,'event> cond, tee (tuple smm >> onTrue), tee (tuple smm >> onFalse)) |> ignore)

    [<CustomOperation "transition">]
    member _.Transition(state, mkState) =
        state |> bind (fun (binder,smm) -> binder.TransitionTo(mkState smm) |> ignore)

    [<CustomOperation "handle">]
    member _.Handle(state, handler) =
        state |> bind (fun (binder,smm) -> binder.Then(handler) |> ignore)

    [<CustomOperation "bind">]
    member _.Bind(state,binding) =
        state |> bind (fun (binder,_) -> binding binder |> ignore)


type StateActivityBuilder<'saga 
                    when 'saga: not struct
                    and 'saga :> ISaga
                    and 'saga :> SagaStateMachineInstance>
        (apply: (EventActivityBinder<'saga,MTState> * IStateMachineModifier<'saga> -> unit) ->
                EventActivityBinder<'saga,MTState> * IStateMachineModifier<'saga> -> 
                unit) =
    let bind (cfg: EventActivityBinder<'saga,MTState> * IStateMachineModifier<'saga> -> unit) 
             (state: EventActivityBinder<'saga,MTState> * IStateMachineModifier<'saga> -> unit) =
        tee state >> apply cfg

    member _.Yield _ = ignore

    [<CustomOperation "conditionally">]
    member _.If(state,cond,onTrue) =
        state |> bind (fun (binder,smm) -> binder.If(StateMachineCondition<'saga,MTState> cond, tee (tuple smm >> onTrue)) |> ignore)

    [<CustomOperation "conditionally">]
    member _.If(state,cond,onTrue) =
        state |> bind (fun (binder,smm) -> binder.IfAsync(StateMachineAsyncCondition<'saga,MTState> cond, tee (tuple smm >> onTrue)) |> ignore)

    [<CustomOperation "ifElse">]
    member _.IfElse(state,cond,onTrue,onFalse) =
        state |> bind (fun (binder,smm) -> binder.IfElse(StateMachineCondition<'saga,MTState> cond, tee (tuple smm >> onTrue), tee (tuple smm >> onFalse)) |> ignore)

    [<CustomOperation "ifElse">]
    member _.IfElse(state,cond,onTrue,onFalse) =
        state |> bind (fun (binder,smm) -> binder.IfElseAsync(StateMachineAsyncCondition<'saga,MTState> cond, tee (tuple smm >> onTrue), tee (tuple smm >> onFalse)) |> ignore)
    
    [<CustomOperation "handle">]
    member _.Handle(state, handler) =
        state |> bind (fun (binder,smm) -> binder.Then(handler) |> ignore)

    [<CustomOperation "bind">]
    member _.Bind(state,binding) =
        state |> bind (fun (binder,_) -> binding binder |> ignore)


type StateMachineBuilder<'saga, 'enum
                when 'saga :> SagaStateMachineInstance
                and 'saga : not struct 
                and 'enum :> Enum and 'enum: struct and 'enum: (new: unit -> 'enum)>() = 
    member _.Yield _ = [State<'enum>.Declare]
        
    [<CustomOperation "events">]
    member _.Events(state: list<IStateMachineModifier<'saga> -> unit>, events: list<IStateMachineModifier<'saga> -> unit>) =
        state @ events
    
    [<CustomOperation "instanceState">]
    member _.InstanceState(state: list<IStateMachineModifier<'saga> -> unit>,
                           property:Linq.Expressions.Expression<Func<'saga,string>>) =
        (fun (smm:IStateMachineModifier<'saga>) -> smm.InstanceState property |> ignore) :: state

    [<CustomOperation "instanceState">]
    member _.InstanceState(state: list<IStateMachineModifier<'saga> -> unit>,
                           property:Linq.Expressions.Expression<Func<'saga,MTState>>) =
        (fun (smm:IStateMachineModifier<'saga>) -> smm.InstanceState property |> ignore) :: state
    
    [<CustomOperation "initially">]
    member _.Initially(state: list<IStateMachineModifier<'saga> -> unit>, 
                       activities: list<IStateMachineEventActivitiesBuilder<'saga>*IStateMachineModifier<'saga> -> unit>) =
        [State<'enum>.Initially >> Activity.BuildAll activities] @ state
    
    [<CustomOperation "finally">]
    member _.Finally(state: list<IStateMachineModifier<'saga> -> unit>,
                     activities: list<EventActivityBinder<'saga>*IStateMachineModifier<'saga> -> unit>) =
        [Activity.Finally activities] @ state

    [<CustomOperation "duringAny">]
    member _.DuringAny(state: list<IStateMachineModifier<'saga> -> unit>,
                       activities: list<IStateMachineEventActivitiesBuilder<'saga>*IStateMachineModifier<'saga> -> unit>) =
        [State<'enum>.DuringAny >> Activity.BuildAll activities] @ state
    
    [<CustomOperation "during">]
    member _.During(state: list<IStateMachineModifier<'saga> -> unit>,
                    mkState: IStateMachineModifier<'saga> -> MTState<'saga>,
                    activities: list<IStateMachineEventActivitiesBuilder<'saga>*IStateMachineModifier<'saga> -> unit>) =
        [State<'enum>.During mkState >> Activity.BuildAll activities] @ state

    [<CustomOperation "whenEnter">]
    member _.WhenEnter(state: list<IStateMachineModifier<'saga> -> unit>,
                       mkState: IStateMachineModifier<'saga> -> MTState<'saga>,
                       activities: list<EventActivityBinder<'saga>*IStateMachineModifier<'saga> -> unit>) =
        [State<'enum>.Tuple mkState >> Activity.WhenEnter activities] @ state

    [<CustomOperation "whenEnterAny">]
    member _.WhenEnterAny(state: list<IStateMachineModifier<'saga> -> unit>,
                          activities: list<EventActivityBinder<'saga>*IStateMachineModifier<'saga> -> unit>) =
        [Activity.WhenEnterAny activities] @ state

    [<CustomOperation "whenLeave">]
    member _.WhenLeave(state: list<IStateMachineModifier<'saga> -> unit>,
                       mkState: IStateMachineModifier<'saga> -> MTState<'saga>,
                       activities: list<EventActivityBinder<'saga>*IStateMachineModifier<'saga> -> unit>) =
        [State<'enum>.Tuple mkState >> Activity.WhenLeave activities] @ state

    [<CustomOperation "whenLeaveAny">]
    member _.WhenLeaveAny(state: list<IStateMachineModifier<'saga> -> unit>,
                          activities: list<EventActivityBinder<'saga>*IStateMachineModifier<'saga> -> unit>) =
        [Activity.WhenLeaveAny activities] @ state

    [<CustomOperation "afterLeave">]
    member _.WhenLeave(state: list<IStateMachineModifier<'saga> -> unit>,
                       mkState: IStateMachineModifier<'saga> -> MTState<'saga>,
                       activities: list<EventActivityBinder<'saga,MTState>*IStateMachineModifier<'saga> -> unit>) =
        [State<'enum>.Tuple mkState >> Activity.AfterLeave activities] @ state

    [<CustomOperation "afterLeaveAny">]
    member _.AfterLeaveAny(state: list<IStateMachineModifier<'saga> -> unit>,
                           activities: list<EventActivityBinder<'saga,MTState>*IStateMachineModifier<'saga> -> unit>) =
        [Activity.AfterLeaveAny activities] @ state

    [<CustomOperation "beforeEnter">]
    member _.BeforeEnter(state: list<IStateMachineModifier<'saga> -> unit>,
                         mkState: IStateMachineModifier<'saga> -> MTState<'saga>,
                         activities: list<EventActivityBinder<'saga,MTState>*IStateMachineModifier<'saga> -> unit>) =
        [State<'enum>.Tuple mkState >> Activity.BeforeEnter activities] @ state

    [<CustomOperation "beforeEnterAny">]
    member _.BeforeEnterAny(state: list<IStateMachineModifier<'saga> -> unit>,
                            activities: list<EventActivityBinder<'saga,MTState>*IStateMachineModifier<'saga> -> unit>) =
        [Activity.BeforeEnterAny activities] @ state


[<AutoOpen>]
module Bindings =
    let stateMachine<'saga, 'enum
                when 'saga :> SagaStateMachineInstance
                and 'saga : not struct 
                and 'enum :> Enum and 'enum: struct and 'enum: (new: unit -> 'enum)> = StateMachineBuilder<'saga, 'enum>()
    
    let on<'saga,'event
                when 'saga: not struct
                and 'saga :> ISaga
                and 'saga :> SagaStateMachineInstance
                and 'event: not struct>
            activity (builder:IStateMachineEventActivitiesBuilder<'saga>,smm:IStateMachineModifier<'saga>) =
        builder.When(Event<'event>.Of smm, tee (tuple smm >> activity)) |> ignore
    
    let onFiltered<'saga,'event
                when 'saga: not struct
                and 'saga :> ISaga
                and 'saga :> SagaStateMachineInstance
                and 'event: not struct> 
            filter activity (builder:IStateMachineEventActivitiesBuilder<'saga>,smm:IStateMachineModifier<'saga>) =
        builder.When(Event<'event>.Of smm, StateMachineCondition<'saga,'event> filter, tee (tuple smm >> activity)) |> ignore
        
    let stateActivity<'saga
                when 'saga: not struct
                and 'saga :> ISaga
                and 'saga :> SagaStateMachineInstance> =
        StateActivityBuilder<'saga>(fun cfg args -> cfg args |> ignore)
    
    let activity<'saga
                when 'saga: not struct
                and 'saga :> ISaga
                and 'saga :> SagaStateMachineInstance> =
        ActivityBuilder<'saga>(fun cfg args -> cfg args |> ignore)

    let eventActivity<'saga,'event
                when 'saga: not struct
                and 'saga :> ISaga
                and 'saga :> SagaStateMachineInstance
                and 'event: not struct> =
        EventActivityBuilder<'saga, 'event>(fun cfg args -> cfg args |> ignore)

    let event<'event when 'event : not struct> = EventBuilder<'event>()