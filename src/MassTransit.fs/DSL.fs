namespace MassTransit.FSharp

open System
open System.Threading.Tasks
open MassTransit

type MTState = MassTransit.State
type MTState<'saga when 'saga: not struct and 'saga :> ISaga> = MassTransit.State<'saga>
type MTEvent = MassTransit.Event
type MTEvent<'e when 'e : not struct> = MassTransit.Event<'e>

[<RequireQualifiedAccess>]
type MissingInstance<'event when 'event : not struct> =
    static member Discard (cfg:IMissingInstanceConfigurator<'saga,'event>) =
        cfg.Discard()
    static member Fault (cfg:IMissingInstanceConfigurator<'saga,'event>) =
        cfg.Fault()
    static member Redeliver callback (cfg:IMissingInstanceConfigurator<'saga,'event>) =
        cfg.Redeliver(Action<IMissingInstanceRedeliveryConfigurator<'saga,'event>> callback)
    static member Execute callback (cfg:IMissingInstanceConfigurator<'saga,'event>) =
        cfg.Execute(Action<ConsumeContext<'event>> callback)
    static member ExecuteAsync callback (cfg:IMissingInstanceConfigurator<'saga,'event>) =
        cfg.ExecuteAsync(Func<ConsumeContext<'event>,Task> callback)

[<RequireQualifiedAccess>]
type State = 
    static member Of (e:#Enum) = 
        fun (smm: IStateMachineModifier<'saga>) ->
            let mutable state: MTState<'saga> = null
            smm.State(string e, &state) |> ignore
            state :> MTState

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
        static member Configure cfg cacheEvent (smm: IStateMachineModifier<'saga>) = 
            let mutable event: MTEvent<'event> = null
            smm.Event(typeof<'event>.Name, Action<IEventCorrelationConfigurator<'saga,'event>> cfg, &event)
            |> fun smm -> cacheEvent(event.Name, event); smm

    [<RequireQualifiedAccess>]
    type State<'enum when 'enum :> Enum and 'enum: struct and 'enum: (new: unit -> 'enum)> =
        static member Declare (smm: IStateMachineModifier<'saga>) = 
            for e in Enum.GetValues<'enum>() do
                let mutable state: MTState<'saga> = null
                smm.State(string e, &state) |> ignore
            smm

        static member Tuple  (mkState: _ -> #MTState) (smm: IStateMachineModifier<'saga>) = mkState smm, smm

        static member Initially (smm: IStateMachineModifier<'saga>) = smm.Initially()

        static member During (mkState: _ -> MTState) (smm: IStateMachineModifier<'saga>) = smm.During(mkState smm)

        static member DuringAny (smm: IStateMachineModifier<'saga>) = smm.DuringAny()

    let private bindActivities smm activities binder = (binder,activities) ||> Seq.fold (fun binder a -> a (binder,smm))
    
    [<RequireQualifiedAccess>]
    type Activity =

        static member BuildAll (activities:#seq<(string->MTEvent)*IStateMachineEventActivitiesBuilder<'saga> -> unit>) getEvent builder =
            for a in activities do a (getEvent,builder)
            builder :> IStateMachineModifier<'saga>

        static member WhenEnter (activities:#seq<EventActivityBinder<'saga>*IStateMachineModifier<'saga> -> EventActivityBinder<'saga>>) (state, smm:IStateMachineModifier<'saga>) =
            smm.WhenEnter(state, bindActivities smm activities)
        
        static member WhenEnterAny (activities:#seq<EventActivityBinder<'saga>*IStateMachineModifier<'saga> -> EventActivityBinder<'saga>>) (smm:IStateMachineModifier<'saga>) =
            smm.WhenEnterAny(bindActivities smm activities)
        
        static member WhenLeave (activities:#seq<EventActivityBinder<'saga>*IStateMachineModifier<'saga> -> EventActivityBinder<'saga>>) (state, smm:IStateMachineModifier<'saga>) =
            smm.WhenLeave(state, bindActivities smm activities)
        
        static member WhenLeaveAny (activities:#seq<EventActivityBinder<'saga>*IStateMachineModifier<'saga> -> EventActivityBinder<'saga>>) (smm:IStateMachineModifier<'saga>) =
            smm.WhenLeaveAny(bindActivities smm activities)
        
        static member Finally (activities:#seq<EventActivityBinder<'saga>*IStateMachineModifier<'saga> -> EventActivityBinder<'saga>>) (smm:IStateMachineModifier<'saga>) =
            smm.Finally(bindActivities smm activities)
        
        static member AfterLeave (activities:#seq<EventActivityBinder<'saga,MTState>*IStateMachineModifier<'saga> -> EventActivityBinder<'saga,MTState>>) (state, smm:IStateMachineModifier<'saga>) =
            smm.AfterLeave(state, bindActivities smm activities)
        
        static member AfterLeaveAny (activities:#seq<EventActivityBinder<'saga,MTState>*IStateMachineModifier<'saga> -> EventActivityBinder<'saga,MTState>>) (smm:IStateMachineModifier<'saga>) =
            smm.AfterLeaveAny(bindActivities smm activities)
        
        static member BeforeEnter (activities:#seq<EventActivityBinder<'saga,MTState>*IStateMachineModifier<'saga> -> EventActivityBinder<'saga,MTState>>) (state, smm:IStateMachineModifier<'saga>) =
            smm.BeforeEnter(state, bindActivities smm activities)
        
        static member BeforeEnterAny (activities:#seq<EventActivityBinder<'saga,MTState>*IStateMachineModifier<'saga> -> EventActivityBinder<'saga,MTState>>) (smm:IStateMachineModifier<'saga>) =
            smm.BeforeEnterAny(bindActivities smm activities)


type EventCorrelationBuilder<'event when 'event : not struct>() = 
    member _.Zero _ = fun (x:IEventCorrelationConfigurator<'saga,'event>) -> ()
    member _.Yield _ = fun (x:IEventCorrelationConfigurator<'saga,'event>) -> ()
    
    [<CustomOperation "by">]
    member _.By(state, correlationExpression) =
        fun (x:IEventCorrelationConfigurator<'saga,'event>) -> x.CorrelateBy correlationExpression |> state

    [<CustomOperation "byId">]
    member _.ById(state, conf) =
        fun (x:IEventCorrelationConfigurator<'saga,'event>) -> x.CorrelateById (Func<ConsumeContext<'event>,Guid> conf) |> state
    
    [<CustomOperation "onMissing">]
    member _.OnMissing(state, conf) =
        fun (x:IEventCorrelationConfigurator<'saga,'event>) -> x.OnMissingInstance(Func<IMissingInstanceConfigurator<'saga,'event>,IPipe<ConsumeContext<'event>>> conf) |> state


type ActivityBuilder<'saga
                    when 'saga: not struct
                    and 'saga :> ISaga
                    and 'saga :> SagaStateMachineInstance>() =
    let bind (cfg: EventActivityBinder<'saga> * IStateMachineModifier<'saga> -> EventActivityBinder<'saga>) 
             (state: EventActivityBinder<'saga> * IStateMachineModifier<'saga> -> EventActivityBinder<'saga>) =
        fun (binder,smm) -> state (binder,smm) |> fun binder -> cfg (binder,smm)

    member _.Yield _ = fun (binder:EventActivityBinder<'saga>,_:IStateMachineModifier<'saga>) -> binder
    
    [<CustomOperation "conditionally">]
    member _.If(state,cond,onTrue) =
        state |> bind (fun (binder,smm) -> binder.If(cond, tuple smm >> onTrue))

    [<CustomOperation "conditionally">]
    member _.If(state,cond,onTrue) =
        state |> bind (fun (binder,smm) -> binder.IfAsync(cond, tuple smm >> onTrue))

    [<CustomOperation "ifElse">]
    member _.IfElse(state,cond,onTrue,onFalse) =
        state |> bind (fun (binder,smm) -> binder.IfElse(cond, tuple smm >> onTrue, tuple smm >> onFalse))

    [<CustomOperation "ifElse">]
    member _.IfElse(state,cond,onTrue,onFalse) =
        state |> bind (fun (binder,smm) -> binder.IfElseAsync(cond, tuple smm >> onTrue, tuple smm >> onFalse))

    [<CustomOperation "transition">]
    member _.Transition(state, mkState) =
        state |> bind (fun (binder,smm) -> binder.TransitionTo(mkState smm))

    [<CustomOperation "handle">]
    member _.Handle(state, handler) =
        state |> bind (fun (binder,smm) -> binder.Then(handler))

    [<CustomOperation "bind">]
    member _.Bind(state,binding) =
        state |> bind (fun (binder,_) -> binding binder)


type EventActivityBuilder<'saga, 'event
                    when 'saga: not struct
                    and 'saga :> ISaga
                    and 'saga :> SagaStateMachineInstance
                    and 'event: not struct> () =
    let bind (cfg: EventActivityBinder<'saga,'event> * IStateMachineModifier<'saga> -> EventActivityBinder<'saga,'event>) 
             (state: EventActivityBinder<'saga,'event> * IStateMachineModifier<'saga> -> EventActivityBinder<'saga,'event>) =
        fun (binder,smm) -> state (binder,smm) |> fun binder -> cfg (binder,smm)

    member _.Yield _ = fun (binder:EventActivityBinder<'saga,'event>,_:IStateMachineModifier<'saga>) -> binder
    
    [<CustomOperation "conditionally">]
    member _.If(state, cond, onTrue) =
        state |> bind (fun (binder,smm) -> binder.If(StateMachineCondition<'saga,'event> cond, tuple smm >> onTrue))

    [<CustomOperation "conditionally">]
    member _.If(state,cond,onTrue) =
        state |> bind (fun (binder,smm) -> binder.IfAsync(StateMachineAsyncCondition<'saga,'event> cond, tuple smm >> onTrue))

    [<CustomOperation "ifElse">]
    member _.IfElse(state,cond,onTrue,onFalse) =
        state |> bind (fun (binder,smm) -> binder.IfElse(StateMachineCondition<'saga,'event> cond, tuple smm >> onTrue, tuple smm >> onFalse))

    [<CustomOperation "ifElse">]
    member _.IfElse(state,cond,onTrue,onFalse) =
        state |> bind (fun (binder,smm) -> binder.IfElseAsync(StateMachineAsyncCondition<'saga,'event> cond, tuple smm >> onTrue, tuple smm >> onFalse))

    [<CustomOperation "transition">]
    member _.Transition(state, mkState) =
        state |> bind (fun (binder,smm) -> binder.TransitionTo(mkState smm))

    [<CustomOperation "handle">]
    member _.Handle(state, handler) =
        state |> bind (fun (binder,smm) -> binder.Then(Action<BehaviorContext<'saga,'event>> handler))

    [<CustomOperation "bind">]
    member _.Bind(state,binding) =
        state |> bind (fun (binder,_) -> binding binder)


type TransitionActivityBuilder<'saga 
                    when 'saga: not struct
                    and 'saga :> ISaga
                    and 'saga :> SagaStateMachineInstance>() =
    let bind (cfg: EventActivityBinder<'saga,MTState> * IStateMachineModifier<'saga> -> EventActivityBinder<'saga,MTState>) 
             (state: EventActivityBinder<'saga,MTState> * IStateMachineModifier<'saga> -> EventActivityBinder<'saga,MTState>) =
       fun (binder,smm) -> state (binder,smm) |> fun binder -> cfg (binder,smm)

    member _.Yield _ = fun (binder:EventActivityBinder<'saga,MTState>,_:IStateMachineModifier<'saga>) -> binder

    [<CustomOperation "conditionally">]
    member _.If(state,cond,onTrue) =
        state |> bind (fun (binder,smm) -> binder.If(StateMachineCondition<'saga,MTState> cond, tuple smm >> onTrue))

    [<CustomOperation "conditionally">]
    member _.If(state,cond,onTrue) =
        state |> bind (fun (binder,smm) -> binder.IfAsync(StateMachineAsyncCondition<'saga,MTState> cond, tuple smm >> onTrue))

    [<CustomOperation "ifElse">]
    member _.IfElse(state,cond,onTrue,onFalse) =
        state |> bind (fun (binder,smm) -> binder.IfElse(StateMachineCondition<'saga,MTState> cond, tuple smm >> onTrue, tuple smm >> onFalse))

    [<CustomOperation "ifElse">]
    member _.IfElse(state,cond,onTrue,onFalse) =
        state |> bind (fun (binder,smm) -> binder.IfElseAsync(StateMachineAsyncCondition<'saga,MTState> cond, tuple smm >> onTrue, tuple smm >> onFalse)) 
    
    [<CustomOperation "handle">]
    member _.Handle(state, handler) =
        state |> bind (fun (binder,smm) -> binder.Then(handler))

    [<CustomOperation "bind">]
    member _.Bind(state,binding) =
        state |> bind (fun (binder,_) -> binding binder)


type StateMachineBuilder<'saga, 'enum
                when 'saga :> SagaStateMachineInstance
                and 'saga : not struct 
                and 'enum :> Enum and 'enum: struct and 'enum: (new: unit -> 'enum)>() = 
    let eventsCache = System.Collections.Generic.Dictionary<string,Event>()
    member _.Yield _ = State<'enum>.Declare
        
    [<CustomOperation "event">]
    member _.Event(state: IStateMachineModifier<'saga> -> IStateMachineModifier<'saga>, event: IEventCorrelationConfigurator<'saga,'event> -> unit) =
        state >> Event<'event>.Configure event eventsCache.Add
    
    [<CustomOperation "instanceState">]
    member _.InstanceState(state: IStateMachineModifier<'saga> -> IStateMachineModifier<'saga>,
                           property:Linq.Expressions.Expression<Func<'saga,string>>) =
        state >> fun (smm:IStateMachineModifier<'saga>) -> smm.InstanceState property

    [<CustomOperation "instanceState">]
    member _.InstanceState(state: IStateMachineModifier<'saga> -> IStateMachineModifier<'saga>,
                           property:Linq.Expressions.Expression<Func<'saga,int>>) =
        state >> fun (smm:IStateMachineModifier<'saga>) -> smm.InstanceState property

    [<CustomOperation "instanceState">]
    member _.InstanceState(state: IStateMachineModifier<'saga> -> IStateMachineModifier<'saga>,
                           property:Linq.Expressions.Expression<Func<'saga,MTState>>) =
        state >> fun (smm:IStateMachineModifier<'saga>) -> smm.InstanceState property
    
    [<CustomOperation "initially">]
    member _.Initially(state: IStateMachineModifier<'saga> -> IStateMachineModifier<'saga>, 
                       activities: list<(string->MTEvent)*IStateMachineEventActivitiesBuilder<'saga> -> unit>) =
        state >> State<'enum>.Initially >> Activity.BuildAll activities eventsCache.get_Item
    
    [<CustomOperation "finally">]
    member _.Finally(state: IStateMachineModifier<'saga> -> IStateMachineModifier<'saga>,
                     activities: list<EventActivityBinder<'saga>*IStateMachineModifier<'saga> -> EventActivityBinder<'saga>>) =
        state >> Activity.Finally activities

    [<CustomOperation "duringAny">]
    member _.DuringAny(state: IStateMachineModifier<'saga> -> IStateMachineModifier<'saga>,
                       activities: list<(string->MTEvent)*IStateMachineEventActivitiesBuilder<'saga> -> unit>) =
        state >> State<'enum>.DuringAny >> Activity.BuildAll activities eventsCache.get_Item
    
    [<CustomOperation "during">]
    member _.During(state: IStateMachineModifier<'saga> -> IStateMachineModifier<'saga>,
                    mkState: IStateMachineModifier<'saga> -> MTState,
                    activities: list<(string->MTEvent)*IStateMachineEventActivitiesBuilder<'saga> -> unit>) =
        state >> State<'enum>.During mkState >> Activity.BuildAll activities eventsCache.get_Item

    [<CustomOperation "whenEnter">]
    member _.WhenEnter(state: IStateMachineModifier<'saga> -> IStateMachineModifier<'saga>,
                       mkState: IStateMachineModifier<'saga> -> MTState,
                       activities: list<EventActivityBinder<'saga>*IStateMachineModifier<'saga> -> EventActivityBinder<'saga>>) =
        state >> State<'enum>.Tuple mkState >> Activity.WhenEnter activities

    [<CustomOperation "whenEnterAny">]
    member _.WhenEnterAny(state: IStateMachineModifier<'saga> -> IStateMachineModifier<'saga>,
                          activities: list<EventActivityBinder<'saga>*IStateMachineModifier<'saga> -> EventActivityBinder<'saga>>) =
        state >> Activity.WhenEnterAny activities

    [<CustomOperation "whenLeave">]
    member _.WhenLeave(state: IStateMachineModifier<'saga> -> IStateMachineModifier<'saga>,
                       mkState: IStateMachineModifier<'saga> -> MTState,
                       activities: list<EventActivityBinder<'saga>*IStateMachineModifier<'saga> -> EventActivityBinder<'saga>>) =
        state >> State<'enum>.Tuple mkState >> Activity.WhenLeave activities

    [<CustomOperation "whenLeaveAny">]
    member _.WhenLeaveAny(state: IStateMachineModifier<'saga> -> IStateMachineModifier<'saga>,
                          activities: list<EventActivityBinder<'saga>*IStateMachineModifier<'saga> -> EventActivityBinder<'saga>>) =
        state >> Activity.WhenLeaveAny activities

    [<CustomOperation "afterLeave">]
    member _.WhenLeave(state: IStateMachineModifier<'saga> -> IStateMachineModifier<'saga>,
                       mkState: IStateMachineModifier<'saga> -> MTState,
                       activities: list<EventActivityBinder<'saga,MTState>*IStateMachineModifier<'saga> -> EventActivityBinder<'saga,MTState>>) =
        state >> State<'enum>.Tuple mkState >> Activity.AfterLeave activities

    [<CustomOperation "afterLeaveAny">]
    member _.AfterLeaveAny(state: IStateMachineModifier<'saga> -> IStateMachineModifier<'saga>,
                           activities: list<EventActivityBinder<'saga,MTState>*IStateMachineModifier<'saga> -> EventActivityBinder<'saga,MTState>>) =
        state >> Activity.AfterLeaveAny activities

    [<CustomOperation "beforeEnter">]
    member _.BeforeEnter(state: IStateMachineModifier<'saga> -> IStateMachineModifier<'saga>,
                         mkState: IStateMachineModifier<'saga> -> MTState,
                         activities: list<EventActivityBinder<'saga,MTState>*IStateMachineModifier<'saga> -> EventActivityBinder<'saga,MTState>>) =
        state >> State<'enum>.Tuple mkState >> Activity.BeforeEnter activities

    [<CustomOperation "beforeEnterAny">]
    member _.BeforeEnterAny(state: IStateMachineModifier<'saga> -> IStateMachineModifier<'saga>,
                            activities: list<EventActivityBinder<'saga,MTState>*IStateMachineModifier<'saga> -> EventActivityBinder<'saga,MTState>>) =
        state >> Activity.BeforeEnterAny activities


[<AutoOpen>]
module Bindings =
    let stateMachine<'saga, 'enum
                when 'saga :> SagaStateMachineInstance
                and 'saga : not struct 
                and 'enum :> Enum and 'enum: struct and 'enum: (new: unit -> 'enum)> = 
        StateMachineBuilder<'saga, 'enum>()
    
    let on<'saga,'event
                when 'saga: not struct
                and 'saga :> ISaga
                and 'saga :> SagaStateMachineInstance
                and 'event: not struct>
            activity (getEvent:string->MTEvent, builder:IStateMachineEventActivitiesBuilder<'saga>) =
        builder.When(getEvent typeof<'event>.Name |> unbox<MTEvent<'event>>,
                     tuple (builder :> IStateMachineModifier<'saga>) >> activity) |> ignore
    
    let ignoreEvent<'saga,'event
                when 'saga: not struct
                and 'saga :> ISaga
                and 'saga :> SagaStateMachineInstance
                and 'event: not struct>
            (getEvent:string->MTEvent, builder:IStateMachineEventActivitiesBuilder<'saga>) =
        builder.Ignore(getEvent typeof<'event>.Name |> unbox<MTEvent<'event>>) |> ignore
    
    let onFiltered<'saga,'event
                when 'saga: not struct
                and 'saga :> ISaga
                and 'saga :> SagaStateMachineInstance
                and 'event: not struct> 
            filter activity (getEvent:string->MTEvent, builder:IStateMachineEventActivitiesBuilder<'saga>) =
        builder.When(getEvent typeof<'event>.Name |> unbox<MTEvent<'event>>, StateMachineCondition<'saga,'event> filter,
                     tuple (builder :> IStateMachineModifier<'saga>) >> activity) |> ignore
        
    let transitionActivity<'saga
                when 'saga: not struct
                and 'saga :> ISaga
                and 'saga :> SagaStateMachineInstance> =
        TransitionActivityBuilder<'saga>()
    
    let activity<'saga
                when 'saga: not struct
                and 'saga :> ISaga
                and 'saga :> SagaStateMachineInstance> =
        ActivityBuilder<'saga>()

    let eventActivity<'saga,'event
                when 'saga: not struct
                and 'saga :> ISaga
                and 'saga :> SagaStateMachineInstance
                and 'event: not struct> =
        EventActivityBuilder<'saga, 'event>()

    let correlated<'event when 'event : not struct> = EventCorrelationBuilder<'event>()