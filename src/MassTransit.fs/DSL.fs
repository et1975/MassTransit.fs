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
        static member Of (smm: IStateMachineModifier<'saga>) = 
            let mutable event: MTEvent<'event> = null
            smm.Event(typeof<'event>.Name, &event) |> ignore
            event
        static member Configure cfg (smm: IStateMachineModifier<'saga>) = 
            let mutable event: MTEvent<'event> = null
            smm.Event(typeof<'event>.Name, Action<IEventCorrelationConfigurator<'saga,'event>> cfg, &event)
            |> (fun smm -> printfn $"{event.Name}"; smm)


    [<RequireQualifiedAccess>]
    type State<'enum when 'enum :> Enum and 'enum: struct and 'enum: (new: unit -> 'enum)> =
        static member Declare (smm: IStateMachineModifier<'saga>) = 
            (smm, Enum.GetValues<'enum>()) ||> Seq.fold (fun m e ->
                let mutable state: MTState<'saga> = null
                m.State(string e, &state))

        static member Initially (smm: IStateMachineModifier<'saga>) = smm.Initially(), smm

        static member During (mkState: _ -> MTState) (smm: IStateMachineModifier<'saga>) = smm.During(mkState smm), smm

        static member DuringAny (smm: IStateMachineModifier<'saga>) = smm.DuringAny(), smm

        static member Tuple  (mkState: _ -> #MTState) (smm: IStateMachineModifier<'saga>) = mkState smm, smm

    [<RequireQualifiedAccess>]
    type Activity =
        static member BuildAll (activities:#seq<IStateMachineEventActivitiesBuilder<'saga>*IStateMachineModifier<'saga> -> unit>) (state, smm:IStateMachineModifier<'saga>) =
            for a in activities do a (state,smm)
            smm

        static member WhenEnter (activities:#seq<EventActivityBinder<'saga>*IStateMachineModifier<'saga> -> unit>) (state, smm:IStateMachineModifier<'saga>) =
            smm.WhenEnter(state, tee (fun binder -> for a in activities do a (binder,smm)))
        
        static member WhenEnterAny (activities:#seq<EventActivityBinder<'saga>*IStateMachineModifier<'saga> -> unit>) (smm:IStateMachineModifier<'saga>) =
            smm.WhenEnterAny(tee (fun binder -> for a in activities do a (binder,smm)))
        
        static member WhenLeave (activities:#seq<EventActivityBinder<'saga>*IStateMachineModifier<'saga> -> unit>) (state, smm:IStateMachineModifier<'saga>) =
            smm.WhenLeave(state, tee (fun binder -> for a in activities do a (binder,smm)))
        
        static member WhenLeaveAny (activities:#seq<EventActivityBinder<'saga>*IStateMachineModifier<'saga> -> unit>) (smm:IStateMachineModifier<'saga>) =
            smm.WhenLeaveAny(tee (fun binder -> for a in activities do a (binder,smm)))
        
        static member Finally (activities:#seq<EventActivityBinder<'saga>*IStateMachineModifier<'saga> -> unit>) (smm:IStateMachineModifier<'saga>) =
            smm.Finally(tee (fun binder -> for a in activities do a (binder,smm)))
        
        static member AfterLeave (activities:#seq<EventActivityBinder<'saga,MTState>*IStateMachineModifier<'saga> -> unit>) (state, smm:IStateMachineModifier<'saga>) =
            smm.AfterLeave(state, tee (fun binder -> for a in activities do a (binder,smm)))
        
        static member AfterLeaveAny (activities:#seq<EventActivityBinder<'saga,MTState>*IStateMachineModifier<'saga> -> unit>) (smm:IStateMachineModifier<'saga>) =
            smm.AfterLeaveAny(tee (fun binder -> for a in activities do a (binder,smm)))
        
        static member BeforeEnter (activities:#seq<EventActivityBinder<'saga,MTState>*IStateMachineModifier<'saga> -> unit>) (state, smm:IStateMachineModifier<'saga>) =
            smm.BeforeEnter(state, tee (fun binder -> for a in activities do a (binder,smm)))
        
        static member BeforeEnterAny (activities:#seq<EventActivityBinder<'saga,MTState>*IStateMachineModifier<'saga> -> unit>) (smm:IStateMachineModifier<'saga>) =
            smm.BeforeEnterAny(tee (fun binder -> for a in activities do a (binder,smm)))


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
                    and 'saga :> SagaStateMachineInstance>
        (apply: (EventActivityBinder<'saga> * IStateMachineModifier<'saga> -> unit) ->
                EventActivityBinder<'saga> * IStateMachineModifier<'saga> -> 
                unit) =
    let bind (cfg: EventActivityBinder<'saga> * IStateMachineModifier<'saga> -> unit) 
             (state: EventActivityBinder<'saga> * IStateMachineModifier<'saga> -> unit) =
        tee state >> apply cfg

    member _.Yield _ = fun (_:EventActivityBinder<'saga>,_:IStateMachineModifier<'saga>) -> ()
    
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

    member _.Yield _ = fun (_:EventActivityBinder<'saga,'event>,_:IStateMachineModifier<'saga>)-> ()
    
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


type TransitionActivityBuilder<'saga 
                    when 'saga: not struct
                    and 'saga :> ISaga
                    and 'saga :> SagaStateMachineInstance>
        (apply: (EventActivityBinder<'saga,MTState> * IStateMachineModifier<'saga> -> unit) ->
                EventActivityBinder<'saga,MTState> * IStateMachineModifier<'saga> -> 
                unit) =
    let bind (cfg: EventActivityBinder<'saga,MTState> * IStateMachineModifier<'saga> -> unit) 
             (state: EventActivityBinder<'saga,MTState> * IStateMachineModifier<'saga> -> unit) =
        tee state >> apply cfg

    member _.Yield _ = fun (_:EventActivityBinder<'saga,MTState>,_:IStateMachineModifier<'saga>)-> ()

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
    member _.Yield _ =
        printfn $"yield"
        State<'enum>.Declare
        
    [<CustomOperation "event">]
    member _.Event(state: IStateMachineModifier<'saga> -> IStateMachineModifier<'saga>, event: IEventCorrelationConfigurator<'saga,'event> -> unit) =
        printfn $"event"
        state >> Event<'event>.Configure event
    
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
        printfn $"instanceState"
        state >> fun (smm:IStateMachineModifier<'saga>) -> smm.InstanceState property
    
    [<CustomOperation "initially">]
    member _.Initially(state: IStateMachineModifier<'saga> -> IStateMachineModifier<'saga>, 
                       activities: list<IStateMachineEventActivitiesBuilder<'saga>*IStateMachineModifier<'saga> -> unit>) =
        printfn $"initially"
        state >> State<'enum>.Initially >> Activity.BuildAll activities
    
    [<CustomOperation "finally">]
    member _.Finally(state: IStateMachineModifier<'saga> -> IStateMachineModifier<'saga>,
                     activities: list<EventActivityBinder<'saga>*IStateMachineModifier<'saga> -> unit>) =
        state >> Activity.Finally activities

    [<CustomOperation "duringAny">]
    member _.DuringAny(state: IStateMachineModifier<'saga> -> IStateMachineModifier<'saga>,
                       activities: list<IStateMachineEventActivitiesBuilder<'saga>*IStateMachineModifier<'saga> -> unit>) =
        state >> State<'enum>.DuringAny >> Activity.BuildAll activities
    
    [<CustomOperation "during">]
    member _.During(state: IStateMachineModifier<'saga> -> IStateMachineModifier<'saga>,
                    mkState: IStateMachineModifier<'saga> -> MTState,
                    activities: list<IStateMachineEventActivitiesBuilder<'saga>*IStateMachineModifier<'saga> -> unit>) =
        state >> State<'enum>.During mkState >> Activity.BuildAll activities

    [<CustomOperation "whenEnter">]
    member _.WhenEnter(state: IStateMachineModifier<'saga> -> IStateMachineModifier<'saga>,
                       mkState: IStateMachineModifier<'saga> -> MTState,
                       activities: list<EventActivityBinder<'saga>*IStateMachineModifier<'saga> -> unit>) =
        state >> State<'enum>.Tuple mkState >> Activity.WhenEnter activities

    [<CustomOperation "whenEnterAny">]
    member _.WhenEnterAny(state: IStateMachineModifier<'saga> -> IStateMachineModifier<'saga>,
                          activities: list<EventActivityBinder<'saga>*IStateMachineModifier<'saga> -> unit>) =
        state >> Activity.WhenEnterAny activities

    [<CustomOperation "whenLeave">]
    member _.WhenLeave(state: IStateMachineModifier<'saga> -> IStateMachineModifier<'saga>,
                       mkState: IStateMachineModifier<'saga> -> MTState,
                       activities: list<EventActivityBinder<'saga>*IStateMachineModifier<'saga> -> unit>) =
        state >> State<'enum>.Tuple mkState >> Activity.WhenLeave activities

    [<CustomOperation "whenLeaveAny">]
    member _.WhenLeaveAny(state: IStateMachineModifier<'saga> -> IStateMachineModifier<'saga>,
                          activities: list<EventActivityBinder<'saga>*IStateMachineModifier<'saga> -> unit>) =
        state >> Activity.WhenLeaveAny activities

    [<CustomOperation "afterLeave">]
    member _.WhenLeave(state: IStateMachineModifier<'saga> -> IStateMachineModifier<'saga>,
                       mkState: IStateMachineModifier<'saga> -> MTState,
                       activities: list<EventActivityBinder<'saga,MTState>*IStateMachineModifier<'saga> -> unit>) =
        state >> State<'enum>.Tuple mkState >> Activity.AfterLeave activities

    [<CustomOperation "afterLeaveAny">]
    member _.AfterLeaveAny(state: IStateMachineModifier<'saga> -> IStateMachineModifier<'saga>,
                           activities: list<EventActivityBinder<'saga,MTState>*IStateMachineModifier<'saga> -> unit>) =
        state >> Activity.AfterLeaveAny activities

    [<CustomOperation "beforeEnter">]
    member _.BeforeEnter(state: IStateMachineModifier<'saga> -> IStateMachineModifier<'saga>,
                         mkState: IStateMachineModifier<'saga> -> MTState,
                         activities: list<EventActivityBinder<'saga,MTState>*IStateMachineModifier<'saga> -> unit>) =
        state >> State<'enum>.Tuple mkState >> Activity.BeforeEnter activities

    [<CustomOperation "beforeEnterAny">]
    member _.BeforeEnterAny(state: IStateMachineModifier<'saga> -> IStateMachineModifier<'saga>,
                            activities: list<EventActivityBinder<'saga,MTState>*IStateMachineModifier<'saga> -> unit>) =
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
            activity (builder:IStateMachineEventActivitiesBuilder<'saga>,smm:IStateMachineModifier<'saga>) =
        builder.When(Event<'event>.Of smm, tee (tuple smm >> activity)) |> ignore
    
    let ignoreEvent<'saga,'event
                when 'saga: not struct
                and 'saga :> ISaga
                and 'saga :> SagaStateMachineInstance
                and 'event: not struct>
            (builder:IStateMachineEventActivitiesBuilder<'saga>,smm:IStateMachineModifier<'saga>) =
        builder.Ignore(Event<'event>.Of smm) |> ignore
    
    let onFiltered<'saga,'event
                when 'saga: not struct
                and 'saga :> ISaga
                and 'saga :> SagaStateMachineInstance
                and 'event: not struct> 
            filter activity (builder:IStateMachineEventActivitiesBuilder<'saga>,smm:IStateMachineModifier<'saga>) =
        builder.When(Event<'event>.Of smm, StateMachineCondition<'saga,'event> filter, tee (tuple smm >> activity)) |> ignore
        
    let transitionActivity<'saga
                when 'saga: not struct
                and 'saga :> ISaga
                and 'saga :> SagaStateMachineInstance> =
        TransitionActivityBuilder<'saga>(fun cfg args -> cfg args |> ignore)
    
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
        EventActivityBuilder<'saga, 'event>(fun cfg args -> 
            printfn "eventActivity.apply"
            cfg args |> ignore)

    let correlated<'event when 'event : not struct> = EventCorrelationBuilder<'event>()