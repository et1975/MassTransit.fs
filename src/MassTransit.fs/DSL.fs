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

[<Struct>]
type StateMachineModifier<'saga, 'm
                when 'saga :> SagaStateMachineInstance
                and 'saga : not struct
                and 'm :> IStateMachineModifier<'saga>> =
    internal { modifier: 'm
               getState: string -> MTState
               getEvent: string -> MTEvent }

type StateMachineModifier<'saga
                when 'saga :> SagaStateMachineInstance
                and 'saga : not struct> = StateMachineModifier<'saga, IStateMachineModifier<'saga>>

module StateMachineModifier =
    let private smm = 
        let mtsm = typeof<MassTransitStateMachine<_>>
        mtsm.Assembly.GetTypes() |> Seq.find (fun t -> t.FullName.StartsWith "MassTransit.Configuration.StateMachineModifier`")
    let mkNew<'saga when 'saga : not struct 
                     and 'saga :> SagaStateMachineInstance> : MassTransitStateMachine<'saga> -> StateMachineModifier<'saga> =
        fun m -> { modifier = System.Activator.CreateInstance(smm.MakeGenericType typeof<'saga>, m ) |> unbox
                   getState = fun name -> m.States |> Seq.find (fun s -> s.Name = name)
                   getEvent = fun name -> m.Events |> Seq.find (fun s -> s.Name = name) }
                   

    let map f (smm: StateMachineModifier<'saga, 'm>) : StateMachineModifier<'saga, 'mm> = 
            { modifier = f smm.modifier
              getState = smm.getState 
              getEvent = smm.getEvent }


[<RequireQualifiedAccess>]
type State = 
    static member Of (e:#Enum) (smm: StateMachineModifier<'saga,'m>) =
        smm.getState (string e)

    static member Final (smm: StateMachineModifier<'saga,'m>) = smm.modifier.Final
    static member Initial (smm: StateMachineModifier<'saga,'m>) = smm.modifier.Initial

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

[<RequireQualifiedAccess>]
type Respond<'saga,'event,'msg
                when 'saga: not struct
                and 'saga :> ISaga
                and 'saga :> SagaStateMachineInstance
                and 'event: not struct
                and 'msg: not struct> = 
    static member With(binder:EventActivityBinder<'saga,'event>) =
        fun mkMsg -> binder.Respond(EventMessageFactory<'saga,'event,'msg> mkMsg, Action<SendContext<'msg>> ignore)

    static member With<'exn when 'exn :> exn>(binder:ExceptionActivityBinder<'saga,'event,'exn>) =
        fun mkMsg -> binder.Respond(EventExceptionMessageFactory<'saga,'event,'exn,'msg> mkMsg, Action<SendContext<'msg>> ignore)

    static member WithCtx(binder:EventActivityBinder<'saga,'event>) =
        fun mkMsg callback -> binder.Respond(EventMessageFactory<'saga,'event,'msg> mkMsg, Action<SendContext<'msg>> callback)

    static member WithCtx<'exn when 'exn :> exn>(binder:ExceptionActivityBinder<'saga,'event,'exn>) =
        fun mkMsg callback -> binder.Respond(EventExceptionMessageFactory<'saga,'event,'exn,'msg> mkMsg, Action<SendContext<'msg>> callback)

[<AutoOpen>]
module private Adapters =
    [<RequireQualifiedAccess>]
    type Event<'event when 'event : not struct> =
        static member Configure cfg (smm: StateMachineModifier<'saga,'m>) = 
            let mutable event: MTEvent<'event> = null
            smm.modifier.Event(typeof<'event>.Name, Action<IEventCorrelationConfigurator<'saga,'event>> cfg, &event) |> ignore
            smm

    [<RequireQualifiedAccess>]
    type State<'enum when 'enum :> Enum and 'enum: struct and 'enum: (new: unit -> 'enum)> =
        static member Declare (smm: StateMachineModifier<'saga,'m>) = 
            for e in Enum.GetValues<'enum>() do
                let mutable state: MTState<'saga> = null
                smm.modifier.State(string e, &state) |> ignore
            smm
            
        static member Tuple  (mkState: _ -> #MTState) (smm: StateMachineModifier<'saga,'m>) = mkState smm, smm

        static member Initially (smm: StateMachineModifier<'saga,'m>) = 
            smm |> StateMachineModifier.map (fun m -> m.Initially())

        static member Property (property:Linq.Expressions.Expression<Func<'saga,string>>) = 
            StateMachineModifier.map (fun m -> m.InstanceState property)
        
        static member Property (property:Linq.Expressions.Expression<Func<'saga,int>>) = 
            StateMachineModifier.map (fun m -> m.InstanceState property)
        
        static member Property (property:Linq.Expressions.Expression<Func<'saga,MTState>>) = 
            StateMachineModifier.map (fun m -> m.InstanceState property)
        
        static member During (mkState: _ -> MTState) = 
            fun smm -> smm |> StateMachineModifier.map (fun m -> m.During(mkState smm))
        
        static member During (mkStates: #seq<_ -> MTState>) = 
            fun smm -> smm |> StateMachineModifier.map (fun m -> m.During(mkStates |> Seq.map (fun mkState -> mkState smm) |> Seq.toArray))

        static member DuringAny (smm: StateMachineModifier<'saga,'m>) = smm |> StateMachineModifier.map (fun m -> m.DuringAny())

    let private bindActivities smm activities binder = (binder,activities) ||> Seq.fold (fun binder a -> a (binder,smm))
    
    [<RequireQualifiedAccess>]
    type Activity =

        static member BuildAll (builders:#seq<StateMachineModifier<'saga,IStateMachineEventActivitiesBuilder<'saga>> -> StateMachineModifier<'saga,_>>) smm =
            (smm,builders) ||> Seq.fold (fun smm a -> a smm) |> StateMachineModifier.map (fun smm -> smm :> IStateMachineModifier<'saga>)

        static member WhenEnter (activities:#seq<EventActivityBinder<'saga>*StateMachineModifier<'saga> -> EventActivityBinder<'saga>>) (state, smm:StateMachineModifier<'saga>) =
            smm |> StateMachineModifier.map (fun m -> m.WhenEnter(state, bindActivities smm activities))
        
        static member WhenEnterAny (activities:#seq<EventActivityBinder<'saga>*StateMachineModifier<'saga> -> EventActivityBinder<'saga>>) (smm:StateMachineModifier<'saga>) =
            smm |> StateMachineModifier.map (fun m -> m.WhenEnterAny(bindActivities smm activities))
        
        static member WhenLeave (activities:#seq<EventActivityBinder<'saga>*StateMachineModifier<'saga> -> EventActivityBinder<'saga>>) (state, smm:StateMachineModifier<'saga>) =
            smm |> StateMachineModifier.map (fun m -> m.WhenLeave(state, bindActivities smm activities))
        
        static member WhenLeaveAny (activities:#seq<EventActivityBinder<'saga>*StateMachineModifier<'saga> -> EventActivityBinder<'saga>>) (smm:StateMachineModifier<'saga>) =
            smm |> StateMachineModifier.map (fun m -> m.WhenLeaveAny(bindActivities smm activities))
        
        static member Finally (activities:#seq<EventActivityBinder<'saga>*StateMachineModifier<'saga> -> EventActivityBinder<'saga>>) (smm:StateMachineModifier<'saga>) =
            smm |> StateMachineModifier.map (fun m -> m.Finally(bindActivities smm activities))
        
        static member AfterLeave (activities:#seq<EventActivityBinder<'saga,MTState>*StateMachineModifier<'saga> -> EventActivityBinder<'saga,MTState>>) (state, smm:StateMachineModifier<'saga>) =
            smm |> StateMachineModifier.map (fun m -> m.AfterLeave(state, bindActivities smm activities))
        
        static member AfterLeaveAny (activities:#seq<EventActivityBinder<'saga,MTState>*StateMachineModifier<'saga> -> EventActivityBinder<'saga,MTState>>) (smm:StateMachineModifier<'saga>) =
            smm |> StateMachineModifier.map (fun m -> m.AfterLeaveAny(bindActivities smm activities))
        
        static member BeforeEnter (activities:#seq<EventActivityBinder<'saga,MTState>*StateMachineModifier<'saga> -> EventActivityBinder<'saga,MTState>>) (state, smm:StateMachineModifier<'saga>) =
            smm |> StateMachineModifier.map (fun m -> m.BeforeEnter(state, bindActivities smm activities))
        
        static member BeforeEnterAny (activities:#seq<EventActivityBinder<'saga,MTState>*StateMachineModifier<'saga> -> EventActivityBinder<'saga,MTState>>) (smm:StateMachineModifier<'saga>) =
            smm |> StateMachineModifier.map (fun m -> m.BeforeEnterAny(bindActivities smm activities))


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
    let bind (cfg: EventActivityBinder<'saga> * StateMachineModifier<'saga,'m> -> EventActivityBinder<'saga>) 
             (state: EventActivityBinder<'saga> * StateMachineModifier<'saga,'m> -> EventActivityBinder<'saga>) =
        fun (binder,smm) -> state (binder,smm) |> fun binder -> cfg (binder,smm)

    member _.Yield _ = fun (binder:EventActivityBinder<'saga>,_:StateMachineModifier<'saga,'m>) -> binder
    
    [<CustomOperation "conditionally">]
    member _.If(state, cond, onTrue) =
        state |> bind (fun (binder,smm) -> binder.If(cond, tuple smm >> onTrue))

    [<CustomOperation "conditionally">]
    member _.If(state, cond, onTrue) =
        state |> bind (fun (binder,smm) -> binder.IfAsync(cond, tuple smm >> onTrue))

    [<CustomOperation "ifElse">]
    member _.IfElse(state, cond, onTrue, onFalse) =
        state |> bind (fun (binder,smm) -> binder.IfElse(cond, tuple smm >> onTrue, tuple smm >> onFalse))

    [<CustomOperation "ifElse">]
    member _.IfElse(state, cond, onTrue, onFalse) =
        state |> bind (fun (binder,smm) -> binder.IfElseAsync(cond, tuple smm >> onTrue, tuple smm >> onFalse))

    [<CustomOperation "transition">]
    member _.Transition(state, mkState) =
        state |> bind (fun (binder,smm) -> binder.TransitionTo(mkState smm))

    [<CustomOperation "handle">]
    member _.Handle(state, handler) =
        state |> bind (fun (binder,smm) -> binder.Then(Action<BehaviorContext<'saga>> handler))

    [<CustomOperation "catch">]
    member _.Catch(state, handler) =
        state |> bind (fun (binder,smm) -> binder.Catch(tuple smm >> handler))

    [<CustomOperation "retry">]
    member _.Retry(state, configure, callback) =
        state |> bind (fun (binder,smm) -> binder.Retry(Action<IRetryConfigurator> configure, tuple smm >> callback))

    [<CustomOperation "bind">]
    member _.Bind(state, binding) =
        state |> bind (fun (binder,_) -> binding binder)


type EventActivityBuilder<'saga, 'event
                    when 'saga: not struct
                    and 'saga :> ISaga
                    and 'saga :> SagaStateMachineInstance
                    and 'event: not struct> () =
    let bind (cfg: EventActivityBinder<'saga,'event> * StateMachineModifier<'saga,'m> -> EventActivityBinder<'saga,'event>) 
             (state: EventActivityBinder<'saga,'event> * StateMachineModifier<'saga,'m> -> EventActivityBinder<'saga,'event>) =
        fun (binder,smm) -> state (binder,smm) |> fun binder -> cfg (binder,smm)

    member _.Yield _ = fun (binder:EventActivityBinder<'saga,'event>,_:StateMachineModifier<'saga,'m>) -> binder
    
    [<CustomOperation "conditionally">]
    member _.If(state, cond, onTrue) =
        state |> bind (fun (binder,smm) -> binder.If(StateMachineCondition<'saga,'event> cond, tuple smm >> onTrue))

    [<CustomOperation "conditionally">]
    member _.If(state, cond, onTrue) =
        state |> bind (fun (binder,smm) -> binder.IfAsync(StateMachineAsyncCondition<'saga,'event> cond, tuple smm >> onTrue))

    [<CustomOperation "ifElse">]
    member _.IfElse(state,cond,onTrue,onFalse) =
        state |> bind (fun (binder,smm) -> binder.IfElse(StateMachineCondition<'saga,'event> cond, tuple smm >> onTrue, tuple smm >> onFalse))

    [<CustomOperation "ifElse">]
    member _.IfElse(state, cond, onTrue, onFalse) =
        state |> bind (fun (binder,smm) -> binder.IfElseAsync(StateMachineAsyncCondition<'saga,'event> cond, tuple smm >> onTrue, tuple smm >> onFalse))

    [<CustomOperation "transition">]
    member _.Transition(state, mkState) =
        state |> bind (fun (binder,smm) -> binder.TransitionTo(mkState smm))

    [<CustomOperation "handle">]
    member _.Handle(state, handler) =
        state |> bind (fun (binder,smm) -> binder.Then(Action<BehaviorContext<'saga,'event>> handler))

    [<CustomOperation "catch">]
    member _.Catch(state, handler) =
        state |> bind (fun (binder,smm) -> binder.Catch(tuple smm >> handler))

    [<CustomOperation "retry">]
    member _.Retry(state, configure, callback) =
        state |> bind (fun (binder,smm) -> binder.Retry(Action<IRetryConfigurator> configure, tuple smm >> callback))

    [<CustomOperation "bind">]
    member _.Bind(state, binding) =
        state |> bind (fun (binder,_) -> binding binder)


type ExceptionActivityBuilder<'saga, 'event, 'exn
                    when 'saga: not struct
                    and 'saga :> ISaga
                    and 'saga :> SagaStateMachineInstance
                    and 'event: not struct
                    and 'exn :> exn> () =
    let bind (cfg: ExceptionActivityBinder<'saga,'event,'exn> * StateMachineModifier<'saga,'m> -> ExceptionActivityBinder<'saga,'event,'exn>) 
             (state: ExceptionActivityBinder<'saga,'event,'exn> * StateMachineModifier<'saga,'m> -> ExceptionActivityBinder<'saga,'event,'exn>) =
        fun (binder,smm) -> state (binder,smm) |> fun binder -> cfg (binder,smm)

    member _.Yield _ = fun (binder:ExceptionActivityBinder<'saga,'event,'exn>,_:StateMachineModifier<'saga,'m>) -> binder
    
    [<CustomOperation "conditionally">]
    member _.If(state, cond, onTrue) =
        state |> bind (fun (binder,smm) -> binder.If(StateMachineExceptionCondition<'saga,'event,'exn> cond, tuple smm >> onTrue))

    [<CustomOperation "conditionally">]
    member _.If(state, cond, onTrue) =
        state |> bind (fun (binder,smm) -> binder.IfAsync(StateMachineAsyncExceptionCondition<'saga,'event,'exn> cond, tuple smm >> onTrue))

    [<CustomOperation "ifElse">]
    member _.IfElse(state,cond,onTrue,onFalse) =
        state |> bind (fun (binder,smm) -> binder.IfElse(StateMachineExceptionCondition<'saga,'event,'exn> cond, tuple smm >> onTrue, tuple smm >> onFalse))

    [<CustomOperation "ifElse">]
    member _.IfElse(state, cond, onTrue, onFalse) =
        state |> bind (fun (binder,smm) -> binder.IfElseAsync(StateMachineAsyncExceptionCondition<'saga,'event,'exn> cond, tuple smm >> onTrue, tuple smm >> onFalse))

    [<CustomOperation "transition">]
    member _.Transition(state, mkState) =
        state |> bind (fun (binder,smm) -> binder.TransitionTo(mkState smm))

    [<CustomOperation "handle">]
    member _.Handle(state, handler) =
        state |> bind (fun (binder,smm) -> binder.Then(Action<BehaviorExceptionContext<'saga,'event,'exn>> handler))

    [<CustomOperation "catch">]
    member _.Catch(state, handler) =
        state |> bind (fun (binder,smm) -> binder.Catch(tuple smm >> handler))

    [<CustomOperation "bind">]
    member _.Bind(state, binding) =
        state |> bind (fun (binder,_) -> binding binder)


type TransitionActivityBuilder<'saga 
                    when 'saga: not struct
                    and 'saga :> ISaga
                    and 'saga :> SagaStateMachineInstance>() =
    let bind (cfg: EventActivityBinder<'saga,MTState> * StateMachineModifier<'saga,'m> -> EventActivityBinder<'saga,MTState>) 
             (state: EventActivityBinder<'saga,MTState> * StateMachineModifier<'saga,'m> -> EventActivityBinder<'saga,MTState>) =
       fun (binder,smm) -> state (binder,smm) |> fun binder -> cfg (binder,smm)

    member _.Yield _ = fun (binder:EventActivityBinder<'saga,MTState>,_:StateMachineModifier<'saga,'m>) -> binder

    [<CustomOperation "conditionally">]
    member _.If(state, cond, onTrue) =
        state |> bind (fun (binder,smm) -> binder.If(StateMachineCondition<'saga,MTState> cond, tuple smm >> onTrue))

    [<CustomOperation "conditionally">]
    member _.If(state,cond,onTrue) =
        state |> bind (fun (binder,smm) -> binder.IfAsync(StateMachineAsyncCondition<'saga,MTState> cond, tuple smm >> onTrue))

    [<CustomOperation "ifElse">]
    member _.IfElse(state, cond, onTrue, onFalse) =
        state |> bind (fun (binder,smm) -> binder.IfElse(StateMachineCondition<'saga,MTState> cond, tuple smm >> onTrue, tuple smm >> onFalse))

    [<CustomOperation "ifElse">]
    member _.IfElse(state,cond,onTrue,onFalse) =
        state |> bind (fun (binder,smm) -> binder.IfElseAsync(StateMachineAsyncCondition<'saga,MTState> cond, tuple smm >> onTrue, tuple smm >> onFalse)) 
    
    [<CustomOperation "handle">]
    member _.Handle(state, handler) =
        state |> bind (fun (binder,smm) -> binder.Then(Action<BehaviorContext<'saga,MTState>> handler))

    [<CustomOperation "catch">]
    member _.Catch(state, handler) =
        state |> bind (fun (binder,smm) -> binder.Catch(tuple smm >> handler))

    [<CustomOperation "retry">]
    member _.Retry(state, configure, callback) =
        state |> bind (fun (binder,smm) -> binder.Retry(Action<IRetryConfigurator> configure, tuple smm >> callback))

    [<CustomOperation "bind">]
    member _.Bind(state, binding) =
        state |> bind (fun (binder,_) -> binding binder)


type StateMachineBuilder<'saga, 'enum
                when 'saga :> SagaStateMachineInstance
                and 'saga : not struct 
                and 'enum :> Enum and 'enum: struct and 'enum: (new: unit -> 'enum)>() = 
    member _.Yield _ = State<'enum>.Declare
        
    [<CustomOperation "event">]
    member _.Event(state: StateMachineModifier<'saga> -> StateMachineModifier<'saga>,
                   event: IEventCorrelationConfigurator<'saga,'event> -> unit) =
        state >> Event<'event>.Configure event
    
    [<CustomOperation "instanceState">]
    member _.InstanceState(state: StateMachineModifier<'saga> -> StateMachineModifier<'saga>,
                           property:Linq.Expressions.Expression<Func<'saga,string>>) =
        state >> State<'enum>.Property property

    [<CustomOperation "instanceState">]
    member _.InstanceState(state: StateMachineModifier<'saga> -> StateMachineModifier<'saga>,
                           property:Linq.Expressions.Expression<Func<'saga,int>>) =
        state >> State<'enum>.Property property

    [<CustomOperation "instanceState">]
    member _.InstanceState(state: StateMachineModifier<'saga> -> StateMachineModifier<'saga>,
                           property:Linq.Expressions.Expression<Func<'saga,MTState>>) =
        state >> State<'enum>.Property property
    
    [<CustomOperation "initially">]
    member _.Initially(state: StateMachineModifier<'saga> -> StateMachineModifier<'saga>, 
                       builders: list<StateMachineModifier<'saga,IStateMachineEventActivitiesBuilder<'saga>> -> StateMachineModifier<'saga,_>>) =
        state >> State<'enum>.Initially >> Activity.BuildAll builders

    [<CustomOperation "duringAny">]
    member _.DuringAny(state: StateMachineModifier<'saga> -> StateMachineModifier<'saga>,
                       builders: list<StateMachineModifier<'saga,IStateMachineEventActivitiesBuilder<'saga>> -> StateMachineModifier<'saga,_>>) =
        state >> State<'enum>.DuringAny >> Activity.BuildAll builders
    
    [<CustomOperation "during">]
    member _.During(state: StateMachineModifier<'saga> -> StateMachineModifier<'saga>,
                    mkState: StateMachineModifier<'saga> -> MTState,
                    builders: list<StateMachineModifier<'saga,IStateMachineEventActivitiesBuilder<'saga>> -> StateMachineModifier<'saga,_>>) =
        state >> State<'enum>.During mkState >> Activity.BuildAll builders

    [<CustomOperation "during">]
    member _.During(state: StateMachineModifier<'saga> -> StateMachineModifier<'saga>,
                    mkStates: #seq<StateMachineModifier<'saga> -> MTState>,
                    builders: list<StateMachineModifier<'saga,IStateMachineEventActivitiesBuilder<'saga>> -> StateMachineModifier<'saga,_>>) =
        state >> State<'enum>.During mkStates >> Activity.BuildAll builders
    
    [<CustomOperation "finally">]
    member _.Finally(state: StateMachineModifier<'saga> -> StateMachineModifier<'saga>,
                     activities: list<EventActivityBinder<'saga>*StateMachineModifier<'saga> -> EventActivityBinder<'saga>>) =
        state >> Activity.Finally activities

    [<CustomOperation "whenEnter">]
    member _.WhenEnter(state: StateMachineModifier<'saga> -> StateMachineModifier<'saga>,
                       mkState: StateMachineModifier<'saga> -> MTState,
                       activities: list<EventActivityBinder<'saga>*StateMachineModifier<'saga> -> EventActivityBinder<'saga>>) =
        state >> State<'enum>.Tuple mkState >> Activity.WhenEnter activities

    [<CustomOperation "whenEnterAny">]
    member _.WhenEnterAny(state: StateMachineModifier<'saga> -> StateMachineModifier<'saga>,
                          activities: list<EventActivityBinder<'saga>*StateMachineModifier<'saga> -> EventActivityBinder<'saga>>) =
        state >> Activity.WhenEnterAny activities

    [<CustomOperation "whenLeave">]
    member _.WhenLeave(state: StateMachineModifier<'saga> -> StateMachineModifier<'saga>,
                       mkState: StateMachineModifier<'saga> -> MTState,
                       activities: list<EventActivityBinder<'saga>*StateMachineModifier<'saga> -> EventActivityBinder<'saga>>) =
        state >> State<'enum>.Tuple mkState >> Activity.WhenLeave activities

    [<CustomOperation "whenLeaveAny">]
    member _.WhenLeaveAny(state: StateMachineModifier<'saga> -> StateMachineModifier<'saga>,
                          activities: list<EventActivityBinder<'saga>*StateMachineModifier<'saga> -> EventActivityBinder<'saga>>) =
        state >> Activity.WhenLeaveAny activities

    [<CustomOperation "afterLeave">]
    member _.WhenLeave(state: StateMachineModifier<'saga> -> StateMachineModifier<'saga>,
                       mkState: StateMachineModifier<'saga> -> MTState,
                       activities: list<EventActivityBinder<'saga,MTState>*StateMachineModifier<'saga> -> EventActivityBinder<'saga,MTState>>) =
        state >> State<'enum>.Tuple mkState >> Activity.AfterLeave activities

    [<CustomOperation "afterLeaveAny">]
    member _.AfterLeaveAny(state: StateMachineModifier<'saga> -> StateMachineModifier<'saga>,
                           activities: list<EventActivityBinder<'saga,MTState>*StateMachineModifier<'saga> -> EventActivityBinder<'saga,MTState>>) =
        state >> Activity.AfterLeaveAny activities

    [<CustomOperation "beforeEnter">]
    member _.BeforeEnter(state: StateMachineModifier<'saga> -> StateMachineModifier<'saga>,
                         mkState: StateMachineModifier<'saga> -> MTState,
                         activities: list<EventActivityBinder<'saga,MTState>*StateMachineModifier<'saga> -> EventActivityBinder<'saga,MTState>>) =
        state >> State<'enum>.Tuple mkState >> Activity.BeforeEnter activities

    [<CustomOperation "beforeEnterAny">]
    member _.BeforeEnterAny(state: StateMachineModifier<'saga> -> StateMachineModifier<'saga>,
                            activities: list<EventActivityBinder<'saga,MTState>*StateMachineModifier<'saga> -> EventActivityBinder<'saga,MTState>>) =
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
                and 'event: not struct> activity (smm: StateMachineModifier<'saga,IStateMachineEventActivitiesBuilder<'saga>>) =
        smm |> StateMachineModifier.map (fun m -> m.When(smm.getEvent typeof<'event>.Name |> unbox<MTEvent<'event>>, tuple smm >> activity))
        
    let ignoreEvent<'saga,'event
                when 'saga: not struct
                and 'saga :> ISaga
                and 'saga :> SagaStateMachineInstance
                and 'event: not struct> (smm: StateMachineModifier<'saga,IStateMachineEventActivitiesBuilder<'saga>>) =
        smm |> StateMachineModifier.map (fun m -> m.Ignore(smm.getEvent typeof<'event>.Name |> unbox<MTEvent<'event>>))
    
    let onFiltered<'saga,'event
                when 'saga: not struct
                and 'saga :> ISaga
                and 'saga :> SagaStateMachineInstance
                and 'event: not struct> filter activity (smm: StateMachineModifier<'saga,IStateMachineEventActivitiesBuilder<'saga>>) =
        smm |> StateMachineModifier.map (fun m -> m.When(smm.getEvent typeof<'event>.Name |> unbox<MTEvent<'event>>, StateMachineCondition<'saga,'event> filter, tuple smm >> activity))
        
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

    let exnActivity<'saga,'event, 'exn
                when 'saga: not struct
                and 'saga :> ISaga
                and 'saga :> SagaStateMachineInstance
                and 'event: not struct
                and 'exn :> exn> =
        ExceptionActivityBuilder<'saga, 'event, 'exn>()

    let correlated<'event when 'event : not struct> = EventCorrelationBuilder<'event>()