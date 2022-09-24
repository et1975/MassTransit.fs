module MassTransit.FSharp.Tests

open System
open MassTransit
open Expecto

type S = A = 0| B = 1
type E1() = 
    member val OrderId = Guid.Empty with get,set
type E2() = 
    member val Id = Guid.Empty with get,set

type TestSaga() =
    interface SagaStateMachineInstance
        member val CorrelationId = Guid.Empty with get,set
    interface ISaga with
        member val CorrelationId = Guid.Empty with get,set
    member val CurrentState = "" with get,set

type TestEventActivity() =
    interface IStateMachineActivity<TestSaga,E1> with
        member this.Accept(visitor) = visitor.Visit(this) 
        member this.Execute(ctx: BehaviorContext<TestSaga, E1>, next: IBehavior<TestSaga, E1>) = next.Execute ctx
        member this.Faulted<'exn when 'exn :> exn>(context: BehaviorExceptionContext<TestSaga, E1, 'exn>, next: IBehavior<TestSaga, E1>) = next.Faulted(context)
        member this.Probe(context:ProbeContext) = context.CreateScope(this.GetType().Name) |> ignore<ProbeContext>

type TestStateActivity() =
    interface IStateMachineActivity<TestSaga,State> with
        member this.Accept(visitor) = visitor.Visit(this) 
        member this.Execute(ctx: BehaviorContext<TestSaga, State>, next: IBehavior<TestSaga, State>) = next.Execute ctx
        member this.Faulted<'exn when 'exn :> exn>(context: BehaviorExceptionContext<TestSaga, State, 'exn>, next: IBehavior<TestSaga, State>) = next.Faulted(context)
        member this.Probe(context:ProbeContext) = context.CreateScope(this.GetType().Name) |> ignore<ProbeContext>

type TestSagaActivity() =
    interface IStateMachineActivity<TestSaga> with
        member this.Accept(visitor) = visitor.Visit(this) 
        member this.Execute(ctx: BehaviorContext<TestSaga>, next: IBehavior<TestSaga>) = next.Execute ctx
        member this.Execute(context: BehaviorContext<TestSaga,'T>, next: IBehavior<TestSaga,'T>) = (this :> IStateMachineActivity<TestSaga>).Execute(context, next)
        member this.Faulted(context: BehaviorExceptionContext<TestSaga,'T,'TException>, next: IBehavior<TestSaga,'T>) = next.Faulted(context)
        member this.Faulted<'exn when 'exn :> exn>(context: BehaviorExceptionContext<TestSaga, 'exn>, next: IBehavior<TestSaga>) = next.Faulted(context)
        member this.Probe(context:ProbeContext) = context.CreateScope(this.GetType().Name) |> ignore<ProbeContext>
        
type TestMachine() =
    inherit ModifierStateMachine<TestSaga>(
        stateMachine<TestSaga, S> {
            instanceState (fun s -> s.CurrentState)
            event (correlated<E1> {
                byId (fun x -> x.Message.OrderId)
                onMissing MissingInstance.Discard
            })
            event (correlated<E2> {
                by (fun saga ctx -> ctx.Message.Id = (saga :> ISaga).CorrelationId)
                onMissing (MissingInstance.Execute ignore)
            })
            initially [
                on<TestSaga,E1> (eventActivity {
                    transition (State.Of S.A)
                    bind Activity.OfType<TestEventActivity>
                    bind Activity.OfInstanceType<TestSagaActivity>
                    conditionally (fun _ -> true) (eventActivity { transition State.Final })
                })
            ]
            during (State.Of S.A) [
                onFiltered<TestSaga,E1> (fun _ ->true) (eventActivity { transition State.Final })
            ]
            beforeEnter (State.Of S.B) [
                stateActivity {
                    bind Activity.OfInstanceType<TestSagaActivity>
                    ifElse (fun _ -> true)
                           (stateActivity { bind Activity.OfType<TestStateActivity> })
                           (stateActivity { handle ignore })
                }
            ]
            whenEnter (State.Of S.B) [
                activity {
                    bind Activity.OfInstanceType<TestSagaActivity>
                }
                activity {
                    transition State.Final
                    conditionally (fun _ -> true) (activity { transition State.Final })
                }
            ]
            afterLeaveAny [
                stateActivity { bind Activity.OfType<TestStateActivity> }
            ]
        }
    )

[<Tests>]
let tests =
    testList "unit" [
        test "Can construct StateMachine" {
            TestMachine() |> ignore
        }
    ]
