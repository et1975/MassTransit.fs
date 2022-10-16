module MassTransit.fs.IntegrationTests

open System
open MassTransit.Testing
open MassTransit
open Microsoft.Extensions.DependencyInjection
open Swensen.Unquote
open MassTransit.FSharp.Tests
open Expecto
open MassTransit.FSharp

type InitiallyMachine() =
    inherit ModifierStateMachine<TestSaga>(
        stateMachine<TestSaga, S> {
            event (correlated<E1> {
                byId (fun x -> x.Message.OrderId)
            })
            event (correlated<E2> {
                byId (fun x -> x.Message.Id)
            })
            instanceState (fun s -> s.CurrentState)
            initially [
                on<TestSaga,E1> (eventActivity {
                    bind Activity.OfType<TestEventActivity>
                    transition (State.Of S.A)
                })
            ]
            duringAny [
                on<TestSaga,E2> (eventActivity { transition State.Final })
            ]
        }
    )

[<Tests>]
let tests =

    let (@) (sagaHarness:ISagaStateMachineTestHarness<_,_>) s =
        sagaHarness.StateMachine.States |> Seq.find(fun x -> x.Name = string s)

    let init add =
        let provider = ServiceCollection().AddMassTransitTestHarness(add >> ignore).BuildServiceProvider(true)
        let harness = provider.GetRequiredService<ITestHarness>()
        harness.Start().ContinueWith(fun _ -> harness)
    let harnessTest name add test = 
        testTask name {
            let! harness = init add
            return! test (harness :> ITestHarness)
        }

    testList "integration" [
        harnessTest "Consumes messages" (fun cfg -> cfg.AddSagaStateMachine<InitiallyMachine,TestSaga>()) <| fun harness -> backgroundTask {
            let sagaHarness = harness.GetSagaStateMachineHarness<InitiallyMachine, TestSaga>()
            let message = E1(OrderId = Guid.NewGuid())
            do! harness.Bus.Publish(message)
            let! sagaConsumed = sagaHarness.Consumed.Any<E1>()
            Expect.isTrue sagaConsumed "saga harness should have consumed E1"
            let! created = sagaHarness.Created.Any(fun (x:TestSaga) -> x.CorrelationId = message.OrderId)
            Expect.isTrue created "harness should have created saga"
            sagaHarness.Created.ContainsInState(message.OrderId, sagaHarness.StateMachine, sagaHarness @ S.A) |> box <>! null
            let message = E2(Id = message.OrderId)
            do! harness.Bus.Publish(message)
            let! sagaConsumed = sagaHarness.Consumed.Any<E2>()
            Expect.isTrue sagaConsumed "saga harness should have consumed E2"
            sagaHarness.Created.ContainsInState(message.Id, sagaHarness.StateMachine, sagaHarness.StateMachine.Final) |> box <>! null
        }
    ]