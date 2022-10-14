module MassTransit.fs.IntegrationTests

open System
open MassTransit.Testing
open MassTransit
open Microsoft.Extensions.DependencyInjection
open Swensen.Unquote
open MassTransit.FSharp.Tests
open Expecto
open MassTransit.FSharp

let (@) (sagaHarness:ISagaStateMachineTestHarness<_,_>) s =
    sagaHarness.StateMachine.States |> Seq.find(fun x -> x.Name = string s)

type InitiallyMachine() =
    inherit ModifierStateMachine<TestSaga>(
        stateMachine<TestSaga, S> {
            event (correlated<E1> {
                byId (fun x -> x.Message.OrderId)
            })
            instanceState (fun s -> s.CurrentState)
            initially [
                on<TestSaga,E1> (eventActivity {
                    transition (State.Of S.A)
                })
            ]
        }
    )

[<Tests>]
let tests =
    testList "integration" [
        testTask "can consume messages" {
            let provider =
                ServiceCollection()
                    .AddMassTransitTestHarness(fun cfg ->
                        cfg.AddSagaStateMachine<InitiallyMachine, TestSaga>() |> ignore
                        )
                    .BuildServiceProvider(true)
            let harness = provider.GetRequiredService<ITestHarness>()
            let sagaHarness = harness.GetSagaStateMachineHarness<InitiallyMachine, TestSaga>()
            do! harness.Start()
            let sagaId = Guid.NewGuid()
            let message = E1(OrderId = sagaId)
            do! harness.Bus.Publish(message)
            let! consumed = harness.Consumed.Any<E1>()
            Expect.isTrue consumed "harness should have consumed message"
            let sagaHarness = harness.GetSagaStateMachineHarness<InitiallyMachine, TestSaga>()
            let expectedState = sagaHarness @ S.A
            sagaHarness.Created.ContainsInState(sagaId, sagaHarness.StateMachine, expectedState) |> box <>! null
        }
    ]