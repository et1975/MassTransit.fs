module MassTransit.fs.IntegrationTests

open System
open MassTransit.Testing
open MassTransit
open Microsoft.Extensions.DependencyInjection
open Microsoft.Extensions.Hosting
open MassTransit.FSharp.Tests
open Expecto
open MassTransit.FSharp

[<Tests>]
let test =
    testTask "can consume messages" {
        let provider =
            ServiceCollection()
                .AddMassTransitTestHarness(fun cfg ->
                    cfg.AddSagaStateMachine<TestMachine, TestSaga>() |> ignore
                    )
                .BuildServiceProvider(true)
        let harness = provider.GetRequiredService<ITestHarness>()
        let sagaHarness = harness.GetSagaStateMachineHarness<TestMachine, TestSaga>()
        do! harness.Start()
        let sagaId = Guid.NewGuid()
        let message = E1(OrderId = sagaId)
        do! harness.Bus.Publish(message)
        let! consumed = harness.Consumed.Any<E1>()
        Expect.isTrue consumed "harness should have consumed message"
        let sagaHarness = harness.GetSagaStateMachineHarness<TestMachine, TestSaga>()
        let expectedState =
            sagaHarness.StateMachine.States
            |> Seq.find(fun x -> x.Name = string S.A)
            
        let sagaCreated = sagaHarness.Created.ContainsInState(sagaId, sagaHarness.StateMachine, expectedState)
        return ()
    }