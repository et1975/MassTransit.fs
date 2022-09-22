module MassTransit.FSharp.Tests

open System
open MassTransit
open Swensen.Unquote
open Expecto

type S = A = 0| B = 1
type E() = 
    member val CorrelationId = Guid.Empty with get,set
type TestSaga() =
    interface SagaStateMachineInstance
    interface ISaga with
        member val CorrelationId = Guid.Empty with get,set

type TestMachine() =
    inherit ModifierStateMachine<TestSaga>(
        stateMachine<TestSaga, S> {
            initially [
                on {
                    transition (State.Of S.A)
                }
            ]
            during (State.Of S.A) [
                on {
                    transition State.Final
                }
            ]
            whenEnter (State.Of S.B) [
                activity {
                    transition State.Final
                }
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
