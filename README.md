# Better MassTransit support for F#

## DSL for sagas (tenantive)
```fsharp

type States = AwaitingUpgradeCommandStarted | AwaitingUpgradeCommandResult
type UpgradeStateMachine() =
    inherit ModifierStateMachine<UpgradeStateMachineState>(
        stateMachine<UpgradeStateMachineState,States> {
            events [
                event<UpgradeExpired> {
                    correlatedBy (fun m -> m.Message.OrderId)
                }
                event<UpgradeCommandStarted> {
                    correlatedBy (fun m -> m.Message.OrderId)
                    onMissing Event.discard
                }
            ]
            initially [
                on<Upgrade> (eventActivity {
                    bind Activity.OfType<CopyInitialData> 
                    ifElse instanceResultIsFailure
                           (eventActivity { transition State.Final })
                           (eventActivity { bind Activity.OfInstanceType<PublishUpgradeExpired>
                                            bind Activity.OfType<PublishUpgradeOrderCreated>
                                            transition (State.Of AwaitingUpgradeCommandStarted) })
                })
            ]
            during (State.Of AwaitingUpgradeCommandStarted) [
                on<UpgradeCommandFinished> (eventActivity {
                    handle handleUpgradeCommandFinishedEvent
                    transition State.Final
                })
            ]
            whenEnter State.Final [
                activity {
                    handle handleUpgradeCommandFinishedEvent
                    transition State.Final
                }
            ]
        })
```

## Building
Pre-requisites:
- .NET SDK 6.0

`dotnet fsi build.fsx`