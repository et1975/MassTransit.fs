# Better MassTransit support for F#

## DSL for sagas (tenantive)
```fsharp

type States = AwaitingUpgradeCommandStarted | AwaitingUpgradeCommandResult
type UpgradeStateMachine() =
    inherit ModifierStateMachine<UpgradeStateMachineState>(
        stateMachine<States> {
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
                on<Upgrade> {
                    ofType<CopyInitialData> 
                    ifElse instanceResultIsFailure
                           activity { transition State.Final }
                           activity { ofInstanceType<PublishUpgradeExpired>
                                      ofType<PublishUpgradeOrderCreated>
                                      transition (State.Of AwaitingUpgradeCommandStarted) }
                }
            ]
            during (State.Of AwaitingUpgradeCommandStarted) [
                on<UpgradeCommandFinished> {
                    handle handleUpgradeCommandFinishedEvent
                    transition State.Final
                }
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