# Better MassTransit support for F#

## DSL for sagas (tenantive)
```fsharp
type States = AwaitingUpgradeCommandStarted | AwaitingUpgradeCommandResult

stateMachine<States> {
    events [
        event<UpgradeExpired> {
            correlatedBy (fun m -> m.Message.OrderId)
        }
        event<UpgradeCommandStarted> {
            correlatedBy (fun m -> m.Message.OrderId)
            onMissing instanceDiscard
        }
    ]
    configure [
        initially {
            on<Upgrade> {
                activity<CopyInitialData> {
                    ifElse instanceResultIsFailure (
                        transition { toFinal },
                        activity<PublishUpgradeExpired {
                            activity<PublishUpgradeOrderCreated> {
                                transition { to AwaitingUpgradeCommandStarted }
                            }
                        }
                    )
                }
            }
        }
        during AwaitingUpgradeCommandStarted {
            on<UpgradeCommandFinished> {
                handle handleUpgradeCommandFinishedEvent
                transition { toFinal }
            }
        }
    ]
}
```

## Building
Pre-requisites:
- .NET SDK 6.0

`dotnet fsi build.fsx`