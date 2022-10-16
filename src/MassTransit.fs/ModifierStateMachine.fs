namespace MassTransit.FSharp

open MassTransit

module internal StateMachineModifier =
    let private smm = 
        let mtsm = typeof<MassTransitStateMachine<_>>
        mtsm.Assembly.GetTypes() |> Seq.find (fun t -> t.FullName.StartsWith "MassTransit.Configuration.StateMachineModifier`")
    let mkNew<'saga when 'saga : not struct 
                     and 'saga :> SagaStateMachineInstance> : MassTransitStateMachine<'saga> -> IStateMachineModifier<'saga> =
        fun m -> System.Activator.CreateInstance(smm.MakeGenericType typeof<'saga>, m ) |> unbox

[<AbstractClass>]
type ModifierStateMachine<'saga when 'saga : not struct and 'saga :> SagaStateMachineInstance>(
    modifier: IStateMachineModifier<'saga> -> IStateMachineModifier<'saga>) as this =
    inherit MassTransitStateMachine<'saga>()
    do
      let m = StateMachineModifier.mkNew this
      modifier m |> ignore
      m.Apply()