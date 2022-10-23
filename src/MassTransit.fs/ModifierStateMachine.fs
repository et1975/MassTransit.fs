namespace MassTransit.FSharp

open MassTransit

[<AbstractClass>]
type ModifierStateMachine<'saga when 'saga : not struct and 'saga :> SagaStateMachineInstance>(
    modifier: StateMachineModifier<'saga> -> StateMachineModifier<'saga>) as this =
    inherit MassTransitStateMachine<'saga>()
    do
      let m = StateMachineModifier.mkNew this
      modifier m |> ignore
      m.modifier.Apply()