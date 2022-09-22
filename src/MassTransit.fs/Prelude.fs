[<AutoOpen>]
module internal MassTransit.FSharp.Prelude
let backgroundTask = FSharp.Control.Tasks.Builders.NonAffine.TaskBuilder() // until https://github.com/dotnet/fsharp/issues/12761 is sorted out

let tee f a = f a; a
let tuple b a  = a,b

module Assembly =
    open System.Runtime.CompilerServices
    
    [<InternalsVisibleTo("MassTransit.fs.Tests")>]
    ()