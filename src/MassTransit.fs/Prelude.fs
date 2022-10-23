[<AutoOpen>]
module internal MassTransit.FSharp.Prelude

let tuple b a  = a,b

module Assembly =
    open System.Runtime.CompilerServices
    
    [<InternalsVisibleTo("MassTransit.fs.Tests")>]
    ()