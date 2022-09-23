[<AutoOpen>]
module internal MassTransit.FSharp.Prelude

let tee f a = f a; a
let tuple b a  = a,b

module Assembly =
    open System.Runtime.CompilerServices
    
    [<InternalsVisibleTo("MassTransit.fs.Tests")>]
    ()