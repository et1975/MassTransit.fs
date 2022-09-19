module MassTransit.FSharp.Program

open Expecto

[<EntryPoint>]
let main argv = 
    Tests.runTestsInAssembly defaultConfig argv
