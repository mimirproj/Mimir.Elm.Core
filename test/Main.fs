namespace Tests

open Expecto
open System

module Main =
    [<EntryPoint>]
    let main argv =
        Tests.runTestsInAssembly defaultConfig argv