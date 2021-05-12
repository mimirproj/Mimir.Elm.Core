module Tests.Array

open Expecto
open Prelude
open Elm.Core


[<Tests>]
let tests =
    let isEmptyTests =
        describe "isEmpty"
            [ test "all empty arrays are equal" <|
                fun () ->
                    Expect.equal Array.empty (Array.fromList [])
              test "empty array" <|
                fun () ->
                    Array.isEmpty Array.empty
                        |> Expect.equal True
              test "empty converted array" <|
                fun () ->
                    Array.isEmpty (Array.fromList [])
                        |> Expect.equal True
              test "non-empty array" <|
                fun () ->
                    Array.isEmpty (Array.fromList [ 1 ])
                        |> Expect.equal False
            ]


    describe "Array" [
        isEmptyTests
    ]