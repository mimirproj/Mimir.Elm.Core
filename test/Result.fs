module Tests.Result

open Expecto
open Prelude
open Elm.Core


let isEven n =
    if modBy 2 n == 0 then
        Ok n
    else
        Error "number is odd"

let toIntResult s =
    match String.toInt s with
        | Just i ->
            Ok i
        | Nothing ->
            Error <| "could not convert string '" ++ s ++ "' to an Int"

let add3 a b c =
    a + b + c


let add4 a b c d =
    a + b + c + d


let add5 a b c d e =
    a + b + c + d + e

[<Tests>]
let tests =
    let mapTests =
        describe "map Tests"
            [ test "map Ok" <| fun () -> Expect.equal (Ok 3) (Result.map ((+) 1) (Ok 2))
              test "map Error" <| fun () -> Expect.equal (Error "error") (Result.map ((+) 1) (Error "error"))
            ]

    let mapNTests =
        describe "mapN Tests"
            [ test "map2 Ok" <| fun () -> Expect.equal (Ok 3) (Result.map2 (+) (Ok 1) (Ok 2))
              test "map2 Error" <| fun () -> Expect.equal (Error "x") (Result.map2 (+) (Ok 1) (Error "x"))
              test "map3 Ok" <| fun () -> Expect.equal (Ok 6) (Result.map3 add3 (Ok 1) (Ok 2) (Ok 3))
              test "map3 Error" <| fun () -> Expect.equal (Error "x") (Result.map3 add3 (Ok 1) (Ok 2) (Error "x"))
              test "map4 Ok" <| fun () -> Expect.equal (Ok 10) (Result.map4 add4 (Ok 1) (Ok 2) (Ok 3) (Ok 4))
              test "map4 Error" <| fun () -> Expect.equal (Error "x") (Result.map4 add4 (Ok 1) (Ok 2) (Ok 3) (Error "x"))
              test "map5 Ok" <| fun () -> Expect.equal (Ok 15) (Result.map5 add5 (Ok 1) (Ok 2) (Ok 3) (Ok 4) (Ok 5))
              test "map5 Error" <| fun () -> Expect.equal (Error "x") (Result.map5 add5 (Ok 1) (Ok 2) (Ok 3) (Ok 4) (Error "x"))
            ]

    let andThenTests =
        describe "andThen Tests"
            [ test "andThen Ok" <| fun () -> Expect.equal (Ok 42) ((toIntResult "42") |> Result.andThen isEven)
              test "andThen first Error" <|
                fun () ->
                    Expect.equal
                        (Error "could not convert string '4.2' to an Int")
                        (toIntResult "4.2" |> Result.andThen isEven)
              test "andThen second Error" <|
                fun () ->
                    Expect.equal
                        (Error "number is odd")
                        (toIntResult "41" |> Result.andThen isEven)
            ]


    describe "Result" [
        mapTests
        mapNTests
        andThenTests
    ]