module Tests.Prelude

open Elm.Core
open Expecto

let describe = testList

let test = testCase

// type FloatingPointTolerance =
//     | Absolute of float
//     | Relative of float

module Expect =
    let inline equal actual expected =
        "Should be equal" |> Expect.equal actual expected

    let inline notEqual actual expected =
        "Should not be equal" |> Expect.notEqual actual expected

    let inline lessThan a b =
        "Should be less than" |> Expect.isLessThan b a

    // let within (accuracy) actual expected =
    //     let x =
    //         match accuracy with
    //         | Absolute -> Accuracy.high

    //     "Should be close" |> Expect.floatClose x actual expected

let goodInt str int =
    test str <| fun _ ->
        Expect.equal (Just int) (String.toInt str)


let badInt str =
    test str <| fun _ ->
        Expect.equal
            Nothing
            (String.toInt str)


let goodFloat str float =
    test str <| fun _ ->
        Expect.equal (Just float) (String.toFloat str)


let badFloat str =
    test str <| fun _ ->
        Expect.equal
            Nothing
            (String.toFloat str)