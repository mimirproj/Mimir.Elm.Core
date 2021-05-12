module Tests.Dict

open Expecto
open Prelude
open Elm.Core

let animals =
    Dict.fromList [ ( "Tom", "cat" ); ( "Jerry", "mouse" ) ]

[<Tests>]
let tests =
    let buildTests =
        describe "build Tests"
            [ test "empty" <| fun () -> Expect.equal (Dict.fromList []) (Dict.empty)
              test "singleton" <| fun () -> Expect.equal (Dict.fromList [ ( "k", "v" ) ]) (Dict.singleton "k" "v")
              test "insert" <| fun () -> Expect.equal (Dict.fromList [ ( "k", "v" ) ]) (Dict.insert "k" "v" Dict.empty)
              test "insert replace" <| fun () -> Expect.equal (Dict.fromList [ ( "k", "vv" ) ]) (Dict.insert "k" "vv" (Dict.singleton "k" "v"))
              test "update" <| fun () -> Expect.equal (Dict.fromList [ ( "k", "vv" ) ]) (Dict.update "k" (fun v -> Just "vv") (Dict.singleton "k" "v"))
              test "update Nothing" <| fun () -> Expect.equal Dict.empty (Dict.update "k" (fun v -> Nothing) (Dict.singleton "k" "v"))
              test "remove" <| fun () -> Expect.equal Dict.empty (Dict.remove "k" (Dict.singleton "k" "v"))
              test "remove not found" <| fun () -> Expect.equal (Dict.singleton "k" "v") (Dict.remove "kk" (Dict.singleton "k" "v"))
            ]

    let queryTests =
        describe "query Tests"
            [ test "member 1" <| fun () -> Expect.equal True (Dict.member' "Tom" animals)
              test "member 2" <| fun () -> Expect.equal False (Dict.member' "Spike" animals)
              test "get 1" <| fun () -> Expect.equal (Just "cat") (Dict.get "Tom" animals)
              test "get 2" <| fun () -> Expect.equal Nothing (Dict.get "Spike" animals)
              test "size of empty dictionary" <| fun () -> Expect.equal 0 (Dict.size Dict.empty)
              test "size of example dictionary" <| fun () -> Expect.equal 2 (Dict.size animals)
            ]

    let combineTests =
        describe "combine Tests"
            [ test "union" <| fun () -> Expect.equal animals (Dict.union (Dict.singleton "Jerry" "mouse") (Dict.singleton "Tom" "cat"))
              test "union collison" <| fun () -> Expect.equal (Dict.singleton "Tom" "cat") (Dict.union (Dict.singleton "Tom" "cat") (Dict.singleton "Tom" "mouse"))
              test "intersect" <| fun () -> Expect.equal (Dict.singleton "Tom" "cat") (Dict.intersect animals (Dict.singleton "Tom" "cat"))
              test "diff" <| fun () -> Expect.equal (Dict.singleton "Jerry" "mouse") (Dict.diff animals (Dict.singleton "Tom" "cat"))
            ]

    let transformTests =
        describe "transform Tests"
            [ test "filter" <| fun () -> Expect.equal (Dict.singleton "Tom" "cat") (Dict.filter (fun k v -> k == "Tom") animals)
              test "partition" <| fun () -> Expect.equal ( Dict.singleton "Tom" "cat", Dict.singleton "Jerry" "mouse" ) (Dict.partition (fun k v -> k == "Tom") animals)
            ]

    let mergeTests =
        let insertBoth key leftVal rightVal dict =
            Dict.insert key (leftVal ++ rightVal) dict

        let s1 =
            Dict.empty |> Dict.insert "u1" [ 1 ]

        let s2 =
            Dict.empty |> Dict.insert "u2" [ 2 ]

        let s23 =
            Dict.empty |> Dict.insert "u2" [ 3 ]

        let b1 =
            List.map (fun i -> ( i, [ i ] )) (List.range 1 10) |> Dict.fromList

        let b2 =
            List.map (fun i -> ( i, [ i ] )) (List.range 5 15) |> Dict.fromList

        let bExpected =
            [ ( 1, [ 1 ] ); ( 2, [ 2 ] ); ( 3, [ 3 ] ); ( 4, [ 4 ] ); ( 5, [ 5; 5 ] ); ( 6, [ 6; 6 ] ); ( 7, [ 7; 7 ] ); ( 8, [ 8; 8 ] ); ( 9, [ 9; 9 ] ); ( 10, [ 10; 10 ] ); ( 11, [ 11 ] ); ( 12, [ 12 ] ); ( 13, [ 13 ] ); ( 14, [ 14 ] ); ( 15, [ 15 ] ) ]

        describe "merge Tests"
            [ test "merge empties" <|
                fun () ->
                    Expect.equal (Dict.empty)
                        (Dict.merge Dict.insert insertBoth Dict.insert Dict.empty Dict.empty Dict.empty)
              test "merge singletons in order" <|
                fun () ->
                    Expect.equal [ ( "u1", [ 1 ] ); ( "u2", [ 2 ] ) ]
                        ((Dict.merge Dict.insert insertBoth Dict.insert s1 s2 Dict.empty) |> Dict.toList)
              test "merge singletons out of order" <|
                fun () ->
                    Expect.equal [ ( "u1", [ 1 ] ); ( "u2", [ 2 ] ) ]
                        ((Dict.merge Dict.insert insertBoth Dict.insert s2 s1 Dict.empty) |> Dict.toList)
              test "merge with duplicate key" <|
                fun () ->
                    Expect.equal [ ( "u2", [ 2; 3 ] ) ]
                        ((Dict.merge Dict.insert insertBoth Dict.insert s2 s23 Dict.empty) |> Dict.toList)
              test "partially overlapping" <|
                fun () ->
                    Expect.equal bExpected
                        ((Dict.merge Dict.insert insertBoth Dict.insert b1 b2 Dict.empty) |> Dict.toList)
            ]

    describe "Dict" [
        buildTests
        queryTests
        combineTests
        transformTests
        mergeTests
    ]