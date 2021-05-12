module Tests.Basics

open Expecto
open Prelude
open Elm.Core
open Elm.Core.Tuple

[<Tests>]
let tests =
    let comparison =
        describe "Comparison"
            [ test "max" <| fun () -> Expect.equal 42 (max 32 42)
              test "min" <| fun () -> Expect.equal 42 (min 91 42)
              test "clamp low" <| fun () -> Expect.equal 10 (clamp 10 20 5)
              test "clamp mid" <| fun () -> Expect.equal 15 (clamp 10 20 15)
              test "clamp high" <| fun () -> Expect.equal 20 (clamp 10 20 25)
              test "5 < 6" <| fun () -> Expect.equal True (5 < 6)
              test "6 < 5" <| fun () -> Expect.equal False (6 < 5)
              test "6 < 6" <| fun () -> Expect.equal False (6 < 6)
              test "5 > 6" <| fun () -> Expect.equal False (5 > 6)
              test "6 > 5" <| fun () -> Expect.equal True (6 > 5)
              test "6 > 6" <| fun () -> Expect.equal False (6 > 6)
              test "5 <= 6" <| fun () -> Expect.equal True (5 <= 6)
              test "6 <= 5" <| fun () -> Expect.equal False (6 <= 5)
              test "6 <= 6" <| fun () -> Expect.equal True (6 <= 6)
              test "compare \"A\" \"B\"" <| fun () -> Expect.equal LT (compare "A" "B")
              test "compare 'f' 'f'" <| fun () -> Expect.equal EQ (compare 'f' 'f')
              test "compare (1, 2, 3) (0, 1, 2)" <| fun () -> Expect.equal GT (compare ( 1, 2, 3 ) ( 0, 1, 2 ))
              test "compare ['a'] ['b']" <| fun () -> Expect.equal LT (compare [ 'a' ] [ 'b' ])
              test "array equality" <| fun () -> Expect.equal (Array.fromList [ 1; 1; 1; 1 ]) (Array.repeat 4 1)
              // test "set equality" <| fun () -> Expect.equal (Set.fromList [ 1; 2 ]) (Set.fromList [ 2; 1 ])
              // test "dict equality" <| fun () -> Expect.equal (Dict.fromList [ ( 1, 1 ); ( 2; 2 ) ]) (Dict.fromList [ ( 2, 2 ); ( 1, 1 ) ])
              test "char equality" <| fun () -> Expect.notEqual '0' '饑'
            ]

    // let toStringTests =
    //     describe "toString Tests"
    //         [ test "toString Int" <| fun () -> Expect.equal "42" (toString 42)
    //           test "toString Float" <| fun () -> Expect.equal "42.52" (toString 42.52)
    //           test "toString Char" <| fun () -> Expect.equal "'c'" (toString 'c')
    //           test "toString Char single quote" <| fun () -> Expect.equal "'\\''" (toString '\'')
    //           test "toString Char double quote" <| fun () -> Expect.equal "'\"'" (toString '"')
    //           test "toString String single quote" <| fun () -> Expect.equal "\"not 'escaped'\"" (toString "not 'escaped'")
    //           test "toString String double quote" <| fun () -> Expect.equal "\"are \\\"escaped\\\"\"" (toString "are \"escaped\"")
    //           test "toString record" <| fun () -> Expect.equal "{ field = [0] }" (toString {| field = [ 0 ] |})
    //           // TODO
    //           //, test "toString record, special case" <| fun () -> Expect.equal "{ ctor = [0] }" (toString { ctor = [ 0 ] })
    //         ]

    let trigTests =
        describe "Trigonometry Tests"
            [ test "radians 0" <| fun () -> Expect.equal 0.0 (radians 0.0)
              test "radians positive" <| fun () -> Expect.equal 5.0 (radians 5.0)
              test "radians negative" <| fun () -> Expect.equal -5.0 (radians -5.0)
              test "degrees 0" <| fun () -> Expect.equal 0.0 (degrees 0.0)
              test "degrees 90" <| fun () -> Expect.lessThan 0.01 (abs (1.57 - degrees 90.0))
              // This should test to enough precision to know if anything's breaking
              test "degrees -145" <| fun () -> Expect.lessThan 0.01 (abs (-2.53 - degrees -145.0))
              // This should test to enough precision to know if anything's breaking
              test "turns 0" <| fun () -> Expect.equal 0.0 (turns 0.0)
              test "turns 8" <| fun () -> Expect.lessThan 0.01 (abs (50.26 - turns 8.0))
              // This should test to enough precision to know if anything's breaking
              test "turns -133" <| fun () -> Expect.lessThan 0.01 (abs (-835.66 - turns -133.0))
              // This should test to enough precision to know if anything's breaking
              test "fromPolar (0, 0)" <| fun () -> Expect.equal ( 0.0, 0.0 ) (fromPolar ( 0.0, 0.0 ))
              test "fromPolar (1, 0)" <| fun () -> Expect.equal ( 1.0, 0.0 ) (fromPolar ( 1.0, 0.0 ))
              test "fromPolar (0, 1)" <| fun () -> Expect.equal ( 0.0, 0.0 ) (fromPolar ( 0.0, 1.0 ))
              test "fromPolar (1, 1)" <|
                fun () ->
                    Expect.equal True
                        (let
                            ( x, y ) =
                                fromPolar ( 1.0, 1.0 )
                         in
                            0.54 - x < 0.01 && 0.84 - y < 0.01
                        )
              test "toPolar (0, 0)" <| fun () -> Expect.equal ( 0.0, 0.0 ) (toPolar ( 0.0, 0.0 ))
              test "toPolar (1, 0)" <| fun () -> Expect.equal ( 1.0, 0.0 ) (toPolar ( 1.0, 0.0 ))
              test "toPolar (0, 1)" <|
                fun () ->
                    Expect.equal True
                        (let
                            ( r, theta ) =
                                toPolar ( 0.0, 1.0 )
                         in
                            r == 1.0 && abs (1.57 - theta) < 0.01
                        )
              test "toPolar (1, 1)" <|
                fun () ->
                    Expect.equal True
                        (let
                            ( r, theta ) =
                                toPolar ( 1.0, 1.0 )
                         in
                            abs (1.41 - r) < 0.01 && abs (0.78 - theta) < 0.01
                        )
              test "cos" <| fun () -> Expect.equal 1.0 (cos 0.0)
              test "sin" <| fun () -> Expect.equal 0.0 (sin 0.0)
              test "tan" <| fun () -> Expect.lessThan 0.01 (abs (12.67 - tan 17.2))
              test "acos" <| fun () -> Expect.lessThan 0.01 (abs (3.14 - acos -1.0))
              test "asin" <| fun () -> Expect.lessThan 0.01 (abs (0.3 - asin 0.3))
              test "atan" <| fun () -> Expect.lessThan 0.01 (abs (1.57 - atan 4567.8))
              test "atan2" <| fun () -> Expect.lessThan 0.01 (abs (1.55 - atan2 36.0 0.65))
              test "pi" <| fun () -> Expect.lessThan 0.01 (abs (3.14 - pi))
            ]

    let basicMathTests =
        describe "Basic Math Tests"
            [ test "add float" <| fun () -> Expect.equal 159.0 (155.6 + 3.4)
              test "add int" <| fun () -> Expect.equal 17 ((round 10.0) + (round 7.0))
            //   test "subtract float" <| fun () -> Expect.within (Absolute 0.00000001) -6.3 (1.0 - 7.3)
              test "subtract int" <| fun () -> Expect.equal 1130 ((round 9432.0) - (round 8302.0))
            //   test "multiply float" <| fun () -> Expect.within (Relative 0.00000001) 432.0 (96.0 * 4.5)
              test "multiply int" <| fun () -> Expect.equal 90 ((round 10.0) * (round 9.0))
            //   test "divide float" <| fun () -> Expect.within (Relative 0.00000001) 13.175 (527.0 / 40.0)
              test "divide int" <| fun () -> Expect.equal 23 (70 / 3)
              test "7 |> remainderBy 2" <| fun () -> Expect.equal 1 (7 |> remainderBy 2)
              test "-1 |> remainderBy 4" <| fun () -> Expect.equal -1 (-1 |> remainderBy 4)
              test "modBy 2 7" <| fun () -> Expect.equal 1 (modBy 2 7)
              test "modBy 4 -1" <| fun () -> Expect.equal 3 (modBy 4 -1)
              test "3^2" <| fun () -> Expect.equal 9.0 (3.0 ^ 2.0)
              test "sqrt" <| fun () -> Expect.equal 9.0 (sqrt 81.0)
              test "negate 42" <| fun () -> Expect.equal -42 (negate 42)
              test "negate -42" <| fun () -> Expect.equal 42 (negate -42)
              test "negate 0" <| fun () -> Expect.equal 0 (negate 0)
              test "abs -25" <| fun () -> Expect.equal 25 (abs -25)
              test "abs 76" <| fun () -> Expect.equal 76 (abs 76)
              test "logBase 10 100" <| fun () -> Expect.equal 2.0 (logBase 10.0 100.0)
              test "logBase 2 256" <| fun () -> Expect.equal 8.0 (logBase 2.0 256.0)
              test "e" <| fun () -> Expect.lessThan 0.01 (abs (2.72 - e))
            ]

    let booleanTests =
        describe "Boolean Tests"
            [ test "False && False" <| fun () -> Expect.equal False (False && False)
              test "False && True" <| fun () -> Expect.equal False (False && True)
              test "True && False" <| fun () -> Expect.equal False (True && False)
              test "True && True" <| fun () -> Expect.equal True (True && True)
              test "False || False" <| fun () -> Expect.equal False (False || False)
              test "False || True" <| fun () -> Expect.equal True (False || True)
              test "True || False" <| fun () -> Expect.equal True (True || False)
              test "True || True" <| fun () -> Expect.equal True (True || True)
              test "xor False False" <| fun () -> Expect.equal False (xor False False)
              test "xor False True" <| fun () -> Expect.equal True (xor False True)
              test "xor True False" <| fun () -> Expect.equal True (xor True False)
              test "xor True True" <| fun () -> Expect.equal False (xor True True)
              test "not True" <| fun () -> Expect.equal False (not True)
              test "not False" <| fun () -> Expect.equal True (not False)
            ]

    let conversionTests =
        describe "Conversion Tests"
            [ test "round 0.6" <| fun () -> Expect.equal 1 (round 0.6)
              test "round 0.4" <| fun () -> Expect.equal 0 (round 0.4)
              test "round 0.5" <| fun () -> Expect.equal 1 (round 0.5)
              test "truncate -2367.9267" <| fun () -> Expect.equal -2367 (truncate -2367.9267)
              test "floor -2367.9267" <| fun () -> Expect.equal -2368 (floor -2367.9267)
              test "ceiling 37.2" <| fun () -> Expect.equal 38 (ceiling 37.2)
              test "toFloat 25" <| fun () -> Expect.equal 25.0 (toFloat 25)
            ]

    let miscTests =
        describe "Miscellaneous Tests"
            [ test "isNaN (0/0)" <| fun () -> Expect.equal True (isNaN (0.0 / 0.0))
              test "isNaN (sqrt -1)" <| fun () -> Expect.equal True (isNaN (sqrt -1.0))
              test "isNaN (1/0)" <| fun () -> Expect.equal False (isNaN (1.0 / 0.0))
              test "isNaN 1" <| fun () -> Expect.equal False (isNaN 1.0)
              test "isInfinite (0/0)" <| fun () -> Expect.equal False (isInfinite (0.0 / 0.0))
              test "isInfinite (sqrt -1)" <| fun () -> Expect.equal False (isInfinite (sqrt -1.0))
              test "isInfinite (1/0)" <| fun () -> Expect.equal True (isInfinite (1.0 / 0.0))
              test "isInfinite 1" <| fun () -> Expect.equal False (isInfinite 1.0)
              test "\"hello\" ++ \"world\"" <| fun () -> Expect.equal "helloworld" ("hello" ++ "world")
              test "[1, 1, 2] ++ [3, 5, 8]" <| fun () -> Expect.equal [ 1; 1; 2; 3; 5; 8 ] ([ 1; 1; 2 ] ++ [ 3; 5; 8 ])
              test "first (1, 2)" <| fun () -> Expect.equal 1 (first ( 1, 2 ))
              test "second (1, 2)" <| fun () -> Expect.equal 2 (second ( 1, 2 ))
            ]

    let higherOrderTests =
        describe "Higher Order Helpers"
            [ test "identity 'c'" <| fun () -> Expect.equal 'c' (identity 'c')
              test "always 42 ()" <| fun () -> Expect.equal 42 (always 42 ())
              test "<|" <| fun () -> Expect.equal 9 (identity <| 3 + 6)
              test "|>" <| fun () -> Expect.equal 9 (3 + 6 |> identity)
              test "<<" <| fun () -> Expect.equal True (not << xor True <| True)
              describe ">>"
                [ test "with xor" <|
                    fun () ->
                        (True |> (xor True >> not))
                            |> Expect.equal True

                  test "with a record accessor" <|
                    fun () ->
                        [ {| foo = "NaS"; bar = "baz" |} ]
                            |> List.map (fun v -> v.foo |> String.reverse)
                            |> Expect.equal [ "SaN" ]
                ]
            ]

    describe "Basics" [
        comparison
        //toStringTests
        trigTests
        basicMathTests
        booleanTests
        conversionTests
        miscTests
        higherOrderTests
    ]