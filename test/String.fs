module Tests.String

open Expecto
open Prelude
open Elm.Core

[<Tests>]
let tests =
    let simpleTests =
        describe "Simple Stuff"
            [ test "is empty" <| fun _ -> Expect.equal true (String.isEmpty "")
              test "is not empty" <| fun _ -> Expect.equal true (not (String.isEmpty ("the world")))
              test "length" <| fun _ -> Expect.equal 11 (String.length "innumerable")
              test "endsWith" <| fun _ -> Expect.equal true <| String.endsWith "ship" "spaceship"
              test "reverse" <| fun _ -> Expect.equal "desserts" (String.reverse "stressed")
              test "repeat" <| fun _ -> Expect.equal "hahaha" (String.repeat 3 "ha")
              test "indexes" <| fun _ -> Expect.equal [ 0; 2 ] (String.indexes "a" "aha")
              test "empty indexes" <| fun _ -> Expect.equal [] (String.indexes "" "aha")
            ]

    let combiningTests =
        describe "Combining Strings"
            [ test "uncons non-empty" <| fun _ -> Expect.equal (Just ( 'a', "bc" )) (String.uncons "abc")
              test "uncons empty" <| fun _ -> Expect.equal Nothing (String.uncons "")
              test "append 1" <| fun _ -> Expect.equal "butterfly" (String.append "butter" "fly")
              test "append 2" <| fun _ -> Expect.equal "butter" (String.append "butter" "")
              test "append 3" <| fun _ -> Expect.equal "butter" (String.append "" "butter")
              test "concat" <| fun _ -> Expect.equal "nevertheless" (String.concat [ "never"; "the"; "less" ])
              test "split commas" <| fun _ -> Expect.equal [ "cat"; "dog"; "cow" ] (String.split "," "cat,dog,cow")
              test "split slashes" <| fun _ -> Expect.equal [ "home"; "steve"; "Desktop"; "" ] (String.split "/" "home/steve/Desktop/")
              test "join spaces" <| fun _ -> Expect.equal "cat dog cow" (String.join " " [ "cat"; "dog"; "cow" ])
              test "join slashes" <| fun _ -> Expect.equal "home/steve/Desktop" (String.join "/" [ "home"; "steve"; "Desktop" ])
              test "slice 1" <| fun _ -> Expect.equal "c" (String.slice 2 3 "abcd")
              test "slice 2" <| fun _ -> Expect.equal "abc" (String.slice 0 3 "abcd")
              test "slice 3" <| fun _ -> Expect.equal "abc" (String.slice 0 -1 "abcd")
              test "slice 4" <| fun _ -> Expect.equal "cd" (String.slice -2 4 "abcd")
            ]

    let intTests =
        describe "String.toInt"
            [ goodInt "1234" 1234
              goodInt "+1234" 1234
              goodInt "-1234" -1234
              badInt "1.34"
              badInt "1e31"
              badInt "123a"
              goodInt "0123" 123
              badInt "0x001A"
              badInt "0x001a"
              badInt "0xBEEF"
              badInt "0x12.0"
              badInt "0x12an"
            ]

    let floatTests =
        describe "String.toFloat"
            [ goodFloat "123" 123.0
              goodFloat "3.14" 3.14
              goodFloat "+3.14" 3.14
              goodFloat "-3.14" -3.14
              goodFloat "0.12" 0.12
              goodFloat ".12" 0.12
              goodFloat "1e-42" 1e-42
              goodFloat "6.022e23" 6.022e23
              goodFloat "6.022E23" 6.022e23
              goodFloat "6.022e+23" 6.022e23
              badFloat "6.022e"
              badFloat "6.022n"
              badFloat "6.022.31"
            ]

    let extraTests =
        describe "Extra"
            [
              test "reverse 1" <| fun _ -> Expect.equal "cba" (String.reverse "abc")
              test "reverse 2" <| fun _ -> Expect.equal "nàm" (String.reverse "màn")
              test "filter" <| fun _ -> Expect.equal "mànabc" (String.filter (fun c -> c <> 'z') "mànzabc")
              test "toList" <| fun _ -> Expect.equal [ 'a'; 'b'; 'c' ] (String.toList "abc")
              test "uncons" <| fun _ -> Expect.equal (Just ( 'a', "bc" )) (String.uncons "abc")
              test "map 1" <| fun _ -> Expect.equal "aaa" (String.map (fun _ -> 'a') "zzz")
              test "map 2" <| fun _ -> Expect.equal "zzz" (String.map (fun _ -> 'z') "aaa")
              test "foldl" <| fun _ -> Expect.equal 3 (String.foldl (fun _ c -> c + 1) 0 "aaa")
              test "foldr" <| fun _ -> Expect.equal 3 (String.foldr (fun _ c -> c + 1) 0 "aaa")
              test "all" <| fun _ -> Expect.equal true (String.all ((=) 'z') "zzz")
              test "any" <| fun _ -> Expect.equal true (String.any ((=) 'z') "abcz123")
            ]

    let encodingTests =
        describe "UTF-16 Encoding"
            [ // An elm char is actually a non-empty string and can therefore represent these emoji characters as a 'char'.
              // In dot net it takes two chars.

              // test "reverse 1" <| fun _ -> Expect.equal "𝌆c𝌆b𝌆a𝌆" (String.reverse "𝌆a𝌆b𝌆c𝌆")
              // test "reverse 2" <| fun _ -> Expect.equal "nàm" (String.reverse "màn")
              // test "reverse 3" <| fun _ -> Expect.equal "😣ba" (String.reverse "ab😣")
              // test "filter" <| fun _ -> Expect.equal "mànabc" (String.filter (fun c -> c <> chr "😣") "màn😣abc")
              // test "toList" <| fun _ -> Expect.equal [chr "𝌆"; 'a'; chr "𝌆"; 'b'; chr "𝌆"] (String.toList "𝌆a𝌆b𝌆")
              // test "uncons" <| fun _ -> Expect.equal (Just ( chr "😃", "bc" )) (String.uncons "😃bc")
              // test "map 1" <| fun _ -> Expect.equal "aaa" (String.map (fun _ -> 'a') "😃😃😃")
              // test "map 2" <| fun _ -> Expect.equal "😃😃😃" (String.map (fun _ -> '😃') "aaa")
              // test "foldl" <| fun _ -> Expect.equal 3 (String.foldl (fun _ c -> c + 1) 0 "😃😃😃")
              // test "foldr" <| fun _ -> Expect.equal 3 (String.foldr (fun _ c -> c + 1) 0 "😃😃😃")
              // test "all" <| fun _ -> Expect.equal true (String.all ((==) '😃') "😃😃😃")
              // test "any" <| fun _ -> Expect.equal true (String.any ((==) '😃') "abc😃123")
            ]

    describe "String" [
        simpleTests
        combiningTests
        intTests
        floatTests
        encodingTests
    ]