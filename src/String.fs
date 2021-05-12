module Elm.Core.String

open System
open Maybe

(* CHAR CONVERSIONS *)

/// Split a non-empty string into its head and tail. This lets you
/// pattern match on strings exactly as you would with lists.
/// <para>uncons "abc" == Just ('a',"bc")</para>
/// <para>uncons ""    == Nothing</para>
let uncons value =
    if String.IsNullOrEmpty value then Nothing
    else
        if value.Length = 1 then Just(value.[0], "")
        else
            Just(value.[0], value.Substring(1))

/// Add a character to the beginning of a string.
/// cons 'T' "he truth is out there" == "The truth is out there"
let inline cons (character:char) value =
    string(character) + value

/// Create a string from a given character.
/// <para>fromChar 'a' == "a"</para>
let inline fromChar char =
    cons char ""


(* LIST CONVERSIONS *)

///Convert a string to a list of characters.
/// <para>toList "abc" == ['a','b','c']</para>
/// <para>toList "🙈🙉🙊" == ['🙈','🙉','🙊']</para>
let toList (string: string) =
    string.ToCharArray()
    |> Array.toList

/// Convert a list of characters into a String. Can be useful if you
/// want to create a string primarily by consing, perhaps for decoding
/// something.
/// <para>fromList ['a','b','c'] == "abc"</para>
/// <para>fromList ['🙈','🙉','🙊'] == "🙈🙉🙊"</para>
let fromList (chars: char list) =
    let charArr = Array.ofList chars
    String(charArr, 0, charArr.Length)


(* HIGHER-ORDER FUNCTIONS *)

/// Transform every character in a string
/// <para>map (fun c -> if c == '/' then '.' else c) "a/b/c" == "a.b.c"</para>
let map = String.map

/// Keep only the characters that pass the test.
/// <para>filter isDigit "R2-D2" == "22"</para>
let filter = String.filter

/// Reduce a string from the left.
/// <para>foldl cons "" "time" == "emit"</para>
let foldl (folder: char -> 'b -> 'b) state (string: string) =
    let l = toList string

    List.fold(fun s c -> folder c s) state l

/// Reduce a string from the right.
/// <para>foldr cons "" "time" == "time"</para>
let foldr (folder: char -> 'b -> 'b) state (string: string) =
    let l = toList string

    List.foldBack folder l state

/// Determine whether *any* characters pass the test.
/// <para>any isDigit "90210" == True</para>
/// <para>any isDigit "R2-D2" == True</para>
/// <para>any isDigit "heart" == False</para>
let any = String.exists

/// Determine whether *all* characters pass the test.
/// <para>all isDigit "90210" == True</para>
/// <para>all isDigit "R2-D2" == False</para>
/// <para>all isDigit "heart" == False</para>
let all = String.forall


(* *)

/// Determine if a string is empty.
let isEmpty (value:string) =
    value = ""

/// Get the length of a string.
/// <para>length "innumerable" == 11</para>
/// <para>length "" == 0</para>
let length = String.length

/// Reverse a string.
/// <para>reverse "stressed" == "desserts"</para>
let reverse (string: string) =
    string.ToCharArray()
    |> Array.rev
    |> fun chars -> String(chars, 0, chars.Length)

/// Repeat a string *n* times.
let repeat = String.replicate


(* BUILDING AND SPLITTING *)


/// Split a string using a given separator.
/// <para>split "," "cat,dog,cow"        == ["cat","dog","cow"]</para>
/// <para>split "/" "home/evan/Desktop/" == ["home","evan","Desktop", ""]</para>
let split (sep: string) (value:string) =
    value.Split([| sep |], StringSplitOptions.None)
    |> List.ofArray

/// Put many strings together with a given separator.
/// <para>join "a" ["H","w","ii","n"]        == "Hawaiian"</para>
/// <para>join " " ["cat","dog","cow"]       == "cat dog cow"</para>
/// <para>join "/" ["home","evan","Desktop"] == "home/evan/Desktop"</para>
let join sep (chunks: string list) =
    String.Join(sep, chunks)

/// Replace all occurrences of some substring.
/// <para>replace "." "-" "Json.Decode.succeed" == "Json-Decode-succeed"</para>
/// <para>replace "," "/" "a,b,c,d,e"           == "a/b/c/d/e"</para>
let replace before after (value: string) =
    join after (split before value)

/// Append two strings.
/// <para>append "butter" "fly" == "butterfly"</para>
let append (a:string) b =
    a + b

/// Concatenate many strings into one.
/// <para>concat ["never","the","less"] == "nevertheless"</para>
let concat =
    join ""

/// Break a string into words, splitting on chunks of whitespace.
/// <para>words "How are \t you? \n Good?" == ["How","are","you?","Good?"]</para>
let words value =
    System.Text.RegularExpressions.Regex.Split(value, "\s+")
    |> List.ofArray

/// Break a string into lines, splitting on newlines.
/// <para>lines "How are you?\nGood?" == ["How are you?", "Good?"]</para>
let lines value =
    System.Text.RegularExpressions.Regex.Split(value, "\r\n|\r|\n")
    |> List.ofArray


(* SUBSTRINGS *)

/// Take a substring given a start and end index. Negative indexes
/// are taken starting from the *end* of the string.
let slice i1 i2 (value: string) =
    let len = value.Length

    let from' = if i1 < 0 then Math.Max(len + i1, 0) else Math.Min(i1, len)
    let to' = if i2 < 0 then Math.Max(len + i2, 0) else Math.Min(i2, len)

    let span = Math.Max(to' - from', 0)

    value.Substring(from', span)

/// Take *n* characters from the left side of a string.
/// <para>left 2 "Mulder" == "Mu"</para>
let left n value =
    if n < 1 then ""
    else slice 0 n value

/// Take *n* characters from the right side of a string.
/// <para>right 2 "Scully" == "ly"</para>
let right n string =
    if n < 1 then ""
    else slice -n (length string) string

/// Drop *n* characters from the left side of a string.
/// <para>dropLeft 2 "The Lone Gunmen" == "e Lone Gunmen"</para>
let dropLeft n string =
    if n < 1 then string
    else slice n (length string) string

/// Drop *n* characters from the right side of a string.
/// <para>dropRight 2 "Cigarette Smoking Man" == "Cigarette Smoking M"</para>
let dropRight n string =
    if n < 1 then string
    else slice 0 -n string

/// See if the second string starts with the first one.
/// <para>startsWith "the" "theory" == True</para>
/// <para>startsWith "ory" "theory" == False</para>
let startsWith (part:string) (string:string) =
    string.StartsWith(part)

/// See if the second string ends with the first one.
/// <para>endsWith "the" "theory" == False</para>
/// <para>endsWith "ory" "theory" == True</para>
let endsWith (part:string) (string:string) =
    string.EndsWith(part)



(* DETECT SUBSTRINGS *)

/// See if the second string contains the first one.
/// <para>contains "the" "theory" == True</para>
/// <para>contains "hat" "theory" == False</para>
/// <para>contains "THE" "theory" == False</para>
let contains (part:string) (string:string) =
    string.Contains(part)

let private indexOf (searchStr:string) (startIndex:int) (inputStr:string) =
    if isNull inputStr || isNull searchStr || startIndex < 0 || startIndex > inputStr.Length then
        Nothing
    else
        let index = inputStr.IndexOf(searchStr, startIndex)
        if index >= 0 then Just index
        else Nothing

/// Get all of the indexes for a substring in another string.
/// <para>indexes "i" "Mississippi"   == [1,4,7,10]</para>
/// <para>indexes "ss" "Mississippi"  == [2,5]</para>
/// <para>indexes "needle" "haystack" == []</para>
let indexes part (string:string) =
    if String.IsNullOrEmpty(part) then
        []

    else
        let subLen = String.length part
        let mutable i = 0
        let is = ResizeArray<_>()

        let indexFound() =
            i <- string.IndexOf(part, i)
            i > -1


        while indexFound() do
            is.Add(i)
            i <- i + subLen

        List.ofSeq is

/// Alias for `indexes`.
let indices = indexes

(* FORMATTING *)

/// Convert a string to all upper case. Useful for case-insensitive comparisons
/// and VIRTUAL YELLING.
/// <para>toUpper "skinner" == "SKINNER"</para>
let toUpper (string: string) =
    string.ToUpper()

/// Convert a string to all lower case. Useful for case-insensitive comparisons.
/// <para>toLower "X-FILES" == "x-files"</para>
let toLower (string: string) =
    string.ToLower()

/// Pad a string on both sides until it has a given length.
/// <para>pad 5 ' ' "1"   == "  1  "</para>
/// <para>pad 5 ' ' "11"  == "  11 "</para>
/// <para>pad 5 ' ' "121" == " 121 "</para>
let pad n char (string: string) =
    let half = float(n - length string) / 2.0
    let halfFloor = int(floor half)
    let halfCeiling = int(ceil half)
    let padChar = fromChar char

    repeat halfCeiling padChar
    + string
    + repeat halfFloor padChar

/// Pad a string on the left until it has a given length.
/// <para>padLeft 5 '.' "1"   == "....1"</para>
/// <para>padLeft 5 '.' "11"  == "...11"</para>
/// <para>padLeft 5 '.' "121" == "..121"</para>
let padLeft n char string =
    repeat (n - length string) (fromChar char)
    + string

/// Pad a string on the right until it has a given length.
/// <para>padRight 5 '.' "1"   == "1...."</para>
/// <para>padRight 5 '.' "11"  == "11..."</para>
/// <para>padRight 5 '.' "121" == "121.."</para>
let padRight n char string =
    string +
    repeat (n - length string) (fromChar char)

/// Get rid of whitespace on both sides of a string.
/// <para>trim "  hats  \n" == "hats"</para>
let trim (string: string) =
    string.Trim()

/// Get rid of whitespace on the left of a string.
/// <para>trimLeft "  hats  \n" == "hats  \n"</para>
let trimLeft (string: string) =
    string.TrimStart()

/// Get rid of whitespace on the right of a string.
/// <para>trimRight "  hats  \n" == "  hats"</para>
let trimRight (string: string) =
    string.TrimEnd()


(* INT CONVERSIONS *)

/// Try to convert a string into an int, failing on improperly formatted strings.
/// <para>String.toInt "123" == Just 123</para>
/// <para>String.toInt "-42" == Just -42</para>
/// <para>String.toInt "3.1" == Nothing</para>
/// <para>String.toInt "31a" == Nothing</para>
/// If you are extracting a number from some raw user input, you will typically
/// want to use [`Maybe.withDefault`](Maybe#withDefault) to handle bad data:
/// <para>Maybe.withDefault 0 (String.toInt "42") == 42</para>
/// <para>Maybe.withDefault 0 (String.toInt "ab") == 0</para>
/// <para>String.fromInt 123 == "123"</para>
/// <para>String.fromInt -42 == "-42"</para>
let toInt (string: String) =
    let (success, value) = Int32.TryParse(string)

    if success then Just value else Nothing

///  Convert an `Int` to a `String`.
/// <para>String.fromInt 123 == "123"</para>
/// <para>String.fromInt -42 == "-42"</para>
let fromInt i =
    sprintf "%i" i


(* FLOAT CONVERSIONS *)

/// Try to convert a string into a float, failing on improperly formatted strings.
/// <para>String.toFloat "123" == Just 123.0</para>
/// <para>String.toFloat "-42" == Just -42.0</para>
/// <para>String.toFloat "3.1" == Just 3.1</para>
/// <para>String.toFloat "31a" == Nothing</para>
/// If you are extracting a number from some raw user input, you will typically
/// want to use [`Maybe.withDefault`](Maybe#withDefault) to handle bad data:
/// <para>Maybe.withDefault 0 (String.toFloat "42.5") == 42.5</para>
/// <para>Maybe.withDefault 0 (String.toFloat "cats") == 0</para>
let toFloat (string: string) =
    let (success, value) = Double.TryParse(string)

    if success then Just value else Nothing

/// Convert a `Float` to a `String`.
/// <para>String.fromFloat 123 == "123"</para>
/// <para>String.fromFloat -42 == "-42"</para>
/// <para>String.fromFloat 3.9 == "3.9"</para>
let fromFloat f =
    sprintf "%f" f