module Elm.Core.Tuple

// CREATE


/// Create a 2-tuple.
/// -- pair 3 4 == (3, 4)
/// zip : List a -> List b -> List (a, b)
/// zip xs ys =
///   List.map2 Tuple.pair xs ys
///

let inline pair a b =
  (a, b)



// ACCESS


/// Extract the first value from a tuple.
/// first (3, 4) == 3
/// first ("john", "doe") == "john"
///
let inline first (x,_) =
    x


/// Extract the second value from a tuple.
/// second (3, 4) == 4
/// second ("john", "doe") == "doe"
///
let inline second (_,y) =
    y



// MAP


/// Transform the first value in a tuple.
/// import String
/// mapFirst String.reverse ("stressed", 16) == ("desserts", 16)
/// mapFirst String.length  ("stressed", 16) == (8, 16)
///
let mapFirst func (x,y) =
    (func x, y)


/// Transform the second value in a tuple.
/// mapSecond sqrt   ("stressed", 16) == ("stressed", 4)
/// mapSecond negate ("stressed", 16) == ("stressed", -16)
///
let mapSecond func (x,y) =
    (x, func y)


/// Transform both parts of a tuple.
/// import String
/// mapBoth String.reverse sqrt  ("stressed", 16) == ("desserts", 4)
/// mapBoth String.length negate ("stressed", 16) == (8, -16)
///
let mapBoth funcA funcB (x,y) =
    ( funcA x, funcB y )