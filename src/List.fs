module Elm.Core.List


/// Create a list with *n* copies of a value:
/// repeat 3 (0,0) == [(0,0),(0,0),(0,0)]
///
let inline repeat n e =
    List.init n (fun _ -> e)


/// Create a list of numbers, every element increasing by one.
/// You give the lowest and highest number that should be in the list.
///     range 3 6 == [3, 4, 5, 6]
///     range 3 3 == [3]
///     range 6 3 == []
///
//range : Int -> Int -> List Int
let inline range (lo:Int) hi =
    [lo .. hi]


/// Add an element to the front of a list.
///     1 :: [2,3] == [1,2,3]
///     1 :: [] == [1]
/// This operator is pronounced *cons* for historical reasons, but you can think
/// of it like pushing an entry onto a stack.
///
let inline cons head list =
    head :: list


/// Same as `map` but the function is also applied to the index of each
/// element (starting at zero).
///     indexedMap Tuple.pair ["Tom","Sue","Bob"] == [ (0,"Tom"), (1,"Sue"), (2,"Bob") ]
///
let inline indexedMap f xs =
    List.map2 f (range 0 (List.length xs - 1)) xs


/// Reduce a list from the left.
///     foldl (+)  0  [1,2,3] == 6
///     foldl (::) [] [1,2,3] == [3,2,1]
/// So `foldl step state [1,2,3]` is like saying:
///     state
///       |> step 1
///       |> step 2
///       |> step 3
///
let inline foldl (func:'a -> 'b -> 'b) acc list =
    List.fold
        (fun state item -> func item state)
        acc
        list


/// Reduce a list from the right.
///     foldr (+)  0  [1,2,3] == 6
///     foldr (::) [] [1,2,3] == [1,2,3]
/// So `foldr step state [1,2,3]` is like saying:
///     state
///       |> step 3
///       |> step 2
///       |> step 1
///
let inline foldr (func:'a -> 'b -> 'b) (acc:'b) (list:List<'a>) : 'b =
    List.foldBack
        func
        list
        acc


let private maybeCons (f: 'a -> Maybe<'b>) (mx:'a) (xs:List<'b>) : List<'b> =
    match f mx with
    | Just x ->
        cons x xs

    | Nothing ->
        xs


/// Filter out certain values. For example, maybe you have a bunch of strings
/// from an untrusted source and you want to turn them into numbers:
///
///     numbers : List Int
///     numbers =
///       filterMap String.toInt ["3", "hi", "12", "4th", "May"]
///     -- numbers == [3, 12]
///
let filterMap (f: 'a -> Maybe<'b>) (xs:List<'a>) : List<'b>=
    foldr (maybeCons f) [] xs


/// Reverse a list.
///     reverse [1,2,3,4] == [4,3,2,1]
let inline reverse list =
    List.rev list


/// Determine if all elements satisfy some test.
///     all isEven [2,4] == True
///     all isEven [2,3] == False
///     all isEven [] == True
///
let inline all isOkay list =
    List.forall isOkay list


/// Determine if any elements satisfy some test.
///     any isEven [2,3] == True
///     any isEven [1,3] == False
///     any isEven [] == False
///
let any isOkay list =
    List.exists isOkay list


/// Figure out whether a list contains a value.
///     member 9 [1,2,3,4] == False
///     member 4 [1,2,3,4] == True
///
let inline member' x xs =
    any (fun a -> a == x) xs


/// Find the maximum element in a non-empty list.
///     maximum [1,4,2] == Just 4
///     maximum []      == Nothing
///
let inline maximum list =
    match list with
    | x :: xs ->
        Just (foldl max x xs)

    | _ ->
        Nothing


/// Find the minimum element in a non-empty list.
///     minimum [3,2,1] == Just 1
///     minimum []      == Nothing
///
let inline minimum list =
    match list with
    | x :: xs ->
        Just (foldl min x xs)

    | _ ->
        Nothing


/// Get the sum of the list elements.
///     sum [1,2,3] == 6
///     sum [1,1,1] == 3
///     sum []      == 0
///
let inline sum numbers =
    foldl (+) 0 numbers


/// Get the product of the list elements.
///     product [2,2,2] == 8
///     product [3,3,3] == 27
///     product []      == 1
///
let inline product numbers =
    foldl (*) 1 numbers


/// Put two lists together.
///     append [1,1,2] [3,5,8] == [1,1,2,3,5,8]
///     append ['a','b'] ['c'] == ['a','b','c']
/// You can also use [the `(++)` operator](Basics#++) to append lists.
///
let inline append xs ys =
    match ys with
    | [] ->
      xs

    | _ ->
      foldr cons ys xs


/// Map a given function onto a list and flatten the resulting lists.
///     concatMap f xs == concat (map f xs)
///
let inline concatMap f list =
    List.collect f list


/// Places the given value between all members of the given list.
///     intersperse "on" ["turtles","turtles","turtles"] == ["turtles","on","turtles","on","turtles"]
///
let intersperse sep xs =
    match xs with
    | [] ->
        []

    | hd :: tl ->
        let step x rest =
            cons sep (cons x rest)

        let spersed =
            foldr step [] tl

        cons hd spersed


let private (<!>) fctList list = List.map2 (fun fct elem -> fct elem) fctList list


let map4 f l1 l2 l3 l4 =
    List.map f l1 <!> l2 <!> l3 <!> l4


let map5 f l1 l2 l3 l4 l5 =
    List.map f l1 <!> l2 <!> l3 <!> l4 <!> l5


/// Sort values with a custom comparison function.
///
///     sortWith flippedComparison [1,2,3,4,5] == [5,4,3,2,1]
///
///     flippedComparison a b =
///         case compare a b of
///           LT -> GT
///           EQ -> EQ
///           GT -> LT
///
/// This is also the most general sort function, allowing you
/// to define any other: `sort == sortWith compare`
///
let sortWith (f : 'a -> 'a -> Order) list =
    list
    |> List.sortWith (fun a b ->
        match f a b with
        | LT -> -1
        | EQ -> 0
        | GT -> 1)


/// Extract the first element of a list.
///
///     head [1,2,3] == Just 1
///     head [] == Nothing
///
/// **Note:** It is usually preferable to use a `case` to deconstruct a `List`
/// because it gives you `(x :: xs)` and you can work with both subparts.
///
let inline head list =
    match list with
    | x :: xs ->
        Just x

    | [] ->
        Nothing


/// Extract the rest of the list.
///
///     tail [1,2,3] == Just [2,3]
///     tail [] == Nothing
///
/// **Note:** It is usually preferable to use a `case` to deconstruct a `List`
/// because it gives you `(x :: xs)` and you can work with both subparts.
///
let inline tail list =
    match list with
    | x :: xs ->
        Just xs

    | [] ->
        Nothing


/// Take the first *n* members of a list.
///
///     take 2 [1,2,3,4] == [1,2]
///
//take : Int -> List a -> List a
let inline take n list =
    List.truncate n list


/// Drop the first *n* members of a list.
///
///     drop 2 [1,2,3,4] == [3,4]
///
let rec drop n list =
    if n <= 0 then
        list

    else
        match list with
        | [] ->
            list

        | _ :: xs ->
            drop (n-1) xs

