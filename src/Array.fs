module Elm.Core.Array


/// Initialize an array. `initialize n f` creates an array of length `n` with
/// the element at index `i` initialized to the result of `(f i)`.
///     initialize 4 identity    == fromList [0,1,2,3]
///     initialize 4 (\n -> n*n) == fromList [0,1,4,9]
///     initialize 4 (always 0)  == fromList [0,0,0,0]
///
let inline initialize len fn =
    if len <= 0 then
        Array.empty
    else
        Array.init len fn


/// Creates an array with a given length, filled with a default element.
///     repeat 5 0     == fromList [0,0,0,0,0]
///     repeat 3 "cat" == fromList ["cat","cat","cat"]
/// Notice that `repeat 3 x` is the same as `initialize 3 (always x)`.
///
let inline repeat n e =
    initialize n (fun _ -> e)


/// Create an array from a `List`.
///
let inline fromList list =
    Array.ofList list


/// Return `Just` the element at the index or `Nothing` if the index is out of
/// range.
///     get  0 (fromList [0,1,2]) == Just 0
///     get  2 (fromList [0,1,2]) == Just 2
///     get  5 (fromList [0,1,2]) == Nothing
///     get -1 (fromList [0,1,2]) == Nothing
///
let inline get index array =
    if index < 0 || index >= Array.length array then
        Nothing
    else
        Array.get array index
        |> Some


/// Set the element at a particular index. Returns an updated array.
/// If the index is out of range, the array is unaltered.
///     set 1 7 (fromList [1,2,3]) == fromList [1,7,3]
///
let inline set index value array =
    if index < 0 || index >= Array.length array then
        array
    else
        let arr2 = Array.copy array
        Array.set arr2 index value
        arr2


/// Push an element onto the end of an array.
///     push 3 (fromList [1,2]) == fromList [1,2,3]
///
let inline push a array =
    Array.append array [| a |]


/// Create an indexed list from an array. Each element of the array will be
/// paired with its index.
///     toIndexedList (fromList ["cat","dog"]) == [(0,"cat"), (1,"dog")]
///
let toIndexedList array =
    array
    |> Array.indexed
    |> Array.toList


/// Reduce an array from the right. Read `foldr` as fold from the right.
///    foldr (+) 0 (repeat 3 5) == 15
///
let inline foldr (func:'a -> 'b -> 'b) baseCase array =
    Array.foldBack func baseCase array


/// Reduce an array from the left. Read `foldl` as fold from the left.
///    foldl (::) [] (fromList [1,2,3]) == [3,2,1]
///
// foldl : (a -> b -> b) -> b -> Array a -> b
let inline foldl (func:'a -> 'b -> 'b) baseCase array =
    Array.fold
        (fun state item -> func item state)
        baseCase
        array


/// Apply a function on every element with its index as first argument.
///     indexedMap (*) (fromList [5,5,5]) == fromList [0,5,10]
///
let indexedMap =
    Array.mapi


/// Given a relative array index, convert it into an absolute one.
///     translateIndex -1 someArray == someArray.length - 1
///     translateIndex -10 someArray == someArray.length - 10
///     translateIndex 5 someArray == 5
///
let private translateIndex index (array:'a array) =
    let len = Array.length array

    let posIndex =
            if index < 0 then
                len + index
            else
                index
    in
        if posIndex < 0 then
            0
        else if posIndex > len then
            len
        else
            posIndex


/// Get a sub-section of an array: `(slice start end array)`. The `start` is a
/// zero-based index where we will start our slice. The `end` is a zero-based index
/// that indicates the end of the slice. The slice extracts up to but not including
/// `end`.
///     slice  0  3 (fromList [0,1,2,3,4]) == fromList [0,1,2]
///     slice  1  4 (fromList [0,1,2,3,4]) == fromList [1,2,3]
/// Both the `start` and `end` indexes can be negative, indicating an offset from
/// the end of the array.
///     slice  1 -1 (fromList [0,1,2,3,4]) == fromList [1,2,3]
///     slice -2  5 (fromList [0,1,2,3,4]) == fromList [3,4]
/// This makes it pretty easy to `pop` the last element off of an array:
/// `slice 0 -1 array`
///
//slice : Int -> Int -> Array a -> Array a
let slice from to' array =
    let correctFrom =
        translateIndex from array

    let correctTo =
        translateIndex to' array

    if correctFrom > correctTo then
        Array.empty
    else
        printfn "from %i, to %i" correctFrom correctTo
        array.[ correctFrom .. (correctTo - 1) ]
