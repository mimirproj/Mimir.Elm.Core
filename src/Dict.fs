namespace Elm.Core

[<AutoOpen>]
module DictPervasives =
    type Dict<'comparable, 'v when 'comparable : comparison> = Map<'comparable, 'v>


module Dict =
    let empty : Dict<'comparable, 'v> =
        Map.empty

    /// Get the value associated with a key. If the key is not found, return
    /// `Nothing`. This is useful when you are not sure if a key will be in the
    /// dictionary.
    ///
    ///    animals = fromList [ ("Tom", Cat), ("Jerry", Mouse) ]
    ///    get "Tom"   animals == Just Cat
    ///    get "Jerry" animals == Just Mouse
    ///    get "Spike" animals == Nothing
    ///
    let get : 'comparable -> Dict<'comparable, 'v> -> Maybe<'v> =
        Map.tryFind


    /// Determine if a key is in a dictionary.
    let inline member' key (dict:Dict<'comparable, 'v>) =
        match get key dict with
        | Just _ ->
            True

        | Nothing ->
            False


    /// Determine the number of key-value pairs in the dictionary.
    let inline size (dict:Dict<'comparable, 'v>) =
        Map.count dict


    /// Insert a key-value pair into a dictionary. Replaces value when there is
    /// a collision.
    ///
    let inline insert key value (dict:Dict<'comparable, 'v>) : Dict<'comparable, 'v> =
        Map.add key value dict

    /// Determine if a dictionary is empty.
    ///
    /// isEmpty empty == True
    ///
    let isEmpty : Dict<'comparable, 'v> -> Bool =
        Map.isEmpty

    /// Remove a key-value pair from a dictionary. If the key is not found,
    /// no changes are made.
    let remove : 'comparable -> Dict<'comparable, 'v> -> Dict<'comparable, 'v> =
        Map.remove


    /// Update the value of a dictionary for a specific key with a given function. -}
    let update targetKey alter (dictionary:Dict<'comparable, 'v>) =
        match alter (get targetKey dictionary) with
        | Just value ->
            insert targetKey value dictionary

        | Nothing ->
            remove targetKey dictionary


    /// Create a dictionary with one key-value pair. -}
    //singleton : comparable -> v -> Dict comparable v
    let inline singleton key value : (Dict<'comparable, 'v>) =
        Map.ofList [ (key, value) ]


    (* TRANSFORM *)

    /// Apply a function to all values in a dictionary.
    let map : ('comparable -> 'a -> 'b) -> Dict<'comparable, 'a> -> Dict<'comparable, 'b> =
        Map.map


    /// Fold over the key-value pairs in a dictionary from lowest key to highest key.
    ///
    /// getAges : Dict String User -> List String
    /// getAges users =
    ///   Dict.foldl addAge [] users
    ///
    /// addAge : String -> User -> List String -> List String
    /// addAge _ user ages =
    ///   user.age :: ages
    ///
    /// -- getAges users == [33,19,28]
    ///
    let inline foldl (func:'comparable -> 'v -> 'b -> 'b) acc (dict:Dict<'comparable, 'v>) =
        Map.fold
            (fun state key value -> func key value state)
            acc
            dict


    /// Fold over the key-value pairs in a dictionary from highest key to lowest key.
    ///
    /// getAges : Dict String User -> List String
    /// getAges users =
    ///   Dict.foldr addAge [] users
    ///
    /// addAge : String -> User -> List String -> List String
    /// addAge _ user ages =
    ///   user.age :: ages
    ///
    /// -- getAges users == [28,19,33]
    ///
    let inline foldr (func:'comparable -> 'v -> 'b -> 'b) acc (dict:Dict<'comparable, 'v>)  =
        Map.foldBack
            func
            dict
            acc


    /// Keep only the key-value pairs that pass the given test.
    let filter : ('comparable -> 'v -> Bool) -> Dict<'comparable, 'v> -> Dict<'comparable, 'v> =
        Map.filter


    /// Partition a dictionary according to some test. The first dictionary
    /// contains all key-value pairs which passed the test, and the second contains
    /// the pairs that did not.
    ///
    let partition isGood (dict:Dict<'comparable, 'v> ) =
        let add key value (t1, t2) =
            if isGood key value then
                (insert key value t1, t2)

            else
                (t1, insert key value t2)

        foldl add (empty, empty) dict


    (* LISTS *)


    /// Get all of the keys in a dictionary, sorted from lowest to highest.
    ///
    /// keys (fromList [(0,"Alice"),(1,"Bob")]) == [0,1]
    ///
    let inline keys (dict:Dict<'comparable, 'v>) =
        foldr (fun key value keyList -> key :: keyList) [] dict


    /// Get all of the values in a dictionary, in the order of their keys.
    ///
    /// values (fromList [(0,"Alice"),(1,"Bob")]) == ["Alice", "Bob"]
    ///
    let values (dict:Dict<'comparable, 'v>) =
        foldr (fun key value valueList -> value :: valueList) [] dict


    /// Convert a dictionary into an association list of key-value pairs, sorted by keys.
    let inline toList (dict:Dict<'comparable, 'v>) =
        foldr (fun key value list -> (key,value) :: list) [] dict


    /// Convert an association list into a dictionary. -}
    let fromList : List<_> -> Dict<'comparable, 'v> =
        Map.ofList



    (* COMBINE *)

    /// Combine two dictionaries. If there is a collision, preference is given
    /// to the first dictionary.
    ///
    let inline union t1 t2 : Dict<'comparable, 'v> =
        foldl insert t2 t1


    /// Keep a key-value pair when its key appears in the second dictionary.
    /// Preference is given to values in the first dictionary.
    ///
    let inline intersect t1 t2 : Dict<'comparable, 'v> =
        filter (fun k _ -> member' k t2) t1


    /// Keep a key-value pair when its key does not appear in the second dictionary.
    let inline diff t1 t2 : Dict<'comparable, 'v> =
        foldl (fun k v t -> remove k t) t1 t2


    /// The most general way of combining two dictionaries. You provide three
    /// accumulators for when a given key appears:
    ///
    ///   1. Only in the left dictionary.
    ///   2. In both dictionaries.
    ///   3. Only in the right dictionary.
    ///
    /// You then traverse all the keys from lowest to highest, building up whatever
    /// you want.
    ///
    let merge
        leftStep
        bothStep
        rightStep
        (leftDict:Dict<'comparable, 'a>)
        (rightDict:Dict<'comparable, 'b>)
        (initialResult:'result) =

        let rec stepState rKey rValue (list, result) =
            match list with
            | [] ->
                (list, rightStep rKey rValue result)

            | (lKey, lValue) :: rest ->
                if lKey < rKey then
                    stepState rKey rValue (rest, leftStep lKey lValue result)

                elif lKey > rKey then
                    (list, rightStep rKey rValue result)

                else
                    (rest, bothStep lKey lValue rValue result)

        let (leftovers, intermediateResult) =
            foldl stepState (toList leftDict, initialResult) rightDict

        List.foldl (fun (k,v) result -> leftStep k v result) intermediateResult leftovers
