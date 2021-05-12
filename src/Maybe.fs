namespace Elm.Core

[<AutoOpen>]
module MaybePervasives =
    /// Represent values that may or may not exist. It can be useful if you have a
    /// record field that is only filled in sometimes. Or if a function takes a value
    /// sometimes, but does not absolutely need it.
    /// <para>-- A person, but maybe we do not know their age.</para>
    /// <para>type Person =</para>
    /// <para>    { name : String</para>
    /// <para>    , age : Maybe Int</para>
    /// <para>    }</para>
    /// <para>let tom = { name = "Tom", age = Just 42 }</para>
    /// <para>let sue = { name = "Sue", age = Nothing }</para>
    type Maybe<'a> = Option<'a>

    let Nothing : Maybe<'a> = None
    let inline Just a : Maybe<'a> = Some a

    let inline (|Nothing|Just|) (value: Maybe<'a>) =
        match value with
        | None -> Nothing
        | Some a ->  Just a


module Maybe =
    /// Provide a default value, turning an optional value into a normal
    /// value.  This comes in handy when paired with functions like
    /// [`Dict.get`](Dict#get) which gives back a `Maybe`.
    /// <para>withDefault 100 (Just 42)   -- 42</para>
    /// <para>withDefault 100 Nothing     -- 100</para>
    /// <para>withDefault "unknown" (Dict.get "Tom" Dict.empty)   -- "unknown"</para>
    /// **Note:** This can be overused! Many cases are better handled by a `case`
    /// expression. And if you end up using `withDefault` a lot, it can be a good sign
    /// that a [custom type][ct] will clean your code up quite a bit!
    /// [ct]: https://guide.elm-lang.org/types/custom_types.html
    let withDefault default' (maybe:Maybe<'a>) =
        match maybe with
        | Just value -> value
        | Nothing -> default'

    /// Transform a `Maybe` value with a given function:
    /// <para>map sqrt (Just 9) == Just 3</para>
    /// <para>map sqrt Nothing  == Nothing</para>
    /// <para>map sqrt (String.toFloat "9") == Just 3</para>
    /// <para>map sqrt (String.toFloat "x") == Nothing</para>
    let map f (maybe:Maybe<'a>) : Maybe<'b> =
        Option.map f maybe

    /// Apply a function if all the arguments are `Just` a value.
    /// <para>map2 (+) (Just 3) (Just 4) == Just 7</para>
    /// <para>map2 (+) (Just 3) Nothing == Nothing</para>
    /// <para>map2 (+) Nothing (Just 4) == Nothing</para>
    /// <para>map2 (+) (String.toInt "1") (String.toInt "123") == Just 124</para>
    /// <para>map2 (+) (String.toInt "x") (String.toInt "123") == Nothing</para>
    /// <para>map2 (+) (String.toInt "1") (String.toInt "1.3") == Nothing</para>
    let map2 (func: 'a -> 'b -> 'value) ma mb =
        match ma with
        | Nothing -> Nothing
        | Just a ->
            match mb with
            | Nothing -> Nothing
            | Just b -> Just(func a b)

    /// Chain together many computations that may fail. It is helpful to see its
    /// definition:
    /// <para>andThen : (a -> Maybe b) -> Maybe a -> Maybe b</para>
    /// <para>andThen callback maybe =</para>
    /// <para>    case maybe of</para>
    /// <para>        Just value -></para>
    /// <para>            callback value</para>
    /// <para>        Nothing -></para>
    /// <para>            Nothing</para>
    /// This means we only continue with the callback if things are going well. For
    /// example, say you need to parse some user input as a month:
    /// <para>parseMonth : String -> Maybe Int</para>
    /// <para>parseMonth userInput =</para>
    /// <para>    String.toInt userInput</para>
    /// <para>    |> andThen toValidMonth</para>
    /// <para>toValidMonth : Int -> Maybe Int</para>
    /// <para>toValidMonth month =</para>
    /// <para>    if 1 <= month && month <= 12 then</para>
    /// <para>        Just month</para>
    /// <para>    else</para>
    /// <para>        Nothing</para>
    /// In the `parseMonth` function, if `String.toInt` produces `Nothing` (because
    /// the `userInput` was not an integer) this entire chain of operations will
    /// short-circuit and result in `Nothing`. If `toValidMonth` results in `Nothing`,
    /// again the chain of computations will result in `Nothing`.
    let andThen callback maybeValue =
        match maybeValue with
        | Just value -> callback value
        | Nothing ->
            Nothing


