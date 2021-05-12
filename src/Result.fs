namespace Elm.Core

module Result =
    /// If the result is `Ok` return the value, but if the result is an `Error` then
    /// return a given default value. The following examples try to parse integers.
    ///     Result.withDefault 0 (Ok 123)   == 123
    ///     Result.withDefault 0 (Error "no") == 0
    ///
    let inline withDefault def (result:Result<'a, 'x>) : 'a =
        match result with
            | Ok a ->
                a

            | Error _ ->
                def


    /// Apply a function to a result. If the result is `Ok`, it will be converted.
    /// If the result is an `Error`, the same error value will propagate through.
    ///     map sqrt (Ok 4.0)          == Ok 2.0
    ///     map sqrt (Error "bad input") == Error "bad input"
    ///
    // map : (a -> value) -> Result x a -> Result x value
    let inline map (func:'a -> 'value) (ra:Result<'a, 'x>) : Result<'value, 'x>  =
        match ra with
        | Ok a ->
            Ok (func a)

        | Error e ->
            Error e


    /// Apply a function if both results are `Ok`. If not, the first `Error` will
    /// propagate through.
    ///
    ///     map2 max (Ok 42)   (Ok 13)   == Ok 42
    ///     map2 max (Error "x") (Ok 13)   == Error "x"
    ///     map2 max (Ok 42)   (Error "y") == Error "y"
    ///     map2 max (Error "x") (Error "y") == Error "x"
    ///
    /// This can be useful if you have two computations that may fail, and you want
    /// to put them together quickly.
    ///
    let map2 func (ra:Result<'a, 'x>) (rb:Result<'b, 'x>) : Result<'value, 'x> =
        match ra with
        | Error x ->
            Error x

        | Ok a ->
            match rb with
            | Error x ->
                Error x

            | Ok b ->
                Ok (func a b)


    let map3 func (ra:Result<'a, 'x>) (rb:Result<'b, 'x>) (rc:Result<'c, 'x>) : Result<'value, 'x> =
        match ra with
        | Error x ->
            Error x

        | Ok a ->
            match rb with
            | Error x ->
                Error x

            | Ok b ->
                match rc with
                | Error x ->
                    Error x

                | Ok c ->
                    Ok (func a b c)


    let map4 func (ra:Result<'a, 'x>) (rb:Result<'b, 'x>) (rc:Result<'c, 'x>) (rd:Result<'d, 'x>) : Result<'value, 'x> =
        match ra with
        | Error x ->
            Error x

        | Ok a ->
            match rb with
            | Error x ->
                Error x

            | Ok b ->
                match rc with
                | Error x ->
                    Error x

                | Ok c ->
                    match rd with
                    | Error x ->
                        Error x

                    | Ok d ->
                        Ok (func a b c d)


    let map5 func (ra:Result<'a, 'x>) (rb:Result<'b, 'x>) (rc:Result<'c, 'x>) (rd:Result<'d, 'x>) (re:Result<'e, 'x>) : Result<'value, 'x> =
        match ra with
        | Error x ->
            Error x

        | Ok a ->
            match rb with
            | Error x ->
                Error x

            | Ok b ->
                match rc with
                | Error x ->
                    Error x

                | Ok c ->
                    match rd with
                    | Error x ->
                        Error x

                    | Ok d ->
                        match re with
                        | Error x ->
                            Error x

                        | Ok e ->
                            Ok (func a b c d e)


    /// Chain together a sequence of computations that may fail. It is helpful
    /// to see its definition:
    ///
    ///     andThen : (a -> Result e b) -> Result e a -> Result e b
    ///     andThen callback result =
    ///         case result of
    ///           Ok value -> callback value
    ///           Error msg -> Error msg
    ///
    /// This means we only continue with the callback if things are going well. For
    /// example, say you need to use (`toInt : String -> Result String Int`) to parse
    /// a month and make sure it is between 1 and 12:
    ///
    ///     toValidMonth : Int -> Result String Int
    ///     toValidMonth month =
    ///         if month >= 1 && month <= 12
    ///             then Ok month
    ///             else Error "months must be between 1 and 12"
    ///
    ///     toMonth : String -> Result String Int
    ///     toMonth rawString =
    ///         toInt rawString
    ///           |> andThen toValidMonth
    ///
    ///     -- toMonth "4" == Ok 4
    ///     -- toMonth "9" == Ok 9
    ///     -- toMonth "a" == Error "cannot parse to an Int"
    ///     -- toMonth "0" == Error "months must be between 1 and 12"
    ///
    /// This allows us to come out of a chain of operations with quite a specific error
    /// message. It is often best to create a custom type that explicitly represents
    /// the exact ways your computation may fail. This way it is easy to handle in your
    /// code.
    ///
    let andThen callback (result:Result<'a, 'x>) : Result<'b, 'x> =
        match result with
        | Ok value ->
            callback value

        | Error msg ->
            Error msg


    /// Transform an `Error` value. For example, say the errors we get have too much
    /// information:
    ///
    ///     parseInt : String -> Result ParseError Int
    ///
    ///     type alias ParseError =
    ///         { message : String
    ///         , code : Int
    ///         , position : (Int,Int)
    ///         }
    ///
    ///     mapError .message (parseInt "123") == Ok 123
    ///     mapError .message (parseInt "abc") == Error "char 'a' is not a number"
    ///
    let inline mapError f (result:Result<'a, 'x>) : Result<'a, 'y> =
        match result with
        | Ok v ->
            Ok v

        | Error e ->
            Error (f e)


    /// Convert to a simpler `Maybe` if the actual error message is not needed or
    /// you need to interact with some code that primarily uses maybes.
    ///
    ///     parseInt : String -> Result ParseError Int
    ///
    ///     maybeParseInt : String -> Maybe Int
    ///     maybeParseInt string =
    ///         toMaybe (parseInt string)
    ///
    let inline toMaybe (result:Result<'a, 'x>) =
        match result with
        | Ok  v -> Just v
        | Error _ -> Nothing


    /// Convert from a simple `Maybe` to interact with some code that primarily
    /// uses `Results`.
    ///
    ///     parseInt : String -> Maybe Int
    ///
    ///     resultParseInt : String -> Result String Int
    ///     resultParseInt string =
    ///         fromMaybe ("error parsing string: " ++ toString string) (parseInt string)
    ///
    let inline fromMaybe err maybe =
        match maybe with
        | Just v  -> Ok v
        | Nothing -> Error err