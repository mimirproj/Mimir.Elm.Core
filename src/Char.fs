module Elm.Core.Char

/// Convert to the corresponding Unicode [code point][cp].
let inline toCode (c:char) =
    int c

/// Detect upper case ASCII characters.
let inline isUpper (char:char) =
    let
        code =
          toCode char
    in
        code <= 0x5A && 0x41 <= code

/// Detect lower case ASCII characters.
let inline isLower (char:char) =
    let
        code =
          toCode char
    in
        0x61 <= code && code <= 0x7A

/// Detect upper case and lower case ASCII characters.
let isAlpha char =
    isLower char || isUpper char

/// Detect digits `0123456789`
let isDigit char =
    let
        code =
          toCode char
    in
        code <= 0x39 && 0x30 <= code

/// Detect upper case and lower case ASCII characters.
let isAlphaNum char =
    isLower char || isUpper char || isDigit char

/// Detect octal digits `01234567`.
let isOctDigit char =
    let
        code =
          toCode char
    in
        code <= 0x37 && 0x30 <= code

/// Detect hexadecimal digits `0123456789abcdefABCDEF`
let isHexDigit char =
    let
        code = toCode char
    in
        (0x30 <= code && code <= 0x39)
        || (0x41 <= code && code <= 0x46)
        || (0x61 <= code && code <= 0x66)


// CONVERSIONS

/// Convert to upper case.
let inline toUpper c = System.Char.ToUpper c

/// Convert to lower case.
let inline toLower c = System.Char.ToLower c

/// Convert a Unicode [code point][cp] to a character.
let inline fromCode (code:int) =
    char code