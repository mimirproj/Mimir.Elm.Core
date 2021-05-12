[<AutoOpen>]
module Elm.Core.Basics

open System

type Int = int32
type Float = Double
type Bool = bool

/// Add two numbers.
let private add a b =
    a + b

/// Subtract numbers like `4 - 3 == 1`.
let private sub minuend subtrahend =
    minuend - subtrahend

/// Multiply numbers like `2 * 3 == 6`.
let private mul multiplier multiplicand =
    multiplier * multiplicand

/// Floating-point division.
let private fdiv (numerator:float) (denominator:float) : float =
    numerator / denominator

/// Integer division.
/// Notice that the remainder is discarded.
let private idiv (numerator:int) (denominator:int) : int =
    numerator / denominator


// OPERATORS

/// Check if values are the same.
let inline (==) a b =
    a = b

/// Check if values are not the same.
let inline (/=) a b =
    a <> b

/// Put two appendable things together. This includes strings and lists.
let inline (++) (x:'T) (y:'T) :'T =
    let inline invoke (_:^M, a:^t, b:^t) = ((^M or ^t) : (static member Append: _*_ -> _) a, b)
    invoke (Unchecked.defaultof<Appendable>, x, y)

/// Exponentiation
let inline (^) (base':Float) (exponent:Float) =
    base' ** exponent


// INT TO FLOAT / FLOAT TO INT

/// Convert an integer into a float.
let inline toFloat (n:Int) : Float  =
    float n

/// Round a number to the nearest integer.
let inline round (n:Float) : Int =
    int (Math.Round(n, MidpointRounding.AwayFromZero))

/// Floor function, rounding down.
let inline floor (n:Float) : Int =
    int (floor n)

/// Ceiling function, rounding up.
let inline ceiling (n:Float) : Int =
    int(ceil n)

/// Truncate a number, rounding towards zero.
let inline truncate (n:Float) : Int =
    int (truncate n)




// COMPARISONS

/// Represents the relative ordering of two things.
/// The relations are less than, equal to, and greater than.
type Order = LT | EQ | GT

/// Compare any two comparable values.
let inline compare a b =
    if a < b then LT
    elif a > b then GT
    else EQ


// BOOLEANS

let True : Bool = true
let False : Bool = false

/// The exclusive-or operator.
let inline xor a b =
    (a || b) && not (a && b)


// FANCIER MATH

/// Perform [modular arithmetic](https://en.wikipedia.org/wiki/Modular_arithmetic).
/// A common trick is to use (n mod 2) to detect even and odd numbers.
let inline modBy modulus (x:Int) =
    let answer = x % modulus
    if modulus = 0 then
        invalidArg "modulus" "Cannot be 0."
    else
        if ((answer > 0 && modulus < 0) || (answer < 0 && modulus > 0)) then
            answer + modulus
        else
            answer

/// Get the remainder after division.
let inline remainderBy b (a:Int) =
    a % b

/// Negate a number.
let inline negate n =
    -n

/// Get the [absolute value][abs] of a number.
let inline abs n = abs n

/// Clamps a number within a given range.
let clamp low high number =
  if number < low then
    low
  elif number > high then
    high
  else
    number

/// Take the square root of a number.
let inline sqrt n = sqrt n

/// Calculate the logarithm of a number with a given base.
let logBase base' number =
    fdiv
        (Math.Log(number))
        (Math.Log(base'))

/// An approximation of e.
let e = Math.E

// TRIGONOMETRY

/// An approximation of pi.
let pi = Math.PI

/// Figure out the cosine given an angle in radians.
let inline cos angleInRadians =
    Math.Cos angleInRadians

/// Figure out the sine given an angle in radians.
let inline sin angleInRadians =
    Math.Sin angleInRadians

/// Figure out the tangent given an angle in radians.
let inline tan angleInRadians =
    Math.Tan angleInRadians

/// Figure out the arccosine for `adjacent / hypotenuse` in radians.
let inline acos angleInRadians =
    Math.Acos angleInRadians

/// Figure out the arcsine for `opposite / hypotenuse` in radians.
let inline asin angleInRadians =
    Math.Asin angleInRadians

/// This helps you find the angle (in radians) to an `(x,y)` coordinate, but
/// in a way that is rarely useful in programming. **You probably want
/// [`atan2`](#atan2) instead!**
let inline atan angleInRadians =
    Math.Atan angleInRadians

/// This helps you find the angle (in radians) to an `(x,y)` coordinate.
/// So rather than saying `atan (y/x)` you say `atan2 y x` and you can get a full
/// range of angles.
let inline atan2 y x =
    Math.Atan2(y, x)

// ANGLES

/// Convert radians to standard Elm angles (radians).
let radians angleInRadians =
    angleInRadians

/// Convert degrees to standard Elm angles (radians).
let degrees angleInDegrees =
    fdiv (mul angleInDegrees pi) 180.0

/// Convert turns to standard Elm angles (radians). One turn is equal to 360°.
let turns angleInTurns =
    mul (mul 2.0 pi) angleInTurns


// POLAR COORDINATES

/// Convert polar coordinates (r,&theta;) to Cartesian coordinates (x,y).
let fromPolar (radius, theta) =
    ( mul radius (cos theta)
    , mul radius (sin theta)
    )

/// Convert Cartesian coordinates (x,y) to polar coordinates (r,&theta;).
let toPolar ( x, y ) =
    ( sqrt (add (mul x x) (mul y y))
    , atan2 y x
    )


// CRAZY FLOATS

/// Determine whether a float is an undefined or unrepresentable number.
/// NaN stands for *not a number* and it is [a standardized part of floating point
/// numbers](https://en.wikipedia.org/wiki/NaN).
let inline isNaN (n:float) =
    Double.IsNaN n

/// Determine whether a float is positive or negative infinity.
let inline isInfinite (n:float) =
    Double.IsInfinity n


// FUNCTION HELPERS

/// Given a value, returns exactly the same value. This is called
/// [the identity function](https://en.wikipedia.org/wiki/Identity_function).
let inline identity x =
    x

/// Create a function that *always* returns the same value.
let inline always a _ =
    a

/// A value that can never happen!
type Never = JustOneMore of Never

/// So the `never` function is basically telling the type system, make sure no one
/// ever calls me!
let rec never (JustOneMore nvr) =
    never nvr