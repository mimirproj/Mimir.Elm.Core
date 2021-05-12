namespace Elm.Core

open System
open System.Text
open System.Runtime.CompilerServices

[<Extension; Sealed>]
type Appendable =
    [<Extension>]
    static member inline Append (x:list<_>, y ) =
        x @ y

    [<Extension>]
    static member inline Append (x:array<_>, y ) =
         Array.append x y

    [<Extension>]
    static member inline Append ((), ()) =
        ()

    [<Extension>]
    static member inline Append (x:Set<_>, y ) =
        Set.union x y

    [<Extension>]
    static member inline Append (x:string, y ) =
        x + y

    #if !FABLE_COMPILER

    static member inline Append (x:StringBuilder, y:StringBuilder) =
        StringBuilder().Append(x).Append(y)

    #endif

    [<Extension>]
    static member inline Append (x:TimeSpan, y:TimeSpan) =
        x + y
