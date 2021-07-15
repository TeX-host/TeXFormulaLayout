namespace TeXFormulaLayout

/// Basic binary stream output
module FontTypes =
    open System

    type CharCode =  Int32
    type Delim    =  Int32
    type Penalty  =  Int32
    type Dist     =  Int32
    type Size     =  Int32

    type FontFamily =
        | RM = 0
        | MI = 1
        | SY = 2
        | EX = 3

    let cmName (cm: FontFamily) =
        match cm with
        | FontFamily.RM -> "cmr"
        | FontFamily.MI -> "cmmi"
        | FontFamily.SY -> "cmsy"
        | FontFamily.EX -> "cmex"
        | _ -> ""

    type FontStyle  =
        | D  = 0
        | T  = 1
        | S  = 2
        | SS = 3
