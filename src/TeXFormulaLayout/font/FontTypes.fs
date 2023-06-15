namespace TeXFormulaLayout

/// Types for font
/// (BasicTypes)
module FontTypes =
    open TeXFormulaLayout.Distance
    open BasicTypes

    type CharCode = BaseIntType
    type Delim = BaseIntType
    type Penalty = BaseIntType
    (* Skip: Distance.Dist *)
    type Size = BaseIntType

    type FontNum = BaseIntType
    type FontSize = BaseIntType

    /// xref: texbook:p199:S546
    type VarCharInfo = {
        top: CharCode option
        // mid: CharCode option
        bot: CharCode option
        rep: CharCode option
    }

    /// xref: neuform:p6:sec3.1, mfbook:p315, texbook:p63
    type CharInfo = {
        // ---- 4D for each character
        /// the width of the bounding box
        width: Dist
        /// the height (above the baseline) of the bounding box
        height: Dist
        /// the depth (below the baseline) of the bounding box
        depth: Dist
        /// the character’s “italic correction”
        italic: Dist

        /// larger version of the same char
        larger: CharCode option
        varChar: VarCharInfo /// NOTE: not used, set to None
    }

    type Font = CharInfo list

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

    type FontStyle =
        | D = 0
        | T = 1
        | S = 2
        | SS = 3

    let fontSize (famliy, style) : FontSize =
        match (famliy, style) with
        | (_, FontStyle.D) -> 10
        | (_, FontStyle.T) -> 10
        | (FontFamily.EX, _) -> 10
        | (_, FontStyle.S) -> 7
        | (_, FontStyle.SS) -> 5
        | _ -> -1

    exception NotImplemented of string
    exception CannotHappen
