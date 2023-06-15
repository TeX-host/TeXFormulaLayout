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


    /// [tex#546p199]  `extensible_recipe`
    type VarCharInfo = {
        top: CharCode option
        // mid: CharCode option
        bot: CharCode option
        rep: CharCode option
    }

    /// xref: neuform:p6:sec3.1, mfbook:p315, texbook:p63
    type CharInfo = {
        // ---- 4D for each character
        /// width of the bounding box
        width: Dist
        /// height (above the baseline) of the bounding box
        height: Dist
        /// depth (below the baseline) of the bounding box
        depth: Dist
        /// italic correction for character
        italic: Dist

        // tag: uint2

        /// larger version of the same char
        larger: CharCode option
        // NOTE: not used, set to None
        varChar: VarCharInfo
    }

    type Font = CharInfo list


    /// Actually corresponds to font name
    type FontFamily =
        | RM
        | MI
        | SY
        | EX

    let fontFamilyIdx fontType =
        match fontType with
        | FontFamily.RM -> 0
        | FontFamily.MI -> 1
        | FontFamily.SY -> 2
        | FontFamily.EX -> 3

    let fontFamilyName fontType =
        match fontType with
        | FontFamily.RM -> "cmr"
        | FontFamily.MI -> "cmmi"
        | FontFamily.SY -> "cmsy"
        | FontFamily.EX -> "cmex"


    (** layout style:
     *  Display style: displayed formulae
     *  | Text style:  inline formulae
     *  | Script style:  ‘scripts’ (superscripts and subscripts)
     *  | Script Script style:  scripts of scripts and other subformulae with tiny typesetting
     *)
    type FontStyle =
        | D
        | T
        | S
        | SS

    let fontStyleIdx fontStyle =
        match fontStyle with
        | FontStyle.D -> 0
        | FontStyle.T -> 1
        | FontStyle.S -> 2
        | FontStyle.SS -> 3

    let fontSize (famliy, style) : FontSize =
        match (famliy, style) with
        | (_, FontStyle.D) -> 10
        | (_, FontStyle.T) -> 10
        | (FontFamily.EX, _) -> 10
        | (_, FontStyle.S) -> 7
        | (_, FontStyle.SS) -> 5

    exception NotImplemented of string
    exception CannotHappen
