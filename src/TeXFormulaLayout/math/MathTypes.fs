namespace TeXFormulaLayout

module MathTypes =
    open Distance
    open FontTypes
    open BoxTypes

    type OpKind =
        | Ord
        | Op
        | Bin
        | Rel
        | Open
        | Close
        | Punct
        | Inner
        | NoneOp

    type Limits =
        | Default
        | Yes
        | No

    type Space =
        | SKern of Dist
        | SGlue of GlueSpec

    type MathSpace = {
        isMu: bool
        always: bool
        entry: Space
    }

    type Noad =
        | MathChar of OpKind * FontFamily * CharCode
        | Radical of Delim * MList
        | Accent of FontFamily * CharCode * MList
        | VCenter of MList
        | Overline of MList
        | Underline of MList
        | GenFraction of GenFraction
        | LeftRight of Delim * MList * Delim
        | Script of Script
        | BigOp of Limits * Script
        | SubBox of box
        | MList of MList
        | Kind of OpKind * MList
        (* `MRule' and some other guys omitted *)
        | MPen of Penalty
        | MSpace of MathSpace
        | Style of FontStyle
        | Choice of (FontStyle -> MList)

    and GenFraction = {
        num: MList
        den: MList
        thickness: Dist option
        (* no thickness -> default value *)
        left: Delim
        right: Delim
    }

    and Script = {
        nucleus: MList
        supOpt: MList option
        subOpt: MList option
    }

    and MList = Noad list
