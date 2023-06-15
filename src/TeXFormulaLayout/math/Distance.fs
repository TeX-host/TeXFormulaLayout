namespace TeXFormulaLayout

/// Functions to deal with distances.
///
/// [p39#104] Dist —— scaled point (sp) as a unit equal to 2^(-16) printer’s points (pt)
///
///     1<sp> = One: Dist = 2^(-16)<pt>
module Distance =
    type Dist = BasicTypes.BaseIntType

    /// Power 2
    let pow2 = pown 2
    let distPow2: (int -> Dist) = pow2

    /// 2^0
    [<Literal>]
    let Zero: Dist = 0

    /// 2^16
    [<Literal>]
    let One: Dist = 65536


    let half (d: Dist) = d / 2

    /// multiply Dist with real factor
    let floatMul (f: float, d: Dist) : Dist = round (f * float d) |> int

    /// int to Dist
    let int2Dist (i: int) : Dist = i * One

    /// fraction to Dist
    let frac2Dist (num: int, den: int) : Dist = (num * One) / den

    /// decimal fraction to Dist
    let float2Dist r = floatMul (r, One)
