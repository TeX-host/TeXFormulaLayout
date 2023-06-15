namespace TeXFormulaLayout

/// Functions to deal with distances.
///
/// [p39#104] Dist —— scaled point (sp) as a unit equal to 2^(-16) printer’s points (pt)
///
///     1<sp> = One: Dist = 2^(-16)<pt>
module Distance =
    open System
    type Dist = int

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
    let distDiv (num, den: int) = (int2Dist num) / den

    /// decimal fraction to Dist
    let real2Dist r = floatMul (r, One)
