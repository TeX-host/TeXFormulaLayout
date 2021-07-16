namespace TeXFormulaLayout

/// Functions to deal with distances.
///
/// [p39#104] Dist —— scaled point (sp) as a unit equal to 2^(-16) printer’s points (pt)
///
///     1<sp> = One: Dist = 2^(-16)<pt>
module Distance =
    open System
    type Dist = Int32

    /// Power 2
    let pow2 = pown 2
    let distPow2 : (int -> Dist) = pow2

    /// 2^0
    [<Literal>]
    let Zero: Dist = 0
    /// 2^16
    [<Literal>]
    let One: Dist = 65536

    let half (d: Dist) = d / 2
    let floatMul (f: float) (d: Int32) : Dist = round (f * float d) |> int32
    let int2Dist (i: Int32) : Dist = i * One
    let distDiv num (den: Int32) = (int2Dist num) / den
    let real2Dist (r: float) = floatMul r One
