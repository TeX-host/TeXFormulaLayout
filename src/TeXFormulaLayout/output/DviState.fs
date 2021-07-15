namespace TeXFormulaLayout
open System

/// Basic binary stream output
module DviState =
    let incN (n: Int32) (r: Int32 ref) = r := !r + n
    let inc = incN  1
    let dec = incN -1

    let xMove = ref 0
    let getX   () = !xMove
    let moveX  dx = incN dx xMove
    let resetX () = xMove := 0

    let yMove = ref 0
    let getY   () = !yMove
    let moveY  dy = incN dy yMove
    let resetY () = yMove := 0

    let NoFont  = -1
    let actFont = ref NoFont
    let isSameFont f = !actFont = f
    let setFont    f = actFont := f
    let resetFont () = actFont := NoFont

