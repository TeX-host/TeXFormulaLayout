namespace TeXFormulaLayout

/// State information needed for dvi compression
module DviState =
    open System
    open TeXFormulaLayout.FontTypes
    open TeXFormulaLayout.BytesOut

    let incN (n: Int32) (r: Int32 ref) = r := !r + n
    let inc = incN  1
    let dec = incN -1

    let xMove = ref 0
    let resetX () = xMove := 0
    let getX   () = !xMove
    let moveX  dx = incN dx xMove

    let yMove = ref 0
    let resetY () = yMove := 0
    let getY   () = !yMove
    let moveY  dy = incN dy yMove

    let NoFont  = -1
    let actFont = ref NoFont
    let resetFont () = actFont := NoFont
    let isSameFont f = !actFont = f
    let setFont    f = actFont := f

    let fontList = ref ([]: FontNum list)
    let resetFontList () = fontList := ([]: FontNum list)
    let isFontDefinded f = List.exists ((=) f) !fontList
    let addFont f = fontList := f :: !fontList
    let defindedFonts () = !fontList

    let pageNum = ref 0
    let actPage  () = !pageNum
    let nextPage () = inc pageNum

    let oldPos = ref (-1)
    let newPos = ref (-1)
    let prevPos () = !oldPos
    let actPos  () = !newPos
    let markPos () =
        oldPos := actPos ()
        newPos := outPos ()

    let ActLevel = ref 0
    let MaxLevel = ref 0
    let incLevel () =
        inc ActLevel
        if !ActLevel > !MaxLevel then inc MaxLevel
        else ()
    let decLevel () = dec ActLevel
    let maxLevel () = !MaxLevel

    let initDviState () =
        resetX ()
        resetY ()
        resetFont ()
        resetFontList ()
        pageNum  := 0
        oldPos   := -1
        newPos   := -1
        ActLevel := 0
        MaxLevel := 0
