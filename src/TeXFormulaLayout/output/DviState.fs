namespace TeXFormulaLayout

/// State information needed for dvi compression
module DviState =
    open System
    open TeXFormulaLayout.FontTypes
    open TeXFormulaLayout.BytesOut

    /// Increase the reference value by `n`
    let incRef (n: int) (r: int ref) = r.Value <- r.Value + n

    let inc = incRef 1
    let dec = incRef -1

    /// `dvi_h`:  horizontal coordinates, from top left -> right.
    let xMove = ref 0
    let resetX () = xMove := 0
    let getX () = !xMove
    let moveX dx = incRef dx xMove

    /// `dvi_v`: vertical coordinates, from upper left -> down.
    let yMove = ref 0
    let resetY () = yMove := 0
    let getY () = !yMove
    let moveY dy = incRef dy yMove

    let NoFont = -1
    /// `dvi_f`: current font.
    ///     is changed only by `FNT_1` and `FNT_NUM` commands
    let actFont = ref NoFont

    let resetFont () = actFont := NoFont

    /// Is the font f the same as the current font (`actFont`)
    let isSameFont = (=) actFont.Value

    let setFont f = actFont := f

    /// List of defined fonts
    let fontList = ref ([]: FontNum list)

    let resetFontList () = fontList := ([]: FontNum list)

    let isFontDefinded f = List.exists ((=) f) !fontList

    let addFont f = fontList := f :: !fontList

    let defindedFonts () = !fontList

    /// `total_pages` The number of pages that have been shipped out
    let pageNum = ref 0
    let actPage () = !pageNum
    let nextPage () = inc pageNum

    /// `last_bop`: location of previous bop in the DVI output
    let oldPos = ref (-1)
    let newPos = ref (-1)
    let prevPos () = !oldPos
    let actPos () = !newPos

    let markPos () =
        oldPos := actPos ()

        newPos := outPos ()

    /// `cur_s`: current depth of output box nesting, initially -1
    let ActLevel = ref 0
    /// `max_push`: deepest nesting of push commands encountered so far
    let MaxLevel = ref 0

    let incLevel () =
        inc ActLevel
        if !ActLevel > !MaxLevel then inc MaxLevel else ()

    let decLevel () = dec ActLevel
    let maxLevel () = !MaxLevel

    let initDviState () =
        resetX ()
        resetY ()
        resetFont ()
        resetFontList ()

        pageNum := 0

        oldPos := -1

        newPos := -1

        ActLevel := 0

        MaxLevel := 0


    (* TODO: The current spacing amounts are given by four numbers w, x, y, and z *)
