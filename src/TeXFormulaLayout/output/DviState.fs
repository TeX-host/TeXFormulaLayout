namespace TeXFormulaLayout

/// State information needed for dvi compression
module DviState =
    open System
    open TeXFormulaLayout.FontTypes
    open TeXFormulaLayout.BytesOut

    /// Increase the reference value by `n`
    let private incRef (n: int) (r: int ref) = r.Value <- r.Value + n
    let private inc = incRef 1
    let private dec = incRef -1

    /// `dvi_h`:  horizontal coordinates, from top left -> right.
    let xMove = ref 0
    let resetX () = xMove.Value <- 0
    let getX () = xMove.Value
    let moveX dx = incRef dx xMove

    /// `dvi_v`: vertical coordinates, from upper left -> down.
    let yMove = ref 0
    let resetY () = yMove.Value <- 0
    let getY () = yMove.Value
    let moveY dy = incRef dy yMove

    let NoFont = -1
    /// `dvi_f`: current font.
    ///     is changed only by `FNT_1` and `FNT_NUM` commands
    let actFont = ref NoFont
    let resetFont () = actFont.Value <- NoFont
    /// Is the font f the same as the current font (`actFont`)
    let isSameFont = (=) actFont.Value
    let setFont f = actFont.Value <- f

    /// List of defined fonts
    let fontList = ref ([]: FontNum list)
    let resetFontList () = fontList.Value <- ([]: FontNum list)
    let defindedFonts () = fontList.Value
    let isFontDefinded f = List.exists ((=) f) fontList.Value
    let addFont f = fontList.Value <- f :: fontList.Value

    /// `total_pages` The number of pages that have been shipped out
    let pageNum = ref 0
    let actPage () = pageNum.Value
    let nextPage () = inc pageNum

    /// `last_bop`: location of previous bop in the DVI output
    let oldPos = ref (-1)
    let newPos = ref (-1)
    let prevPos () = oldPos.Value
    let actPos () = newPos.Value

    let markPos () =
        oldPos.Value <- actPos ()
        newPos.Value <- outPos ()

    /// `cur_s`: current depth of output box nesting, initially -1
    let ActLevel = ref 0
    /// `max_push`: deepest nesting of push commands encountered so far
    let MaxLevel = ref 0

    let incLevel () =
        inc ActLevel
        if ActLevel.Value > MaxLevel.Value then inc MaxLevel else ()

    let decLevel () = dec ActLevel
    let maxLevel () = MaxLevel.Value

    let initDviState () =
        resetX ()
        resetY ()
        resetFont ()
        resetFontList ()

        pageNum.Value <- 0
        oldPos.Value <- -1
        newPos.Value <- -1
        ActLevel.Value <- 0
        MaxLevel.Value <- 0


(* TODO: The current spacing amounts are given by four numbers w, x, y, and z *)
