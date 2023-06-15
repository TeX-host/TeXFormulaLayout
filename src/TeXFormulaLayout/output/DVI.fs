namespace TeXFormulaLayout

/// High level DVI func
module Dvi =
    open TeXFormulaLayout.FontTypes
    open TeXFormulaLayout.DviOut
    open TeXFormulaLayout.DviState

    /// Define font.
    let FontDef f =
        if isFontDefinded f then
            ()
        else
            fontDef f
            addFont f

    /// Define and set font.
    let Font f =
        if isSameFont f then
            ()
        else
            FontDef f
            font f
            setFont f

    /// Reset (h, v) to Top left (0, 0)
    let private resetMove () =
        resetX ()
        resetY ()

    /// Move a step (h, v), and reset it.
    let Move () =
        right (getX ())
        down (getY ())
        resetMove ()

    let private DoChar (charFunc: CharCode -> unit) (font, ch) =
        Font font
        Move()
        charFunc ch

    let SetChar = DoChar setChar
    let PutChar = DoChar putChar

    let SetRule =
        Move()
        setRule

    let PutRule =
        Move()
        putRule

    let Right dx = moveX dx
    let Left dx = moveX -dx
    let Down dy = moveY dy
    let Up dy = moveY -dy

    let Push () =
        Move()
        push ()
        incLevel ()

    let Pop () =
        resetMove ()
        pop ()
        decLevel ()

    let Bop () =
        markPos ()
        nextPage ()
        bop (actPage ()) (prevPos ())

    let Eop () =
        resetMove ()
        resetFont ()
        eop ()

    let Pre mag =
        initDviState ()
        pre mag

    let Post mag =
        markPos ()
        post mag (actPage (), prevPos (), maxLevel ())
        fontDefs (defindedFonts ())
        postpost (actPos ())
        markPos ()
        tailFill (actPos ())

    /// Skip distance horizontally, dist can be negative
    let HSkip = right
    /// Skip distance vertically, dist can be negative
    let VSkip = down

    /// Draw a a solid black rectangle
    let BlackBox width height depth =
        VSkip depth
        putRule (height + depth, width)
        VSkip -depth
