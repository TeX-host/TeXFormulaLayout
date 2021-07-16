namespace TeXFormulaLayout

/// High level DVI func
module DVI =
    open System
    open TeXFormulaLayout.FontTypes
    open TeXFormulaLayout.OutDVI
    open TeXFormulaLayout.DviState

    let FontDef f =
        if isFontDefinded f then ()
        else fontDef f; addFont f
    let Font f =
        if isSameFont f then ()
        else FontDef f; font f; setFont f

    let resetMove () =
        resetX ()
        resetY ()
    let Move () =
        getX () |> right
        getY () |> down
        resetMove ()

    let DoChar (charFunc: CharCode -> unit) font ch =
        Font font
        Move ()
        charFunc ch
    let SetChar = DoChar setChar
    let PutChar = DoChar putChar
    let SetRule = Move (); setRule
    let PutRule = Move (); putRule

    let Right dx = moveX  dx
    let Left  dx = moveX -dx
    let Down  dy = moveY  dy
    let Up    dy = moveY -dy

    let Push () =      Move (); push (); incLevel ()
    let Pop  () = resetMove (); pop  (); decLevel ()

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
        post mag (actPage ()) (prevPos ()) (maxLevel ())
        fontDefs (defindedFonts ())
        postpost (actPos ())
        markPos ()
        tail (actPage ())
