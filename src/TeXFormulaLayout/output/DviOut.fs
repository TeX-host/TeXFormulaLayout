namespace TeXFormulaLayout

/// Low level DVI instructions output
module DviOut =
    open System
    open TeXFormulaLayout.DviTypes
    open TeXFormulaLayout.FontTypes
    open TeXFormulaLayout.OutHelper

    let setChar (ch: CharCode) =
        if ch < 128 then dvicmd DVICmd.SET1
        dviout ch

    let putChar (ch: CharCode) =
        dvicmd DVICmd.PUT1
        dviout ch

    let rule cmd (a: Dist) (b: Dist) =
        dvicmd cmd
        outNat1 a
        outNat1 b

    let setRule = rule DVICmd.SET_RULE
    let putRule = rule DVICmd.PUT_RULE

    let down = outCmdV DVICmd.X4
    let right = outCmdV DVICmd.SET_RULE

    let push () = dvicmd DVICmd.PUSH
    let pop () = dvicmd DVICmd.POP

    let font f = (f + int32 DVICmd.FNT_NUM_0) |> dviout

    // TODO: distInt
    let int2Dist = id
    let fontDef nr =
        let famSizeVector = []
        let (fam, s) = famSizeVector.[nr]

        let size = int2Dist s
        let fileName = cmName fam + string s

        dvicmd DVICmd.FNT_DEF_1
        dviout nr
        out2Zero 4
        outNat4 size
        outNat4 size
        out2Zero 1
        outString fileName

    let rec fontDefs l =
        match l with
        | [] -> ()
        | h :: t -> fontDef h; fontDefs t

    let bop pageNum prevPos =
        dvicmd DVICmd.BOP
        outNat4 pageNum
        out2Zero 36
        outNat4 prevPos

    let eop () = dvicmd DVICmd.EOP
    let version () = outNat1 2
    let numDen () =
        outNat4 25400000
        outNat4 473628672
    let banner () = outString "Inky's Formula Formatter"

    let pre mag =
        dvicmd DVICmd.PRE
        version ()
        numDen ()
        outNat4 mag
        banner ()

    let rec trailer n =
        match n with
        | 0 -> ()
        | _ -> dviout 223; trailer (n - 1)

    let post mag pageNum prevPos maxLv =
        let maxVSize = int2Dist 10 * 72
        let maxWidth = int2Dist  7 * 72

        dvicmd DVICmd.POST
        outNat4 prevPos
        numDen ()
        outNat4 mag
        outNat4 maxVSize
        outNat4 maxWidth
        outNat2 maxLv
        outNat2 pageNum

    let postpost postPos =
        dvicmd DVICmd.POST_POST
        outNat4 postPos
        version ()
        trailer 3

    let rec tail ownPos = trailer (4 - ownPos % 4)
