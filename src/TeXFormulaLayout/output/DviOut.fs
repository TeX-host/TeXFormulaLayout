namespace TeXFormulaLayout


module Power2Const =
    [<Literal>]
    let Two6 = 64

    [<Literal>]
    let Two7 = 128
    [<Literal>]
    let Two8 = 256

    [<Literal>]
    let Two15 = 32768
    [<Literal>]
    let Two16 = 65536

    [<Literal>]
    let Two23 = 8_388_608
    [<Literal>]
    let Two24 = 16_777_216

    [<Literal>]
    let Two29 = 536_870_912

    /// 2^31 - 1
    [<Literal>]
    let Int32Max = 2_147_483_647


    let inRange lo hi i = lo <= i && i <= hi
    let in1ByteRange = inRange 0 (Two8  - 1)
    let in2ByteRange = inRange 0 (Two16 - 1)
    let in3ByteRange = inRange 0 (Two24 - 1)
    let in4ByteRange = inRange 0 Int32Max

    let inCharRange = in1ByteRange
    let isASCII c = c |> int |> inCharRange


/// String and Int outoput.
/// 封装 outByte 提供字符串和数字的输出接口。
module DviOutHelper =
    open System
    open Power2Const
    open TeXFormulaLayout.BytesOut
    open TeXFormulaLayout.DviTypes

    let outNat1 (i: Int32) =
        i |> byte |> outByte
    let outNat2 n =
        outNat1 (n / Two8)
        outNat1 (n % Two8)
    let outNat3 n =
        outNat1 (n / Two16)
        outNat2 (n % Two16)
    let outNat4 n =
        outNat1 (n / Two24)
        outNat3 (n % Two24)

    let makeNat twoN n =
        if n >= 0 then n
        else n + twoN

    let outCmdN (cmd: DVICmd) n =
        let cmdN (l: Int32) = outNat1 (l + int32 cmd)
        if  abs n >= Two23  then ( cmdN 3;  outNat4 n                 ) else
        if  abs n >= Two15  then ( cmdN 2;  outNat3 (makeNat Two24 n) ) else
        if  abs n >= Two7   then ( cmdN 1;  outNat2 (makeNat Two16 n) ) else
        if      n <> 0      then ( cmdN 0;  outNat1 (makeNat Two8  n) ) else  ()


    /// output N zeros.
    let rec out2Zero n =
        match n with
        | 0 -> ()
        | n -> outNat1 0; out2Zero (n - 1)

    let outChar (c: Char) =
        // Only accept ASCII [0,256], ignore other Unicode range.
        assert isASCII c
        c |> byte |> outByte
    let outStr = String.iter (byte >> outByte)
    let outString s =
        s |> String.length |> outNat1
        outStr s

    let dviout = outNat1
    let dvicmd (cmd: DVICmd) = cmd |> byte |> outByte
    

/// Low level DVI instructions output
module DviOut =
    open TeXFormulaLayout.Distance
    open TeXFormulaLayout.DviTypes
    open TeXFormulaLayout.FontTypes
    open DviOutHelper

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

    let down = outCmdN DVICmd.DOWN1
    let right = outCmdN DVICmd.RIGHT1

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
