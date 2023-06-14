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

    (** -- Output 1~4 bytes -- **)
    /// Output 1 byte.
    let outNat1 n =
        // NOTE: byte(-1) == byte(255);  byte(256) == byte(0)
        n |> byte |> outByte
    /// Output 2 byte.
    let outNat2 n =
        outNat1 (n / Two8)
        outNat1 (n % Two8)
    /// Output 3 byte.
    let outNat3 n =
        outNat1 (n / Two16)
        outNat2 (n % Two16)
    /// Output 4 byte.
    let outNat4 n =
        outNat1 (n / Two24)
        outNat3 (n % Two24)


    /// Output value n times
    let rec outValNTimes value n =
        match n with
        | 0 -> ()
        | n -> outNat1 value; outValNTimes value (n - 1)

    /// output N zeros.
    let outZerosN = outValNTimes 0


    /// Output one `Char`
    let outChar (c: Char) =
        // Only accept ASCII [0,256], ignore other Unicode range.
        assert isASCII c
        c |> byte |> outByte
    /// output string in bytes
    let private outStr = String.iter outChar
    /// Output a string with its length. (len[1], str[len])
    let outString s =
        let len = String.length s
        assert in1ByteRange len
        // output string length
        outNat1 len
        // output raw string
        outStr s


    /// Output 1 `int`.
    let dviout = outNat1
    /// Output only 1 `DviCmd`.
    let dvicmd (cmd: DviCmd) = cmd |> byte |> outByte
    /// Output 1 `DviCmd` with 1 `int` arg.
    let dviCmdArg1 (cmd: DviCmd) arg1 =
        dvicmd cmd
        dviout arg1


    let private makeNat twoN n =
        if n >= 0 then n
        else n + twoN
    /// Auto choose instruction `DviCmd_i`, with i in [1, 4]
    let outCmdN (cmd: DviCmd) n =
        /// Output DviCmd with offset i
        let cmdN i = outNat1 (i + int cmd)
        // TODO: 使用更精确的范围 [-128, 128)
        if  abs n >= Two23  then ( cmdN 3;  outNat4 n                 ) else
        if  abs n >= Two15  then ( cmdN 2;  outNat3 (makeNat Two24 n) ) else
        if  abs n >= Two7   then ( cmdN 1;  outNat2 (makeNat Two16 n) ) else
        if      n <> 0      then ( cmdN 0;  outNat1 (makeNat Two8  n) ) else  ()
        /// 处理 n == 0


/// Low level DVI instructions output
module DviOut =
    open Power2Const
    open TeXFormulaLayout.Distance
    open TeXFormulaLayout.DviTypes
    open TeXFormulaLayout.FontTypes
    open DviOutHelper

    /// Check input char range.
    let private (|SET_CHAR_i|SET1|INVALID_RANGE|) ch =
        match ch with
        // [  0, 128) => SET_CHAR_i
        | c when 0 <= c && c < 128 -> SET_CHAR_i
        // [128, 256) => SET1 i
        | c when 128 <= c && c < 256 -> SET1
        // (-Inf, 0) && [256, Inf)
        | _ -> INVALID_RANGE

    /// SetChar1: Typeset 1 character from font,
    ///     then increase `h` by the width of that character.
    let setChar (ch: CharCode) =
        match ch with
        | SET_CHAR_i    -> dviout ch
        | SET1          -> dviCmdArg1 DviCmd.SET1 ch
        | INVALID_RANGE -> invalidArg (nameof ch) "Char not in [0, 256)"

    /// PutChar1, like `SetChar1`, but not move.
    let putChar = dviCmdArg1 DviCmd.PUT1


    let private rule cmd (a: Dist) (b: Dist) =
        dvicmd cmd
        outNat4 a
        outNat4 b
    /// Typeset a solid black rectangle of height a and width b,
    ///     with its bottom left corner at (h, v), and move.
    let setRule = rule DviCmd.SET_RULE
    /// Typeset a solid black rectangle of height a and width b,
    ///     with its bottom left corner at (h, v), but not move.
    let putRule = rule DviCmd.PUT_RULE

    (**
        begin_of_page      1 ubyte     (BOP)
        page_nr            4 sbytes    (page number)
        do_be_do          36 bytes     (filler ????)
        prev_page_offset   4 sbytes    (offset in file where previous page starts, -1 for none)

        xref: DVI.format
     **)
    /// Beginning of a page.
    let bop pageNum prevPos =
        // TODO: clear state, ref [tex82+p215]
        dvicmd DviCmd.BOP
        // c0[4]
        outNat4 pageNum
        // c1[4] ~ c9[4]
        outZerosN (4 * 9)
        // p[4]
        outNat4 prevPos
    /// End of page.
    let eop () = dvicmd DviCmd.EOP
    /// <summary>
    /// Push the current state (h, v, w, x, y, z) onto the top of the stack;
    ///     without change any of these values.
    /// </summary>
    /// Note that f is not pushed.
    let push () = dvicmd DviCmd.PUSH
    /// Pop the top state off of the stack and re-assign them.
    let pop () = dvicmd DviCmd.POP

    /// Move right b[1..4] units., Set h ← h+b
    let right = outCmdN DviCmd.RIGHT1
    /// Move down a[1..4] units. Set v ← v+a
    let down = outCmdN DviCmd.DOWN1

    /// Check input font range.
    let private (|FNT_NUM|FNT_1|FNT_i|INVALID_FNT|) fnt =
        match fnt with
        // [  0, 64) => FNT_NUM_i
        | f when 0 <= f && f < 64 -> FNT_NUM
        // [64, 256) => FNT_1
        | f when 64 <= f && f < 256 -> FNT_1
        // [64, 2^32) => FNT_i, i in [2，4]
        | f when 256 <= f && f <= Int32Max -> FNT_i
        // (-Inf, 0) && [2^32, Inf)
        | _ -> INVALID_FNT
    /// Set font `f`
    let font f =
        match f with
        | FNT_NUM -> dviout (f + int DviCmd.FNT_NUM_0)
        | FNT_1 -> dviCmdArg1 DviCmd.FNT1 f
        | FNT_i | INVALID_FNT -> invalidArg (nameof f) "Font number not in [0, 256)"

    // TODO: distInt
    let int2Dist = id
    (* [tex#588p218]
        fnt_defi (1 <= i <= 4); k[i], c[4], s[4], d[4], a[1], l[1], n[a+l]

                  1,2,3,4 ubytes    TeXfontnumber for FNTDEF1 .. FNTDEF4
                        4 ubytes    checksum
                        4 ubytes    scale
                        4 ubytes    design size
                        1 byte      deflen1
                        1 byte      deflen2
        deflen1 + deflen2 bytes     fontname.

        xref: DVI.format
    *)
    /// Define font k.
    let fontDef nr =
        /// TODO: global var
        let famSizeVector = []
        let (fam, s) = famSizeVector.[nr]

        /// TODO: 添加字体可用性检查
        let size = int2Dist s
        let fileName = cmName fam + string s

        dvicmd DviCmd.FNT_DEF_1
        // k[1]: font number
        dviout nr
        // c[4]: checksum
        outZerosN 4
        // s[4]: scale
        outNat4 size
        // d[4]: design size
        outNat4 size
        // a[1]: deflen1
        outZerosN 1
        // l[1]: name length
        // n[a+l]: font name
        outString fileName
    /// Define a list of font k[].
    let rec fontDefs l =
        match l with
        |   []   -> ()
        | h :: t -> fontDef h; fontDefs t


    (** -- help func for `pre` -- **)
    /// DVI format version (`id_byte` in TeX): i[1]
    let private version () = outNat1 2
    /// num[4]/den[4] = 25400000/473628672
    let private numDen () =
        // 254cm * 10e5
        outNat4 25_400_000
        // 7227pt * 2e16sp
        outNat4 473_628_672
    /// print banner: k[1], x[k]
    let banner () = outString "Inky's Formula Formatter"

    (* [tex#587p218]  PRE: i[1];  num[4], den[4];  mag[4];  k[1], x[k]

        version_id         1 ubyte     (should be version 2)
        numerator          4 ubytes    (numerater must equal the one in postamble)
        denominator        4 ubytes    (denominator must equal the one in postamble)
        magnification      4 ubytes    (magnification must equal the one in postamble)
        id_len             1 ubyte     (lenght of identification string)
        id_string     id_len ubytes    (identification string)

        xref: DVI.format
     *)
    /// preamble
    let pre mag =
        dvicmd DviCmd.PRE
        version ()
        numDen ()
        outNat4 mag
        banner ()

    (* [tex#590p219]  POST: p[4], num[4], den[4], mag[4], l[4], u[4], s[2], t[2];

        last_page_offset   4 sbytes    (offset in file where last page starts)
        numerator          4 ubytes    (numerater must equal the one in preamble)
        denominator        4 ubytes    (denominator must equal the one in preamble)
        magnification      4 ubytes    (magnification must equal the one in preamble)
        max_page_height    4 ubytes    (maximum page height)
        max_page_width     4 ubytes    (maximum page width)
        max_stack          2 ubytes    (maximum stack depth needed)
        total_pages        2 ubytes    (number of pages in file)

        xref: DVI.format
     *)
    let post mag (pageNum, prevPos, maxLv) =
        let maxVSize = int2Dist 10 * 72
        let maxWidth = int2Dist  7 * 72

        dvicmd DviCmd.POST
        outNat4 prevPos
        numDen ()
        outNat4 mag
        outNat4 maxVSize
        outNat4 maxWidth
        outNat2 maxLv
        outNat2 pageNum


    (** -- help func for `postpost` -- **)
    /// Output N 233
    let private trailer n = outValNTimes 233
    (* [tex-p220#590]  post_post; q[4], i[1]; 223's

        postamble_offset   4 sbytes    (offset in file where postamble starts)
        version_id         1 ubyte     (should be version 2)
        trailer         >= 4 ubytes    (TRAILER)

        xref: DVI.format
     *)
    /// signifies the end of the font definitions
    let postpost postPos =
        dvicmd DviCmd.POST_POST
        // a pointer to the post command that started the postamble
        outNat4 postPos
        version ()
        // NOTE: output more 233 in `tail`
        trailer 3

    /// At least out one 233, Make final ownPos is a multiple of four bytes
    let tailFill ownPos = trailer (4 - ownPos % 4)
