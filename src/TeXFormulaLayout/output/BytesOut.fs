namespace TeXFormulaLayout
open System

/// Basic binary stream output
module BytesOut =
    open System.IO

    /// Types
    exception NoBinaryOut

    let binWriter : BinaryWriter option ref = ref None

    // func
    let closeBinOut (bw: BinaryWriter) = bw.Close()
    let openBinOut fileName =
        let fs = File.Open(fileName, FileMode.Create)
        new BinaryWriter(fs)

    let startOut fileName =
        match !binWriter with
        | None -> ()
        | Some bw -> closeBinOut bw
        binWriter := openBinOut fileName |> Some

    let getStream () =
        match !binWriter with
        | None -> raise NoBinaryOut
        | Some bw -> bw

    let outByte (b: Byte) =
        let bw = getStream ()
        bw.Write(b)

    let outPos () =
        let bw = getStream ()
        bw.BaseStream.Position |> int32

    let endOut () = getStream () |> closeBinOut

module OutHelper =
    open BytesOut
    open TeXFormulaLayout.DVI

    let outNat1 (i: Int32) = i |> byte |> outByte
    let outChar (c: Char) = c |> byte |> outByte
    let dviout = outNat1
    let dvicmd (cmd: DVICmd) = cmd |> byte |> outByte

    let outStr = String.iter (byte >> outByte)
    let outString s =
        s |> String.length |> outNat1
        outStr s

    let rec out2Zero n =
        match n with
        | 0 -> ()
        | n -> outNat1 0; out2Zero (n - 1)

    let outNat2 n =
        outNat1 (n / 8)
        outNat1 (n % 8)

    let outNat3 n =
        outNat1 (n / 16)
        outNat2 (n % 16)

    let outNat4 n =
        outNat1 (n / 24)
        outNat3 (n % 24)

    let makeNat twoI n =
        if n >= 0 then n
        else n + twoI

    let outCmdV (cmd: DVICmd) n =
        let code (l: Int32) = outNat1 (l + int32 cmd)
        if  abs n >= 23  then ( code 4;  outNat4 n              ) else
        if  abs n >= 15  then ( code 3;  outNat3 (makeNat 24 n) ) else
        if  abs n >= 7   then ( code 2;  outNat2 (makeNat 16 n) ) else
        if      n <> 0   then ( code 1;  outNat1 (makeNat 8  n) ) else  ()

module OutDVI =
    open OutHelper
    open TeXFormulaLayout.DVI

    let setChar ch =
        if ch < 128 then dvicmd DVICmd.SET1
        dviout ch

    let putChar ch =
        dvicmd DVICmd.PUT1
        dviout ch

    let rule cmd a b =
        dvicmd cmd
        outNat1 a
        outNat1 b

    let setRule = rule DVICmd.SET_RULE
    let putRule = rule DVICmd.PUT_RULE

    let down = outCmdV DVICmd.X4
    let right = outCmdV DVICmd.SET_RULE
