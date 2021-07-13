namespace TeXFormulaLayout

/// Basic binary stream output
module BytesOut =
    open System
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
