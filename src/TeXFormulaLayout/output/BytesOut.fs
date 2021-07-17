namespace TeXFormulaLayout

/// Basic binary stream output
module BytesOut =
    open System
    open System.IO

    /// Types
    exception NoBinaryOutException


    /// Hold `BinaryWriter` ref
    let binWriter : BinaryWriter option ref = ref None
    let getStream () =
        match !binWriter with
        | None    -> raise NoBinaryOutException
        | Some bw -> bw


    let openBinOut fileName =
        let fs = File.Open(fileName, FileMode.Create)
        new BinaryWriter(fs)
    let closeBinOut (bw: BinaryWriter) = bw.Close()

    /// Open file for output.
    let startDviOut fileName =
        match !binWriter with
        | None    -> ()
        | Some bw -> closeBinOut bw
        binWriter := openBinOut fileName |> Some
    /// Close file.
    let endDviOut () = getStream () |> closeBinOut


    /// Output one byte.
    let outByte (b: Byte) =
        let bw = getStream ()
        bw.Write(b)

    /// Get out stream position.
    let outPos () =
        let bw = getStream ()
        bw.BaseStream.Position |> int32
