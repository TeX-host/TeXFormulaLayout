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
    let private closeStream () =
        match !binWriter with
        | None    -> ()
        | Some bw -> bw.Close()

    /// Open file for output.
    let startDviOut fileName =
        closeStream ()
        let fs = File.Open(fileName, FileMode.Create)
        binWriter := new BinaryWriter(fs) |> Some
    /// Close file.
    let endDviOut () =
        closeStream ()
        binWriter := None


    /// Output one byte.
    let outByte (b: Byte) =
        let bw = getStream ()
        bw.Write(b)

    /// Get out stream position.
    let outPos () =
        let bw = getStream ()
        bw.BaseStream.Position |> int32
