namespace TeXFormulaLayout

/// Basic binary stream output
module BytesOut =
    open System
    open System.IO

    /// Uninitialized global output stream
    exception NoBinaryOutException


    /// Hold ref to global output stream (`BinaryWriter`).
    let gBinaryWriterRef : BinaryWriter option ref = ref None

    /// Get global output stream.
    let getStream () =
        match gBinaryWriterRef.Value with
        | None    -> raise NoBinaryOutException
        | Some bw -> bw
    /// Close global output stream.
    let private closeStream () =
        match gBinaryWriterRef.Value with
        | None    -> ()
        | Some bw -> bw.Close()

    /// <summary>Open file for output.</summary>
    /// <param name="fileName">Output filename</param>
    let startDviOut fileName =
        closeStream ()
        let fs = File.Open(fileName, FileMode.Create)
        gBinaryWriterRef.Value <- new BinaryWriter(fs) |> Some
    /// Close file.
    let endDviOut () =
        closeStream ()
        gBinaryWriterRef.Value <- None


    /// Output one byte.
    let outByte (b: Byte) =
        let bw = getStream ()
        bw.Write(b)

    /// Get out stream position.
    let outPos () =
        let bw = getStream ()
        bw.BaseStream.Position |> int32
