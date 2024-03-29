module TeXFormulaLayout.Tests.BytesOut

open Expecto
open System.IO
open TeXFormulaLayout.BytesOut


// ---- 4test
let memStream: MemoryStream option ref = ref None

let getMemByteArray () =
    let mem = !memStream |> Option.get

    mem.ToArray()

/// [TestOnly] Use `MemoryStream` as DVI file.
let startMemDvi () =
    // Close old Stream
    match gBinaryWriterRef.Value with
    | None -> ()
    | Some bw -> bw.Close()
    // Set new Stream
    let mem = new MemoryStream()
    memStream.Value <- Some mem

    gBinaryWriterRef.Value <- new BinaryWriter(mem) |> Some

/// [TestOnly] Close `MemoryStream`.
let endMemDvi = endOut


[<Tests>]
let tests =
    testList "BinaryWriter" [
        testCase "binWriter (init)"
        <| fun _ -> Expect.equal gBinaryWriterRef (ref None) "Bad init state!"
        testCase "getStream raise"
        <| fun _ ->
            Expect.throwsT<BinaryWriterUninitializedException>
                (getStream >> ignore)
                "Not raise NoBinaryOutException"

        test "startMemDvi" {
            startMemDvi ()

            Expect.equal
                (getStream ())
                (!gBinaryWriterRef |> Option.get)
                "Bad getStream func with startMemDvi"
        }
        test "endMemDvi" {
            endMemDvi ()

            Expect.throwsT<BinaryWriterUninitializedException>
                (getStream >> ignore)
                "Bad getStream func with endMemDvi"
        }

        test "outByte" {
            startMemDvi ()
            outByte 123uy
            Expect.equal (getMemByteArray ()) [| 123uy |] "Bad getStream func with startMemDvi"
            endMemDvi ()
        }
        test "outPos" {
            startMemDvi ()
            Expect.equal (outPos ()) 0 "Bad outPos start 0"
            outByte 123uy
            Expect.equal (outPos ()) 1 "Bad outPos +1"
            endMemDvi ()
        }
    ]
    |> testSequenced // test init first
