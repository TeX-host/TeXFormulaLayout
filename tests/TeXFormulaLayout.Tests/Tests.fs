namespace TeXFormulaLayout.Tests

open Expecto
open TeXFormulaLayout
open TeXFormulaLayout.FontTypes
open TeXFormulaLayout.LoadFont

module OutputTests =
    open System.IO
    open TeXFormulaLayout.BytesOut

    // ---- 4test
    let memStream : MemoryStream option ref = ref None
    let getMemByteArray () =
        let mem = !memStream |> Option.get
        mem.ToArray()

    let startMemDvi () =
        match !binWriter with
        | None    -> ()
        | Some bw -> bw.Close()
        let mem = new MemoryStream()
        memStream := Some mem
        binWriter := new BinaryWriter(mem) |> Some
    let endMemDvi = endDviOut


    [<Tests>]
    let bytesOut =
        testList "BinaryWriter" [
            testCase "binWriter (init)" <| fun _ ->
                Expect.equal binWriter (ref None) "Bad init state!"
            testCase "getStream raise" <| fun _ ->
                Expect.throwsT<NoBinaryOutException> (getStream >> ignore) "Not raise NoBinaryOutException"

            test "startMemDvi" {
                startMemDvi ()
                Expect.equal (getStream ()) (!binWriter |> Option.get) "Bad getStream func with startMemDvi"
            }
            test "endMemDvi" {
                endMemDvi ()
                Expect.throwsT<NoBinaryOutException> (getStream >> ignore) "Bad getStream func with endMemDvi"
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
        ] |> testSequenced  // test init first


module SayTests =

    [<Tests>]
    let tests =
        testList "samples"
            [ testCase "Say nothing" <| fun _ ->
                let subject = Say.nothing()
                Expect.equal subject () "Not an absolute unit"
              testCase "Say hello all" <| fun _ ->
                  let subject = Say.hello "all"
                  Expect.equal subject "Hello all" "You didn't say hello" ]

    let TS10Font = [
        { width = 5; height = 0; depth = 12; italic = 0; larger = Some 16;
            varChar = { top = None; bot = None; rep = None } };
        { width = 4; height = 0; depth = 12; italic = 0; larger = None;
            varChar = { top = Some 105; bot = Some 121; rep = Some 63 } }
    ]


    let fake_LoadFont (famliyName: string, fontSize: FontSize) =
        // some work around
        let fakeTestFontDir = System.AppContext.BaseDirectory +/ ".." +/ "fonts"
        let fontDir =
            match System.IO.Directory.Exists(fontDirPath) with
            | true -> fontDirPath
            | false -> fakeTestFontDir
        getFontPath fontDir famliyName fontSize
            |> (readFontLines fontSize)

    [<Tests>]
    let fonts =
        testList "LoadFont test"
            [ testCase "read font info" <| fun _ ->
                let font = fake_LoadFont("TS", 10)
                Expect.equal font TS10Font "Bad font!"
            ]
