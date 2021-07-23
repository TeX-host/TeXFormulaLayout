namespace TeXFormulaLayout.Tests
open Expecto


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
        

module Power2ConstTests =
    open TeXFormulaLayout.Power2Const

    let twoN = pown 2
    [<Tests>]
    let tests = testList "Power2Const" [
        test "all const" {
            Expect.equal Two6 (twoN 6) "2^6"

            Expect.equal Two7 (twoN 7) "2^7"
            Expect.equal Two8 (twoN 8) "2^8"

            Expect.equal Two15 (twoN 15) "2^15"
            Expect.equal Two16 (twoN 16) "2^16"

            Expect.equal Two23 (twoN 23) "2^23"
            Expect.equal Two24 (twoN 24) "2^24"

            Expect.equal Two29 (twoN 29) "2^29"
        }
    ]


module DviOutHelperTests =
    open OutputTests
    open TeXFormulaLayout.DviOutHelper
    open TeXFormulaLayout.DviTypes
    open TeXFormulaLayout.Power2Const

    [<Tests>]
    let tests =
        testList "DviOutHelper" [
            test "outNat1" {
                startMemDvi ()
                outNat1 0
                Expect.equal (getMemByteArray ()) [| 0uy |] "0"

                startMemDvi ()
                outNat1 255
                Expect.equal (getMemByteArray ()) [| 255uy |] "255"

                startMemDvi ()
                outNat1 0
                outNat1 255
                outNat1 127
                outNat1 128
                Expect.equal (getMemByteArray ()) [| 0uy; 255uy; 127uy; 128uy |] "[0,255,127,128]"
                endMemDvi ()
            }

            test "outNat2~4" {
                startMemDvi ()
                outNat2 Two7
                Expect.equal (getMemByteArray ()) [| 0uy; 128uy |] "2^7"
                startMemDvi ()
                outNat2 (Two15 - 1)
                Expect.equal (getMemByteArray ()) [| 127uy; 255uy |] "2^15-1"

                startMemDvi ()
                outNat3 Two15
                Expect.equal (getMemByteArray ()) [| 0uy; 128uy; 0uy |] "2^15"
                startMemDvi ()
                outNat3 (Two23 - 1)
                Expect.equal (getMemByteArray ()) [| 127uy; 255uy; 255uy |] "2^23-1"

                startMemDvi ()
                outNat4 Two23
                Expect.equal (getMemByteArray ()) [| 0uy; 128uy; 0uy; 0uy |] "2^23"
                startMemDvi ()
                outNat4 Int32Max
                Expect.equal (getMemByteArray ()) [| 127uy; 255uy; 255uy; 255uy |] "2^31-1"
                endMemDvi ()
            }

            test "out2Zero" {
                startMemDvi ()
                out2Zero 0
                Expect.equal (getMemByteArray ()) [| |] "[]"

                startMemDvi ()
                out2Zero 1
                Expect.equal (getMemByteArray ()) [| 0uy |] "[0]"

                startMemDvi ()
                out2Zero 3
                Expect.equal (getMemByteArray ()) [| 0uy; 0uy; 0uy; |] "[0,0,0]"
                endMemDvi ()
            }

            test "outStr" {
                startMemDvi ()
                outStr ""
                Expect.equal (getMemByteArray ()) [| |] "[]"

                startMemDvi ()
                outStr "abc"
                Expect.equal (getMemByteArray ()) [| 97uy; 98uy; 99uy; |] "[97,98,99]"

                startMemDvi ()
                outStr "123"
                Expect.equal (getMemByteArray ()) [| 49uy; 50uy; 51uy; |] "[49,50,51]"
                endMemDvi ()
            }

            test "outString" {
                startMemDvi ()
                outString ""
                Expect.equal (getMemByteArray ()) [| 0uy |] "[0]"

                startMemDvi ()
                outString "abc"
                Expect.equal (getMemByteArray ()) [| 3uy; 97uy; 98uy; 99uy; |] "[3,97,98,99]"

                startMemDvi ()
                outString "123"
                Expect.equal (getMemByteArray ()) [| 3uy; 49uy; 50uy; 51uy; |] "[3,49,50,51]"
                endMemDvi ()
            }

            test "dvicmd" {
                startMemDvi ()
                dvicmd DVICmd.SET_CHAR_0
                Expect.equal (getMemByteArray ()) [| 0uy |] "[0]"

                startMemDvi ()
                dvicmd DVICmd.POST_POST
                Expect.equal (getMemByteArray ()) [| 249uy |] "[249]"
                endMemDvi ()
            }
        ] |> testSequenced


module SayTests =
    open TeXFormulaLayout
    open TeXFormulaLayout.FontTypes
    open TeXFormulaLayout.LoadFont

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
