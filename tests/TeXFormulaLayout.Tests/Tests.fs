namespace TeXFormulaLayout.Tests

open Expecto
open TeXFormulaLayout
open TeXFormulaLayout.FontTypes
open TeXFormulaLayout.LoadFont

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

module OutputTests =
    open TeXFormulaLayout.BytesOut

    [<Tests>]
    let bytesOut =
        testList "BinaryWriter"
            [ testCase "init state" <| fun _ ->
                Expect.equal binWriter (ref None) "Bad init state!"
            ]
