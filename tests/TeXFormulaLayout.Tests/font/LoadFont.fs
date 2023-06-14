module TeXFormulaLayout.Tests.LoadFont

open Expecto
open TeXFormulaLayout.FontTypes
open TeXFormulaLayout.LoadFont


let TS10Font = [
    {
        width = 5
        height = 0
        depth = 12
        italic = 0
        larger = Some 16
        varChar = { top = None; bot = None; rep = None }
    }
    {
        width = 4
        height = 0
        depth = 12
        italic = 0
        larger = None
        varChar = {
            top = Some 105
            bot = Some 121
            rep = Some 63
        }
    }
]

let fake_LoadFont (famliyName: string, fontSize: FontSize) =
    // some work around
    let fakeTestFontDir =
        System.AppContext.BaseDirectory
        +/ ".."
        +/ "fonts"

    let fontDir =
        match System.IO.Directory.Exists(fontDirPath) with
        | true -> fontDirPath
        | false -> fakeTestFontDir

    getFontPath fontDir famliyName fontSize
    |> (readFontLines fontSize)


[<Tests>]
let fonts =
    testList "LoadFont test" [
        testCase "read font info"
        <| fun _ ->
            let font = fake_LoadFont ("TS", 10)
            Expect.equal font TS10Font "Bad font!"
    ]
