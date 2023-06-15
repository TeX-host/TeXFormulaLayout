namespace TeXFormulaLayout.Tests

open Expecto


module SayTests =
    open TeXFormulaLayout

    [<Tests>]
    let tests =
        testList "Samples" [
            testCase "Say nothing"
            <| fun _ ->
                let subject = Say.nothing ()
                Expect.equal subject () "Not an absolute unit"
            testCase "Say hello all"
            <| fun _ ->
                let subject = Say.hello "all"
                Expect.equal subject "Hello all" "You didn't say hello"
        ]


module ExpectoTemplate =
    open TeXFormulaLayout
    open BytesOut
    open Dvi

    let writeEmptyDvi () =
        startOut "empty.dvi"
        let mag = 2000
        Pre mag
        Bop ()
        (* empty DVI *)
        Eop ()
        Post mag
        endOut ()

    let writeDviBox () =
        startOut "box.dvi"
        let mag = 2000
        Pre mag
        Bop()
        BlackBox 1000_000 200_000 0
        Eop()
        Post mag
        endOut ()

    [<EntryPoint>]
    let main argv =
        // Tests.runTestsInAssemblyWithCLIArgs [] argv
        writeEmptyDvi ()
        writeDviBox ()
        0
