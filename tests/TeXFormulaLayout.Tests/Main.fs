namespace TeXFormulaLayout.Tests
open Expecto


module SayTests =
    open TeXFormulaLayout

    [<Tests>]
    let tests = testList "Samples" [
        testCase "Say nothing" <| fun _ ->
            let subject = Say.nothing()
            Expect.equal subject () "Not an absolute unit"
        testCase "Say hello all" <| fun _ ->
            let subject = Say.hello "all"
            Expect.equal subject "Hello all" "You didn't say hello"
    ]


module ExpectoTemplate =

    [<EntryPoint>]
    let main argv =
        Tests.runTestsInAssembly defaultConfig argv
