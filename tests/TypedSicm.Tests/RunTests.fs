namespace TypedSicm.Tests

open Expecto

module RunTests =

    [<EntryPoint>]
    let main args =
        Tests.runTestsInAssembly defaultConfig args
