namespace TypedSicm.Tests

open Expecto
//open FsCheck
//open GeneratorsCode
open TypedSicm

module Tests =
    let config10k = { FsCheckConfig.defaultConfig with maxTest = 10000}
    // bug somewhere:  registering arbitrary generators causes Expecto VS test adapter not to work
    //let config10k = { FsCheckConfig.defaultConfig with maxTest = 10000; arbitrary = [typeof<Generators>] }
    let configReplay = { FsCheckConfig.defaultConfig with maxTest = 10000 ; replay = Some <| (1940624926, 296296394) }

    [<Tests>]
    let ch1LagrangianMechanicsTests =
        
        testList "Ch1_LagrangianMechanics" [
            testCase "particle action" <| fun () ->
                let result = Ch1_LagrangianMechanics.S4ComputingActions.test1()
                Expect.floatClose Accuracy.high result 435. "Expected 435"

            testCase "varied minimum free particle action" <| fun () ->
                let result = Ch1_LagrangianMechanics.S4ComputingActions.test2()
                Expect.floatClose Accuracy.medium result 436.29 "Expected 436.29"

            testCase "minimumize free particle action" <| fun () ->
                match Ch1_LagrangianMechanics.S4ComputingActions.test3() with
                | Ok result ->
                    Expect.floatClose Accuracy.medium result.Minimum 435. "Result Ok, but result.Minimum wrong"
                    Expect.equal result.Iterations 6 "Result Ok, but result.Iterations wrong"

                | Error result ->
                    Expect.isTrue false <| sprintf "Result was error %A" result

        ]

