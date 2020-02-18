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
    let testSimpleTests =
        
        testList "Ch1_LagrangianMechanics" [
            testCase "S4 Computing Actions" <| fun () ->
                let result = Ch1_LagrangianMechanics.S4ComputingActions.test1()
                Expect.floatClose Accuracy.high result 435. "Expected 435"

            testCase "varied minimum action" <| fun () ->
                let result = Ch1_LagrangianMechanics.S4ComputingActions.test2()
                Expect.floatClose Accuracy.medium result 436.29 "Expected 436.29"

            //testPropertyWithConfig config10k "whitespace" <|
            //    fun  () ->
            //        Prop.forAll (Arb.fromGen <| whitespaceString())
            //            (fun (x : string) -> 
            //                x = x)
        ]

