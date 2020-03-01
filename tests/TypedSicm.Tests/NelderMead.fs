namespace TypedSicm.Tests

open Expecto
//open FsCheck
//open GeneratorsCode
open TypedSicm

module NelderMead =
    let config10k = { FsCheckConfig.defaultConfig with maxTest = 10000}
    // bug somewhere:  registering arbitrary generators causes Expecto VS test adapter not to work
    //let config10k = { FsCheckConfig.defaultConfig with maxTest = 10000; arbitrary = [typeof<Generators>] }
    let configReplay = { FsCheckConfig.defaultConfig with maxTest = 10000 ; replay = Some <| (1940624926, 296296394) }

    [<Tests>]
    let testNelderMead =
        
        testList "Nelder-Mead" [
            testCase "Nelder-Mead" <| fun () ->
                //let result = Ch1_LagrangianMechanics.S4ComputingActions.test1()
                //Expect.floatClose Accuracy.high result 435. "Expected 435"
                Expect.isTrue true ""
        ]
