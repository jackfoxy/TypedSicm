namespace TypedSicm.Tests


open Expecto
open FSharpx.Collections
//open FsCheck
//open GeneratorsCode
open TypedSicm
open GenericArithmetic
open System.Numerics

module GenericArithmetic =
    let config10k = { FsCheckConfig.defaultConfig with maxTest = 10000}
    // bug somewhere:  registering arbitrary generators causes Expecto VS test adapter not to work
    //let config10k = { FsCheckConfig.defaultConfig with maxTest = 10000; arbitrary = [typeof<Generators>] }
    let configReplay = { FsCheckConfig.defaultConfig with maxTest = 10000 ; replay = Some <| (1940624926, 296296394) }

    [<Tests>]
    let genericArithmeticComparison =
        
        testList "comparable" [
            testCase "greater than" <| fun () ->
                Expect.isTrue ((Scalar.BigInt <| BigInteger 3) > Scalar.Float 2.) "Scalar.BigInt, Scalar.Float failed"
                Expect.isTrue ((Scalar.BigInt <| BigInteger 3) > Scalar.Int 2) "Scalar.BigInt, Scalar.Int failed"
                Expect.isTrue ((Scalar.BigInt <| BigInteger 3) > (Scalar.Int64 <| int64 2)) "Scalar.BigInt, Scalar.Int64 failed"
                Expect.isTrue (Scalar.Float 3. > (Scalar.BigInt <| BigInteger 2)) "Scalar.Float, Scalar.BigInt failed"
                Expect.isTrue (Scalar.Float 3. > Scalar.Int 2) "Scalar.Float, Scalar.Int failed"
                Expect.isTrue (Scalar.Float 3. > (Scalar.Int64 <| int64 2)) "Scalar.Float, Scalar.Int64 failed"
                Expect.isTrue (Scalar.Int 3 > (Scalar.BigInt <| BigInteger 2)) "Scalar.Int, Scalar.BigInt failed"
                Expect.isTrue (Scalar.Int 3 > Scalar.Float 2.) "Scalar.Int, Scalar.Float failed"
                Expect.isTrue (Scalar.Int 3 > (Scalar.Int64 <| int64 2)) "Scalar.Int, Scalar.Int64 failed"
                Expect.isTrue ((Scalar.Int64 <| int64 3) > (Scalar.BigInt <| BigInteger 2)) "Scalar.Int64, Scalar.BigInt failed"
                Expect.isTrue ((Scalar.Int64 <| int64 3) > Scalar.Float 2.) "Scalar.Int64, Scalar.Float failed"
                Expect.isTrue ((Scalar.Int64 <| int64 3) > Scalar.Int 2) "Scalar.Int64, Scalar.Int failed"

            testCase "less than" <| fun () ->
                Expect.isTrue ((Scalar.BigInt <| BigInteger 2) < Scalar.Float 3.) "Scalar.BigInt, Scalar.Float failed"
                Expect.isTrue ((Scalar.BigInt <| BigInteger 2) < Scalar.Int 3) "Scalar.BigInt, Scalar.Int failed"
                Expect.isTrue ((Scalar.BigInt <| BigInteger 2) < (Scalar.Int64 <| int64 3)) "Scalar.BigInt, Scalar.Int64 failed"
                Expect.isTrue (Scalar.Float 2. < (Scalar.BigInt <| BigInteger 3)) "Scalar.Float, Scalar.BigInt failed"
                Expect.isTrue (Scalar.Float 2. < Scalar.Int 3) "Scalar.Float, Scalar.Int failed"
                Expect.isTrue (Scalar.Float 2. < (Scalar.Int64 <| int64 3)) "Scalar.Float, Scalar.Int64 failed"
                Expect.isTrue (Scalar.Int 2 < (Scalar.BigInt <| BigInteger 3)) "Scalar.Int, Scalar.BigInt failed"
                Expect.isTrue (Scalar.Int 2 < Scalar.Float 3.) "Scalar.Int, Scalar.Float failed"
                Expect.isTrue (Scalar.Int 2 < (Scalar.Int64 <| int64 3)) "Scalar.Int, Scalar.Int64 failed"
                Expect.isTrue ((Scalar.Int64 <| int64 2) < (Scalar.BigInt <| BigInteger 3)) "Scalar.Int64, Scalar.BigInt failed"
                Expect.isTrue ((Scalar.Int64 <| int64 2) < Scalar.Float 3.) "Scalar.Int64, Scalar.Float failed"
                Expect.isTrue ((Scalar.Int64 <| int64 2) < Scalar.Int 3) "Scalar.Int64, Scalar.Int failed"

            testCase "equality" <| fun () ->
                Expect.isTrue ((Scalar.BigInt <| BigInteger 2) = Scalar.Float 2.) "Scalar.BigInt, Scalar.Float failed"
                Expect.isTrue ((Scalar.BigInt <| BigInteger 2) = Scalar.Int 2) "Scalar.BigInt, Scalar.Int failed"
                Expect.isTrue ((Scalar.BigInt <| BigInteger 2) = (Scalar.Int64 <| int64 2)) "Scalar.BigInt, Scalar.Int64 failed"
                Expect.isTrue (Scalar.Float 2. = (Scalar.BigInt <| BigInteger 2)) "Scalar.Float, Scalar.BigInt failed"
                Expect.isTrue (Scalar.Float 2. = Scalar.Int 2) "Scalar.Float, Scalar.Int failed"
                Expect.isTrue (Scalar.Float 2. = (Scalar.Int64 <| int64 2)) "Scalar.Float, Scalar.Int64 failed"
                Expect.isTrue (Scalar.Int 2 = (Scalar.BigInt <| BigInteger 2)) "Scalar.Int, Scalar.BigInt failed"
                Expect.isTrue (Scalar.Int 2 = Scalar.Float 2.) "Scalar.Int, Scalar.Float failed"
                Expect.isTrue (Scalar.Int 2 = (Scalar.Int64 <| int64 2)) "Scalar.Int, Scalar.Int64 failed"
                Expect.isTrue ((Scalar.Int64 <| int64 2) = (Scalar.BigInt <| BigInteger 2)) "Scalar.Int64, Scalar.BigInt failed"
                Expect.isTrue ((Scalar.Int64 <| int64 2) = Scalar.Float 2.) "Scalar.Int64, Scalar.Float failed"
                Expect.isTrue ((Scalar.Int64 <| int64 2) = Scalar.Int 2) "Scalar.Int64, Scalar.Int failed"

            testCase "not equal" <| fun () ->
                Expect.isTrue ((Scalar.BigInt <| BigInteger 3) <> Scalar.Float 2.) "Scalar.BigInt, Scalar.Float failed"
                Expect.isTrue ((Scalar.BigInt <| BigInteger 3) <> Scalar.Int 2) "Scalar.BigInt, Scalar.Int failed"
                Expect.isTrue ((Scalar.BigInt <| BigInteger 3) <> (Scalar.Int64 <| int64 2)) "Scalar.BigInt, Scalar.Int64 failed"
                Expect.isTrue (Scalar.Float 3. <> (Scalar.BigInt <| BigInteger 2)) "Scalar.Float, Scalar.BigInt failed"
                Expect.isTrue (Scalar.Float 3. <> Scalar.Int 2) "Scalar.Float, Scalar.Int failed"
                Expect.isTrue (Scalar.Float 3. <> (Scalar.Int64 <| int64 2)) "Scalar.Float, Scalar.Int64 failed"
                Expect.isTrue (Scalar.Int 3 <> (Scalar.BigInt <| BigInteger 2)) "Scalar.Int, Scalar.BigInt failed"
                Expect.isTrue (Scalar.Int 3 <> Scalar.Float 2.) "Scalar.Int, Scalar.Float failed"
                Expect.isTrue (Scalar.Int 3 <> (Scalar.Int64 <| int64 2)) "Scalar.Int, Scalar.Int64 failed"
                Expect.isTrue ((Scalar.Int64 <| int64 3) <> (Scalar.BigInt <| BigInteger 2)) "Scalar.Int64, Scalar.BigInt failed"
                Expect.isTrue ((Scalar.Int64 <| int64 3) <> Scalar.Float 2.) "Scalar.Int64, Scalar.Float failed"
                Expect.isTrue ((Scalar.Int64 <| int64 3) <> Scalar.Int 2) "Scalar.Int64, Scalar.Int failed"

            testCase "greater than or equal to" <| fun () ->
                Expect.isTrue ((Scalar.BigInt <| BigInteger 3) >= Scalar.Float 2.) "Scalar.BigInt, Scalar.Float failed"
                Expect.isTrue ((Scalar.BigInt <| BigInteger 3) >= Scalar.Int 2) "Scalar.BigInt, Scalar.Int failed"
                Expect.isTrue ((Scalar.BigInt <| BigInteger 3) >= (Scalar.Int64 <| int64 2)) "Scalar.BigInt, Scalar.Int64 failed"
                Expect.isTrue (Scalar.Float 3. >= (Scalar.BigInt <| BigInteger 2)) "Scalar.Float, Scalar.BigInt failed"
                Expect.isTrue (Scalar.Float 3. >= Scalar.Int 2) "Scalar.Float, Scalar.Int failed"
                Expect.isTrue (Scalar.Float 3. >= (Scalar.Int64 <| int64 2)) "Scalar.Float, Scalar.Int64 failed"
                Expect.isTrue (Scalar.Int 3 >= (Scalar.BigInt <| BigInteger 2)) "Scalar.Int, Scalar.BigInt failed"
                Expect.isTrue (Scalar.Int 3 >= Scalar.Float 2.) "Scalar.Int, Scalar.Float failed"
                Expect.isTrue (Scalar.Int 3 >= (Scalar.Int64 <| int64 2)) "Scalar.Int, Scalar.Int64 failed"
                Expect.isTrue ((Scalar.Int64 <| int64 3) >= (Scalar.BigInt <| BigInteger 2)) "Scalar.Int64, Scalar.BigInt failed"
                Expect.isTrue ((Scalar.Int64 <| int64 3) >= Scalar.Float 2.) "Scalar.Int64, Scalar.Float failed"
                Expect.isTrue ((Scalar.Int64 <| int64 3) >= Scalar.Int 2) "Scalar.Int64, Scalar.Int failed"
                
                Expect.isTrue ((Scalar.BigInt <| BigInteger 2) >= Scalar.Float 2.) "Scalar.BigInt, Scalar.Float failed"
                Expect.isTrue ((Scalar.BigInt <| BigInteger 2) >= Scalar.Int 2) "Scalar.BigInt, Scalar.Int failed"
                Expect.isTrue ((Scalar.BigInt <| BigInteger 2) >= (Scalar.Int64 <| int64 2)) "Scalar.BigInt, Scalar.Int64 failed"
                Expect.isTrue (Scalar.Float 2. >= (Scalar.BigInt <| BigInteger 2)) "Scalar.Float, Scalar.BigInt failed"
                Expect.isTrue (Scalar.Float 2. >= Scalar.Int 2) "Scalar.Float, Scalar.Int failed"
                Expect.isTrue (Scalar.Float 2. >= (Scalar.Int64 <| int64 2)) "Scalar.Float, Scalar.Int64 failed"
                Expect.isTrue (Scalar.Int 2 >= (Scalar.BigInt <| BigInteger 2)) "Scalar.Int, Scalar.BigInt failed"
                Expect.isTrue (Scalar.Int 2 >= Scalar.Float 2.) "Scalar.Int, Scalar.Float failed"
                Expect.isTrue (Scalar.Int 2 >= (Scalar.Int64 <| int64 2)) "Scalar.Int, Scalar.Int64 failed"
                Expect.isTrue ((Scalar.Int64 <| int64 2) >= (Scalar.BigInt <| BigInteger 2)) "Scalar.Int64, Scalar.BigInt failed"
                Expect.isTrue ((Scalar.Int64 <| int64 2) >= Scalar.Float 2.) "Scalar.Int64, Scalar.Float failed"
                Expect.isTrue ((Scalar.Int64 <| int64 2) >= Scalar.Int 2) "Scalar.Int64, Scalar.Int failed"

            testCase "less than or equal to" <| fun () ->
                Expect.isTrue ((Scalar.BigInt <| BigInteger 2)<=Scalar.Float 3.) "Scalar.BigInt, Scalar.Float failed"
                Expect.isTrue ((Scalar.BigInt <| BigInteger 2)<=Scalar.Int 3) "Scalar.BigInt, Scalar.Int failed"
                Expect.isTrue ((Scalar.BigInt <| BigInteger 2)<=(Scalar.Int64 <| int64 3)) "Scalar.BigInt, Scalar.Int64 failed"
                Expect.isTrue (Scalar.Float 2.<=(Scalar.BigInt <| BigInteger 3)) "Scalar.Float, Scalar.BigInt failed"
                Expect.isTrue (Scalar.Float 2.<=Scalar.Int 3) "Scalar.Float, Scalar.Int failed"
                Expect.isTrue (Scalar.Float 2.<=(Scalar.Int64 <| int64 3)) "Scalar.Float, Scalar.Int64 failed"
                Expect.isTrue (Scalar.Int 2<=(Scalar.BigInt <| BigInteger 3)) "Scalar.Int, Scalar.BigInt failed"
                Expect.isTrue (Scalar.Int 2<=Scalar.Float 3.) "Scalar.Int, Scalar.Float failed"
                Expect.isTrue (Scalar.Int 2<=(Scalar.Int64 <| int64 3)) "Scalar.Int, Scalar.Int64 failed"
                Expect.isTrue ((Scalar.Int64 <| int64 2)<=(Scalar.BigInt <| BigInteger 3)) "Scalar.Int64, Scalar.BigInt failed"
                Expect.isTrue ((Scalar.Int64 <| int64 2)<=Scalar.Float 3.) "Scalar.Int64, Scalar.Float failed"
                Expect.isTrue ((Scalar.Int64 <| int64 2)<=Scalar.Int 3) "Scalar.Int64, Scalar.Int failed"
                
                Expect.isTrue ((Scalar.BigInt <| BigInteger 2) <= Scalar.Float 2.) "Scalar.BigInt, Scalar.Float failed"
                Expect.isTrue ((Scalar.BigInt <| BigInteger 2) <= Scalar.Int 2) "Scalar.BigInt, Scalar.Int failed"
                Expect.isTrue ((Scalar.BigInt <| BigInteger 2) <= (Scalar.Int64 <| int64 2)) "Scalar.BigInt, Scalar.Int64 failed"
                Expect.isTrue (Scalar.Float 2. <= (Scalar.BigInt <| BigInteger 2)) "Scalar.Float, Scalar.BigInt failed"
                Expect.isTrue (Scalar.Float 2. <= Scalar.Int 2) "Scalar.Float, Scalar.Int failed"
                Expect.isTrue (Scalar.Float 2. <= (Scalar.Int64 <| int64 2)) "Scalar.Float, Scalar.Int64 failed"
                Expect.isTrue (Scalar.Int 2 <= (Scalar.BigInt <| BigInteger 2)) "Scalar.Int, Scalar.BigInt failed"
                Expect.isTrue (Scalar.Int 2 <= Scalar.Float 2.) "Scalar.Int, Scalar.Float failed"
                Expect.isTrue (Scalar.Int 2 <= (Scalar.Int64 <| int64 2)) "Scalar.Int, Scalar.Int64 failed"
                Expect.isTrue ((Scalar.Int64 <| int64 2) <= (Scalar.BigInt <| BigInteger 2)) "Scalar.Int64, Scalar.BigInt failed"
                Expect.isTrue ((Scalar.Int64 <| int64 2) <= Scalar.Float 2.) "Scalar.Int64, Scalar.Float failed"
                Expect.isTrue ((Scalar.Int64 <| int64 2) <= Scalar.Int 2) "Scalar.Int64, Scalar.Int failed"
        ]

    [<Tests>]
    let genericArithmetic =
        
        testList "arithmetic" [
            testCase "addition" <| fun () ->
                Expect.equal (BigInteger 2 + (Scalar.BigInt <| BigInteger 3)) (Scalar.Int 5) "BigInteger, Scalar.BigInt failed"
                Expect.equal (BigInteger 2 + Scalar.Float 3.) (Scalar.Int 5) "BigInteger, Scalar.Float failed"
                Expect.equal (BigInteger 2 + Scalar.Int 3) (Scalar.Int 5) "BigInteger, Scalar.Int failed"
                Expect.equal (BigInteger 2 + (Scalar.Int64 <| int64 3)) (Scalar.Int 5) "BigInteger, Scalar.Int64 failed"               

                Expect.equal ((Scalar.BigInt <| BigInteger 2) + BigInteger 3) (Scalar.Int 5) "Scalar.BigInt, BigInteger failed"
                Expect.equal ((Scalar.BigInt <| BigInteger 2) + Scalar.Float 3.) (Scalar.Int 5) "Scalar.BigInt, Scalar.Float failed"
                Expect.equal ((Scalar.BigInt <| BigInteger 2) + Scalar.Int 3) (Scalar.Int 5) "Scalar.BigInt, Scalar.Int failed"
                Expect.equal ((Scalar.BigInt <| BigInteger 2) + (Scalar.Int64 <| int64 3)) (Scalar.Int 5) "Scalar.BigInt, Scalar.Int64 failed"
                Expect.equal ((Scalar.BigInt <| BigInteger 2) + float 3) (Scalar.Int 5) "Scalar.BigInt, float failed"
                Expect.equal ((Scalar.BigInt <| BigInteger 2) + 3) (Scalar.Int 5) "Scalar.BigInt, int failed"
                Expect.equal ((Scalar.BigInt <| BigInteger 2) + int64 3) (Scalar.Int 5) "Scalar.BigInt, int64 failed"

                Expect.equal (Scalar.Float 2. + BigInteger 3) (Scalar.Int 5) "Scalar.Float, BigInteger failed"
                Expect.equal (Scalar.Float 2. + (Scalar.BigInt <| BigInteger 3)) (Scalar.Int 5) "Scalar.Float, Scalar.BigInt failed"
                Expect.equal (Scalar.Float 2. + Scalar.Int 3) (Scalar.Int 5) "Scalar.Float, Scalar.Int failed"
                Expect.equal (Scalar.Float 2. + (Scalar.Int64 <| int64 3)) (Scalar.Int 5) "Scalar.Float, Scalar.Int64 failed"
                Expect.equal (Scalar.Float 2. + float 3) (Scalar.Int 5) "Scalar.Float, float failed"
                Expect.equal (Scalar.Float 2. + 3) (Scalar.Int 5) "Scalar.Float, int failed"
                Expect.equal (Scalar.Float 2. + int64 3) (Scalar.Int 5) "Scalar.Float, int64 failed"

                Expect.equal (Scalar.Int 2 + BigInteger 3) (Scalar.Int 5) "Scalar.Int, BigInteger failed"
                Expect.equal (Scalar.Int 2 + (Scalar.BigInt <| BigInteger 3)) (Scalar.Int 5) "Scalar.Int, Scalar.BigInt failed"
                Expect.equal (Scalar.Int 2 + Scalar.Float 3.) (Scalar.Int 5) "Scalar.Int, Scalar.Float failed"
                Expect.equal (Scalar.Int 2 + (Scalar.Int64 <| int64 3)) (Scalar.Int 5) "Scalar.Int, Scalar.Int64 failed"
                Expect.equal (Scalar.Int 2 + float 3) (Scalar.Int 5) "Scalar.Int, float failed"
                Expect.equal (Scalar.Int 2 + 3) (Scalar.Int 5) "Scalar.Int, int failed"
                Expect.equal (Scalar.Int 2 + int64 3) (Scalar.Int 5) "Scalar.Int, int64 failed"

                Expect.equal ((Scalar.Int64 <| int64 2) + BigInteger 3) (Scalar.Int 5) "Scalar.Int64, BigInteger failed"
                Expect.equal ((Scalar.Int64 <| int64 2) + (Scalar.BigInt <| BigInteger 3)) (Scalar.Int 5) "Scalar.Int64, Scalar.BigInt failed"
                Expect.equal ((Scalar.Int64 <| int64 2) + Scalar.Float 3.) (Scalar.Int 5) "Scalar.Int64, Scalar.Float failed"
                Expect.equal ((Scalar.Int64 <| int64 2) + Scalar.Int 3) (Scalar.Int 5) "Scalar.Int64, Scalar.Int failed"
                Expect.equal ((Scalar.Int64 <| int64 2) + float 3) (Scalar.Int 5) "Scalar.Int64, float failed"
                Expect.equal ((Scalar.Int64 <| int64 2) + 3) (Scalar.Int 5) "Scalar.Int64, int failed"
                Expect.equal ((Scalar.Int64 <| int64 2) + int64 3) (Scalar.Int 5) "Scalar.Int64, int64 failed"

                Expect.equal (float 2 + (Scalar.BigInt <| BigInteger 3)) (Scalar.Int 5) "float, Scalar.BigInt failed"
                Expect.equal (float 2 + Scalar.Float 3.) (Scalar.Int 5) "float, Scalar.Float failed"
                Expect.equal (float 2 + Scalar.Int 3) (Scalar.Int 5) "float, Scalar.Int failed"
                Expect.equal (float 2 + (Scalar.Int64 <| int64 3)) (Scalar.Int 5) "float, Scalar.Int64 failed"

                Expect.equal (2 + (Scalar.BigInt <| BigInteger 3)) (Scalar.Int 5) "int, Scalar.BigInt failed"
                Expect.equal (2 + Scalar.Float 3.) (Scalar.Int 5) "int, Scalar.Float failed"
                Expect.equal (2 + Scalar.Int 3) (Scalar.Int 5) "int, Scalar.Int failed"
                Expect.equal (2 + (Scalar.Int64 <| int64 3)) (Scalar.Int 5) "int, Scalar.Int64 failed"

                Expect.equal (int64 2 + (Scalar.BigInt <| BigInteger 3)) (Scalar.Int 5) "int64, Scalar.BigInt failed"
                Expect.equal (int64 2 + Scalar.Float 3.) (Scalar.Int 5) "int64, Scalar.Float failed"
                Expect.equal (int64 2 + Scalar.Int 3) (Scalar.Int 5) "int64, Scalar.Int failed"
                Expect.equal (int64 2 + (Scalar.Int64 <| int64 3)) (Scalar.Int 5) "int64, Scalar.Int64 failed"
                
            testCase "multiplication" <| fun () ->
                Expect.equal (BigInteger 2 * (Scalar.BigInt <| BigInteger 3)) (Scalar.Int 6) "BigInteger, Scalar.BigInt failed"
                Expect.equal (BigInteger 2 * Scalar.Float 3.) (Scalar.Int 6) "BigInteger, Scalar.Float failed"
                Expect.equal (BigInteger 2 * Scalar.Int 3) (Scalar.Int 6) "BigInteger, Scalar.Int failed"
                Expect.equal (BigInteger 2 * (Scalar.Int64 <| int64 3)) (Scalar.Int 6) "BigInteger, Scalar.Int64 failed"               

                Expect.equal ((Scalar.BigInt <| BigInteger 2) * BigInteger 3) (Scalar.Int 6) "Scalar.BigInt, BigInteger failed"
                Expect.equal ((Scalar.BigInt <| BigInteger 2) * Scalar.Float 3.) (Scalar.Int 6) "Scalar.BigInt, Scalar.Float failed"
                Expect.equal ((Scalar.BigInt <| BigInteger 2) * Scalar.Int 3) (Scalar.Int 6) "Scalar.BigInt, Scalar.Int failed"
                Expect.equal ((Scalar.BigInt <| BigInteger 2) * (Scalar.Int64 <| int64 3)) (Scalar.Int 6) "Scalar.BigInt, Scalar.Int64 failed"
                Expect.equal ((Scalar.BigInt <| BigInteger 2) * float 3) (Scalar.Int 6) "Scalar.BigInt, float failed"
                Expect.equal ((Scalar.BigInt <| BigInteger 2) * 3) (Scalar.Int 6) "Scalar.BigInt, int failed"
                Expect.equal ((Scalar.BigInt <| BigInteger 2) * int64 3) (Scalar.Int 6) "Scalar.BigInt, int64 failed"

                Expect.equal (Scalar.Float 2. * BigInteger 3) (Scalar.Int 6) "Scalar.Float, BigInteger failed"
                Expect.equal (Scalar.Float 2. * (Scalar.BigInt <| BigInteger 3)) (Scalar.Int 6) "Scalar.Float, Scalar.BigInt failed"
                Expect.equal (Scalar.Float 2. * Scalar.Int 3) (Scalar.Int 6) "Scalar.Float, Scalar.Int failed"
                Expect.equal (Scalar.Float 2. * (Scalar.Int64 <| int64 3)) (Scalar.Int 6) "Scalar.Float, Scalar.Int64 failed"
                Expect.equal (Scalar.Float 2. * float 3) (Scalar.Int 6) "Scalar.Float, float failed"
                Expect.equal (Scalar.Float 2. * 3) (Scalar.Int 6) "Scalar.Float, int failed"
                Expect.equal (Scalar.Float 2. * int64 3) (Scalar.Int 6) "Scalar.Float, int64 failed"

                Expect.equal (Scalar.Int 2 * BigInteger 3) (Scalar.Int 6) "Scalar.Int, BigInteger failed"
                Expect.equal (Scalar.Int 2 * (Scalar.BigInt <| BigInteger 3)) (Scalar.Int 6) "Scalar.Int, Scalar.BigInt failed"
                Expect.equal (Scalar.Int 2 * Scalar.Float 3.) (Scalar.Int 6) "Scalar.Int, Scalar.Float failed"
                Expect.equal (Scalar.Int 2 * (Scalar.Int64 <| int64 3)) (Scalar.Int 6) "Scalar.Int, Scalar.Int64 failed"
                Expect.equal (Scalar.Int 2 * float 3) (Scalar.Int 6) "Scalar.Int, float failed"
                Expect.equal (Scalar.Int 2 * 3) (Scalar.Int 6) "Scalar.Int, int failed"
                Expect.equal (Scalar.Int 2 * int64 3) (Scalar.Int 6) "Scalar.Int, int64 failed"

                Expect.equal ((Scalar.Int64 <| int64 2) * BigInteger 3) (Scalar.Int 6) "Scalar.Int64, BigInteger failed"
                Expect.equal ((Scalar.Int64 <| int64 2) * (Scalar.BigInt <| BigInteger 3)) (Scalar.Int 6) "Scalar.Int64, Scalar.BigInt failed"
                Expect.equal ((Scalar.Int64 <| int64 2) * Scalar.Float 3.) (Scalar.Int 6) "Scalar.Int64, Scalar.Float failed"
                Expect.equal ((Scalar.Int64 <| int64 2) * Scalar.Int 3) (Scalar.Int 6) "Scalar.Int64, Scalar.Int failed"
                Expect.equal ((Scalar.Int64 <| int64 2) * float 3) (Scalar.Int 6) "Scalar.Int64, float failed"
                Expect.equal ((Scalar.Int64 <| int64 2) * 3) (Scalar.Int 6) "Scalar.Int64, int failed"
                Expect.equal ((Scalar.Int64 <| int64 2) * int64 3) (Scalar.Int 6) "Scalar.Int64, int64 failed"

                Expect.equal (float 2 * (Scalar.BigInt <| BigInteger 3)) (Scalar.Int 6) "float, Scalar.BigInt failed"
                Expect.equal (float 2 * Scalar.Float 3.) (Scalar.Int 6) "float, Scalar.Float failed"
                Expect.equal (float 2 * Scalar.Int 3) (Scalar.Int 6) "float, Scalar.Int failed"
                Expect.equal (float 2 * (Scalar.Int64 <| int64 3)) (Scalar.Int 6) "float, Scalar.Int64 failed"

                Expect.equal (2 * (Scalar.BigInt <| BigInteger 3)) (Scalar.Int 6) "int, Scalar.BigInt failed"
                Expect.equal (2 * Scalar.Float 3.) (Scalar.Int 6) "int, Scalar.Float failed"
                Expect.equal (2 * Scalar.Int 3) (Scalar.Int 6) "int, Scalar.Int failed"
                Expect.equal (2 * (Scalar.Int64 <| int64 3)) (Scalar.Int 6) "int, Scalar.Int64 failed"

                Expect.equal (int64 2 * (Scalar.BigInt <| BigInteger 3)) (Scalar.Int 6) "int64, Scalar.BigInt failed"
                Expect.equal (int64 2 * Scalar.Float 3.) (Scalar.Int 6) "int64, Scalar.Float failed"
                Expect.equal (int64 2 * Scalar.Int 3) (Scalar.Int 6) "int64, Scalar.Int failed"
                Expect.equal (int64 2 * (Scalar.Int64 <| int64 3)) (Scalar.Int 6) "int64, Scalar.Int64 failed"

            testCase "subtraction" <| fun () ->
                Expect.equal (BigInteger 5 - (Scalar.BigInt <| BigInteger 3)) (Scalar.Int 2) "BigInteger, Scalar.BigInt failed"
                Expect.equal (BigInteger 5 - Scalar.Float 3.) (Scalar.Int 2) "BigInteger, Scalar.Float failed"
                Expect.equal (BigInteger 5 - Scalar.Int 3) (Scalar.Int 2) "BigInteger, Scalar.Int failed"
                Expect.equal (BigInteger 5 - (Scalar.Int64 <| int64 3)) (Scalar.Int 2) "BigInteger, Scalar.Int64 failed"               

                Expect.equal ((Scalar.BigInt <| BigInteger 5) - BigInteger 3) (Scalar.Int 2) "Scalar.BigInt, BigInteger failed"
                Expect.equal ((Scalar.BigInt <| BigInteger 5) - Scalar.Float 3.) (Scalar.Int 2) "Scalar.BigInt, Scalar.Float failed"
                Expect.equal ((Scalar.BigInt <| BigInteger 5) - Scalar.Int 3) (Scalar.Int 2) "Scalar.BigInt, Scalar.Int failed"
                Expect.equal ((Scalar.BigInt <| BigInteger 5) - (Scalar.Int64 <| int64 3)) (Scalar.Int 2) "Scalar.BigInt, Scalar.Int64 failed"
                Expect.equal ((Scalar.BigInt <| BigInteger 5) - float 3) (Scalar.Int 2) "Scalar.BigInt, float failed"
                Expect.equal ((Scalar.BigInt <| BigInteger 5) - 3) (Scalar.Int 2) "Scalar.BigInt, int failed"
                Expect.equal ((Scalar.BigInt <| BigInteger 5) - int64 3) (Scalar.Int 2) "Scalar.BigInt, Int64 failed"

                Expect.equal (Scalar.Float 5. - BigInteger 3) (Scalar.Int 2) "Scalar.Float, BigInteger failed"
                Expect.equal (Scalar.Float 5. - (Scalar.BigInt <| BigInteger 3)) (Scalar.Int 2) "Scalar.Float, Scalar.BigInt failed"
                Expect.equal (Scalar.Float 5. - Scalar.Int 3) (Scalar.Int 2) "Scalar.Float, Scalar.Int failed"
                Expect.equal (Scalar.Float 5. - (Scalar.Int64 <| int64 3)) (Scalar.Int 2) "Scalar.Float, Scalar.Int64 failed"
                Expect.equal (Scalar.Float 5. - float 3) (Scalar.Int 2) "Scalar.Float, float failed"
                Expect.equal (Scalar.Float 5. - 3) (Scalar.Int 2) "Scalar.Float, int failed"
                Expect.equal (Scalar.Float 5. - int64 3) (Scalar.Int 2) "Scalar.Float, Int64 failed"

                Expect.equal (Scalar.Int 5 - BigInteger 3) (Scalar.Int 2) "Scalar.Int, BigInteger failed"
                Expect.equal (Scalar.Int 5 - (Scalar.BigInt <| BigInteger 3)) (Scalar.Int 2) "Scalar.Int, Scalar.BigInt failed"
                Expect.equal (Scalar.Int 5 - Scalar.Float 3.) (Scalar.Int 2) "Scalar.Int, Scalar.Float failed"
                Expect.equal (Scalar.Int 5 - (Scalar.Int64 <| int64 3)) (Scalar.Int 2) "Scalar.Int, Scalar.Int64 failed"
                Expect.equal (Scalar.Int 5 - float 3) (Scalar.Int 2) "Scalar.Int, float failed"
                Expect.equal (Scalar.Int 5 - 3) (Scalar.Int 2) "Scalar.Int, int failed"
                Expect.equal (Scalar.Int 5 - int64 3) (Scalar.Int 2) "Scalar.Int, Int64 failed"

                Expect.equal ((Scalar.Int64 <| int64 5) - BigInteger 3) (Scalar.Int 2) "Scalar.Int64, BigInteger failed"
                Expect.equal ((Scalar.Int64 <| int64 5) - (Scalar.BigInt <| BigInteger 3)) (Scalar.Int 2) "Scalar.Int64, Scalar.BigInt failed"
                Expect.equal ((Scalar.Int64 <| int64 5) - Scalar.Float 3.) (Scalar.Int 2) "Scalar.Int64, Scalar.Float failed"
                Expect.equal ((Scalar.Int64 <| int64 5) - Scalar.Int 3) (Scalar.Int 2) "Scalar.Int64, Scalar.Int failed"
                Expect.equal ((Scalar.Int64 <| int64 5) - float 3) (Scalar.Int 2) "Scalar.Int64, float failed"
                Expect.equal ((Scalar.Int64 <| int64 5) - 3) (Scalar.Int 2) "Scalar.Int64, int failed"
                Expect.equal ((Scalar.Int64 <| int64 5) - int64 3) (Scalar.Int 2) "Scalar.Int64, Int64 failed"

                Expect.equal (float 5 - (Scalar.BigInt <| BigInteger 3)) (Scalar.Int 2) "float, Scalar.BigInt failed"
                Expect.equal (float 5 - Scalar.Float 3.) (Scalar.Int 2) "float, Scalar.Float failed"
                Expect.equal (float 5 - Scalar.Int 3) (Scalar.Int 2) "float, Scalar.Int failed"
                Expect.equal (float 5 - (Scalar.Int64 <| int64 3)) (Scalar.Int 2) "float, Scalar.Int64 failed"

                Expect.equal (5 - (Scalar.BigInt <| BigInteger 3)) (Scalar.Int 2) "int, Scalar.BigInt failed"
                Expect.equal (5 - Scalar.Float 3.) (Scalar.Int 2) "int, Scalar.Float failed"
                Expect.equal (5 - Scalar.Int 3) (Scalar.Int 2) "int, Scalar.Int failed"
                Expect.equal (5 - (Scalar.Int64 <| int64 3)) (Scalar.Int 2) "int, Scalar.Int64 failed"

                Expect.equal (int64 5 - (Scalar.BigInt <| BigInteger 3)) (Scalar.Int 2) "Int64, Scalar.BigInt failed"
                Expect.equal (int64 5 - Scalar.Float 3.) (Scalar.Int 2) "Int64, Scalar.Float failed"
                Expect.equal (int64 5 - Scalar.Int 3) (Scalar.Int 2) "Int64, Scalar.Int failed"
                Expect.equal (int64 5 - (Scalar.Int64 <| int64 3)) (Scalar.Int 2) "Int64, Scalar.Int64 failed"

            testCase "division" <| fun () ->
                Expect.equal (BigInteger 10 / (Scalar.BigInt <| BigInteger 5)) (Scalar.Int 2) "BigInteger, Scalar.BigInt failed"
                Expect.equal (BigInteger 10 / Scalar.Float 5.) (Scalar.Int 2) "BigInteger, Scalar.Float failed"
                Expect.equal (BigInteger 10 / Scalar.Int 5) (Scalar.Int 2) "BigInteger, Scalar.Int failed"
                Expect.equal (BigInteger 10 / (Scalar.Int64 <| int64 5)) (Scalar.Int 2) "BigInteger, Scalar.Int64 failed"               

                Expect.equal ((Scalar.BigInt <| BigInteger 10) / BigInteger 5) (Scalar.Int 2) "Scalar.BigInt, BigInteger failed"
                Expect.equal ((Scalar.BigInt <| BigInteger 10) / Scalar.Float 5.) (Scalar.Int 2) "Scalar.BigInt, Scalar.Float failed"
                Expect.equal ((Scalar.BigInt <| BigInteger 10) / Scalar.Int 5) (Scalar.Int 2) "Scalar.BigInt, Scalar.Int failed"
                Expect.equal ((Scalar.BigInt <| BigInteger 10) / (Scalar.Int64 <| int64 5)) (Scalar.Int 2) "Scalar.BigInt, Scalar.Int64 failed"
                Expect.equal ((Scalar.BigInt <| BigInteger 10) / float 5) (Scalar.Int 2) "Scalar.BigInt, float failed"
                Expect.equal ((Scalar.BigInt <| BigInteger 10) / 5) (Scalar.Int 2) "Scalar.BigInt, int failed"
                Expect.equal ((Scalar.BigInt <| BigInteger 10) / int64 5) (Scalar.Int 2) "Scalar.BigInt, Int64 failed"

                Expect.equal (Scalar.Float 10. / BigInteger 5) (Scalar.Int 2) "Scalar.Float, BigInteger failed"
                Expect.equal (Scalar.Float 10. / (Scalar.BigInt <| BigInteger 5)) (Scalar.Int 2) "Scalar.Float, Scalar.BigInt failed"
                Expect.equal (Scalar.Float 10. / Scalar.Int 5) (Scalar.Int 2) "Scalar.Float, Scalar.Int failed"
                Expect.equal (Scalar.Float 10. / (Scalar.Int64 <| int64 5)) (Scalar.Int 2) "Scalar.Float, Scalar.Int64 failed"
                Expect.equal (Scalar.Float 10. / float 5) (Scalar.Int 2) "Scalar.Float, float failed"
                Expect.equal (Scalar.Float 10. / 5) (Scalar.Int 2) "Scalar.Float, int failed"
                Expect.equal (Scalar.Float 10. / int64 5) (Scalar.Int 2) "Scalar.Float, Int64 failed"

                Expect.equal (Scalar.Int 10 / BigInteger 5) (Scalar.Int 2) "Scalar.Int, BigInteger failed"
                Expect.equal (Scalar.Int 10 / (Scalar.BigInt <| BigInteger 5)) (Scalar.Int 2) "Scalar.Int, Scalar.BigInt failed"
                Expect.equal (Scalar.Int 10 / Scalar.Float 5.) (Scalar.Int 2) "Scalar.Int, Scalar.Float failed"
                Expect.equal (Scalar.Int 10 / (Scalar.Int64 <| int64 5)) (Scalar.Int 2) "Scalar.Int, Scalar.Int64 failed"
                Expect.equal (Scalar.Int 10 / float 5) (Scalar.Int 2) "Scalar.Int, float failed"
                Expect.equal (Scalar.Int 10 / 5) (Scalar.Int 2) "Scalar.Int, int failed"
                Expect.equal (Scalar.Int 10 / int64 5) (Scalar.Int 2) "Scalar.Int, Int64 failed"

                Expect.equal ((Scalar.Int64 <| int64 10) / BigInteger 5) (Scalar.Int 2) "Scalar.Int64, BigInteger failed"
                Expect.equal ((Scalar.Int64 <| int64 10) / (Scalar.BigInt <| BigInteger 5)) (Scalar.Int 2) "Scalar.Int64, Scalar.BigInt failed"
                Expect.equal ((Scalar.Int64 <| int64 10) / Scalar.Float 5.) (Scalar.Int 2) "Scalar.Int64, Scalar.Float failed"
                Expect.equal ((Scalar.Int64 <| int64 10) / Scalar.Int 5) (Scalar.Int 2) "Scalar.Int64, Scalar.Int failed"
                Expect.equal ((Scalar.Int64 <| int64 10) / float 5) (Scalar.Int 2) "Scalar.Int64, float failed"
                Expect.equal ((Scalar.Int64 <| int64 10) / 5) (Scalar.Int 2) "Scalar.Int64, int failed"
                Expect.equal ((Scalar.Int64 <| int64 10) / int64 5) (Scalar.Int 2) "Scalar.Int64, Int64 failed"

                Expect.equal (float 10 / (Scalar.BigInt <| BigInteger 5)) (Scalar.Int 2) "float, Scalar.BigInt failed"
                Expect.equal (float 10 / Scalar.Float 5.) (Scalar.Int 2) "float, Scalar.Float failed"
                Expect.equal (float 10 / Scalar.Int 5) (Scalar.Int 2) "float, Scalar.Int failed"
                Expect.equal (float 10 / (Scalar.Int64 <| int64 5)) (Scalar.Int 2) "float, Scalar.Int64 failed"

                Expect.equal (10 / (Scalar.BigInt <| BigInteger 5)) (Scalar.Int 2) "int, Scalar.BigInt failed"
                Expect.equal (10 / Scalar.Float 5.) (Scalar.Int 2) "int, Scalar.Float failed"
                Expect.equal (10 / Scalar.Int 5) (Scalar.Int 2) "int, Scalar.Int failed"
                Expect.equal (10 / (Scalar.Int64 <| int64 5)) (Scalar.Int 2) "int, Scalar.Int64 failed"

                Expect.equal (int64 10 / (Scalar.BigInt <| BigInteger 5)) (Scalar.Int 2) "Int64, Scalar.BigInt failed"
                Expect.equal (int64 10 / Scalar.Float 5.) (Scalar.Int 2) "Int64, Scalar.Float failed"
                Expect.equal (int64 10 / Scalar.Int 5) (Scalar.Int 2) "Int64, Scalar.Int failed"
                Expect.equal (int64 10 / (Scalar.Int64 <| int64 5)) (Scalar.Int 2) "Int64, Scalar.Int64 failed"
        ]

    let wFunc = ScalarFunc (fun z -> z * Scalar.Int 2)
    let upIndexed = RandomAccessList.ofSeq [| Indexable.Func wFunc |]

    [<Tests>]
    let genericArithmeticUpIndexed =
        
        testList "UpIndexed" [
            testCase "addition left" <| fun () ->
                let z = upIndexed + (Scalar.Int64 <| int64 3)
                let scalarFunc = 
                    match z.[0] with
                    | Scalar _ -> invalidArg "UpIndexed addition" "can't get here"
                    | Func f -> f

                Expect.equal (scalarFunc.Invoke (Scalar.Float 2.)) (Scalar.Int 7) 
                    (sprintf "actual %A, expected %A failed" (scalarFunc.Invoke (Scalar.Float 2.)) (Scalar.Int 7) )

            testCase "addition right" <| fun () ->
                let z = (Scalar.Int64 <| int64 3) + upIndexed
                let scalarFunc = 
                    match z.[0] with
                    | Scalar _ -> invalidArg "UpIndexed addition" "can't get here"
                    | Func f -> f

                Expect.equal (scalarFunc.Invoke (Scalar.Float 2.)) (Scalar.Int 7) 
                    (sprintf "actual %A, expected %A failed" (scalarFunc.Invoke (Scalar.Float 2.)) (Scalar.Int 7) )

            testCase "multiplication left" <| fun () ->
                let z = upIndexed * (Scalar.Int64 <| int64 3)
                let scalarFunc = 
                    match z.[0] with
                    | Scalar _ -> invalidArg "UpIndexed addition" "can't get here"
                    | Func f -> f

                Expect.equal (scalarFunc.Invoke (Scalar.Float 2.)) (Scalar.Int 12) 
                    (sprintf "actual %A, expected %A failed" (scalarFunc.Invoke (Scalar.Float 2.)) (Scalar.Int 12) )

            testCase "multiplication right" <| fun () ->
                let z = (Scalar.Int64 <| int64 3) * upIndexed
                let scalarFunc = 
                    match z.[0] with
                    | Scalar _ -> invalidArg "UpIndexed addition" "can't get here"
                    | Func f -> f

                Expect.equal (scalarFunc.Invoke (Scalar.Float 2.)) (Scalar.Int 12) 
                    (sprintf "actual %A, expected %A failed" (scalarFunc.Invoke (Scalar.Float 2.)) (Scalar.Int 12) )

            testCase "subtraction left" <| fun () ->
                let z = upIndexed - (Scalar.Int64 <| int64 4)
                let scalarFunc = 
                    match z.[0] with
                    | Scalar _ -> invalidArg "UpIndexed addition" "can't get here"
                    | Func f -> f

                Expect.equal (scalarFunc.Invoke (Scalar.Float 6.)) (Scalar.Int 8) 
                    (sprintf "actual %A, expected %A failed" (scalarFunc.Invoke (Scalar.Float 6.)) (Scalar.Int 8) )

            testCase "subtraction right" <| fun () ->
                let z = (Scalar.Int64 <| int64 6) - upIndexed
                let scalarFunc = 
                    match z.[0] with
                    | Scalar _ -> invalidArg "UpIndexed addition" "can't get here"
                    | Func f -> f

                Expect.equal (scalarFunc.Invoke (Scalar.Float 2.)) (Scalar.Int 2) 
                    (sprintf "actual %A, expected %A failed" (scalarFunc.Invoke (Scalar.Float 2.)) (Scalar.Int 2) )

            testCase "division left" <| fun () ->
                let z = upIndexed / (Scalar.Int64 <| int64 4)
                let scalarFunc = 
                    match z.[0] with
                    | Scalar _ -> invalidArg "UpIndexed addition" "can't get here"
                    | Func f -> f

                Expect.equal (scalarFunc.Invoke (Scalar.Float 6.)) (Scalar.Int 3) 
                    (sprintf "actual %A, expected %A failed" (scalarFunc.Invoke (Scalar.Float 6.)) (Scalar.Int 3) )

            testCase "division right" <| fun () ->
                let z = (Scalar.Int64 <| int64 12) / upIndexed
                let scalarFunc = 
                    match z.[0] with
                    | Scalar _ -> invalidArg "UpIndexed addition" "can't get here"
                    | Func f -> f

                Expect.equal (scalarFunc.Invoke (Scalar.Float 3.)) (Scalar.Int 2) 
                    (sprintf "actual %A, expected %A failed" (scalarFunc.Invoke (Scalar.Float 3.)) (Scalar.Int 2) )
        ]

    [<Tests>]
    let genericArithmeticScalarFunc =
        
        testList "ScalarFunc" [
            testCase "scalar addition left" <| fun () ->
                let newFunc = Scalar.Int 2 + wFunc
                Expect.equal (newFunc.Invoke (Scalar.Float 2.)) (Scalar.Int 6)
                    (sprintf "actual %A, expected %A failed" (newFunc.Invoke (Scalar.Float 2.)) (Scalar.Int 6) )

            testCase "scalar addition right" <| fun () ->
                let newFunc = wFunc + Scalar.Int 2
                Expect.equal (newFunc.Invoke (Scalar.Float 2.)) (Scalar.Int 6)
                    (sprintf "actual %A, expected %A failed" (newFunc.Invoke (Scalar.Float 2.)) (Scalar.Int 6) )

            testCase "int addition left" <| fun () ->
                let newFunc = 2 + wFunc
                Expect.equal (newFunc.Invoke (Scalar.Float 2.)) (Scalar.Int 6)
                    (sprintf "actual %A, expected %A failed" (newFunc.Invoke (Scalar.Float 2.)) (Scalar.Int 6) )

            testCase "int addition right" <| fun () ->
                let newFunc = wFunc + 2
                Expect.equal (newFunc.Invoke (Scalar.Float 2.)) (Scalar.Int 6)
                    (sprintf "actual %A, expected %A failed" (newFunc.Invoke (Scalar.Float 2.)) (Scalar.Int 6) )

            testCase "int64 addition left" <| fun () ->
                let newFunc = int64 2 + wFunc
                Expect.equal (newFunc.Invoke (Scalar.Float 2.)) (Scalar.Int 6)
                    (sprintf "actual %A, expected %A failed" (newFunc.Invoke (Scalar.Float 2.)) (Scalar.Int 6) )

            testCase "int64 addition right" <| fun () ->
                let newFunc = wFunc + int64 2
                Expect.equal (newFunc.Invoke (Scalar.Float 2.)) (Scalar.Int 6)
                    (sprintf "actual %A, expected %A failed" (newFunc.Invoke (Scalar.Float 2.)) (Scalar.Int 6) )

            testCase "BigInteger addition left" <| fun () ->
                let newFunc = BigInteger 2 + wFunc
                Expect.equal (newFunc.Invoke (Scalar.Float 2.)) (Scalar.Int 6)
                    (sprintf "actual %A, expected %A failed" (newFunc.Invoke (Scalar.Float 2.)) (Scalar.Int 6) )

            testCase "BigInteger addition right" <| fun () ->
                let newFunc = wFunc + BigInteger 2
                Expect.equal (newFunc.Invoke (Scalar.Float 2.)) (Scalar.Int 6)
                    (sprintf "actual %A, expected %A failed" (newFunc.Invoke (Scalar.Float 2.)) (Scalar.Int 6) )

            testCase "float addition left" <| fun () ->
                let newFunc = 2. + wFunc
                Expect.equal (newFunc.Invoke (Scalar.Float 2.)) (Scalar.Int 6)
                    (sprintf "actual %A, expected %A failed" (newFunc.Invoke (Scalar.Float 2.)) (Scalar.Int 6) )

            testCase "float addition right" <| fun () ->
                let newFunc = wFunc + 2.
                Expect.equal (newFunc.Invoke (Scalar.Float 2.)) (Scalar.Int 6)
                    (sprintf "actual %A, expected %A failed" (newFunc.Invoke (Scalar.Float 2.)) (Scalar.Int 6) )

            testCase "scalar multiplication left" <| fun () ->
                let newFunc = Scalar.Int 2 * wFunc
                Expect.equal (newFunc.Invoke (Scalar.Float 2.)) (Scalar.Int 8)
                    (sprintf "actual %A, expected %A failed" (newFunc.Invoke (Scalar.Float 2.)) (Scalar.Int 8) )

            testCase "scalar multiplication right" <| fun () ->
                let newFunc = wFunc * Scalar.Int 2
                Expect.equal (newFunc.Invoke (Scalar.Float 2.)) (Scalar.Int 8)
                    (sprintf "actual %A, expected %A failed" (newFunc.Invoke (Scalar.Float 2.)) (Scalar.Int 8) )

            testCase "int multiplication left" <| fun () ->
                let newFunc = 2 * wFunc
                Expect.equal (newFunc.Invoke (Scalar.Float 2.)) (Scalar.Int 8)
                    (sprintf "actual %A, expected %A failed" (newFunc.Invoke (Scalar.Float 2.)) (Scalar.Int 8) )

            testCase "int multiplication right" <| fun () ->
                let newFunc = wFunc * 2
                Expect.equal (newFunc.Invoke (Scalar.Float 2.)) (Scalar.Int 8)
                    (sprintf "actual %A, expected %A failed" (newFunc.Invoke (Scalar.Float 2.)) (Scalar.Int 8) )

            testCase "int64 multiplication left" <| fun () ->
                let newFunc = int64 2 * wFunc
                Expect.equal (newFunc.Invoke (Scalar.Float 2.)) (Scalar.Int 8)
                    (sprintf "actual %A, expected %A failed" (newFunc.Invoke (Scalar.Float 2.)) (Scalar.Int 8) )

            testCase "int64 multiplication right" <| fun () ->
                let newFunc = wFunc * int64 2
                Expect.equal (newFunc.Invoke (Scalar.Float 2.)) (Scalar.Int 8)
                    (sprintf "actual %A, expected %A failed" (newFunc.Invoke (Scalar.Float 2.)) (Scalar.Int 8) )

            testCase "BigInteger multiplication left" <| fun () ->
                let newFunc = BigInteger 2 * wFunc
                Expect.equal (newFunc.Invoke (Scalar.Float 2.)) (Scalar.Int 8)
                    (sprintf "actual %A, expected %A failed" (newFunc.Invoke (Scalar.Float 2.)) (Scalar.Int 8) )

            testCase "BigInteger multiplication right" <| fun () ->
                let newFunc = wFunc * BigInteger 2
                Expect.equal (newFunc.Invoke (Scalar.Float 2.)) (Scalar.Int 8)
                    (sprintf "actual %A, expected %A failed" (newFunc.Invoke (Scalar.Float 2.)) (Scalar.Int 8) )

            testCase "float multiplication left" <| fun () ->
                let newFunc = 2. * wFunc
                Expect.equal (newFunc.Invoke (Scalar.Float 2.)) (Scalar.Int 8)
                    (sprintf "actual %A, expected %A failed" (newFunc.Invoke (Scalar.Float 2.)) (Scalar.Int 8) )

            testCase "float multiplication right" <| fun () ->
                let newFunc = wFunc * 2.
                Expect.equal (newFunc.Invoke (Scalar.Float 2.)) (Scalar.Int 8)
                    (sprintf "actual %A, expected %A failed" (newFunc.Invoke (Scalar.Float 2.)) (Scalar.Int 8) )

            testCase "scalar subtraction left" <| fun () ->
                let newFunc = Scalar.Int 12 - wFunc
                Expect.equal (newFunc.Invoke (Scalar.Float 2.)) (Scalar.Int 8)
                    (sprintf "actual %A, expected %A failed" (newFunc.Invoke (Scalar.Float 2.)) (Scalar.Int 8) )

            testCase "scalar subtraction right" <| fun () ->
                let newFunc = wFunc - Scalar.Int 2
                Expect.equal (newFunc.Invoke (Scalar.Float 2.)) (Scalar.Int 2)
                    (sprintf "actual %A, expected %A failed" (newFunc.Invoke (Scalar.Float 2.)) (Scalar.Int 2) )

            testCase "int subtraction left" <| fun () ->
                let newFunc = 12 - wFunc
                Expect.equal (newFunc.Invoke (Scalar.Float 2.)) (Scalar.Int 8)
                    (sprintf "actual %A, expected %A failed" (newFunc.Invoke (Scalar.Float 2.)) (Scalar.Int 8) )

            testCase "int subtraction right" <| fun () ->
                let newFunc = wFunc - 2
                Expect.equal (newFunc.Invoke (Scalar.Float 2.)) (Scalar.Int 2)
                    (sprintf "actual %A, expected %A failed" (newFunc.Invoke (Scalar.Float 2.)) (Scalar.Int 2) )

            testCase "int64 subtraction left" <| fun () ->
                let newFunc = int64 12 - wFunc
                Expect.equal (newFunc.Invoke (Scalar.Float 2.)) (Scalar.Int 8)
                    (sprintf "actual %A, expected %A failed" (newFunc.Invoke (Scalar.Float 2.)) (Scalar.Int 8) )

            testCase "int64 subtraction right" <| fun () ->
                let newFunc = wFunc - int64 2
                Expect.equal (newFunc.Invoke (Scalar.Float 2.)) (Scalar.Int 2)
                    (sprintf "actual %A, expected %A failed" (newFunc.Invoke (Scalar.Float 2.)) (Scalar.Int 2) )

            testCase "BigInteger subtraction left" <| fun () ->
                let newFunc = BigInteger 12 - wFunc
                Expect.equal (newFunc.Invoke (Scalar.Float 2.)) (Scalar.Int 8)
                    (sprintf "actual %A, expected %A failed" (newFunc.Invoke (Scalar.Float 2.)) (Scalar.Int 8) )

            testCase "BigInteger subtraction right" <| fun () ->
                let newFunc = wFunc - BigInteger 2
                Expect.equal (newFunc.Invoke (Scalar.Float 2.)) (Scalar.Int 2)
                    (sprintf "actual %A, expected %A failed" (newFunc.Invoke (Scalar.Float 2.)) (Scalar.Int 2) )

            testCase "float subtraction left" <| fun () ->
                let newFunc = 12. - wFunc
                Expect.equal (newFunc.Invoke (Scalar.Float 2.)) (Scalar.Int 8)
                    (sprintf "actual %A, expected %A failed" (newFunc.Invoke (Scalar.Float 2.)) (Scalar.Int 8) )

            testCase "float subtraction right" <| fun () ->
                let newFunc = wFunc - 2.
                Expect.equal (newFunc.Invoke (Scalar.Float 2.)) (Scalar.Int 2)
                    (sprintf "actual %A, expected %A failed" (newFunc.Invoke (Scalar.Float 2.)) (Scalar.Int 2) )

            testCase "scalar division left" <| fun () ->
                let newFunc = Scalar.Int 12 / wFunc
                Expect.equal (newFunc.Invoke (Scalar.Float 2.)) (Scalar.Int 3)
                    (sprintf "actual %A, expected %A failed" (newFunc.Invoke (Scalar.Float 2.)) (Scalar.Int 3) )

            testCase "scalar division right" <| fun () ->
                let newFunc = wFunc / Scalar.Int 2
                Expect.equal (newFunc.Invoke (Scalar.Float 2.)) (Scalar.Int 2)
                    (sprintf "actual %A, expected %A failed" (newFunc.Invoke (Scalar.Float 2.)) (Scalar.Int 2) )

            testCase "int division left" <| fun () ->
                let newFunc = 12 / wFunc
                Expect.equal (newFunc.Invoke (Scalar.Float 2.)) (Scalar.Int 3)
                    (sprintf "actual %A, expected %A failed" (newFunc.Invoke (Scalar.Float 2.)) (Scalar.Int 3) )

            testCase "int division right" <| fun () ->
                let newFunc = wFunc / 2
                Expect.equal (newFunc.Invoke (Scalar.Float 2.)) (Scalar.Int 2)
                    (sprintf "actual %A, expected %A failed" (newFunc.Invoke (Scalar.Float 2.)) (Scalar.Int 2) )

            testCase "int64 division left" <| fun () ->
                let newFunc = int64 12 / wFunc
                Expect.equal (newFunc.Invoke (Scalar.Float 2.)) (Scalar.Int 3)
                    (sprintf "actual %A, expected %A failed" (newFunc.Invoke (Scalar.Float 2.)) (Scalar.Int 3) )

            testCase "int64 division right" <| fun () ->
                let newFunc = wFunc / int64 2
                Expect.equal (newFunc.Invoke (Scalar.Float 2.)) (Scalar.Int 2)
                    (sprintf "actual %A, expected %A failed" (newFunc.Invoke (Scalar.Float 2.)) (Scalar.Int 2) )

            testCase "BigInteger division left" <| fun () ->
                let newFunc = BigInteger 12 / wFunc
                Expect.equal (newFunc.Invoke (Scalar.Float 2.)) (Scalar.Int 3)
                    (sprintf "actual %A, expected %A failed" (newFunc.Invoke (Scalar.Float 2.)) (Scalar.Int 3) )

            testCase "BigInteger division right" <| fun () ->
                let newFunc = wFunc / BigInteger 2
                Expect.equal (newFunc.Invoke (Scalar.Float 2.)) (Scalar.Int 2)
                    (sprintf "actual %A, expected %A failed" (newFunc.Invoke (Scalar.Float 2.)) (Scalar.Int 2) )

            testCase "float division left" <| fun () ->
                let newFunc = 12. / wFunc
                Expect.equal (newFunc.Invoke (Scalar.Float 2.)) (Scalar.Int 3)
                    (sprintf "actual %A, expected %A failed" (newFunc.Invoke (Scalar.Float 2.)) (Scalar.Int 3) )

            testCase "float division right" <| fun () ->
                let newFunc = wFunc / 2.
                Expect.equal (newFunc.Invoke (Scalar.Float 2.)) (Scalar.Int 2)
                    (sprintf "actual %A, expected %A failed" (newFunc.Invoke (Scalar.Float 2.)) (Scalar.Int 2) )
        ]

        //to do: remainder
        //to do: exponentiation
        //to do: integer overflow on addition
        //to do: integer overflow on multiplication
        //to do: integer overflow on exponentiation
