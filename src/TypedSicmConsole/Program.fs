namespace TypedSicm

open CommandLine
open Prelude
open System
open Ch1_LagrangianMechanics

module console1 =

    [<EntryPoint>]
    let main argv = 
        //printfn "%A" argv

        //let parsedCommand = parse (System.Reflection.Assembly.GetExecutingAssembly().GetName().Name) argv

        //match parsedCommand.Error with
        //    | Some e -> 
        //        printfn "%s" <| formatExceptionDisplay e
        //        printfn "%s" parsedCommand.Usage
        //    | None -> 
        //        printfn "%A" parsedCommand

        printfn "test1: %f" <| S4ComputingActions.test1()
        printfn "test2: %f" <| S4ComputingActions.test2()

        let machineEpsilon =
            let rec loop e =
                if 1.0 = (e + 1.0) then
                    2.0 * e
                else
                        loop (e / 2.0)

            loop 1.0

        printfn "machine epsilon: %s" <| machineEpsilon.ToString()

        printfn "test3: %A" <| S4ComputingActions.test3()

        printfn "Hit any key to exit."
        System.Console.ReadKey() |> ignore
        0
