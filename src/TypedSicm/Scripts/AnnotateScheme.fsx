open System
open System.IO

let fileName = "linearInterpolants"

let filePath =  sprintf @"E:\GitRepos\TypedSicm\src\TypedSicm\Scripts\%s.scm" fileName
let outPath = sprintf  @"E:\GitRepos\TypedSicm\src\TypedSicm\Scripts\%sAnnotated.scm" fileName

let data = File.ReadAllLines filePath

let lookupAnnotation =
    [|
        0, "0"
        1, "1"
        2, "2"
        3, "3"
        4, "4"
        5, "5"
        6, "6"
        7, "7"
        8, "8"
        9, "9"
        10, "A"
        11, "B"
        12, "C"
        13, "D"
        14, "E"
        15, "F"
        16, "G"
        17, "H"
        18, "I"
        19, "J"
        20, "K"
        21, "L"
        22, "M"
    |]
    |> dict

let updated =
    data
    |> Array.map (fun x -> 
        let rec loop (value : string) =
            let i =  value.IndexOf("-")
            if i = - 1 then
                value
            else
                let y = value.Substring(i + 1, 1)
                let sub = sprintf "-%s" y
                loop <| value.Replace(sub, y.ToUpper())
        loop x
    )
let _,  _, output = 
    ((0, '(', []), updated)
    ||> Array.fold (fun (count, last, out) t ->
        let count, last, annotation = 
            ((count, last, String.Empty), t.ToCharArray()) 
            ||> Array.fold (fun (count, last, ann) x -> 
                match x, last with
                | '(', ')' -> 
                    count, '(', ann + lookupAnnotation.[count] 
                | '(', _ -> 
                    (count + 1), '(', ann + lookupAnnotation.[count + 1] 
                | ')', ')' -> 
                    (count - 1), ')', ann + lookupAnnotation.[count - 1]
                | ')', _ -> 
                    count, ')', ann + lookupAnnotation.[count]
                | _ ->
                    count, last, ann + " "
            )

        (count, last, t::annotation::out)
    )

File.WriteAllLines(outPath, output |> List.rev)
