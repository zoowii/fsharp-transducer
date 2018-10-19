open System
open fsharp_transducer

open Folds

let strTransducer (prefix: string) (xf: Folds.Reducer<string, string list>) (data: Folds.TransducerArgs<'a, string list>) =
    match data with
    | Folds.Init  -> Folds.Reduced []
    | Folds.Complete acc -> Folds.Reduced acc
    | Folds.Step(item, acc) -> 
        let itemStr = item.ToString()
        xf (Folds.Step(prefix + ":" + itemStr, acc))

[<EntryPoint>]
let main argv =
    printfn "Hello World from F#!"
    let conj b a = b @ [a]
    let list1 = [1; 2; 3; 4; 5]
    let a = Folds.transduce (strTransducer "hi") conj [] list1
    let mystrTrans1 = strTransducer "hi1"
    let mystrTrans2 = strTransducer "hi2"
    let b = Folds.transduce (Folds.comp mystrTrans1 mystrTrans2) conj [] list1
    printfn "a=%A, b=%A" a b
    0 // return an integer exit code
