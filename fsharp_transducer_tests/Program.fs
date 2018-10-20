open System
open fsharp_transducer.Folds

let strTransducer (prefix: string) (xf: Reducer<string, string list>) (data: TransducerArgs<'a, string list>) =
    match data with
    | Init  -> Reduced []
    | Complete acc -> Reduced acc
    | Step(item, acc) -> 
        let itemStr = item.ToString()
        xf (Step(prefix + ":" + itemStr, acc))

[<EntryPoint>]
let main argv =
    printfn "Hello World from F#!"
    let conj b a = [a] @ b
    let list1 = [1; 2; 3; 4; 5]
    let a = transduce (strTransducer "hi") conj [] list1
    let mystrTrans1 = strTransducer "hi1"
    let mystrTrans2 = strTransducer "hi2"
    let b = transduce (comp mystrTrans1 mystrTrans2) conj [] list1
    let c = eduction (strTransducer "hi") list1
    let d = into ["hello"] (strTransducer "hi") list1
    printfn "a=%A, b=%A, c=%A, d=%A" a b c d
    0 // return an integer exit code
