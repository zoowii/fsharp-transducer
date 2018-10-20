fsharp-transducer
=========================

demo transducer implementation for F#


# Usage

	open fsharp_transducer

	open Folds

	let strTransducer (prefix: string) (xf: Folds.Reducer<string, string list>) (data: Folds.TransducerArgs<'a, string list>) =
		match data with
		| Folds.Init  -> Folds.Reduced []
		| Folds.Complete acc -> Folds.Reduced acc
		| Folds.Step(item, acc) -> 
			let itemStr = item.ToString()
			xf (Folds.Step(prefix + ":" + itemStr, acc))

	let conj b a = [a] @ b
    let list1 = [1; 2; 3; 4; 5]
    let a = Folds.transduce (strTransducer "hi") conj [] list1
    let mystrTrans1 = strTransducer "hi1"
    let mystrTrans2 = strTransducer "hi2"
    let b = Folds.transduce (Folds.comp mystrTrans1 mystrTrans2) conj [] list1
    let c = Folds.eduction (strTransducer "hi") list1
    let d = Folds.into ["hello"] (strTransducer "hi") list1
    printfn "a=%A, b=%A, c=%A, d=%A" a b c d