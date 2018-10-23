fsharp-transducer
=========================

demo transducer implementation for F#

# Introduction

* This is a F# implementation of Clojure's transducer lib
* Clojure's transducer: [https://clojure.org/reference/transducers](https://clojure.org/reference/transducers)

# Usage

	open fsharp_transducer.Folds

	let strTransducer (prefix: string) (xf: Reducer<string, string list>) (data: TransducerArgs<'a, string list>) =
		match data with
		| Init  -> Reduced []
		| Complete acc -> Reduced acc
		| Step(item, acc) -> 
			let itemStr = item.ToString()
			xf (Step(prefix + ":" + itemStr, acc))

	let conj b a = [a] @ b
    let isOdd v = (v % 2) <> 0
    let list1 = [1; 2; 3; 4; 5]
    let a = transduce (strTransducer "hi") conj [] list1 // ["hi:5"; "hi:4"; "hi:3"; "hi:2"; "hi:1"]
    let mystrTrans1 = strTransducer "hi1"
    let mystrTrans2 = strTransducer "hi2"
    let b = transduce (comp mystrTrans1 mystrTrans2) conj [] list1 // ["hi2:hi1:5"; "hi2:hi1:4"; "hi2:hi1:3"; "hi2:hi1:2"; "hi2:hi1:1"]
    let c = sequence (strTransducer "hi") list1 // sequence returns a cached lazy seq, c=cached-seq("hi:1", "hi:2", "hi:3", "hi:4", "hi:5")
    Seq.iteri (fun index item ->
        printfn "c[%d]=%A" index item
    ) c
    Seq.iteri (fun index item ->
        printfn "c[%d]=%A" index item
    ) c
    let c2 = eduction (strTransducer "hi") list1 // eduction returns a lazy seq, c2=seq("hi:1", "hi:2", "hi:3", "hi:4", "hi:5")
    Seq.iteri (fun index item ->
        printfn "c2[%d]=%A" index item
    ) c2
    Seq.iteri (fun index item ->
        printfn "c2[%d]=%A" index item
    ) c2
    let d = into ["hello"] (strTransducer "hi") list1 // ["hello"; "hi:1"; "hi:2"; "hi:3"; "hi:4"; "hi:5"]
    let e = into [0] (filter isOdd) list1 // [0; 1; 3; 5]
    let f = into ["map-demo"] (comp mystrTrans1 (map (fun x->"map-"+x))) list1 // ["map-demo"; "map-hi1:1"; "map-hi1:2"; "map-hi1:3"; "map-hi1:4"; "map-hi1:5"]
    let take3 = into [] (take 3) list1 // [1; 2; 3]
    let skip2 = into [] (skip 2) list1 // [3; 4; 5]
    let partitionByOdd = transduce (partition isOdd) (fun pr item->pr) ([], []) list1 // ([1; 3; 5], [2; 4])
    let distinctValues = into [] distinct [1; 2; 3; 2; 1] // [1; 2; 3]
