namespace fsharp_transducer

open System
open System.Collections.Generic
open Microsoft.FSharp.Collections

module Folds =

    type ReducingFunc<'a, 'b> = 'b -> 'a -> 'b
    type ReducedType<'a> = Reduced of 'a | Continue of 'a | Ignore of 'a
    type TransducerArgs<'a, 'b> = Init | Complete of 'b | Step of 'a * 'b
    type Reducer<'a, 'b> = TransducerArgs<'a, 'b> -> ReducedType<'b>
    type Transducer<'a, 'b, 'c> = Reducer<'c, 'b> -> Reducer<'a, 'b>

    let isReduced (value: ReducedType<'a>) =
        match value with
        | Reduced _ -> true
        | _ -> false
    
    let comp transducer1 transducer2 xf = fun args ->
        let xf2 args2 = transducer2 xf args2
        transducer1 xf2 args

    let transduce (transducer: Transducer<'a, 'b, 'c>) (reducingFunc: ReducingFunc<'c, 'b>) (initial: 'b) (source: #seq<'a>) : 'b =
        use enumer = source.GetEnumerator()

        let xf (transArgs: TransducerArgs<'c, 'b>) = 
            match transArgs with
            | Init -> Continue initial // TODO: use transducer's init value if not provided
            | Complete acc -> Reduced acc
            | Step(input, acc) -> Continue (reducingFunc acc input)
        
        let rec loop (state_result: 'b) =
            match enumer.MoveNext() with
            | false -> 
                state_result
            | true      ->
                let cur = enumer.Current
                let step = Step(cur, state_result)
                let result = transducer xf step
                match result with
                | Reduced x -> x
                | Continue x -> loop x
                | Ignore x -> loop x
        loop initial

    let into result transducer source =
        let reducingFunc acc input = acc @ [input]
        transduce transducer reducingFunc result source

    let eduction (transducer: Transducer<'a, 'b, 'c>) (source: #seq<'a>) =
        use enumer = source.GetEnumerator()
        let reducingFunc acc input = [input]
        let xf (transArgs: TransducerArgs<'c, 'b>) = 
            match transArgs with
            | Init -> Continue []
            | Complete acc -> Reduced []
            | Step(input, acc) ->
                Continue (reducingFunc acc input)
        let rec loop (state_result: 'b) =
            seq {
                match enumer.MoveNext() with
                | false -> ignore null
                | true      ->
                    let cur = enumer.Current
                    let step = Step(cur, state_result)
                    let result = transducer xf step
                    match result with
                    | Reduced x ->
                        yield! x
                    | Continue x -> 
                        yield! x
                        yield! (loop x)
                    | Ignore x ->
                        loop x |> ignore
            }
        loop []

    let sequence = eduction // sequence now plays like eduction. TODO: cache its result
    
    let inline foldl (stepfn:'b->'a->'b)(acc:'b)(coll:#seq<'a>) : 'b = 
        use enumer = coll.GetEnumerator()
        let rec loop acc' = 
            match enumer.MoveNext() with
            | false     ->  acc'  
            | true      ->  loop ( stepfn acc' enumer.Current ) 
        loop acc 
        
    let filter predictFn xf data =
        match data with
        | Init  -> Reduced []
        | Complete acc -> Reduced acc
        | Step(item, acc) -> 
            if (predictFn item) then
                xf (Step(item, acc))
            else
                Ignore acc

    let map mapFn xf data =
        match data with
        | Init  -> Reduced []
        | Complete acc -> Reduced acc
        | Step(item, acc) -> 
            xf (Step((mapFn item), acc))

    let take n =
        let mutable count = 0
        fun xf data ->
            match data with
            | Init  -> Reduced []
            | Complete acc -> Reduced acc
            | Step(item, acc) -> 
                if count >= n then
                    Reduced acc
                else
                    count <- count + 1
                    xf (Step(item, acc))

    let skip n =
        let mutable skippedCount = 0
        fun xf data ->
            match data with
            | Init  -> Reduced []
            | Complete acc -> Reduced acc
            | Step(item, acc) -> 
                if skippedCount < n then
                    skippedCount <- skippedCount + 1
                    Ignore acc
                else
                    xf (Step(item, acc))
    let distinct<'a> =
        let exists = HashSet<'a>()
        fun xf data ->
            match data with
            | Init  -> Reduced []
            | Complete acc -> Reduced acc
            | Step(item, acc) -> 
                if exists.Contains(item) then
                    Ignore acc
                else
                    exists.Add(item) |> ignore
                    xf (Step(item, acc))
    let partition (predict: 'a -> bool) xf data =
        match data with
        | Init  -> Reduced ([], [])
        | Complete acc -> Reduced acc
        | Step(item, acc) -> 
            match acc with
            | (trueAcc, falseAcc) ->
                match predict item with
                | true -> xf (Step(item, (trueAcc @[item], falseAcc)))
                | false -> xf (Step(item, (trueAcc, falseAcc @ [item])))
    