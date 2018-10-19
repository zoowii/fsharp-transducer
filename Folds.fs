namespace fsharp_transducer

open System
open System.Collections.Generic
open Microsoft.FSharp.Collections

module Folds =

    type ReducingFunc<'a, 'b> = 'b -> 'a -> 'b
    type ReducedType<'a> = Reduced of 'a | Continue of 'a
    type TransducerArgs<'a, 'b> = Init | Complete of 'b | Step of 'a * 'b
    type Reducer<'a, 'b> = TransducerArgs<'a, 'b> -> ReducedType<'b>
    type Transducer<'a, 'b, 'c> = Reducer<'c, 'b> -> Reducer<'a, 'b>

    
    let comp transducer1 transducer2 xf = fun args ->
        let xf2 args2 = transducer2 xf args2
        transducer1 xf2 args

    let transduce (transducer: Transducer<'a, 'b, 'c>) (reducingFunc: ReducingFunc<'c, 'b>) (initial: 'b) (source: #seq<'a>) : 'b =
        use enumer = source.GetEnumerator()

        let xf (transArgs: TransducerArgs<'c, 'b>) = 
            match transArgs with
            | Init -> Continue initial
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
        loop initial
    
    let inline foldl (stepfn:'b->'a->'b)(acc:'b)(coll:#seq<'a>) : 'b = 
        use enumer = coll.GetEnumerator()
        let rec loop acc' = 
            match enumer.MoveNext() with
            | false     ->  acc'  
            | true      ->  loop ( stepfn acc' enumer.Current ) 
        loop acc 
        
    // TODO: sequence, eduction, into
