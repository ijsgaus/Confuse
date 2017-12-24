namespace T1

type SMnd<'t> =
        abstract Null : 't
        abstract Sum : 't -> 't -> 't

module Adding =
    [<Struct>]
    type AddMonoid = 
        interface SMnd<int> with
            member this.Null = 0
            member this.Sum a b = a + b

module Multiply = 
    [<Struct>]
    type ProductMonoid = 
        interface SMnd<int> with
            member this.Null = 1
            member this.Sum a b = a * b
    
module Proc =
    let reduce<'tr, 't when 'tr : struct  and 'tr : (new: unit -> 'tr) and 'tr :> SMnd<'t>> (a: 't seq) =
        let instance : SMnd<'t> = Unchecked.defaultof<'tr> :> SMnd<'t>;
        a |> Seq.fold (instance.Sum) (instance.Null)

//let a = Proc.reduce<Multiply.ProductMonoid, _> [1;2;3;4]
//let b = Proc.reduce<Adding.AddMonoid, _> [1;2;3;4]