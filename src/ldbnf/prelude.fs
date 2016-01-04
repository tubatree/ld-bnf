namespace Bnf

module prelude = 
    let (|?) = defaultArg

    let (>=>) a b x =
      match (a x) with
        | Some x -> b x
        | None -> None

    let (>>|) a b x =
      match (a x) with
        | Some x -> Some(b x)
        | None -> None

    let (>>=) a b = Option.bind b a

    //Generic list that allows optional values to collapse out
    type OptionlistBuilder<'a> () =
        member this.Bind(m, f) = m |> List.collect f
        member this.Zero() = []
        member this.Yield(x:'a option) = match x with | Some x -> [x] | None -> [] //collapse None into an empty list
        member this.Yield(x:'a) = [x]
        member this.YieldFrom(x:'a list) = x
        member this.ReturnFrom(x:'a list option) = match x with | Some x -> x | None -> []  //naughty but convenient
        member this.Combine (a,b) = List.concat [a;b]
        member this.Delay(f) = f()

    open Microsoft.FSharp.Reflection

    //get a string reprasentaiton of a DU
    let toString (x:'a) = 
      match FSharpValue.GetUnionFields(x, typeof<'a>) with
        | case, _ -> case.Name

    let fromString<'a> (s:string) =
      match FSharpType.GetUnionCases typeof<'a> |> Array.filter (fun case -> case.Name = s) with
        |[|case|] -> Some(FSharpValue.MakeUnion(case,[||]) :?> 'a)
        |_ -> None


    let inline name arg =
      ( ^a : (member Name : string) arg)

    let inline (|HasName|_|) n x =
      if (name x) = n then Some(x)
      else None

    let inline hasName s x = name x = s

    let inline outputclass arg = ( ^a : (member Outputclass : string) arg)

    let inline (|HasOutputClass|_|) (n:string) x =
      let cs = (outputclass x).Split [|' '|]
      if (cs |> Array.exists (fun  c -> c = n)) then Some(x)
      else None
