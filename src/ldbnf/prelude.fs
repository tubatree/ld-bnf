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

    let (~~) o l =
      match o with
        | Some i -> i :: l
        | None -> l

    type ListBuilder<'a> () =
        member this.Bind(m, f) = 
            m |> List.collect f

        member this.Zero() = 
            printfn "Zero"
            []

        member this.Yield(x:'a option) = match x with | Some x -> [x] | None -> []

        member this.Yield(x:'a) = 
            printfn "Yield an unwrapped %A as a list" x
            [x]

        member this.Combine (a,b) = 
            printfn "combining %A and %A" a b 
            List.concat [a;b]

        member this.Delay(f) = 
            printfn "Delay"
            f()

    open Microsoft.FSharp.Reflection

    let toString (x:'a) = 
      match FSharpValue.GetUnionFields(x, typeof<'a>) with
        | case, _ -> case.Name

    let fromString<'a> (s:string) =
      match FSharpType.GetUnionCases typeof<'a> |> Array.filter (fun case -> case.Name = s) with
        |[|case|] -> Some(FSharpValue.MakeUnion(case,[||]) :?> 'a)
        |_ -> None
