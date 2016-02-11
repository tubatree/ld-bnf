namespace Bnf

//lift2 : ('a -> 'b -> 'c) -> 'a option -> 'b option -> 'c option
module Option =
  let lift2 f x y =
    match x with
    | None -> None
    | Some x' ->
      match y with
      | None -> None
            | Some y' -> Some <| f x' y'

module StringBuilder =
  open System

  type StringBuilder = B of (Text.StringBuilder -> unit)

  let build (B f) =
    let b = new Text.StringBuilder()
    do f b
    b.ToString ()

  type StringBuilderM () =
    let (!) = function B f -> f
    member __.Yield (txt : string) = B(fun b -> b.Append txt |> ignore)
    member __.Yield (c : char) = B(fun b -> b.Append c |> ignore)
    member __.YieldFrom f = f : StringBuilder

    member __.Combine(f,g) = B(fun b -> !f b; !g b)
    member __.Delay f = B(fun b -> !(f ()) b) : StringBuilder
    member __.Zero () = B(fun _ -> ())
    member __.For (xs : 'a seq, f : 'a -> StringBuilder) =
                        B(fun b ->
                            let e = xs.GetEnumerator ()
                            while e.MoveNext() do
                                !(f e.Current) b)
    member __.While (p : unit -> bool, f : StringBuilder) =
                        B(fun b -> while p () do !f b)

  let string = new StringBuilderM ()

module guid =
  open System

  let webguid () =
    let guid = Guid.NewGuid()
    let id = Convert.ToBase64String(guid.ToByteArray())
    id.Replace("/", "_").Replace("+", "-").Substring(0, 22)


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

    let (<!>) a b = Option.map b a

    //Generic list that allows optional values to collapse out
    type OptionlistBuilder<'a> () =
        member this.Bind(m, f) = m |> List.collect f
        member this.Zero() = []
        member this.Yield(x:'a option) = match x with | Some x -> [x] | None -> [] //collapse None into an empty list
        member this.Yield(x:'a) = [x]
        member this.YieldFrom(x:'a list) = x
        member this.Combine (a,b) = List.concat [a;b]
        member this.Delay(f) = f()

    let unwrap (x:'a list option) =
      match x with
        | Some x -> x
        | None -> []

    open Microsoft.FSharp.Reflection

    //get a string reprasentaiton of a DU
    let toString (x:'a) = 
      match FSharpValue.GetUnionFields(x, typeof<'a>) with
        | case, _ -> case.Name

    let fromString<'a> (s:string) =
      match FSharpType.GetUnionCases typeof<'a> |> Array.filter (fun case -> case.Name = s) with
        |[|case|] -> Some(FSharpValue.MakeUnion(case,[||]) :?> 'a)
        |_ -> None

    let typename o = match o.GetType().BaseType.Name, o.GetType().Name with
                     | "Object", n -> n
                     | n, _ -> n

    //deal with names and output class properties on type providers
    let inline name arg =
        ( ^a : (member Name : string) arg)

    let inline hasName s x =
        if (name x) = s then Some(x)
        else None

    let inline (|HasName|_|) n x = hasName n x

    let inline outputclasso  arg =
        ( ^a : (member Outputclass : Option<string>) arg)

    let inline outputclass arg =
        ( ^a : (member Outputclass : string) arg) 

    let overlap s (o:string) x =
      if (o.Split [|' '|] |> Array.exists (fun  c -> c = s)) then Some(x)
      else None

    let inline hasOutputclass (s:string) x = overlap s (outputclass x) x

    let inline hasOutputclasso (s:string) x =
        match outputclasso x with
          | Some o -> overlap s o x
          | None -> None

    let inline (|HasOutputClass|_|) (n:string) x = hasOutputclass n x

    let inline (|HasOutputClasso|_|) (n:string) x = hasOutputclasso n x

    let titleCase s =
      let culture = System.Globalization.CultureInfo.GetCultureInfo("en-US")
      culture.TextInfo.ToTitleCase s

    let firstupper (s:string) =
      s.Substring(0,1).ToUpper() + s.Substring(1);

    let splitCamelCase (s:string)=
      System.Text.RegularExpressions.Regex.Replace(s, "([a-z](?=[A-Z])|[A-Z](?=[A-Z][a-z]))", "$1-").ToLower()
