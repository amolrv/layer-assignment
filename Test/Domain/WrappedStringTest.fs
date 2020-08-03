module Domain.WrappedString.Test

open FsUnit.Xunit
open Xunit

[<Fact>]
let ``should return none when input is whitespace`` () =
  nonEmptyString "  " |> Option.isNone |> should equal true

[<Fact>]
let ``should return none when input is null`` () =
  nonEmptyString null |> Option.isNone |> should equal true

[<Fact>]
let ``should return none when input is empty`` () =
  nonEmptyString "" |> Option.isNone |> should equal true

[<Fact>]
let ``should return some value when input is valid`` () =
  let testInput = "Some valid no empty\n input"
  let result = nonEmptyString testInput
  result |> Option.isSome |> should equal true
  result |> Option.get |> value |> should equal testInput

[<Fact>]
let ``should trim whitespace`` () =
  let result = nonEmptyString " Some Text " |> Option.get |> value
  result |> should equal "Some Text"

