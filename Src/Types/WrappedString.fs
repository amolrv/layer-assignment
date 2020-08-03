namespace Types

type IWrappedString =
  abstract Value : string

type NonEmptyString =
  private
  | NonEmptyString of string
  interface IWrappedString with
    member this.Value =
      let (NonEmptyString value) = this
      value

[<RequireQualifiedAccess>]
module WrappedString =
  let private create sanitize isValid ctor (input : string) =
    if isNull input then
      None
    else
      let input' = sanitize input
      if isValid input' then Some(ctor input') else None

  let apply fn (wrappedString : IWrappedString) = wrappedString.Value |> fn
  let value wrappedString = wrappedString |> apply id

  let nonEmptyString =
    let isNonEmpty input =
      not (System.String.IsNullOrWhiteSpace(input))

    let trim (s : string) = s.Trim()
    NonEmptyString |> create trim isNonEmpty
