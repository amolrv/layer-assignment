[<RequireQualifiedAccess>]
module Types.WrappedString

type IWrappedString =
  abstract Value : string

let private create sanitize isValid ctor (input : string) =
  if isNull input then
    None
  else
    let input' = sanitize input
    if isValid input' then Some(ctor input') else None

let apply fn (wrappedString : IWrappedString) = wrappedString.Value |> fn
let value wrappedString = wrappedString |> apply id

type NonEmptyString =
  private
  | NonEmptyString of string
  interface IWrappedString with
    member this.Value =
      let (NonEmptyString value) = this
      value

let private isNonEmpty input =
  not (System.String.IsNullOrWhiteSpace(input))

let private trim (s : string) = s.Trim()

let nonEmptyString = NonEmptyString |> create trim isNonEmpty
