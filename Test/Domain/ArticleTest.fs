module Domain.``Article should``

open Xunit
open FsUnit.Xunit
open Domain
open Domain.Projection

let Given = project Article.articleProjection
let When = Article.exec
let Then expectation actual = actual |> expectation

let article =
  { Title = "Title#1"
    Content = "Some nice content"
    OwnerId = JournalistId.NewGuid() }

[<Fact>]
let ``draft from drafted event`` () =
  Given []
  |> When(Article.Command.Draft article)
  |> Then should equal [ article |> Article.Event.Drafted ]

[<Fact>]
let ``emit already drafted event when drafted twice`` () =
  Given [ article |> Article.Event.Drafted ]
  |> When(article |> Article.Command.Draft)
  |> Then should equal [ Article.Event.AlreadyDrafted ]
