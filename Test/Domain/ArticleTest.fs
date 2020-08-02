module Domain.Article

open Domain.Article
open FsUnit.Xunit
open Xunit

let Given = id
let When = articleEventProducer
let Then expectation actual = actual |> expectation

let empty a = Empty.Matches(a) |> should be True

let article =
  { Title = "Title#1"
    Content = "Some nice content"
    CreatorId = JournalistId.NewGuid() }

let change =
  { Title = "new title"
    Content = "new fancy content"
    CreatorId = article.CreatorId }

let copywriter1 = CopyWriterId.NewGuid()
let copywriter2 = CopyWriterId.NewGuid()

module DraftArticle =
  [<Fact>]
  let ``should draft from drafted event`` () =
    Given []
    |> When(Draft article)
    |> Then should equal [ Drafted article ]

  [<Fact>]
  let ``should emit already drafted event when article is drafted already`` () =
    Given [ Drafted article ]
    |> When(Draft article)
    |> Then should equal [ AlreadyDrafted ]

module ChangeContent =
  [<Fact>]
  let ``should return article was not present`` () =
    Given []
    |> When(ChangeContent change)
    |> Then should equal [ ArticleWasNotPresent ]

  [<Fact>]
  let ``should update content when article is inDraft state`` () =
    Given [ Drafted article ]
    |> When(ChangeContent change)
    |> Then should equal [ ContentUpdated("new title", "new fancy content") ]

  [<Fact>]
  let ``should not change content of an article from another journalist`` () =
    let draftFromOtherJournalist =
      { change with
          CreatorId = JournalistId.NewGuid() }

    Given [ Drafted article ]
    |> When(ChangeContent draftFromOtherJournalist)
    |> Then should equal [ TriedToChangeContentsOfOthersArticle draftFromOtherJournalist ]

  [<Fact>]
  let ``should not update content when content are exactly same`` () =
    Given
      [ Drafted article
        ContentUpdated("new title", "new fancy content") ]
    |> When(ChangeContent change)
    |> Then be empty

  [<Fact>]
  let ``should update content when article is in Review state`` () =
    Given
      [ Drafted article
        Assigned copywriter1 ]
    |> When(ChangeContent change)
    |> Then should equal [ ContentUpdated("new title", "new fancy content") ]

module Reviewer =
  [<Fact>]
  let ``should assign reviewer to article`` () =
    Given [ Drafted article ]
    |> When(AssignReviewer copywriter1)
    |> Then should equal [ Assigned copywriter1 ]

  [<Fact>]
  let ``should not re assign another reviewer`` () =
    Given
      [ Drafted article
        Assigned copywriter1 ]
    |> When(AssignReviewer copywriter2)
    |> Then should equal [ AlreadyAssigned copywriter1 ]
