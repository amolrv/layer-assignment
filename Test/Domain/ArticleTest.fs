module Domain.Article

open Domain.Article
open FsUnit.Xunit
open Xunit

let Given = id
let When = articleEventProducer
let Then expectation actual = actual |> expectation

let empty a = Empty.Matches(a) |> should be True
let should = id

let equal expected (actual : Result<'a, 'e>) =
  Assert.Equal<Result<'a, 'e>>(expected, actual)

let article =
  { Title = "Title#1"
    Content = "Some nice content"
    Topics =
      [ "corona"
        "life-style"
        "employment" ] }

let change =
  { Title = "new title"
    Content = "new fancy content"
    Topics = [ "corona" ; "life-style" ] }

let journalist = JournalistId.NewGuid()
let anotherJournalist = JournalistId.NewGuid()
let copywriter1 = CopyWriterId.NewGuid()
let copywriter2 = CopyWriterId.NewGuid()

module DraftArticle =
  [<Fact>]
  let ``should draft from drafted event`` () =
    Given []
    |> When(Draft(article, journalist))
    |> Then should equal
         ([ Drafted(article, journalist)
            StateChanged InDraft ]
          |> Ok)

  [<Fact>]
  let ``should emit already drafted event when article is drafted already`` () =
    Given [ Drafted(article, journalist) ]
    |> When(Draft(article, journalist))
    |> Then should equal (AlreadyDrafted |> Error)

module ChangeContent =
  [<Fact>]
  let ``should return article was not present`` () =
    Given []
    |> When(ChangeContent(change, journalist))
    |> Then should equal (ArticleWasNotPresent |> Error)

  [<Fact>]
  let ``should update content when article is inDraft state`` () =
    Given [ Drafted(article, journalist) ]
    |> When(ChangeContent(change, journalist))
    |> Then should equal (Ok [ ContentUpdated change ])

  [<Fact>]
  let ``should update content when article is in Review state`` () =
    Given
      [ Drafted(article, journalist)
        Assigned copywriter1 ]
    |> When(ChangeContent(change, journalist))
    |> Then should equal (Ok [ ContentUpdated change ])

  [<Fact>]
  let ``should not change content of published article`` () =
    Given
      [ Drafted(article, journalist)
        Assigned copywriter1
        StateChanged Published ]
    |> When(ChangeContent(change, journalist))
    |> Then should equal (Published |> ArticleInvalidState |> Error)


  [<Fact>]
  let ``should not change content of an article from another journalist`` () =

    Given [ Drafted(article, journalist) ]
    |> When(ChangeContent(change, anotherJournalist))
    |> Then should equal
         ((change, anotherJournalist)
          |> TriedToChangeContentsOfOthersArticle
          |> Error)

  [<Fact>]
  let ``should not update content when content are exactly same`` () =
    Given
      [ Drafted (article,journalist)
        ContentUpdated (change) ]
    |> When(ChangeContent (change,journalist))
    |> Then should equal (Ok [])


module Reviewer =
  [<Fact>]
  let ``should assign reviewer to article`` () =
    Given [ Drafted (article,journalist) ]
    |> When(AssignReviewer copywriter1)
    |> Then should equal
         (Ok
           [ Assigned copywriter1
             StateChanged InReview ])

  [<Fact>]
  let ``should not re assign another reviewer`` () =
    Given
      [ Drafted (article,journalist)
        Assigned copywriter1
        StateChanged InReview ]
    |> When(AssignReviewer copywriter2)
    |> Then should equal (copywriter1 |> AlreadyAssigned |> Error)
