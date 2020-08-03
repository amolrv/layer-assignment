module Domain.Article.Test
open Domain
open Domain.Article
open FsUnit.Xunit
open Xunit

let Given = id
let When = articleEventProducer
let Then expectation actual = actual |> expectation

let empty a = Empty.Matches(a) |> should be True
let should = id

let equal expected actual =
  let str = sprintf "%A"
  Assert.Equal(str expected, str actual)

let nonEmptyStr = WrappedString.nonEmptyString >> Option.get
let nonEmptyStrList = List.map nonEmptyStr

let article =
  { Title = nonEmptyStr "Title#1"
    Content = nonEmptyStr "Some nice content"
    Topics = nonEmptyStrList
      [ "corona"
        "life-style"
        "employment" ] }

let change =
  { Title = nonEmptyStr "new title"
    Content = nonEmptyStr "new fancy content"
    Topics = nonEmptyStrList [ "corona" ; "life-style" ] }

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
  let ``should change content when article is inDraft state`` () =
    Given [ Drafted(article, journalist) ]
    |> When(ChangeContent(change, journalist))
    |> Then should equal (Ok [ ContentUpdated change ])

  [<Fact>]
  let ``should change content when article is in Review state`` () =
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
  let ``should not change content when content are exactly same`` () =
    Given
      [ Drafted(article, journalist)
        ContentUpdated(change) ]
    |> When(ChangeContent(change, journalist))
    |> Then should equal (Ok [])

module Reviewer =
  [<Fact>]
  let ``should assign reviewer to article`` () =
    Given [ Drafted(article, journalist) ]
    |> When(AssignReviewer copywriter1)
    |> Then should equal
         (Ok
           [ Assigned copywriter1
             StateChanged InReview ])

  [<Fact>]
  let ``should not re assign another reviewer`` () =
    Given
      [ Drafted(article, journalist)
        Assigned copywriter1
        StateChanged InReview ]
    |> When(AssignReviewer copywriter2)
    |> Then should equal (copywriter1 |> AlreadyAssigned |> Error)

module Comments =
  let commentId1 = CommentId.NewGuid()
  let commentId2 = CommentId.NewGuid()

  [<Fact>]
  let ``should comment on article when article is inReview state`` () =
    Given
      [ Drafted(article, journalist)
        Assigned copywriter1
        StateChanged InReview ]
    |> When(Command.Comment( nonEmptyStr "Comment#1", commentId1, copywriter1))
    |> Then should equal (Ok [ Commented( nonEmptyStr "Comment#1", commentId1) ])

  [<Fact>]
  let ``should resolve comment`` () =
    Given
      [ Drafted(article, journalist)
        Assigned copywriter1
        StateChanged InReview
        Commented( nonEmptyStr "Comment#1", commentId1)
        ContentUpdated change
        Commented(nonEmptyStr "Comment#2", commentId2) ]
    |> When(Resolve(commentId1, copywriter1))
    |> Then should equal (Ok [ Event.Resolved commentId1 ])

  [<Fact>]
  let ``should not resolve comment resolved comment again`` () =
    Given
      [ Drafted(article, journalist)
        Assigned copywriter1
        StateChanged InReview
        Commented(nonEmptyStr "Comment#1", commentId1)
        ContentUpdated change
        Event.Resolved commentId1 ]
    |> When(Resolve(commentId1, copywriter1))
    |> Then should equal (Ok [])

  [<Fact>]
  let ``should not add same comment twice`` () =
    let commentId = CommentId.NewGuid()
    Given
      [ Drafted(article, journalist)
        Assigned copywriter1
        StateChanged InReview
        Commented(nonEmptyStr "Comment#1", commentId) ]
    |> When(Command.Comment(nonEmptyStr "Comment#1", commentId, copywriter1))
    |> Then should equal (Ok [])

  [<Fact>]
  let ``should not comment on others article`` () =
    let commentId = CommentId.NewGuid()
    Given
      [ Drafted(article, journalist)
        Assigned copywriter1
        StateChanged InReview
        Commented(nonEmptyStr "Comment#1", commentId) ]
    |> When(Command.Comment(nonEmptyStr "Comment#2", commentId, copywriter2))
    |> Then should equal (Error(CommentingOnOthersArticle copywriter2))

  [<Fact>]
  let ``should not add comment on article when article is inDraft/Published state`` () =
    let commentId = CommentId.NewGuid()
    [ InDraft ; Published ]
    |> List.iter (fun state ->
         Given
           [ Drafted(article, journalist)
             Assigned copywriter1
             StateChanged state ]
         |> When(Command.Comment(nonEmptyStr "Comment#1", commentId, copywriter1))
         |> Then should equal (Error(ArticleInvalidState state)))

module publish =

  [<Fact>]
  let ``should able to publish article only when all the comments are resolved`` () =
    Given
      [ Drafted(article, journalist)
        Assigned copywriter1
        StateChanged InReview
        Commented(nonEmptyStr "Comment#1", Comments.commentId1)
        Commented(nonEmptyStr "Comment#2", Comments.commentId2)
        Event.Resolved Comments.commentId1
        Event.Resolved Comments.commentId2 ]
    |> When(Publish journalist)
    |> Then should equal (Ok [ StateChanged Published ])

  [<Fact>]
  let ``should not publish article when there are no comments`` () =
    Given
      [ Drafted(article, journalist)
        Assigned copywriter1
        StateChanged InReview ]
    |> When(Publish journalist)
    |> Then should equal (Error ArticleIsNotReviewed)

  [<Fact>]
  let ``should not publish article of someone else`` () =
    Given
      [ Drafted(article, journalist)
        Assigned copywriter1
        StateChanged InReview
        Commented(nonEmptyStr "Comment#1", Comments.commentId1)
        Commented(nonEmptyStr "Comment#2", Comments.commentId2)
        Event.Resolved Comments.commentId1
        Event.Resolved Comments.commentId2 ]
    |> When(Publish anotherJournalist)
    |> Then should equal (Error TriedToPublishArticleOfOther)

  [<Fact>]
  let ``should not publish article of some comments are yet to resolved`` () =
    Given
      [ Drafted(article, journalist)
        Assigned copywriter1
        StateChanged InReview
        Commented(nonEmptyStr "Comment#1", Comments.commentId1)
        Commented(nonEmptyStr "Comment#2", Comments.commentId2)
        Event.Resolved Comments.commentId2 ]
    |> When(Publish journalist)
    |> Then should equal (Error AllCommentsAreNotResolvedYet)
