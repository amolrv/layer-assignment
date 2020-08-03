module Src.Aggregate.Article.CommandHandler

open Src.Domain

let changeContent (draft : ArticleData) journalistId article =
  match article.State with
  | InDraft
  | InReview ->
      if article.Data = draft then
        [] |> Ok
      else if journalistId <> article.OwnerId then
        (draft, journalistId)
        |> TriedToChangeContentsOfOthersArticle
        |> Error
      else
        [ ContentUpdated draft ] |> Ok
  | state ->
      state
      |> ArticleInvalidState
      |> Error

let assignReviewer reviewer article =
  match article.State with
  | InDraft ->
      [ Assigned reviewer
        StateChanged InReview ]
      |> Ok
  | _ ->
      article.Reviewer.Value
      |> AlreadyAssigned
      |> Error

let addComment (comment, id, reviewer) article =
  match article.State with
  | InReview when article.Reviewer.Value = reviewer ->
      let existingComment = (article.Comments.TryFind id)
      Ok(if existingComment.IsSome then [] else [ Commented(comment, id) ])
  | InReview when article.Reviewer.Value <> reviewer -> Error(CommentingOnOthersArticle reviewer)
  | state -> Error(ArticleInvalidState state)

let resolveComment ((commentId : CommentId), reviewer) article =
  let toEvents =
    function
    | Comment.Comment _ -> [ Event.Resolved commentId ]
    | Comment.Resolved _ -> []

  commentId
  |> article.Comments.TryFind
  |> Option.map (toEvents >> Ok)
  |> Option.defaultValue (Error(CommentNotFound commentId))

let resolvedComments article =
  article.Comments
  |> Map.toSeq
  |> Seq.map snd
  |> Seq.sumBy (function
       | Comment.Resolved _ -> 1
       | _ -> 0)

let publishArticle journalistId article =
  if journalistId <> article.OwnerId then
    Error(ArticleError.TriedToPublishArticleOfOther)
  else
    match article.Comments.Count with
    | 0 -> Error(ArticleIsNotReviewed)
    | n when n = (resolvedComments article) -> Ok [ StateChanged Published ]
    | _ -> Error(AllCommentsAreNotResolvedYet)

let handle cmd article =
  match (cmd, article) with
  | (Draft (articleDraft, creatorId), None) ->
      [ Drafted(articleDraft, creatorId)
        StateChanged InDraft ]
      |> Ok
  | (Draft _, Some _) -> AlreadyDrafted |> Error
  | (ChangeContent (draft, journalistId), Some article) -> article |> changeContent draft journalistId
  | (AssignReviewer reviewer, Some article) -> article |> assignReviewer reviewer
  | (Command.Comment (comment, id, reviewer), Some article) -> article |> addComment (comment, id, reviewer)
  | (Command.Resolve (commentId, reviewer), Some article) -> article |> resolveComment (commentId, reviewer)
  | (Publish journalistId), Some article -> article |> publishArticle journalistId
  | (_, None) -> ArticleWasNotPresent |> Error
