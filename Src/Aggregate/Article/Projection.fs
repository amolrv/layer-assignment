module Src.Aggregate.Article.Projection

open Src.Domain

let create creatorId (draft : ArticleData) : Article =
  { Data = draft
    OwnerId = creatorId
    State = InDraft
    Reviewer = None
    Comments = Map.empty }

let apply (article : Article option) event =
  match (event, article) with
  | (Drafted (draft, creatorId), None) ->
      draft
      |> create creatorId
      |> Some
  | (ContentUpdated data, Some article) -> { article with Data = data } |> Some
  | (Assigned reviewer, Some article) ->
      { article with Reviewer = Some reviewer } |> Some
  | (StateChanged state, Some article) -> { article with State = state } |> Some
  | (Commented (content, commentId), Some article) ->
      let comment = Comment.Comment content
      let newComments =
        article.Comments |> Map.add commentId comment
      { article with Comments = newComments } |> Some
  | (Event.Resolved commentId, Some article) ->
      let comment = article.Comments |> Map.find commentId

      let newState =
        match comment with
        | Comment.Comment c -> Comment.Resolved c
        | _ -> comment

      let newComments = article.Comments |> Map.add commentId newState
      { article with Comments = newComments } |> Some
  | _ -> article

let projection =
  { Zero = None
    Fold = apply }
