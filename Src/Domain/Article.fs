module Domain.Article

open Domain.Projection

type ArticleState =
  | InDraft
  | InReview
  | Published

type ArticleData =
  { Title : string
    Content : string
    Topics : string list }

type Article =
  { Data : ArticleData
    OwnerId : JournalistId
    State : ArticleState
    Reviewer : CopyWriterId option
    Comments : Map<CommentId, Comment> }

let create creatorId (draft : ArticleData) : Article =
  { Data = draft
    OwnerId = creatorId
    State = InDraft
    Reviewer = None
    Comments = Map.empty }

type Command =
  | Draft of ArticleData * JournalistId
  | ChangeContent of ArticleData * JournalistId
  | AssignReviewer of CopyWriterId
// | Comment of CopyWriterId * Comment * CommentId
// | ResolveComment of CopyWriterId * CommentId
// | Publish of JournalistId

type ArticleError =
  | ArticleWasNotPresent
  | AlreadyDrafted
  | TriedToChangeContentsOfOthersArticle of ArticleData * JournalistId
  | AlreadyAssigned of CopyWriterId
  | ArticleInvalidState of ArticleState

type Event =
  | Drafted of ArticleData * JournalistId
  | ContentUpdated of ArticleData
  | Assigned of CopyWriterId
  | StateChanged of ArticleState

let apply (article : Article option) event =
  match (event, article) with
  | (Drafted (draft, creatorId), None) -> draft |> create creatorId |> Some
  | (ContentUpdated data, Some article) -> { article with Data = data } |> Some
  | (Assigned reviewer, Some article) -> { article with Reviewer = Some reviewer } |> Some
  | (StateChanged state, Some article) -> { article with State = state } |> Some
  | _ -> article

let private changeContent (draft : ArticleData) journalistId article =
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
  | state -> state |> ArticleInvalidState |> Error

let private assignReviewer reviewer article =
  match article.State with
  | InDraft ->
      [ Assigned reviewer
        StateChanged InReview ]
      |> Ok
  | _ -> article.Reviewer.Value |> AlreadyAssigned |> Error

let exec cmd article =
  match (cmd, article) with
  | (Draft (articleDraft, creatorId), None) ->
      [ Drafted(articleDraft, creatorId)
        StateChanged InDraft ]
      |> Ok
  | (Draft _, Some _) -> AlreadyDrafted |> Error
  | (ChangeContent (draft, journalistId), Some article) -> article |> changeContent draft journalistId
  | (AssignReviewer reviewer, Some article) -> article |> assignReviewer reviewer
  | (_, None) -> ArticleWasNotPresent |> Error

let articleProjection = { Zero = None ; Fold = apply }

let articleEventProducer cmd : EventProducer<Event, ArticleError> =
  (project articleProjection) >> (exec cmd)
