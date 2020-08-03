module Domain.Article

open Domain.Projection

type ArticleState =
  | InDraft
  | InReview
  | Published

type ArticleDraft =
  { Title : string
    Content : string
    CreatorId : JournalistId }

type Article =
  { Title : string
    Content : string
    OwnerId : JournalistId
    State : ArticleState
    Reviewer : CopyWriterId option
    Comments : Map<CommentId, Comment> }

let create (draft : ArticleDraft) : Article =
  { Title = draft.Title
    Content = draft.Content
    OwnerId = draft.CreatorId
    State = InDraft
    Reviewer = None
    Comments = Map.empty }

type Command =
  | Draft of ArticleDraft
  | ChangeContent of ArticleDraft
  | AssignReviewer of CopyWriterId
// | Comment of CopyWriterId * Comment * CommentId
// | ResolveComment of CopyWriterId * CommentId
// | Publish of JournalistId

type ArticleError =
  | ArticleWasNotPresent
  | AlreadyDrafted
  | TriedToChangeContentsOfOthersArticle of ArticleDraft
  | AlreadyAssigned of CopyWriterId
  | ArticleInvalidState of ArticleState

type Event =
  | Drafted of ArticleDraft
  | ContentUpdated of string * string
  | Assigned of CopyWriterId
  | StateChanged of ArticleState
// | Commented of CopyWriterId * Comment * CommentId
// | CommentAlreadyExist of CommentId
// | CommentResolved of CopyWriterId * CommentId
// | CommentAlreadyResolved of CommentId
// | ArticlePublished
// | ArticleAlreadyPublished

let apply (article : Article option) event =
  match (event, article) with
  | (Drafted draft, None) -> draft |> create |> Some
  | (ContentUpdated (title, content), Some article) ->
      { article with
          Title = title
          Content = content }
      |> Some
  | (Assigned reviewer, Some article) ->
      { article with
          Reviewer = Some reviewer }
      |> Some
  | (StateChanged state, Some article) -> { article with State = state } |> Some
  | _ -> article

let private changeContent (draft : ArticleDraft) article =
  match article.State with
  | InDraft
  | InReview ->
      if (article.Title, article.Content) = (draft.Title, draft.Content) then
        [] |> Ok
      else if draft.CreatorId <> article.OwnerId then
        draft
        |> TriedToChangeContentsOfOthersArticle
        |> Error
      else
        [ ContentUpdated(draft.Title, draft.Content) ]
        |> Ok
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
  | (Draft articleDraft, None) ->
      [ Drafted articleDraft
        StateChanged InDraft ]
      |> Ok
  | (Draft _, Some _) -> AlreadyDrafted |> Error
  | (ChangeContent draft, Some article) -> article |> changeContent draft
  | (AssignReviewer reviewer, Some article) -> article |> assignReviewer reviewer
  | (_, None) -> ArticleWasNotPresent |> Error

let articleProjection = { Zero = None ; Fold = apply }

let articleEventProducer cmd : EventProducer<Event, ArticleError> =
  (project articleProjection) >> (exec cmd)
