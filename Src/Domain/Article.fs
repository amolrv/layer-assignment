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


type Event =
  | ArticleWasNotPresent
  | Drafted of ArticleDraft
  | StateChanged of ArticleState
  | AlreadyDrafted
  | ContentUpdated of string * string
  | TriedToChangeContentsOfOthersArticle of ArticleDraft
  | Assigned of CopyWriterId
  | AlreadyAssigned of CopyWriterId
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
  if article.Title = draft.Title
     && article.Content = draft.Content then
    []
  else if draft.CreatorId <> article.OwnerId then
    [ TriedToChangeContentsOfOthersArticle draft ]
  else
    [ ContentUpdated(draft.Title, draft.Content) ]

let private assignReviewer reviewer article =
  match article.State with
  | InDraft -> [ Assigned reviewer; StateChanged InReview ]
  | _ -> [ article.Reviewer |> Option.get |> AlreadyAssigned ]

let exec cmd article =
  match (cmd, article) with
  | (Draft articleDraft, None) ->
      [ Drafted articleDraft
        StateChanged InDraft ]
  | (Draft _, Some _) -> [ AlreadyDrafted ]
  | (ChangeContent draft, Some state) -> state |> changeContent draft
  | (AssignReviewer reviewer, Some state) -> state |> assignReviewer reviewer
  | (_, None) -> [ ArticleWasNotPresent ]

let articleProjection = { Zero = None ; Fold = apply }

let articleEventProducer cmd : EventProducer<Event> =
  (project articleProjection) >> (exec cmd)
