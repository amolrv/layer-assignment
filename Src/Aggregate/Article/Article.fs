namespace Src.Aggregate.Article

open Src.Domain

type ArticleState =
  | InDraft
  | InReview
  | Published

type ArticleData =
  { Title : NonEmptyString
    Content : NonEmptyString
    Topics : NonEmptyString list }

type Article =
  { Data : ArticleData
    OwnerId : JournalistId
    State : ArticleState
    Reviewer : CopyWriterId option
    Comments : Map<CommentId, Comment> }

type Command =
  | Draft of ArticleData * JournalistId
  | ChangeContent of ArticleData * JournalistId
  | AssignReviewer of CopyWriterId
  | Comment of NonEmptyString * CommentId * CopyWriterId
  | Resolve of CommentId * CopyWriterId
  | Publish of JournalistId

type ArticleError =
  | ArticleWasNotPresent
  | AlreadyDrafted
  | TriedToChangeContentsOfOthersArticle of ArticleData * JournalistId
  | AlreadyAssigned of CopyWriterId
  | CommentingOnOthersArticle of CopyWriterId
  | ArticleInvalidState of ArticleState
  | CommentNotFound of CommentId
  | ArticleIsNotReviewed
  | AllCommentsAreNotResolvedYet
  | TriedToPublishArticleOfOther

type Event =
  | Drafted of ArticleData * JournalistId
  | ContentUpdated of ArticleData
  | Assigned of CopyWriterId
  | StateChanged of ArticleState
  | Commented of NonEmptyString * CommentId
  | Resolved of CommentId