[<RequireQualifiedAccess>]
module Domain.Article

open System
type CommentId = Guid
type CopyWriterId = Guid
type JournalistId = Guid

type ArticleData =
    { Title: string
      Content: string
      OwnerId: JournalistId }

type Comment =
    | Comment of string
    | Resolved of string

type Article =
    | Draft of ArticleData
    | InReview of {| ArticleData: ArticleData
                     Reviewer: CopyWriterId
                     Comments: Map<CommentId, Comment> |}
    | Published of {| ArticleData: ArticleData
                      Reviewer: CopyWriterId |}

type Events =
    | Drafted of {| Title: string; Content: string |}
    | AlreadyDrafted
    | ContentUpdated of {| Title: string; Content: string |}
    | Assiged of CopyWriterId
    | AlreadyAssigned of CopyWriterId
    | Commentted of {| CommentId: CommentId
                       Comment: string
                       CopyWriterId: CopyWriterId |}
    | CommentAlreadyExist of CommentId
    | CommentResolved of CopyWriterId * CommentId
    | CommentAlreadyAlreadyResolved of CommentId
    | ArticlePublished
    | ArticleAlreadyPublished
