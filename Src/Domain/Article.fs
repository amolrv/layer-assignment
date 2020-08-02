namespace Domain

type Article =
  { Title : string
    Content : string
    OwnerId : JournalistId }

[<RequireQualifiedAccess>]
module Article =
  type ArticleState =
    private
    | Draft of Article
    | InReview of {| Article : Article
                     Reviewer : CopyWriterId
                     Comments : Map<CommentId, Comment> |}
    | Published of {| Article : Article
                      Reviewer : CopyWriterId |}

  type Command =
    | Draft of Article
    | ChangeContent of string * string
    | AssignReviewer of CopyWriterId
    | Comment of CopyWriterId * Comment * CommentId
    | ResolveComment of CopyWriterId * CommentId
    | Publish of JournalistId

  type Event =
    | Drafted of Article
    | AlreadyDrafted
    | ContentUpdated of string * string
    | Assigned of CopyWriterId
    | AlreadyAssigned of CopyWriterId
    | Commented of CopyWriterId * Comment * CommentId
    | CommentAlreadyExist of CommentId
    | CommentResolved of CopyWriterId * CommentId
    | CommentAlreadyResolved of CommentId
    | ArticlePublished
    | ArticleAlreadyPublished

  let apply state event =
    match event with
    | Drafted article -> article |> ArticleState.Draft |> Some
    | _ -> state

  let exec cmd state =
    match cmd, state with
    | (Draft article, None) -> [ Drafted article ]
    | (Draft _, _) -> [ AlreadyDrafted ]
    | _ -> []

  let articleProjection = { Zero = None ; Fold = apply }
