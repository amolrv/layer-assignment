module Src.Aggregate.Article.EventProducer

open Src.Domain

let project projection = List.fold projection.Fold projection.Zero

let articleEventProducer cmd : EventProducer<Event, ArticleError> =
  (project Projection.projection) >> (CommandHandler.handle cmd)
