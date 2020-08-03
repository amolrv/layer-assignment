module Infra.``Event store should``

open Domain.Types
open Xunit
open FsUnit.Xunit
open Infra

let envelope aggregateId event =
  { MetaData =
      { AggregateId = aggregateId
        RecordedAtUtc = System.DateTime.UtcNow }
    Event = event }

type DummyEvent =
  | ArticleCreated of string * string
  | ArticleTitleChanged of string

[<Fact>]
let ``append events`` () =
  async {
    let store = EventStore.init ()
    let aggregateId = System.Guid.NewGuid()

    let events =
      [ ArticleCreated("bla", "some content")
        ArticleTitleChanged("new title") ]
      |> List.map (envelope aggregateId)

    let expected : Result<Unit, string> = Ok()
    let! result = events |> store.Append
    result |> should equal expected
  }

[<Fact>]
let ``get events by aggregateId`` () =
  async {
    let store = EventStore.init ()
    let aggregateId1 = System.Guid.NewGuid()
    let aggregateId2 = System.Guid.NewGuid()

    let events =
      [ ArticleCreated("Article#1", "some content")
        ArticleTitleChanged("new title") ]
      |> List.map (envelope aggregateId1)

    let! _ = store.Append events
    let! _ =
      [ ArticleCreated("Article#2", "some content")
        |> envelope aggregateId2 ]
      |> store.Append
    let expected : EventResult<DummyEvent> = events |> Ok
    let! result = store.GetStream aggregateId1
    result |> should equal expected
  }

[<Fact>]
let ``get all events`` () =
  async {
    let store = EventStore.init ()
    let aggregateId1 = System.Guid.NewGuid()
    let aggregateId2 = System.Guid.NewGuid()

    let events1 =
      [ ArticleCreated("Article#1", "some content")
        ArticleTitleChanged("new title") ]
      |> List.map (envelope aggregateId1)

    let events2 =
      [ ArticleCreated("Article#2", "some content")
        |> envelope aggregateId2 ]

    let! _ = events1 |> store.Append
    let! _ = events2 |> store.Append
    let expected : EventResult<DummyEvent> = (events1 @ events2) |> Ok
    let! result = store.Get()
    result |> should equal expected
  }
