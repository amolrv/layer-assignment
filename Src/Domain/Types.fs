namespace Domain.Types

open System

type CommentId = Guid
type CopyWriterId = Guid
type JournalistId = Guid

type Comment =
  | Comment of NonEmptyString
  | Resolved of NonEmptyString

type AggregateId = Guid
type EventProducer<'Event, 'Error> = 'Event list -> Result<'Event list, 'Error>

type Projection<'State, 'Event> =
  { Zero : 'State
    Fold : 'State -> 'Event -> 'State }

type EventMetaData =
  { AggregateId : AggregateId
    RecordedAtUtc : DateTime }

type EventEnvelope<'Event> =
  { MetaData : EventMetaData
    Event : 'Event }

type EventResult<'Event> = Result<EventEnvelope<'Event> list, string>

type EventStore<'Event> =
  { Get : unit -> Async<EventResult<'Event>>
    GetStream : AggregateId -> Async<EventResult<'Event>>
    Append : EventEnvelope<'Event> list -> Async<Result<Unit, string>> }

module Projection =
  let project projection =
    List.fold projection.Fold projection.Zero
