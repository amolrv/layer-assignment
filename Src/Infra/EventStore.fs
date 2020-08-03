module Infra.EventStore

open Domain.Types

type Msg<'Event> =
  | Get of AsyncReplyChannel<EventResult<'Event>>
  | GetStream of AggregateId * AsyncReplyChannel<EventResult<'Event>>
  | Append of EventEnvelope<'Event> list * AsyncReplyChannel<Result<unit, string>>

let streamOf aggregateId events =
  events
  |> List.filter (fun ee -> ee.MetaData.AggregateId = aggregateId)

let init () : EventStore<'Event> =
  let handle (inbox : MailboxProcessor<Msg<'Event>>) =
    let rec loop history =
      async {
        match! inbox.Receive() with
        | Get replyChannel ->
            history |> (Ok >> replyChannel.Reply)
            return! loop history
        | GetStream (aggregateId, replyChannel) ->
            history
            |> streamOf aggregateId
            |> Ok
            |> replyChannel.Reply
            return! loop history
        | Append (events, replyChannel) ->
            () |> Ok |> replyChannel.Reply
            let newHistory = history @ events
            return! loop newHistory
      }

    loop []

  let processor = new MailboxProcessor<_>(handle)
  processor.Start()
  { Get = fun () -> processor.PostAndAsyncReply(fun reply -> Get reply)
    GetStream = fun aggregateId -> processor.PostAndAsyncReply(fun reply -> (aggregateId, reply) |> GetStream)
    Append = fun events -> processor.PostAndAsyncReply(fun reply -> (events, reply) |> Append) }
