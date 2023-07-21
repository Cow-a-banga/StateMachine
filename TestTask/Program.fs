open System
open Microsoft.FSharp.Collections

type Event =
    | APP_PASSIVE_OPEN = 0
    | APP_ACTIVE_OPEN = 1
    | APP_SEND = 2
    | APP_CLOSE = 3
    | APP_TIMEOUT = 4
    | RCV_SYN = 5
    | RCV_ACK = 6
    | RCV_SYN_ACK = 7
    | RCV_FIN = 8
    | RCV_FIN_ACK = 9

type State =
    | ERROR = 0
    | CLOSED = 1
    | LISTEN = 2
    | SYN_SENT = 3
    | SYN_RCVD = 4
    | ESTABLISHED = 5
    | CLOSE_WAIT = 6
    | LAST_ACK = 7
    | FIN_WAIT_1 = 8
    | FIN_WAIT_2 = 9
    | CLOSING = 10
    | TIME_WAIT = 11
    
let closed event =
  match event with
  | Event.APP_PASSIVE_OPEN -> State.LISTEN
  | Event.APP_ACTIVE_OPEN -> State.SYN_SENT
  | _ -> State.ERROR

let listen event =
  match event with
  | Event.RCV_SYN -> State.SYN_RCVD
  | Event.APP_SEND -> State.SYN_SENT
  | Event.APP_CLOSE -> State.CLOSED
  | _ -> State.ERROR

let syn_rcvd event =
  match event with
  | Event.APP_CLOSE -> State.FIN_WAIT_1
  | Event.RCV_ACK -> State.ESTABLISHED
  | _ -> State.ERROR

let syn_sent event =
  match event with
  | Event.RCV_SYN -> State.SYN_RCVD
  | Event.RCV_SYN_ACK -> State.ESTABLISHED
  | Event.APP_CLOSE -> State.CLOSED
  | _ -> State.ERROR

let established event =
  match event with
  | Event.APP_CLOSE -> State.FIN_WAIT_1
  | Event.RCV_FIN -> State.CLOSE_WAIT
  | _ -> State.ERROR
  
let fin_wait_1 event =
  match event with
  | Event.RCV_FIN -> State.CLOSING 
  | Event.RCV_FIN_ACK -> State.TIME_WAIT 
  | Event.RCV_ACK -> State.FIN_WAIT_2
  | _ -> State.ERROR
  
let closing event =
  match event with
  | Event.RCV_ACK -> State.TIME_WAIT
  | _ -> State.ERROR
  
let fin_wait_2 event =
  match event with
  | Event.RCV_FIN -> State.TIME_WAIT
  | _ -> State.ERROR

let time_wait event =
  match event with
  | Event.APP_TIMEOUT -> State.CLOSED
  | _ -> State.ERROR

let close_wait event =
  match event with
  | Event.APP_CLOSE -> State.LAST_ACK
  | _ -> State.ERROR

let last_ack event =
  match event with
  | Event.RCV_ACK -> State.CLOSED
  | _ -> State.ERROR



let transition state event =
  match state with 
  | State.CLOSED -> closed event 
  | State.LISTEN -> listen event 
  | State.SYN_RCVD -> syn_rcvd event 
  | State.SYN_SENT -> syn_sent event 
  | State.ESTABLISHED -> established event
  | State.FIN_WAIT_1 -> fin_wait_1 event
  | State.CLOSING -> closing event
  | State.FIN_WAIT_2 -> fin_wait_2 event 
  | State.TIME_WAIT -> time_wait event
  | State.LAST_ACK -> last_ack event 
  | State.CLOSE_WAIT -> close_wait event
  | _ -> State.ERROR

let stateMachine events = events |> List.fold transition State.CLOSED

let Parse (value: string) =
    Enum.Parse(typeof<Event>, value, true) :?> Event
    
let main () =
  //APP_PASSIVE_OPEN APP_SEND RCV_SYN_ACK -> ESTABLISHED
  //APP_ACTIVE_OPEN -> SYN_SENT
  //APP_ACTIVE_OPEN RCV_SYN_ACK APP_CLOSE RCV_FIN_ACK RCV_ACK -> ERROR
  
  let events = stdin.ReadLine().Split(" ") |> Array.toList |> List.filter(fun s -> s <> "") |> List.map Parse
  printfn $"{stateMachine events}"
  
main()