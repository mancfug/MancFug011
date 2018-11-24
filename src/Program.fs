module DomainTypes =
    type Candidate = Candidate of string
    type Ballot = Candidate list

module VoteActors =
    open Akka.FSharp

    let greetingActor (mailbox: Actor<_>) = 
        let rec loop () = actor {
            let! message = mailbox.Receive ()
            printfn "Hello %s" message
            return! loop ()
        }
        loop ()

module Program =
    open System
    open Akka.FSharp
    open VoteActors

    [<EntryPoint>]
    let main argv =
        let system = System.create "system" <| Configuration.load ()
        
        let actorRef = spawn system "greetingActor" greetingActor

        actorRef <! "this!"
        actorRef <! "that!"
        actorRef <! "the other!"

        Console.ReadKey() |> ignore

        0
