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
    
    let countingActor (mailbox: Actor<_>) =
        let rec loop(oldValue) = actor {
            let! message = mailbox.Receive ()
            let newValue = oldValue + 1
            printfn "Counted %d" newValue
            return! loop(newValue)
        }
        loop(0)

module Program =
    open System
    open Akka.FSharp
    open VoteActors

    [<EntryPoint>]
    let main _ =
        let system = System.create "system" <| Configuration.load ()
        
        let greetingActorRef = spawn system "greetingActor" greetingActor
        let countingActorRef = spawn system "countingActor" countingActor

        greetingActorRef <! "this!"
        greetingActorRef <! "that!"
        greetingActorRef <! "the other!"

        countingActorRef <! 1
        countingActorRef <! " "
        countingActorRef <! greetingActorRef

        Console.ReadKey() |> ignore

        0
