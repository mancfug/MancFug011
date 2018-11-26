module DomainTypes =
    type Candidate = Candidate of string
    type Ballot = Candidate list

module VoteActors =
    open Akka.Actor
    open Akka.FSharp

    (*let greetingActor (mailbox: Actor<_>) = 
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
        loop(0)*)
    
    let consoleActor (mailbox: Actor<_>) =
        let rec loop() = actor {
            let! message = mailbox.Receive ()
            printfn "%s" message
            return! loop ()
        }
        loop ()
    
    let greetingActor consoleActor (mailbox: Actor<_>) = 
        let rec loop () = actor {
            let! message = mailbox.Receive ()
            consoleActor <! sprintf "Hello %s" message
            return! loop ()
        }
        loop ()
    
    let countingActor consoleActor (mailbox: Actor<_>) =
        let rec loop(oldValue) = actor {
            let! message = mailbox.Receive ()
            let newValue = oldValue + 1
            consoleActor <! sprintf "Counted %d" newValue
            return! loop newValue
        }
        loop(0)

    let idCountingActor consoleActor id (mailbox: Actor<_>) =
        let rec loop oldValue = actor {
            let! message = mailbox.Receive ()
            let newValue = oldValue + 1
            consoleActor <! sprintf "Counted %d for %s" newValue id
            return! loop newValue
        }
        loop 0
    
    let counterSupervisorActor consoleActor (mailbox: Actor<_>) =
        let getCounter id =
            let actorRef = mailbox.Context.Child id
            if actorRef.IsNobody() then
                spawn mailbox id (idCountingActor consoleActor id)
            else
                actorRef
        
        let rec loop () = actor {
            let! message = mailbox.Receive ()
            let counter = getCounter message
            counter <! message
            return! loop ()
        }
        loop ()

module Program =
    open System
    open Akka.FSharp
    open VoteActors

    [<EntryPoint>]
    let main _ =
        let system = System.create "system" <| Configuration.load ()

        let consoleActorRef = spawn system "consoleActor" consoleActor
        
        let greetingActorRef = spawn system "greetingActor" (greetingActor consoleActorRef)
        let countingActorRef = spawn system "countingActor" (countingActor consoleActorRef)
        let counterSupervisorActorRef = spawn system "counterSupervisorActor" (counterSupervisorActor consoleActorRef)

        greetingActorRef <! "this!"
        greetingActorRef <! "that!"
        greetingActorRef <! "the other!"

        countingActorRef <! "A"
        countingActorRef <! "A"
        countingActorRef <! "A"

        counterSupervisorActorRef <! "A"
        counterSupervisorActorRef <! "A"
        counterSupervisorActorRef <! "B"

        Console.ReadKey() |> ignore

        0
