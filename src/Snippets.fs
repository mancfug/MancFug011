module Snippets

open System
open Akka.Actor
open Akka.FSharp
open Messages

let system = System.create "my-actor-system" <| Configuration.load ()

let superGreetingActor (mailbox: Actor<_>) =
    let rec loop () = actor {
        let! (name, time) = mailbox.Receive ()
        match time with
        | Morning -> printfn "Good morning %s" name
        | Afternoon -> printfn "Good afternoon %s" name
        | Evening -> printfn "Good evening %s" name
        return! loop ()
    }
    loop ()

let superGreetingActorRef = spawn system "superGreetingActor" superGreetingActor

superGreetingActorRef <! ("this!", Morning)
superGreetingActorRef <! ("that!", Afternoon)
superGreetingActorRef <! ("the other!", Evening)

let consoleActor (mailbox: Actor<_>) =
    let rec loop() = actor {
        let! message = mailbox.Receive ()
        printfn "%s" message
        return! loop ()
    }
    loop ()

let consoleGreetingActor console (mailbox: Actor<_>) = 
    let rec loop () = actor {
        let! message = mailbox.Receive ()
        console <! sprintf "Hello %s" message
        return! loop ()
    }
    loop ()

let consoleSuperGreetingActor console (mailbox: Actor<_>) =
    let rec loop () = actor {
        let! (name, time) = mailbox.Receive ()
        match time with
        | Morning -> console <! sprintf  "Good morning %s" name
        | Afternoon -> console <! sprintf  "Good afternoon %s" name
        | Evening -> console <! sprintf  "Good evening %s" name
        return! loop ()
    }
    loop ()

let consoleActorRef = spawn system "consoleActor" consoleActor
let consoleGreetingActorRef = spawn system "consoleGreetingActor" (consoleGreetingActor consoleActorRef)
let consoleSuperGreetingActorRef = spawn system "consoleSuperGreetingActor" (consoleSuperGreetingActor consoleActorRef)

consoleGreetingActorRef <! "this!"
consoleGreetingActorRef <! "that!"
consoleGreetingActorRef <! "the other!"

consoleSuperGreetingActorRef <! ("this!", Morning)
consoleSuperGreetingActorRef <! ("that!", Afternoon)
consoleSuperGreetingActorRef <! ("the other!", Evening)

let loggingActor (mailbox: Actor<_>) =
    let rec loop () = actor {
        let! message = mailbox.Receive ()
        logInfo mailbox message
        return! loop ()
    }
    loop ()

let loggingActorRef = spawn system "loggingActor" loggingActor 
loggingActorRef <! "Hello log!"

let mathsActor (mailbox: Actor<_>) =
    let rec loop currentValue = actor {
        let! message = mailbox.Receive ()
        let newValue =
            match message with
            | Add x -> currentValue + x
            | Subtract x -> currentValue - x
            | Multiply x -> currentValue * x
        logInfo mailbox <| sprintf "New total %d" newValue
        return! loop newValue
    }
    loop 0

let mathsActorRef = spawn system "mathsActor" mathsActor
let mathsActorRefAlt = select "akka://my-actor-system/user/mathsActor" system

mathsActorRef <! Add 1
mathsActorRef <! Add 5
mathsActorRefAlt <! Multiply 2
mathsActorRefAlt <! Subtract 4

let idCountingActor id (mailbox: Actor<_>) =
    let rec loop oldValue = actor {
        let! message = mailbox.Receive ()
        let newValue = oldValue + 1
        logInfo mailbox <| sprintf "Counted %d for %s" newValue id
        return! loop newValue
    }
    loop 0

let counterSupervisorActor (mailbox: Actor<_>) =
    let getCounter id =
        let actorRef = mailbox.Context.Child id
        if actorRef.IsNobody() then
            spawn mailbox id (idCountingActor id)
        else
            actorRef
    
    let rec loop () = actor {
        let! message = mailbox.Receive ()
        let counter = getCounter message
        counter <! message
        return! loop ()
    }
    loop ()

let counterSupervisorActorRef = spawn system "counterSupervisorActor" counterSupervisorActor

counterSupervisorActorRef <! "A"
counterSupervisorActorRef <! "A"
counterSupervisorActorRef <! "B"