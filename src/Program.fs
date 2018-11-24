module DomainTypes =
    type Candidate = Candidate of string
    type Ballot = Candidate list

module Messages =
    type Hello = Hello of string

module Program =
    open System
    open Akka.FSharp

    open Messages

    [<EntryPoint>]
    let main argv =
        let system = System.create "system" <| Configuration.load ()

        let myActor (mailbox: Actor<_>) = 
            let rec loop () = actor {
                let! message = mailbox.Receive ()
                match message with
                | Hello x -> printfn "Hello %s" x
                return! loop ()
            }
            loop ()
        
        let actorRef = spawn system "myActor" myActor

        actorRef <! Hello "World!"

        Console.ReadKey() |> ignore

        0
