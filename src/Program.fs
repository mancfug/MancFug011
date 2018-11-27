module Program =
    open System
    open Akka.Actor
    open Akka.FSharp
    open Messages

    [<EntryPoint>]
    let main _ =
        let system = System.create "my-actor-system" <| Configuration.load ()

        let greetingActor (mailbox: Actor<_>) = 
            let rec loop () = actor {
                let! message = mailbox.Receive ()
                printfn "Hello %s" message
                return! loop ()
            }
            loop ()

        let greetingActorRef = spawn system "greetingActor" greetingActor

        greetingActorRef <! "this!"
        greetingActorRef <! "that!"
        greetingActorRef <! "the other!"

        Console.ReadKey() |> ignore
        0
