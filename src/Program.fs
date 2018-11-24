module DomainTypes =
    type Candidate = Candidate of string
    type Ballot = Candidate list

module Program =
    [<EntryPoint>]
    let main argv =
        printfn "Hello World from F#!"
        0
