module Tests

open Xunit
open FsUnit.Xunit

[<Fact>]
let ``My test`` () =
    true |> should be True
