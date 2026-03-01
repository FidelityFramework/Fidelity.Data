/// Test entry point for Fidelity.Data.TOML tests.
module Fidelity.Data.TOML.Tests.Main

open Expecto

[<EntryPoint>]
let main args =
    runTestsInAssemblyWithCLIArgs [] args
