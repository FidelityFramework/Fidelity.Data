# Fidelity.Data

[![License: MIT](https://img.shields.io/badge/License-MIT-blue.svg)](https://opensource.org/licenses/MIT)

Dependency-free data format libraries for the Fidelity ecosystem, built with [XParsec](https://github.com/roboz0r/XParsec) parser combinators.

## Overview

Fidelity.Data is a unified library providing parsers and serializers for common data formats. Each format is a self-contained module sharing a common XParsec foundation. No BCL parsing dependencies — designed for the self-hosting path.

| Module | Namespace | Spec | Status |
|--------|-----------|------|--------|
| **TOML** | `Fidelity.Data.TOML` | [TOML 1.0.0](https://toml.io/en/v1.0.0) | Complete |
| **XML** | `Fidelity.Data.XML` | XML 1.0 | Planned |
| **JSON** | `Fidelity.Data.JSON` | RFC 8259 | Planned |
| **CSV** | `Fidelity.Data.CSV` | RFC 4180 | Planned |

## Part of the Fidelity Framework

This library is a component of the [Fidelity Framework](https://github.com/FidelityFramework) — an ecosystem for compiling [Clef](https://clef-lang.org) to native targets.

Every format parser uses XParsec combinators rather than BCL classes (`System.Xml.Linq`, `System.Text.Json`, etc.). This ensures the entire toolchain can eventually compile itself — no .NET runtime dependency stands between the source and a native binary.

## Installation

```bash
dotnet add package Fidelity.Data
```

## TOML Module

Full TOML 1.0.0 compliance — all data types, structural features, and round-trip serialization.

### Usage

```fsharp
open Fidelity.Data.TOML

let toml = """
[package]
name = "my-project"
version = "1.0.0"

[build]
sources = ["Main.fs", "Lib.fs"]
output = "myapp"
"""

match Toml.parse toml with
| Ok doc ->
    Toml.getString "package.name" doc       // Some "my-project"
    Toml.getStringArray "build.sources" doc  // Some ["Main.fs"; "Lib.fs"]
    Toml.getTable "build" doc               // Some <table>
| Error msg ->
    eprintfn "Parse error: %s" msg
```

### Supported Types

| Type | Example |
|------|---------|
| Strings | `"basic"`, `'literal'`, `"""multiline"""` |
| Integers | `42`, `0xDEAD`, `0o755`, `0b1101` |
| Floats | `3.14`, `5e10`, `inf`, `nan` |
| Booleans | `true`, `false` |
| Date-Times | `1979-05-27T07:32:00Z`, local variants |
| Tables | `[section]`, `{ inline = true }` |
| Arrays | `[1, 2, 3]`, `[[array.of.tables]]` |
| Dotted Keys | `physical.color = "red"` |

### API

```fsharp
module Toml =
    val parse         : string -> Result<TomlDocument, string>
    val serialize     : TomlDocument -> string
    val getString     : string -> TomlDocument -> string option
    val getInt        : string -> TomlDocument -> int64 option
    val getFloat      : string -> TomlDocument -> float option
    val getBool       : string -> TomlDocument -> bool option
    val getStringArray: string -> TomlDocument -> string list option
    val getTable      : string -> TomlDocument -> TomlTable option
    val getValue      : string -> TomlDocument -> TomlValue option
```

## Building from Source

```bash
git clone https://github.com/FidelityFramework/Fidelity.Data.git
cd Fidelity.Data
dotnet build
dotnet run --project test/Fidelity.Data.Tests/
```

## License

MIT License — see [LICENSE](LICENSE) for details.

---

*Part of the [Fidelity Framework](https://github.com/FidelityFramework) — Native Systems for everyone.*
