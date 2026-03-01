# Fidelity.Data

[![License: MIT](https://img.shields.io/badge/License-MIT-blue.svg)](https://opensource.org/licenses/MIT)

Dependency-free data format libraries for the Fidelity ecosystem, built with [XParsec](https://github.com/roboz0r/XParsec) parser combinators.

## Overview

Fidelity.Data is a unified library providing parsers and serializers for common data formats. Each format is a self-contained module sharing a common XParsec foundation. No BCL parsing dependencies — designed for the self-hosting path.

| Module | Namespace | Spec | Status |
|--------|-----------|------|--------|
| **TOML** | `Fidelity.Data.TOML` | [TOML 1.0.0](https://toml.io/en/v1.0.0) | Complete |
| **XML** | `Fidelity.Data.XML` | XML 1.0 | Complete |
| **JSON** | `Fidelity.Data.JSON` | [RFC 8259](https://datatracker.ietf.org/doc/html/rfc8259) | Complete |
| **CSV** | `Fidelity.Data.CSV` | [RFC 4180](https://datatracker.ietf.org/doc/html/rfc4180) | Complete |
| **YAML** | `Fidelity.Data.YAML` | YAML 1.2 Core Schema | Complete |

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

## XML Module

XML 1.0 parser with elements, attributes, text, CDATA, comments, processing instructions, and namespaces.

### Usage

```fsharp
open Fidelity.Data.XML

let xml = """<?xml version="1.0" encoding="UTF-8"?>
<project name="demo">
  <file path="Main.fs"/>
  <file path="Lib.fs"/>
</project>"""

let doc = Xml.parseOrFail xml
let root = Xml.root doc
XmlNode.name root          // Some "project"
XmlNode.attr "name" root   // Some "demo"
XmlNode.children root      // [Element "file"; Element "file"]
```

### API

```fsharp
module Xml =
    val parse      : string -> Result<XmlDocument, string>
    val parseOrFail: string -> XmlDocument
    val serialize  : XmlDocument -> string
    val root       : XmlDocument -> XmlNode
    val rootName   : XmlDocument -> string option
```

## JSON Module

RFC 8259 compliant JSON parser and serializer with compact and pretty-print output.

### Usage

```fsharp
open Fidelity.Data.JSON

let json = """{"name": "Alice", "scores": [98, 87, 95]}"""

let v = Json.parseOrFail json
Json.prop "name" v |> Option.bind Json.asString   // Some "Alice"
Json.prop "scores" v |> Option.map Json.items      // Some [Number 98; ...]

// Round-trip
Json.serialize v          // compact
Json.serializePretty v    // indented
```

### API

```fsharp
module Json =
    val parse         : string -> Result<JsonValue, string>
    val parseOrFail   : string -> JsonValue
    val serialize     : JsonValue -> string
    val serializePretty: JsonValue -> string
    val prop          : string -> JsonValue -> JsonValue option
    val asString      : JsonValue -> string option
    val asNumber      : JsonValue -> float option
    val asInt         : JsonValue -> int64 option
    val asBool        : JsonValue -> bool option
    val items         : JsonValue -> JsonValue list
```

## CSV Module

RFC 4180 compliant CSV parser with header support, TSV variant, and field-level navigation.

### Usage

```fsharp
open Fidelity.Data.CSV

let csv = "name,age,city\nAlice,30,NYC\nBob,25,LA\n"

let doc = Csv.parseOrFail csv
Csv.rowCount doc                    // 2
Csv.column "name" doc               // Some ["Alice"; "Bob"]
Csv.field 0 "city" doc              // Some "NYC"

// TSV
let tsv = Csv.parseTsv "a\tb\n1\t2\n"
```

### API

```fsharp
module Csv =
    val parse          : string -> Result<CsvDocument, string>
    val parseWith      : CsvConfig -> string -> Result<CsvDocument, string>
    val parseOrFail    : string -> CsvDocument
    val parseWithOrFail: CsvConfig -> string -> CsvDocument
    val parseTsv       : string -> Result<CsvDocument, string>
    val serialize      : CsvDocument -> string
    val serializeWith  : char -> CsvDocument -> string
    val rowCount       : CsvDocument -> int
    val columnCount    : CsvDocument -> int
    val column         : string -> CsvDocument -> string list option
    val field          : int -> string -> CsvDocument -> string option
```

## YAML Module

YAML 1.2 core schema parser supporting block/flow mappings, block/flow sequences, all scalar styles (plain, single-quoted, double-quoted, literal, folded), and comments.

### Usage

```fsharp
open Fidelity.Data.YAML

let yaml = """
server:
  host: localhost
  port: 8080
  features:
    - auth
    - logging
"""

let v = Yaml.parseOrFail yaml
Yaml.prop "server" v
|> Option.bind (Yaml.prop "host")
|> Option.bind Yaml.asString           // Some "localhost"

Yaml.prop "server" v
|> Option.bind (Yaml.prop "port")
|> Option.bind Yaml.asInt              // Some 8080L

// Round-trip
Yaml.serialize v
```

### API

```fsharp
module Yaml =
    val parse      : string -> Result<YamlValue, string>
    val parseOrFail: string -> YamlValue
    val serialize  : YamlValue -> string
    val prop       : string -> YamlValue -> YamlValue option
    val items      : YamlValue -> YamlValue list
    val asString   : YamlValue -> string option
    val asInt      : YamlValue -> int64 option
    val asFloat    : YamlValue -> float option
    val asBool     : YamlValue -> bool option
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
