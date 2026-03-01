/// Tests for YAML value parsing: scalars, mappings, sequences.
module Fidelity.Data.YAML.Tests.YamlValueTests

open Expecto
open Fidelity.Data.YAML

[<Tests>]
let scalarTests =
    testList "YAML Scalars" [
        test "parse plain scalar" {
            let v = Yaml.parseOrFail "hello"
            Expect.equal (YamlValue.asString v) (Some "hello") "plain"
        }

        test "parse double-quoted scalar" {
            let v = Yaml.parseOrFail "\"hello world\""
            Expect.equal (YamlValue.asString v) (Some "hello world") "double-quoted"
        }

        test "parse single-quoted scalar" {
            let v = Yaml.parseOrFail "'hello world'"
            Expect.equal (YamlValue.asString v) (Some "hello world") "single-quoted"
        }

        test "parse double-quoted with escapes" {
            let v = Yaml.parseOrFail "\"line1\\nline2\""
            Expect.equal (YamlValue.asString v) (Some "line1\nline2") "escapes"
        }

        test "parse single-quoted with escaped quote" {
            let v = Yaml.parseOrFail "'it''s'"
            Expect.equal (YamlValue.asString v) (Some "it's") "escaped single quote"
        }

        test "parse null" {
            let v = Yaml.parseOrFail "null"
            Expect.isTrue (YamlValue.isNull v) "null"
        }

        test "parse tilde as null" {
            let v = Yaml.parseOrFail "~"
            Expect.isTrue (YamlValue.isNull v) "tilde"
        }

        test "parse empty input as null" {
            let v = Yaml.parseOrFail ""
            Expect.isTrue (YamlValue.isNull v) "empty"
        }
    ]

[<Tests>]
let typedScalarTests =
    testList "YAML Typed Scalars" [
        test "asInt parses integer" {
            let v = YamlValue.Scalar "42"
            Expect.equal (YamlValue.asInt v) (Some 42L) "int"
        }

        test "asFloat parses float" {
            let v = YamlValue.Scalar "3.14"
            Expect.equal (YamlValue.asFloat v) (Some 3.14) "float"
        }

        test "asBool parses true" {
            let v = YamlValue.Scalar "true"
            Expect.equal (YamlValue.asBool v) (Some true) "true"
        }

        test "asBool parses False" {
            let v = YamlValue.Scalar "False"
            Expect.equal (YamlValue.asBool v) (Some false) "False"
        }

        test "asFloat parses infinity" {
            let v = YamlValue.Scalar ".inf"
            Expect.equal (YamlValue.asFloat v) (Some infinity) "inf"
        }

        test "asFloat parses nan" {
            let v = YamlValue.Scalar ".nan"
            Expect.isSome (YamlValue.asFloat v) "nan"
        }
    ]

[<Tests>]
let flowSequenceTests =
    testList "YAML Flow Sequences" [
        test "parse flow sequence" {
            let v = Yaml.parseOrFail "[1, 2, 3]"
            let items = YamlValue.items v
            Expect.equal items.Length 3 "three items"
            Expect.equal (YamlValue.asString items.[0]) (Some "1") "first"
        }

        test "parse empty flow sequence" {
            let v = Yaml.parseOrFail "[]"
            Expect.equal (YamlValue.items v) [] "empty"
        }

        test "parse nested flow sequences" {
            let v = Yaml.parseOrFail "[[1, 2], [3, 4]]"
            let items = YamlValue.items v
            Expect.equal items.Length 2 "two inner"
        }

        test "parse mixed flow sequence" {
            let v = Yaml.parseOrFail "[hello, \"world\", 42]"
            let items = YamlValue.items v
            Expect.equal items.Length 3 "three items"
        }
    ]

[<Tests>]
let flowMappingTests =
    testList "YAML Flow Mappings" [
        test "parse flow mapping" {
            let v = Yaml.parseOrFail "{name: Alice, age: 30}"
            Expect.equal (YamlValue.prop "name" v |> Option.bind YamlValue.asString) (Some "Alice") "name"
            Expect.equal (YamlValue.prop "age" v |> Option.bind YamlValue.asString) (Some "30") "age"
        }

        test "parse empty flow mapping" {
            let v = Yaml.parseOrFail "{}"
            Expect.equal (YamlValue.keys v) [] "empty"
        }

        test "parse flow mapping with quoted keys" {
            let v = Yaml.parseOrFail "{\"key\": \"value\"}"
            Expect.equal (YamlValue.prop "key" v |> Option.bind YamlValue.asString) (Some "value") "quoted"
        }
    ]

[<Tests>]
let blockSequenceTests =
    testList "YAML Block Sequences" [
        test "parse block sequence" {
            let v = Yaml.parseOrFail "- one\n- two\n- three\n"
            let items = YamlValue.items v
            Expect.equal items.Length 3 "three items"
            Expect.equal (YamlValue.asString items.[0]) (Some "one") "first"
            Expect.equal (YamlValue.asString items.[2]) (Some "three") "third"
        }

        test "parse block sequence with quoted items" {
            let v = Yaml.parseOrFail "- \"hello world\"\n- 'single quoted'\n"
            let items = YamlValue.items v
            Expect.equal items.Length 2 "two items"
            Expect.equal (YamlValue.asString items.[0]) (Some "hello world") "double"
            Expect.equal (YamlValue.asString items.[1]) (Some "single quoted") "single"
        }
    ]

[<Tests>]
let blockMappingTests =
    testList "YAML Block Mappings" [
        test "parse block mapping" {
            let v = Yaml.parseOrFail "name: Alice\nage: 30\n"
            Expect.equal (YamlValue.prop "name" v |> Option.bind YamlValue.asString) (Some "Alice") "name"
            Expect.equal (YamlValue.prop "age" v |> Option.bind YamlValue.asString) (Some "30") "age"
        }

        test "parse block mapping with quoted values" {
            let v = Yaml.parseOrFail "greeting: \"hello world\"\n"
            Expect.equal (YamlValue.prop "greeting" v |> Option.bind YamlValue.asString) (Some "hello world") "quoted value"
        }

        test "parse nested block mapping" {
            let v = Yaml.parseOrFail "outer:\n  inner: value\n"
            let outer = YamlValue.prop "outer" v
            Expect.isSome outer "outer exists"
            Expect.equal (YamlValue.prop "inner" outer.Value |> Option.bind YamlValue.asString) (Some "value") "inner"
        }

        test "parse mapping with sequence value" {
            let v = Yaml.parseOrFail "items:\n  - one\n  - two\n"
            let items = YamlValue.prop "items" v |> Option.map YamlValue.items
            Expect.equal (items |> Option.map List.length) (Some 2) "two items"
        }
    ]

[<Tests>]
let navigationTests =
    testList "YAML Navigation" [
        test "keys returns mapping keys" {
            let v = Yaml.parseOrFail "{a: 1, b: 2}"
            Expect.equal (YamlValue.keys v) ["a"; "b"] "keys"
        }

        test "values returns mapping values" {
            let v = Yaml.parseOrFail "{a: 1, b: 2}"
            Expect.equal (YamlValue.values v |> List.length) 2 "values"
        }

        test "item returns by index" {
            let v = Yaml.parseOrFail "[a, b, c]"
            Expect.equal (YamlValue.item 1 v |> Option.bind YamlValue.asString) (Some "b") "item 1"
        }

        test "count for sequence" {
            let v = Yaml.parseOrFail "[1, 2, 3]"
            Expect.equal (YamlValue.count v) 3 "count"
        }

        test "count for mapping" {
            let v = Yaml.parseOrFail "{a: 1, b: 2}"
            Expect.equal (YamlValue.count v) 2 "count"
        }

        test "prop returns None for missing key" {
            let v = Yaml.parseOrFail "{a: 1}"
            Expect.isNone (YamlValue.prop "missing" v) "missing"
        }
    ]

[<Tests>]
let documentTests =
    testList "YAML Document" [
        test "document with --- marker" {
            let v = Yaml.parseOrFail "---\nhello\n"
            Expect.equal (YamlValue.asString v) (Some "hello") "doc start"
        }

        test "document with ... marker" {
            let v = Yaml.parseOrFail "hello\n..."
            Expect.equal (YamlValue.asString v) (Some "hello") "doc end"
        }

        test "document with comments" {
            let v = Yaml.parseOrFail "# comment\nvalue\n"
            Expect.equal (YamlValue.asString v) (Some "value") "comment"
        }
    ]
