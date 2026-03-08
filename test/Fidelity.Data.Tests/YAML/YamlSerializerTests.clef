/// Tests for YAML serialization.
module Fidelity.Data.YAML.Tests.YamlSerializerTests

open Expecto
open Fidelity.Data.YAML

[<Tests>]
let serializationTests =
    testList "YAML Serialization" [
        test "serialize null" {
            Expect.equal (Yaml.serialize YamlValue.Null) "null\n" "null"
        }

        test "serialize scalar" {
            Expect.equal (Yaml.serialize (YamlValue.Scalar "hello")) "hello\n" "scalar"
        }

        test "serialize scalar needing quotes" {
            let s = Yaml.serialize (YamlValue.Scalar "true")
            Expect.stringContains s "\"true\"" "quoted true"
        }

        test "serialize scalar with colon" {
            let s = Yaml.serialize (YamlValue.Scalar "http://example.com")
            Expect.stringContains s "\"http://example.com\"" "quoted colon"
        }

        test "serialize empty sequence" {
            Expect.equal (Yaml.serialize (YamlValue.Sequence [])) "[]\n" "empty seq"
        }

        test "serialize sequence" {
            let v = YamlValue.Sequence [YamlValue.Scalar "a"; YamlValue.Scalar "b"]
            let s = Yaml.serialize v
            Expect.stringContains s "- a" "first item"
            Expect.stringContains s "- b" "second item"
        }

        test "serialize empty mapping" {
            Expect.equal (Yaml.serialize (YamlValue.Mapping [])) "{}\n" "empty map"
        }

        test "serialize mapping" {
            let v = YamlValue.Mapping [("name", YamlValue.Scalar "Alice")]
            let s = Yaml.serialize v
            Expect.stringContains s "name: Alice" "mapping entry"
        }

        test "serialize nested mapping" {
            let inner = YamlValue.Mapping [("key", YamlValue.Scalar "value")]
            let outer = YamlValue.Mapping [("nested", inner)]
            let s = Yaml.serialize outer
            Expect.stringContains s "nested:" "outer key"
            Expect.stringContains s "key: value" "inner pair"
        }
    ]
