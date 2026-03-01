/// Tests for JSON serialization and round-trip consistency.
module Fidelity.Data.JSON.Tests.JsonSerializerTests

open Expecto
open Fidelity.Data.JSON

[<Tests>]
let serializationTests =
    testList "JSON Serialization" [
        test "serialize null" {
            Expect.equal (Json.serialize JsonValue.Null) "null" "null"
        }

        test "serialize true" {
            Expect.equal (Json.serialize (JsonValue.Bool true)) "true" "true"
        }

        test "serialize false" {
            Expect.equal (Json.serialize (JsonValue.Bool false)) "false" "false"
        }

        test "serialize integer number" {
            Expect.equal (Json.serialize (JsonValue.Number 42.0)) "42" "integer"
        }

        test "serialize float number" {
            let s = Json.serialize (JsonValue.Number 3.14)
            Expect.stringContains s "3.14" "float"
        }

        test "serialize simple string" {
            Expect.equal (Json.serialize (JsonValue.String "hello")) "\"hello\"" "string"
        }

        test "serialize string with escapes" {
            let s = Json.serialize (JsonValue.String "a\nb\tc")
            Expect.equal s "\"a\\nb\\tc\"" "escapes"
        }

        test "serialize string with quotes" {
            let s = Json.serialize (JsonValue.String "say \"hi\"")
            Expect.equal s "\"say \\\"hi\\\"\"" "quotes"
        }

        test "serialize empty array" {
            Expect.equal (Json.serialize (JsonValue.Array [])) "[]" "empty array"
        }

        test "serialize array" {
            let v = JsonValue.Array [JsonValue.Number 1.0; JsonValue.Number 2.0]
            Expect.equal (Json.serialize v) "[1,2]" "array"
        }

        test "serialize empty object" {
            Expect.equal (Json.serialize (JsonValue.Object [])) "{}" "empty object"
        }

        test "serialize object" {
            let v = JsonValue.Object [("a", JsonValue.Number 1.0); ("b", JsonValue.String "two")]
            Expect.equal (Json.serialize v) """{"a":1,"b":"two"}""" "object"
        }
    ]

[<Tests>]
let roundTripTests =
    testList "JSON Round-Trip" [
        test "null round-trips" {
            let input = "null"
            Expect.equal (Json.serialize (Json.parseOrFail input)) input "null"
        }

        test "boolean round-trips" {
            let input = "true"
            Expect.equal (Json.serialize (Json.parseOrFail input)) input "bool"
        }

        test "integer round-trips" {
            let input = "42"
            Expect.equal (Json.serialize (Json.parseOrFail input)) input "integer"
        }

        test "string round-trips" {
            let input = "\"hello world\""
            Expect.equal (Json.serialize (Json.parseOrFail input)) input "string"
        }

        test "array round-trips" {
            let input = "[1,2,3]"
            Expect.equal (Json.serialize (Json.parseOrFail input)) input "array"
        }

        test "object round-trips" {
            let input = """{"key":"value"}"""
            Expect.equal (Json.serialize (Json.parseOrFail input)) input "object"
        }

        test "nested structure round-trips" {
            let input = """{"data":[1,2,{"nested":true}]}"""
            Expect.equal (Json.serialize (Json.parseOrFail input)) input "nested"
        }
    ]

[<Tests>]
let prettyPrintTests =
    testList "JSON Pretty Print" [
        test "pretty print simple object" {
            let v = JsonValue.Object [("a", JsonValue.Number 1.0)]
            let pretty = Json.serializePretty v
            Expect.stringContains pretty "\"a\"" "contains key"
            Expect.stringContains pretty "1" "contains value"
        }

        test "pretty print empty collections" {
            Expect.equal (Json.serializePretty (JsonValue.Array [])) "[]" "empty array"
            Expect.equal (Json.serializePretty (JsonValue.Object [])) "{}" "empty object"
        }
    ]
