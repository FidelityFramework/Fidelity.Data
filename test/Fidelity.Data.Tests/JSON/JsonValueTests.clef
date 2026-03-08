/// Tests for JSON value parsing: objects, arrays, strings, numbers, booleans, null.
module Fidelity.Data.JSON.Tests.JsonValueTests

open Expecto
open Fidelity.Data.JSON

[<Tests>]
let nullTests =
    testList "JSON Null" [
        test "parse null" {
            Expect.equal (Json.parseOrFail "null") JsonValue.Null "null"
        }

        test "parse null with whitespace" {
            Expect.equal (Json.parseOrFail "  null  ") JsonValue.Null "null with ws"
        }
    ]

[<Tests>]
let boolTests =
    testList "JSON Booleans" [
        test "parse true" {
            Expect.equal (Json.parseOrFail "true") (JsonValue.Bool true) "true"
        }

        test "parse false" {
            Expect.equal (Json.parseOrFail "false") (JsonValue.Bool false) "false"
        }
    ]

[<Tests>]
let numberTests =
    testList "JSON Numbers" [
        test "parse integer" {
            Expect.equal (Json.parseOrFail "42") (JsonValue.Number 42.0) "integer"
        }

        test "parse negative integer" {
            Expect.equal (Json.parseOrFail "-7") (JsonValue.Number -7.0) "negative"
        }

        test "parse float" {
            Expect.equal (Json.parseOrFail "3.14") (JsonValue.Number 3.14) "float"
        }

        test "parse scientific notation" {
            Expect.equal (Json.parseOrFail "1.5e10") (JsonValue.Number 1.5e10) "scientific"
        }

        test "parse negative exponent" {
            Expect.equal (Json.parseOrFail "2.5E-3") (JsonValue.Number 2.5e-3) "neg exponent"
        }

        test "parse zero" {
            Expect.equal (Json.parseOrFail "0") (JsonValue.Number 0.0) "zero"
        }
    ]

[<Tests>]
let stringTests =
    testList "JSON Strings" [
        test "parse simple string" {
            Expect.equal (Json.parseOrFail "\"hello\"") (JsonValue.String "hello") "simple"
        }

        test "parse empty string" {
            Expect.equal (Json.parseOrFail "\"\"") (JsonValue.String "") "empty"
        }

        test "parse string with escapes" {
            let v = Json.parseOrFail "\"a\\nb\\tc\""
            Expect.equal v (JsonValue.String "a\nb\tc") "escape sequences"
        }

        test "parse string with unicode escape" {
            let v = Json.parseOrFail "\"\\u0041\""
            Expect.equal v (JsonValue.String "A") "unicode escape"
        }

        test "parse string with escaped quote" {
            let v = Json.parseOrFail "\"say \\\"hi\\\"\""
            Expect.equal v (JsonValue.String "say \"hi\"") "escaped quotes"
        }

        test "parse string with backslash" {
            let v = Json.parseOrFail "\"a\\\\b\""
            Expect.equal v (JsonValue.String "a\\b") "backslash"
        }

        test "parse string with solidus" {
            let v = Json.parseOrFail "\"a\\/b\""
            Expect.equal v (JsonValue.String "a/b") "solidus"
        }
    ]

[<Tests>]
let arrayTests =
    testList "JSON Arrays" [
        test "parse empty array" {
            Expect.equal (Json.parseOrFail "[]") (JsonValue.Array []) "empty"
        }

        test "parse single-element array" {
            Expect.equal (Json.parseOrFail "[1]") (JsonValue.Array [JsonValue.Number 1.0]) "single"
        }

        test "parse multi-element array" {
            let v = Json.parseOrFail "[1, 2, 3]"
            let expected = JsonValue.Array [JsonValue.Number 1.0; JsonValue.Number 2.0; JsonValue.Number 3.0]
            Expect.equal v expected "multi"
        }

        test "parse mixed-type array" {
            let v = Json.parseOrFail "[1, \"two\", true, null]"
            let expected = JsonValue.Array [
                JsonValue.Number 1.0
                JsonValue.String "two"
                JsonValue.Bool true
                JsonValue.Null
            ]
            Expect.equal v expected "mixed types"
        }

        test "parse nested arrays" {
            let v = Json.parseOrFail "[[1, 2], [3, 4]]"
            let inner1 = JsonValue.Array [JsonValue.Number 1.0; JsonValue.Number 2.0]
            let inner2 = JsonValue.Array [JsonValue.Number 3.0; JsonValue.Number 4.0]
            Expect.equal v (JsonValue.Array [inner1; inner2]) "nested"
        }
    ]

[<Tests>]
let objectTests =
    testList "JSON Objects" [
        test "parse empty object" {
            Expect.equal (Json.parseOrFail "{}") (JsonValue.Object []) "empty"
        }

        test "parse simple object" {
            let v = Json.parseOrFail """{"name": "Alice"}"""
            Expect.equal (JsonValue.prop "name" v) (Some (JsonValue.String "Alice")) "name"
        }

        test "parse multi-key object" {
            let v = Json.parseOrFail """{"a": 1, "b": 2, "c": 3}"""
            Expect.equal (JsonValue.prop "a" v) (Some (JsonValue.Number 1.0)) "a"
            Expect.equal (JsonValue.prop "b" v) (Some (JsonValue.Number 2.0)) "b"
            Expect.equal (JsonValue.prop "c" v) (Some (JsonValue.Number 3.0)) "c"
        }

        test "parse nested object" {
            let v = Json.parseOrFail """{"outer": {"inner": 42}}"""
            let outer = JsonValue.prop "outer" v
            Expect.isSome outer "outer exists"
            Expect.equal (JsonValue.prop "inner" outer.Value) (Some (JsonValue.Number 42.0)) "inner"
        }

        test "parse object with array value" {
            let v = Json.parseOrFail """{"items": [1, 2, 3]}"""
            let items = JsonValue.prop "items" v
            Expect.isSome items "items exists"
            Expect.equal (JsonValue.items items.Value |> List.length) 3 "three items"
        }
    ]

[<Tests>]
let navigationTests =
    testList "JSON Navigation" [
        test "prop returns None for missing key" {
            let v = Json.parseOrFail """{"a": 1}"""
            Expect.isNone (JsonValue.prop "b" v) "missing key"
        }

        test "prop returns None for non-object" {
            Expect.isNone (JsonValue.prop "key" (JsonValue.Number 1.0)) "non-object"
        }

        test "asString extracts string" {
            Expect.equal (JsonValue.asString (JsonValue.String "hi")) (Some "hi") "string"
        }

        test "asNumber extracts number" {
            Expect.equal (JsonValue.asNumber (JsonValue.Number 3.14)) (Some 3.14) "number"
        }

        test "asInt extracts integer-valued number" {
            Expect.equal (JsonValue.asInt (JsonValue.Number 42.0)) (Some 42L) "int"
        }

        test "asInt returns None for fractional" {
            Expect.isNone (JsonValue.asInt (JsonValue.Number 3.14)) "fractional"
        }

        test "asBool extracts boolean" {
            Expect.equal (JsonValue.asBool (JsonValue.Bool true)) (Some true) "bool"
        }

        test "isNull detects null" {
            Expect.isTrue (JsonValue.isNull JsonValue.Null) "null"
        }

        test "isNull false for non-null" {
            Expect.isFalse (JsonValue.isNull (JsonValue.Number 0.0)) "non-null"
        }

        test "keys returns object keys" {
            let v = Json.parseOrFail """{"x": 1, "y": 2}"""
            Expect.equal (JsonValue.keys v) ["x"; "y"] "keys"
        }

        test "items returns array items" {
            let v = Json.parseOrFail "[1, 2]"
            Expect.equal (JsonValue.items v |> List.length) 2 "items"
        }

        test "item returns by index" {
            let v = Json.parseOrFail "[10, 20, 30]"
            Expect.equal (JsonValue.item 1 v) (Some (JsonValue.Number 20.0)) "item at 1"
        }

        test "count for array" {
            let v = Json.parseOrFail "[1, 2, 3]"
            Expect.equal (JsonValue.count v) 3 "count"
        }

        test "count for object" {
            let v = Json.parseOrFail """{"a": 1, "b": 2}"""
            Expect.equal (JsonValue.count v) 2 "count"
        }
    ]

[<Tests>]
let errorTests =
    testList "JSON Parse Errors" [
        test "empty input" {
            Expect.isError (Json.parse "") "empty"
        }

        test "invalid token" {
            Expect.isError (Json.parse "undefined") "invalid"
        }

        test "trailing comma in array" {
            Expect.isError (Json.parse "[1, 2,]") "trailing comma"
        }

        test "unclosed string" {
            Expect.isError (Json.parse "\"unclosed") "unclosed string"
        }

        test "unclosed array" {
            Expect.isError (Json.parse "[1, 2") "unclosed array"
        }
    ]
