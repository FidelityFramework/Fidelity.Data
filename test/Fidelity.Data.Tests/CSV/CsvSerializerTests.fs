/// Tests for CSV serialization and round-trip consistency.
module Fidelity.Data.CSV.Tests.CsvSerializerTests

open Expecto
open Fidelity.Data.CSV

[<Tests>]
let serializationTests =
    testList "CSV Serialization" [
        test "serialize simple CSV" {
            let doc = { Headers = ["a"; "b"]; Rows = [["1"; "2"]; ["3"; "4"]] }
            let output = Csv.serialize doc
            Expect.equal output "a,b\r\n1,2\r\n3,4\r\n" "simple"
        }

        test "serialize without headers" {
            let doc = { Headers = []; Rows = [["1"; "2"]; ["3"; "4"]] }
            let output = Csv.serialize doc
            Expect.equal output "1,2\r\n3,4\r\n" "no headers"
        }

        test "serialize quotes field with comma" {
            let doc = { Headers = ["val"]; Rows = [["a,b"]] }
            let output = Csv.serialize doc
            Expect.stringContains output "\"a,b\"" "quoted comma"
        }

        test "serialize quotes field with quote" {
            let doc = { Headers = ["val"]; Rows = [["say \"hi\""]] }
            let output = Csv.serialize doc
            Expect.stringContains output "\"say \"\"hi\"\"\"" "doubled quote"
        }

        test "serialize quotes field with newline" {
            let doc = { Headers = ["val"]; Rows = [["line1\nline2"]] }
            let output = Csv.serialize doc
            Expect.stringContains output "\"line1\nline2\"" "quoted newline"
        }

        test "serialize with custom delimiter" {
            let doc = { Headers = ["a"; "b"]; Rows = [["1"; "2"]] }
            let output = Csv.serializeWith '\t' doc
            Expect.equal output "a\tb\r\n1\t2\r\n" "tab delimiter"
        }
    ]

[<Tests>]
let roundTripTests =
    testList "CSV Round-Trip" [
        test "simple CSV round-trips" {
            let input = "a,b\r\n1,2\r\n3,4\r\n"
            let doc = Csv.parseOrFail input
            Expect.equal (Csv.serialize doc) input "round-trip"
        }

        test "quoted fields round-trip" {
            let doc = { Headers = ["val"]; Rows = [["a,b"]; ["c\"d"]] }
            let serialized = Csv.serialize doc
            let reparsed = Csv.parseOrFail serialized
            Expect.equal reparsed.Rows.[0].[0] "a,b" "comma preserved"
            Expect.equal reparsed.Rows.[1].[0] "c\"d" "quote preserved"
        }
    ]
