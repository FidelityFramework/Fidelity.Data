/// Tests for CSV parsing: fields, quoting, delimiters, headers.
module Fidelity.Data.CSV.Tests.CsvParserTests

open Expecto
open Fidelity.Data.CSV

[<Tests>]
let basicTests =
    testList "CSV Basic Parsing" [
        test "parse simple CSV" {
            let input = "name,age\nAlice,30\nBob,25\n"
            let doc = Csv.parseOrFail input
            Expect.equal doc.Headers ["name"; "age"] "headers"
            Expect.equal doc.Rows.Length 2 "two rows"
            Expect.equal doc.Rows.[0] ["Alice"; "30"] "first row"
            Expect.equal doc.Rows.[1] ["Bob"; "25"] "second row"
        }

        test "parse without trailing newline" {
            let input = "a,b\n1,2"
            let doc = Csv.parseOrFail input
            Expect.equal doc.Rows.Length 1 "one row"
            Expect.equal doc.Rows.[0] ["1"; "2"] "row data"
        }

        test "parse three columns" {
            let input = "x,y,z\n1,2,3\n4,5,6\n"
            let doc = Csv.parseOrFail input
            Expect.equal doc.Headers ["x"; "y"; "z"] "headers"
            Expect.equal doc.Rows.Length 2 "two rows"
        }
    ]

[<Tests>]
let quotedFieldTests =
    testList "CSV Quoted Fields" [
        test "quoted field with comma" {
            let input = "name,city\n\"Smith, John\",NYC\n"
            let doc = Csv.parseOrFail input
            Expect.equal doc.Rows.[0].[0] "Smith, John" "comma in field"
        }

        test "quoted field with escaped quote" {
            let input = "val\n\"say \"\"hello\"\"\"\n"
            let doc = Csv.parseOrFail input
            Expect.equal doc.Rows.[0].[0] "say \"hello\"" "escaped quote"
        }

        test "quoted field with newline" {
            let input = "val\n\"line1\nline2\"\n"
            let doc = Csv.parseOrFail input
            Expect.equal doc.Rows.[0].[0] "line1\nline2" "newline in field"
        }

        test "empty quoted field" {
            let input = "a,b\n\"\",x\n"
            let doc = Csv.parseOrFail input
            Expect.equal doc.Rows.[0].[0] "" "empty quoted"
        }
    ]

[<Tests>]
let noHeaderTests =
    testList "CSV Without Headers" [
        test "parse headerless CSV" {
            let config = { CsvConfig.defaults with HasHeaders = false }
            let input = "1,2,3\n4,5,6\n"
            let doc = Csv.parseWithOrFail config input
            Expect.isEmpty doc.Headers "no headers"
            Expect.equal doc.Rows.Length 2 "two rows"
            Expect.equal doc.Rows.[0] ["1"; "2"; "3"] "first row"
        }
    ]

[<Tests>]
let tsvTests =
    testList "CSV Tab-Separated" [
        test "parse TSV" {
            let input = "name\tage\nAlice\t30\nBob\t25\n"
            match Csv.parseTsv input with
            | Ok doc ->
                Expect.equal doc.Headers ["name"; "age"] "headers"
                Expect.equal doc.Rows.[0] ["Alice"; "30"] "first row"
            | Error e -> failtest e
        }
    ]

[<Tests>]
let navigationTests =
    testList "CSV Navigation" [
        test "rowCount" {
            let doc = Csv.parseOrFail "a,b\n1,2\n3,4\n5,6\n"
            Expect.equal (Csv.rowCount doc) 3 "three rows"
        }

        test "columnCount" {
            let doc = Csv.parseOrFail "a,b,c\n1,2,3\n"
            Expect.equal (Csv.columnCount doc) 3 "three columns"
        }

        test "column by name" {
            let doc = Csv.parseOrFail "name,age\nAlice,30\nBob,25\n"
            Expect.equal (Csv.column "age" doc) (Some ["30"; "25"]) "age column"
        }

        test "field by row and column name" {
            let doc = Csv.parseOrFail "name,age\nAlice,30\nBob,25\n"
            Expect.equal (Csv.field 1 "name" doc) (Some "Bob") "Bob"
        }

        test "column by name returns None for missing" {
            let doc = Csv.parseOrFail "a\n1\n"
            Expect.isNone (Csv.column "missing" doc) "missing column"
        }

        test "fieldAt by indices" {
            let doc = Csv.parseOrFail "a,b\n1,2\n3,4\n"
            Expect.equal (CsvDocument.fieldAt 1 1 doc) (Some "4") "field at 1,1"
        }
    ]

[<Tests>]
let edgeCaseTests =
    testList "CSV Edge Cases" [
        test "single column" {
            let doc = Csv.parseOrFail "name\nAlice\nBob\n"
            Expect.equal doc.Headers ["name"] "one header"
            Expect.equal doc.Rows.Length 2 "two rows"
        }

        test "empty fields" {
            let doc = Csv.parseOrFail "a,b,c\n,,\n"
            Expect.equal doc.Rows.[0] [""; ""; ""] "empty fields"
        }

        test "CRLF line endings" {
            let input = "a,b\r\n1,2\r\n3,4\r\n"
            let doc = Csv.parseOrFail input
            Expect.equal doc.Rows.Length 2 "two rows"
        }
    ]
