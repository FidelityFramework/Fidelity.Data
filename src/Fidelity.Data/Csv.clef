/// Public API for Fidelity.Data.CSV.
namespace Fidelity.Data.CSV

/// Main entry point for CSV parsing, serialization, and navigation.
[<RequireQualifiedAccess>]
module Csv =

    /// Parse a CSV string with default configuration (comma-delimited, with headers).
    let parse (input: string) : Result<CsvDocument, string> =
        CsvParser.parse input

    /// Parse a CSV string with custom configuration.
    let parseWith (config: CsvConfig) (input: string) : Result<CsvDocument, string> =
        CsvParser.parseWith config input

    /// Parse a CSV string, throwing an exception on failure.
    let parseOrFail (input: string) : CsvDocument =
        match parse input with
        | Ok doc -> doc
        | Error msg -> failwith msg

    /// Parse a CSV string with custom config, throwing on failure.
    let parseWithOrFail (config: CsvConfig) (input: string) : CsvDocument =
        match parseWith config input with
        | Ok doc -> doc
        | Error msg -> failwith msg

    /// Parse a TSV (tab-separated) string.
    let parseTsv (input: string) : Result<CsvDocument, string> =
        CsvParser.parseWith CsvConfig.tsv input

    /// Serialize a CSV document with default comma delimiter.
    let serialize (doc: CsvDocument) : string =
        CsvSerializer.serialize doc

    /// Serialize a CSV document with a custom delimiter.
    let serializeWith (delim: char) (doc: CsvDocument) : string =
        CsvSerializer.serializeWith delim doc

    /// Get the number of data rows.
    let rowCount (doc: CsvDocument) : int =
        CsvDocument.rowCount doc

    /// Get the number of columns.
    let columnCount (doc: CsvDocument) : int =
        CsvDocument.columnCount doc

    /// Get a column by header name.
    let column (name: string) (doc: CsvDocument) : string list option =
        CsvDocument.columnByName name doc

    /// Get a field by row index and column name.
    let field (rowIndex: int) (colName: string) (doc: CsvDocument) : string option =
        CsvDocument.field rowIndex colName doc
