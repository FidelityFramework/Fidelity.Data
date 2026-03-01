/// CSV types for representing parsed CSV data.
/// Covers records, fields, headers, and configurable delimiters.
/// Designed to be BCL-minimal, to support Clef dimensional types.
namespace Fidelity.Data.CSV

/// Configuration for CSV parsing and serialization.
[<Struct>]
type CsvConfig = {
    /// Field delimiter character (default: ',').
    Delimiter: char
    /// Whether the first row is a header row (default: true).
    HasHeaders: bool
}

/// Module for CsvConfig defaults and construction.
module CsvConfig =
    /// Default CSV configuration: comma-delimited with headers.
    let defaults = { Delimiter = ','; HasHeaders = true }

    /// Tab-separated values configuration with headers.
    let tsv = { Delimiter = '\t'; HasHeaders = true }

    /// Create a config with a custom delimiter.
    let withDelimiter (delim: char) (config: CsvConfig) =
        { config with Delimiter = delim }

    /// Create a config with or without headers.
    let withHeaders (hasHeaders: bool) (config: CsvConfig) =
        { config with HasHeaders = hasHeaders }

/// A parsed CSV document.
type CsvDocument = {
    /// Column headers (empty list if no headers).
    Headers: string list
    /// Data rows, each a list of field values.
    Rows: string list list
}

/// Module for navigating and querying CSV documents.
module CsvDocument =

    /// Get the number of data rows.
    let rowCount (doc: CsvDocument) : int =
        doc.Rows.Length

    /// Get the number of columns (from headers, or first row if no headers).
    let columnCount (doc: CsvDocument) : int =
        if not doc.Headers.IsEmpty then doc.Headers.Length
        elif not doc.Rows.IsEmpty then doc.Rows.[0].Length
        else 0

    /// Get a row by index.
    let row (index: int) (doc: CsvDocument) : string list option =
        if index >= 0 && index < doc.Rows.Length then Some doc.Rows.[index]
        else None

    /// Get a column by index across all rows.
    let column (index: int) (doc: CsvDocument) : string list option =
        if index < 0 then None
        else
            let values = doc.Rows |> List.choose (fun r ->
                if index < r.Length then Some r.[index] else None)
            if values.IsEmpty then None else Some values

    /// Get a column by header name.
    let columnByName (name: string) (doc: CsvDocument) : string list option =
        doc.Headers
        |> List.tryFindIndex (fun h -> h = name)
        |> Option.bind (fun i -> column i doc)

    /// Get a field by row index and column name.
    let field (rowIndex: int) (colName: string) (doc: CsvDocument) : string option =
        doc.Headers
        |> List.tryFindIndex (fun h -> h = colName)
        |> Option.bind (fun colIndex ->
            match row rowIndex doc with
            | Some r when colIndex < r.Length -> Some r.[colIndex]
            | _ -> None)

    /// Get a field by row and column indices.
    let fieldAt (rowIndex: int) (colIndex: int) (doc: CsvDocument) : string option =
        match row rowIndex doc with
        | Some r when colIndex >= 0 && colIndex < r.Length -> Some r.[colIndex]
        | _ -> None
