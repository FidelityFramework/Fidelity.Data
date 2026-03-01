/// CSV parser built with XParsec.
/// Implements RFC 4180 (Common Format and MIME Type for CSV Files).
/// BCL-minimal design to support Clef dimensional types.
namespace Fidelity.Data.CSV

open XParsec
open XParsec.Parsers
open XParsec.Combinators
open XParsec.CharParsers

/// Parser implementation for CSV.
module CsvParser =

    // ============================================================================
    // Field Parsers
    // ============================================================================

    /// Parse a quoted field: "..." with escaped quotes ("").
    let private pQuotedField =
        parser {
            let! _ = pchar '"'
            let! segments = many (
                (pstring "\"\"" >>% "\"") <|>
                (satisfy (fun c -> c <> '"') |>> string)
            )
            let! _ = pchar '"'
            return segments |> Seq.toList |> List.fold (+) ""
        }

    /// Parse an unquoted field (no delimiter, no quote, no newline).
    let private pUnquotedField (delim: char) =
        manyChars (satisfy (fun c -> c <> delim && c <> '"' && c <> '\n' && c <> '\r'))

    /// Parse a single field (quoted or unquoted).
    let private pField (delim: char) =
        pQuotedField <|> pUnquotedField delim

    // ============================================================================
    // Record (Row) Parser
    // ============================================================================

    /// Parse a single record (row of fields separated by delimiter).
    let private pRecord (delim: char) =
        sepBy1 (pField delim) (pchar delim) |>> fun struct (fields, _) -> fields |> Seq.toList

    /// Line ending: CRLF or LF.
    let private pLineEnd =
        (pstring "\r\n" >>% ()) <|> (pchar '\n' >>% ())

    /// Optional trailing line ending at EOF.
    let private pOptLineEnd =
        opt pLineEnd >>% ()

    // ============================================================================
    // Document Parser
    // ============================================================================

    /// Parse the full CSV document with the given config.
    let private pDocument (config: CsvConfig) =
        parser {
            let delim = config.Delimiter
            if config.HasHeaders then
                let! headerRow = pRecord delim
                let! _ = pLineEnd
                let! rows, _ = sepBy (pRecord delim) pLineEnd
                let! _ = pOptLineEnd
                let! _ = eof
                let dataRows = rows |> Seq.toList |> List.filter (fun r -> not (r.Length = 1 && r.[0] = ""))
                return { Headers = headerRow; Rows = dataRows }
            else
                let! rows, _ = sepBy (pRecord delim) pLineEnd
                let! _ = pOptLineEnd
                let! _ = eof
                let dataRows = rows |> Seq.toList |> List.filter (fun r -> not (r.Length = 1 && r.[0] = ""))
                return { Headers = []; Rows = dataRows }
        }

    /// Parse a CSV string with the given configuration.
    let parseWith (config: CsvConfig) (input: string) : Result<CsvDocument, string> =
        let reader = Reader.ofString input ()
        match pDocument config reader with
        | Ok result -> Ok result.Parsed
        | Error err ->
            let pos = err.Position.Index
            Error $"CSV parse error at position {pos}: {err.Errors}"

    /// Parse a CSV string with default configuration (comma-delimited, with headers).
    let parse (input: string) : Result<CsvDocument, string> =
        parseWith CsvConfig.defaults input
