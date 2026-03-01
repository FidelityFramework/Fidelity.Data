/// CSV Serializer — converts CsvDocument back to valid CSV text.
///
/// Follows the RFC 4180 quoting rules: fields containing the delimiter,
/// quotes, or newlines are quoted; embedded quotes are doubled.
/// Design: Pure, no dependencies beyond the types in CsvTypes.fs.
namespace Fidelity.Data.CSV

open System.Text

module CsvSerializer =

    // =========================================================================
    // Field Formatting
    // =========================================================================

    /// Check whether a field needs quoting.
    let private needsQuoting (delim: char) (field: string) : bool =
        field.IndexOf(delim) >= 0 ||
        field.IndexOf('"') >= 0 ||
        field.IndexOf('\n') >= 0 ||
        field.IndexOf('\r') >= 0

    /// Quote a field value (double any embedded quotes).
    let private quoteField (field: string) : string =
        let sb = StringBuilder(field.Length + 4)
        sb.Append('"') |> ignore
        for c in field do
            if c = '"' then sb.Append("\"\"") |> ignore
            else sb.Append(c) |> ignore
        sb.Append('"') |> ignore
        sb.ToString()

    /// Format a single field, quoting if necessary.
    let private formatField (delim: char) (field: string) : string =
        if needsQuoting delim field then quoteField field
        else field

    // =========================================================================
    // Serialization
    // =========================================================================

    /// Serialize a CSV document with the given delimiter.
    let serializeWith (delim: char) (doc: CsvDocument) : string =
        let sb = StringBuilder()
        let writeRow (fields: string list) =
            fields |> List.iteri (fun i field ->
                if i > 0 then sb.Append(delim) |> ignore
                sb.Append(formatField delim field) |> ignore)
            sb.Append("\r\n") |> ignore
        if not doc.Headers.IsEmpty then
            writeRow doc.Headers
        for row in doc.Rows do
            writeRow row
        sb.ToString()

    /// Serialize a CSV document with default comma delimiter.
    let serialize (doc: CsvDocument) : string =
        serializeWith ',' doc
