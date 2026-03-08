/// JSON Serializer — converts JsonValue back to valid JSON text.
///
/// Follows the DU->text pattern: match each JsonValue case, emit the appropriate
/// JSON representation. Design: Pure, no dependencies beyond the types in JsonTypes.fs.
namespace Fidelity.Data.JSON

open System.Text

module JsonSerializer =

    // =========================================================================
    // String Escaping
    // =========================================================================

    /// Escape a string for JSON output.
    let private escapeString (s: string) : string =
        let sb = StringBuilder(s.Length + 2)
        sb.Append('"') |> ignore
        for c in s do
            match c with
            | '"' -> sb.Append("\\\"") |> ignore
            | '\\' -> sb.Append("\\\\") |> ignore
            | '\b' -> sb.Append("\\b") |> ignore
            | '\f' -> sb.Append("\\f") |> ignore
            | '\n' -> sb.Append("\\n") |> ignore
            | '\r' -> sb.Append("\\r") |> ignore
            | '\t' -> sb.Append("\\t") |> ignore
            | c when c < '\u0020' ->
                sb.Append($"\\u{int c:X4}") |> ignore
            | c -> sb.Append(c) |> ignore
        sb.Append('"') |> ignore
        sb.ToString()

    // =========================================================================
    // Number Formatting
    // =========================================================================

    /// Format a number for JSON output.
    let private formatNumber (n: float) : string =
        if System.Double.IsPositiveInfinity(n) then "null"
        elif System.Double.IsNegativeInfinity(n) then "null"
        elif System.Double.IsNaN(n) then "null"
        elif n = float (int64 n) && System.Math.Abs(n) < 1e15 then
            // Integer-valued: emit without decimal point
            let i = int64 n
            string i
        else
            n.ToString("G17", System.Globalization.CultureInfo.InvariantCulture)

    // =========================================================================
    // Serialization
    // =========================================================================

    /// Serialize a JSON value to a string (compact, no whitespace).
    let rec serializeValue (sb: StringBuilder) (value: JsonValue) =
        match value with
        | JsonValue.Null -> sb.Append("null") |> ignore
        | JsonValue.Bool true -> sb.Append("true") |> ignore
        | JsonValue.Bool false -> sb.Append("false") |> ignore
        | JsonValue.Number n -> sb.Append(formatNumber n) |> ignore
        | JsonValue.String s -> sb.Append(escapeString s) |> ignore
        | JsonValue.Array items ->
            sb.Append('[') |> ignore
            items |> List.iteri (fun i item ->
                if i > 0 then sb.Append(',') |> ignore
                serializeValue sb item)
            sb.Append(']') |> ignore
        | JsonValue.Object pairs ->
            sb.Append('{') |> ignore
            pairs |> List.iteri (fun i (key, value) ->
                if i > 0 then sb.Append(',') |> ignore
                sb.Append(escapeString key) |> ignore
                sb.Append(':') |> ignore
                serializeValue sb value)
            sb.Append('}') |> ignore

    /// Serialize a JSON value to a compact string.
    let serialize (value: JsonValue) : string =
        let sb = StringBuilder()
        serializeValue sb value
        sb.ToString()

    /// Serialize a JSON value with indentation for readability.
    let serializePretty (value: JsonValue) : string =
        let sb = StringBuilder()
        let rec go indent value =
            let prefix = System.String(' ', indent)
            let innerPrefix = System.String(' ', indent + 2)
            match value with
            | JsonValue.Null -> sb.Append("null") |> ignore
            | JsonValue.Bool true -> sb.Append("true") |> ignore
            | JsonValue.Bool false -> sb.Append("false") |> ignore
            | JsonValue.Number n -> sb.Append(formatNumber n) |> ignore
            | JsonValue.String s -> sb.Append(escapeString s) |> ignore
            | JsonValue.Array [] -> sb.Append("[]") |> ignore
            | JsonValue.Array items ->
                sb.AppendLine("[") |> ignore
                items |> List.iteri (fun i item ->
                    if i > 0 then sb.AppendLine(",") |> ignore
                    sb.Append(innerPrefix) |> ignore
                    go (indent + 2) item)
                sb.AppendLine() |> ignore
                sb.Append(prefix).Append(']') |> ignore
            | JsonValue.Object [] -> sb.Append("{}") |> ignore
            | JsonValue.Object pairs ->
                sb.AppendLine("{") |> ignore
                pairs |> List.iteri (fun i (key, value) ->
                    if i > 0 then sb.AppendLine(",") |> ignore
                    sb.Append(innerPrefix).Append(escapeString key).Append(": ") |> ignore
                    go (indent + 2) value)
                sb.AppendLine() |> ignore
                sb.Append(prefix).Append('}') |> ignore
        go 0 value
        sb.ToString()
