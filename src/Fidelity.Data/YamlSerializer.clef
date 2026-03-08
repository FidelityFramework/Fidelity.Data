/// YAML Serializer — converts YamlValue back to valid YAML text.
///
/// Emits block-style YAML (the most human-readable form).
/// Design: Pure, no dependencies beyond the types in YamlTypes.fs.
namespace Fidelity.Data.YAML

open System.Text

module YamlSerializer =

    // =========================================================================
    // String Formatting
    // =========================================================================

    /// Check if a string needs quoting.
    let private needsQuoting (s: string) : bool =
        if s = "" then true
        elif s = "~" || s = "null" || s = "Null" || s = "NULL" then true
        elif s = "true" || s = "True" || s = "TRUE" then true
        elif s = "false" || s = "False" || s = "FALSE" then true
        elif s.Contains(':') || s.Contains('#') || s.Contains('\n') then true
        elif s.Contains('"') || s.Contains('\'') then true
        elif s.StartsWith(' ') || s.EndsWith(' ') then true
        elif s.StartsWith('-') || s.StartsWith('[') || s.StartsWith('{') then true
        else
            match System.Double.TryParse(s, System.Globalization.NumberStyles.Float,
                                          System.Globalization.CultureInfo.InvariantCulture) with
            | true, _ -> true
            | _ -> false

    /// Quote a string with double quotes, escaping as needed.
    let private quoteString (s: string) : string =
        let sb = StringBuilder(s.Length + 4)
        sb.Append('"') |> ignore
        for c in s do
            match c with
            | '"' -> sb.Append("\\\"") |> ignore
            | '\\' -> sb.Append("\\\\") |> ignore
            | '\n' -> sb.Append("\\n") |> ignore
            | '\r' -> sb.Append("\\r") |> ignore
            | '\t' -> sb.Append("\\t") |> ignore
            | c when c < '\u0020' -> sb.Append($"\\x{int c:X2}") |> ignore
            | c -> sb.Append(c) |> ignore
        sb.Append('"') |> ignore
        sb.ToString()

    /// Format a scalar for output, quoting if necessary.
    let private formatScalar (s: string) : string =
        if needsQuoting s then quoteString s
        else s

    // =========================================================================
    // Serialization
    // =========================================================================

    /// Serialize a YAML value to a string with block style.
    let serialize (value: YamlValue) : string =
        let sb = StringBuilder()
        let rec go indent value =
            let prefix = System.String(' ', indent)
            match value with
            | YamlValue.Null -> sb.Append("null") |> ignore
            | YamlValue.Scalar s -> sb.Append(formatScalar s) |> ignore
            | YamlValue.Sequence [] -> sb.Append("[]") |> ignore
            | YamlValue.Sequence items ->
                items |> List.iteri (fun i item ->
                    if i > 0 || indent > 0 then
                        sb.Append(prefix) |> ignore
                    sb.Append("- ") |> ignore
                    match item with
                    | YamlValue.Mapping _ | YamlValue.Sequence _ ->
                        sb.AppendLine() |> ignore
                        go (indent + 2) item
                    | _ ->
                        go 0 item
                        sb.AppendLine() |> ignore
                )
            | YamlValue.Mapping [] -> sb.Append("{}") |> ignore
            | YamlValue.Mapping pairs ->
                pairs |> List.iteri (fun i (key, value) ->
                    if i > 0 || indent > 0 then
                        sb.Append(prefix) |> ignore
                    sb.Append(formatScalar key).Append(':') |> ignore
                    match value with
                    | YamlValue.Mapping _ | YamlValue.Sequence _ ->
                        sb.AppendLine() |> ignore
                        go (indent + 2) value
                    | _ ->
                        sb.Append(' ') |> ignore
                        go 0 value
                        sb.AppendLine() |> ignore
                )
        go 0 value
        sb.ToString().TrimEnd('\n', '\r')
        |> fun s -> if s.Length > 0 then s + "\n" else s
