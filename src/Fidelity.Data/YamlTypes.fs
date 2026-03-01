/// YAML value types for representing parsed YAML documents.
/// Covers mappings, sequences, and scalar values.
/// Designed to be BCL-minimal, to support Clef dimensional types.
namespace Fidelity.Data.YAML

/// A YAML value node.
[<RequireQualifiedAccess>]
type YamlValue =
    /// A mapping (ordered key-value pairs). Keys are strings.
    | Mapping of (string * YamlValue) list
    /// A sequence (ordered list of values).
    | Sequence of YamlValue list
    /// A scalar string value. YAML scalars are always strings at the
    /// representation level; typed interpretation is left to consumers.
    | Scalar of string
    /// An explicit null (~, null, or empty value).
    | Null

/// Module for navigating and querying YAML values.
module YamlValue =

    /// Get a value from a mapping by key, or None.
    let prop (key: string) (value: YamlValue) : YamlValue option =
        match value with
        | YamlValue.Mapping pairs -> pairs |> List.tryFind (fun (k, _) -> k = key) |> Option.map snd
        | _ -> None

    /// Get the keys of a mapping.
    let keys (value: YamlValue) : string list =
        match value with
        | YamlValue.Mapping pairs -> pairs |> List.map fst
        | _ -> []

    /// Get the values of a mapping.
    let values (value: YamlValue) : YamlValue list =
        match value with
        | YamlValue.Mapping pairs -> pairs |> List.map snd
        | _ -> []

    /// Get items of a sequence, or empty list.
    let items (value: YamlValue) : YamlValue list =
        match value with
        | YamlValue.Sequence items -> items
        | _ -> []

    /// Get a sequence item by index.
    let item (index: int) (value: YamlValue) : YamlValue option =
        match value with
        | YamlValue.Sequence items when index >= 0 && index < items.Length -> Some items.[index]
        | _ -> None

    /// Extract the scalar string value.
    let asString (value: YamlValue) : string option =
        match value with
        | YamlValue.Scalar s -> Some s
        | _ -> None

    /// Interpret a scalar as an integer.
    let asInt (value: YamlValue) : int64 option =
        match value with
        | YamlValue.Scalar s ->
            match System.Int64.TryParse(s) with
            | true, v -> Some v
            | _ -> None
        | _ -> None

    /// Interpret a scalar as a float.
    let asFloat (value: YamlValue) : float option =
        match value with
        | YamlValue.Scalar s ->
            match s with
            | ".inf" | ".Inf" | ".INF" -> Some infinity
            | "-.inf" | "-.Inf" | "-.INF" -> Some (-infinity)
            | ".nan" | ".NaN" | ".NAN" -> Some nan
            | _ ->
                match System.Double.TryParse(s, System.Globalization.NumberStyles.Float,
                                              System.Globalization.CultureInfo.InvariantCulture) with
                | true, v -> Some v
                | _ -> None
        | _ -> None

    /// Interpret a scalar as a boolean (YAML 1.2 core schema).
    let asBool (value: YamlValue) : bool option =
        match value with
        | YamlValue.Scalar s ->
            match s with
            | "true" | "True" | "TRUE" -> Some true
            | "false" | "False" | "FALSE" -> Some false
            | _ -> None
        | _ -> None

    /// Check if the value is null.
    let isNull (value: YamlValue) : bool =
        match value with
        | YamlValue.Null -> true
        | YamlValue.Scalar s -> s = "~" || s = "null" || s = "Null" || s = "NULL" || s = ""
        | _ -> false

    /// Count of elements (sequence length or mapping key count).
    let count (value: YamlValue) : int =
        match value with
        | YamlValue.Mapping pairs -> pairs.Length
        | YamlValue.Sequence items -> items.Length
        | _ -> 0
