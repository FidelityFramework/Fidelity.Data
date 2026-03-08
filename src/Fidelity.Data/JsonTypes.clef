/// JSON value types for representing parsed JSON.
/// Covers objects, arrays, strings, numbers, booleans, and null.
/// Designed to be BCL-minimal, to support Clef dimensional types.
namespace Fidelity.Data.JSON

/// A JSON value.
[<RequireQualifiedAccess>]
type JsonValue =
    /// A JSON object: ordered list of key-value pairs.
    | Object of (string * JsonValue) list
    /// A JSON array.
    | Array of JsonValue list
    /// A JSON string.
    | String of string
    /// A JSON number (stored as float64 per IEEE 754, the JSON standard).
    | Number of float
    /// A JSON boolean.
    | Bool of bool
    /// A JSON null.
    | Null

/// Module for navigating and querying JSON values.
module JsonValue =

    /// Get a property from an object by key, or None.
    let prop (key: string) (value: JsonValue) : JsonValue option =
        match value with
        | JsonValue.Object pairs -> pairs |> List.tryFind (fun (k, _) -> k = key) |> Option.map snd
        | _ -> None

    /// Get a property value with a default.
    let propDefault (key: string) (defaultValue: JsonValue) (value: JsonValue) : JsonValue =
        match prop key value with
        | Some v -> v
        | None -> defaultValue

    /// Get the keys of an object.
    let keys (value: JsonValue) : string list =
        match value with
        | JsonValue.Object pairs -> pairs |> List.map fst
        | _ -> []

    /// Get all values from an object.
    let values (value: JsonValue) : JsonValue list =
        match value with
        | JsonValue.Object pairs -> pairs |> List.map snd
        | _ -> []

    /// Get elements of an array, or empty list.
    let items (value: JsonValue) : JsonValue list =
        match value with
        | JsonValue.Array items -> items
        | _ -> []

    /// Get the item at an index in an array.
    let item (index: int) (value: JsonValue) : JsonValue option =
        match value with
        | JsonValue.Array items when index >= 0 && index < items.Length -> Some items.[index]
        | _ -> None

    /// Extract a string value.
    let asString (value: JsonValue) : string option =
        match value with
        | JsonValue.String s -> Some s
        | _ -> None

    /// Extract a number value.
    let asNumber (value: JsonValue) : float option =
        match value with
        | JsonValue.Number n -> Some n
        | _ -> None

    /// Extract an integer value (if the number has no fractional part).
    let asInt (value: JsonValue) : int64 option =
        match value with
        | JsonValue.Number n when n = float (int64 n) -> Some (int64 n)
        | _ -> None

    /// Extract a boolean value.
    let asBool (value: JsonValue) : bool option =
        match value with
        | JsonValue.Bool b -> Some b
        | _ -> None

    /// Check if the value is null.
    let isNull (value: JsonValue) : bool =
        match value with
        | JsonValue.Null -> true
        | _ -> false

    /// Count of elements (array length or object key count).
    let count (value: JsonValue) : int =
        match value with
        | JsonValue.Object pairs -> pairs.Length
        | JsonValue.Array items -> items.Length
        | _ -> 0
