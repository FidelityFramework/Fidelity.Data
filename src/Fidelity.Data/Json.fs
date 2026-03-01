/// Public API for Fidelity.Data.JSON.
namespace Fidelity.Data.JSON

/// Main entry point for JSON parsing, serialization, and navigation.
[<RequireQualifiedAccess>]
module Json =

    /// Parse a JSON string into a value.
    let parse (input: string) : Result<JsonValue, string> =
        JsonParser.parse input

    /// Parse a JSON string, throwing an exception on failure.
    let parseOrFail (input: string) : JsonValue =
        match parse input with
        | Ok value -> value
        | Error msg -> failwith msg

    /// Serialize a JSON value to a compact string.
    let serialize (value: JsonValue) : string =
        JsonSerializer.serialize value

    /// Serialize a JSON value with indentation for readability.
    let serializePretty (value: JsonValue) : string =
        JsonSerializer.serializePretty value

    /// Get a property from a JSON object by key.
    let prop (key: string) (value: JsonValue) : JsonValue option =
        JsonValue.prop key value

    /// Extract a string value.
    let asString (value: JsonValue) : string option =
        JsonValue.asString value

    /// Extract a number value.
    let asNumber (value: JsonValue) : float option =
        JsonValue.asNumber value

    /// Extract an integer value.
    let asInt (value: JsonValue) : int64 option =
        JsonValue.asInt value

    /// Extract a boolean value.
    let asBool (value: JsonValue) : bool option =
        JsonValue.asBool value

    /// Get array items.
    let items (value: JsonValue) : JsonValue list =
        JsonValue.items value
