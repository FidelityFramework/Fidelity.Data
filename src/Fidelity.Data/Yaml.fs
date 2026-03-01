/// Public API for Fidelity.Data.YAML.
namespace Fidelity.Data.YAML

/// Main entry point for YAML parsing, serialization, and navigation.
[<RequireQualifiedAccess>]
module Yaml =

    /// Parse a YAML string into a value.
    let parse (input: string) : Result<YamlValue, string> =
        YamlParser.parse input

    /// Parse a YAML string, throwing an exception on failure.
    let parseOrFail (input: string) : YamlValue =
        match parse input with
        | Ok value -> value
        | Error msg -> failwith msg

    /// Serialize a YAML value to block-style YAML text.
    let serialize (value: YamlValue) : string =
        YamlSerializer.serialize value

    /// Get a property from a mapping by key.
    let prop (key: string) (value: YamlValue) : YamlValue option =
        YamlValue.prop key value

    /// Get items of a sequence.
    let items (value: YamlValue) : YamlValue list =
        YamlValue.items value

    /// Extract a scalar string value.
    let asString (value: YamlValue) : string option =
        YamlValue.asString value

    /// Interpret a scalar as an integer.
    let asInt (value: YamlValue) : int64 option =
        YamlValue.asInt value

    /// Interpret a scalar as a float.
    let asFloat (value: YamlValue) : float option =
        YamlValue.asFloat value

    /// Interpret a scalar as a boolean.
    let asBool (value: YamlValue) : bool option =
        YamlValue.asBool value
