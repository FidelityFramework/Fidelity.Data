/// JSON parser built with XParsec.
/// Implements RFC 8259 (The JavaScript Object Notation Data Interchange Format).
/// BCL-minimal design to support Clef dimensional types.
namespace Fidelity.Data.JSON

open XParsec
open XParsec.Parsers
open XParsec.Combinators
open XParsec.CharParsers

#nowarn "40" // Recursive value definitions

/// Parser implementation for JSON.
module JsonParser =

    // ============================================================================
    // Whitespace
    // ============================================================================

    let private isJsonWs (c: char) =
        c = ' ' || c = '\t' || c = '\n' || c = '\r'

    let private pWs = skipManySatisfies isJsonWs

    // ============================================================================
    // Null, Boolean
    // ============================================================================

    let private pNull =
        pstring "null" >>% JsonValue.Null

    let private pTrue =
        pstring "true" >>% JsonValue.Bool true

    let private pFalse =
        pstring "false" >>% JsonValue.Bool false

    // ============================================================================
    // Numbers
    // ============================================================================

    let private isDigit (c: char) = c >= '0' && c <= '9'

    /// JSON number: optional minus, integer part, optional fraction, optional exponent.
    let private pNumber =
        parser {
            let! chars, _ =
                manyCharsTill anyChar (
                    fun (reader: Reader<char, unit, _, _>) ->
                        match reader.Peek() with
                        | ValueSome c when isDigit c || c = '+' || c = '-' || c = '.' || c = 'e' || c = 'E' ->
                            fail (Message "still in number") reader
                        | _ ->
                            preturn () reader
                )
            match System.Double.TryParse(chars, System.Globalization.NumberStyles.Float, System.Globalization.CultureInfo.InvariantCulture) with
            | true, v -> return JsonValue.Number v
            | false, _ -> return! fail (Message $"Invalid number: {chars}")
        }

    // ============================================================================
    // Strings
    // ============================================================================

    let private pHexDigit =
        satisfy (fun c ->
            (c >= '0' && c <= '9') ||
            (c >= 'a' && c <= 'f') ||
            (c >= 'A' && c <= 'F'))

    let private hexValue (c: char) =
        if c >= '0' && c <= '9' then int c - int '0'
        elif c >= 'a' && c <= 'f' then int c - int 'a' + 10
        else int c - int 'A' + 10

    /// Parse a 4-hex-digit unicode escape: \uXXXX
    let private pUnicodeEscape =
        parser {
            let! d1 = pHexDigit
            let! d2 = pHexDigit
            let! d3 = pHexDigit
            let! d4 = pHexDigit
            let value = (hexValue d1 <<< 12) ||| (hexValue d2 <<< 8) ||| (hexValue d3 <<< 4) ||| hexValue d4
            return value
        }

    /// Parse a single escape sequence after the backslash.
    let private pEscape =
        parser {
            let! c = anyChar
            match c with
            | '"' -> return "\""
            | '\\' -> return "\\"
            | '/' -> return "/"
            | 'b' -> return "\b"
            | 'f' -> return "\f"
            | 'n' -> return "\n"
            | 'r' -> return "\r"
            | 't' -> return "\t"
            | 'u' ->
                let! high = pUnicodeEscape
                // Handle surrogate pairs
                if high >= 0xD800 && high <= 0xDBFF then
                    let! _ = pchar '\\'
                    let! _ = pchar 'u'
                    let! low = pUnicodeEscape
                    let codePoint = 0x10000 + ((high - 0xD800) <<< 10) + (low - 0xDC00)
                    return System.Char.ConvertFromUtf32(codePoint)
                else
                    return string (char high)
            | _ -> return! fail (Message $"Invalid escape: \\{c}")
        }

    /// Parse a JSON string character (not quote, not backslash, not control char).
    let private pStringChar =
        satisfy (fun c -> c <> '"' && c <> '\\' && c > '\u001F') |>> string

    /// Parse a string segment: either a regular char or an escape sequence.
    let private pStringSegment =
        (pchar '\\' >>. pEscape) <|> pStringChar

    /// Parse a JSON string (the content between quotes).
    let private pJsonString =
        pchar '"' >>. many pStringSegment .>> pchar '"'
        |>> fun parts -> parts |> Seq.toList |> List.fold (+) ""

    let private pStringValue =
        pJsonString |>> JsonValue.String

    // ============================================================================
    // Recursive structures (Object, Array)
    // ============================================================================

    let private pValueRef = RefParser<JsonValue, char, unit, _, _>()

    /// Array: [ value, value, ... ]
    let private pArray =
        parser {
            let! _ = pchar '['
            let! _ = pWs
            let! items, _ = sepBy (pWs >>. pValueRef.Parser .>> pWs) (pchar ',')
            let! _ = pWs
            let! _ = pchar ']'
            return JsonValue.Array (items |> Seq.toList)
        }

    /// Object key-value pair: "key" : value
    let private pKeyValue =
        parser {
            let! _ = pWs
            let! key = pJsonString
            let! _ = pWs
            let! _ = pchar ':'
            let! _ = pWs
            let! value = pValueRef.Parser
            let! _ = pWs
            return (key, value)
        }

    /// Object: { "key": value, ... }
    let private pObject =
        parser {
            let! _ = pchar '{'
            let! _ = pWs
            let! pairs, _ = sepBy pKeyValue (pchar ',')
            let! _ = pWs
            let! _ = pchar '}'
            return JsonValue.Object (pairs |> Seq.toList)
        }

    // ============================================================================
    // Value (any JSON value)
    // ============================================================================

    let private pValue =
        fun (reader: Reader<char, unit, _, _>) ->
            match reader.Peek() with
            | ValueSome '"' -> pStringValue reader
            | ValueSome '{' -> pObject reader
            | ValueSome '[' -> pArray reader
            | ValueSome 't' -> pTrue reader
            | ValueSome 'f' -> pFalse reader
            | ValueSome 'n' -> pNull reader
            | ValueSome c when c = '-' || isDigit c -> pNumber reader
            | ValueSome c -> fail (Message $"Unexpected character: '{c}'") reader
            | ValueNone -> fail (Message "Unexpected end of input") reader

    do pValueRef.Set(pValue)

    // ============================================================================
    // Document
    // ============================================================================

    let private pDocument =
        parser {
            let! _ = pWs
            let! value = pValue
            let! _ = pWs
            let! _ = eof
            return value
        }

    /// Parse a JSON string into a value.
    let parse (input: string) : Result<JsonValue, string> =
        let reader = Reader.ofString input ()
        match pDocument reader with
        | Ok result -> Ok result.Parsed
        | Error err ->
            let pos = err.Position.Index
            Error $"JSON parse error at position {pos}: {err.Errors}"
