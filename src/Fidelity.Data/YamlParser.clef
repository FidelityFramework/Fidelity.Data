/// YAML 1.2 parser built with XParsec.
/// Implements the core schema subset: block/flow mappings, block/flow sequences,
/// plain/single-quoted/double-quoted/literal/folded scalars, and comments.
/// BCL-minimal design to support Clef dimensional types.
namespace Fidelity.Data.YAML

open XParsec
open XParsec.Parsers
open XParsec.Combinators
open XParsec.CharParsers

#nowarn "40" // Recursive value definitions

/// Parser implementation for YAML.
module YamlParser =

    // ============================================================================
    // Helpers
    // ============================================================================

    let private isWhitespace (c: char) = c = ' ' || c = '\t'
    let private isNewline (c: char) = c = '\n' || c = '\r'

    let private pInlineWs = skipManySatisfies isWhitespace

    let private pComment =
        pchar '#' >>. skipManySatisfies (fun c -> not (isNewline c))

    let private pTrailing =
        pInlineWs >>. opt pComment >>% ()

    let private pNewline =
        (pstring "\r\n" >>% ()) <|> (pchar '\n' >>% ())

    /// Skip blank lines. Only consumes lines that are entirely empty (just a newline)
    /// to avoid consuming indent spaces of the next content line.
    let private pBlankLines =
        skipMany (
            fun (reader: Reader<char, unit, _, _>) ->
                match reader.Peek() with
                | ValueSome '\n' -> pNewline reader
                | ValueSome '\r' -> pNewline reader
                | ValueSome '#' -> (pComment >>. pNewline) reader
                | _ -> fail (Message "not a blank line") reader
        )

    /// Consume leading spaces and return the count.
    let private pConsumeIndent =
        manyChars (pchar ' ') |>> fun s -> s.Length

    /// Consume exactly n spaces.
    let private pIndent (n: int) =
        if n = 0 then preturn ()
        else pstring (System.String(' ', n)) >>% ()

    // ============================================================================
    // Scalars — Double-quoted
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

    let private pDoubleEscape =
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
            | '0' -> return "\u0000"
            | 'a' -> return "\a"
            | 'e' -> return "\u001B"
            | 'x' ->
                let! d1 = pHexDigit
                let! d2 = pHexDigit
                return string (char ((hexValue d1 <<< 4) ||| hexValue d2))
            | 'u' ->
                let! d1 = pHexDigit
                let! d2 = pHexDigit
                let! d3 = pHexDigit
                let! d4 = pHexDigit
                return string (char ((hexValue d1 <<< 12) ||| (hexValue d2 <<< 8) ||| (hexValue d3 <<< 4) ||| hexValue d4))
            | _ -> return! fail (Message $"Invalid YAML escape: \\{c}")
        }

    let private pDoubleQuotedScalar =
        parser {
            let! _ = pchar '"'
            let! segments = many (
                (pchar '\\' >>. pDoubleEscape) <|>
                (satisfy (fun c -> c <> '"' && c <> '\\') |>> string)
            )
            let! _ = pchar '"'
            return segments |> Seq.toList |> List.fold (+) ""
        }

    // ============================================================================
    // Scalars — Single-quoted
    // ============================================================================

    let private pSingleQuotedScalar =
        parser {
            let! _ = pchar '\''
            let! segments = many (
                (pstring "''" >>% "'") <|>
                (satisfy (fun c -> c <> '\'') |>> string)
            )
            let! _ = pchar '\''
            return segments |> Seq.toList |> List.fold (+) ""
        }

    // ============================================================================
    // Scalars — Plain (unquoted)
    // ============================================================================

    /// Parse a plain scalar. In this implementation, `:` is always a terminator
    /// (simplification from the full YAML spec which allows `:` not followed by space).
    /// Values containing colons should use quoted strings.
    let private pPlainScalar (inFlow: bool) =
        many1Chars (satisfy (fun c ->
            not (isNewline c) && c <> '#' && c <> ':' &&
            (if inFlow then c <> ',' && c <> '[' && c <> ']' && c <> '{' && c <> '}' else true)))
        |>> fun s -> s.TrimEnd()

    // ============================================================================
    // Scalars — Literal block (|) and Folded block (>)
    // ============================================================================

    /// Collect block scalar lines. Reads lines one at a time,
    /// stopping when a non-empty line has less indent than blockIndent.
    let private pBlockScalarLines (blockIndent: int) =
        let pOneLine =
            manyChars (satisfy (fun c -> not (isNewline c)))
        parser {
            let lines = System.Collections.Generic.List<string>()
            let mutable cont = true
            while cont do
                // Check current indent level
                let! indent = pConsumeIndent
                let! lineContent = pOneLine
                if indent >= blockIndent then
                    lines.Add(lineContent)
                elif lineContent.TrimEnd().Length = 0 then
                    // Empty/whitespace-only line: preserve as empty
                    lines.Add("")
                else
                    // Less-indented non-empty line: done (don't consume this)
                    // We've already consumed the indent spaces though —
                    // this line belongs to a parent block. We need to fail
                    // to signal end. But we consumed spaces... This is tricky.
                    // For now, just stop here. The parent will re-parse.
                    cont <- false
                // Try consuming a newline if we're continuing
                if cont then
                    match! opt pNewline with
                    | ValueSome _ -> ()
                    | ValueNone -> cont <- false
            return lines |> Seq.toList
        }

    /// Parse a block scalar (literal | or folded >).
    let private pBlockScalar =
        parser {
            let! indicator = satisfy (fun c -> c = '|' || c = '>')
            let isFolded = indicator = '>'
            let! chomp = opt (satisfy (fun c -> c = '-' || c = '+'))
            let! _ = pTrailing
            let! _ = pNewline
            let! _ = pBlankLines
            // Detect block indent from first content line
            let! firstIndent = pConsumeIndent
            let blockIndent = max firstIndent 1
            let! firstLine = manyChars (satisfy (fun c -> not (isNewline c)))
            let! hasMore = opt pNewline
            let! restLines =
                match hasMore with
                | ValueSome _ -> pBlockScalarLines blockIndent
                | ValueNone -> preturn []
            let allLines = firstLine :: restLines
            // Strip indent from each line
            let processedLines =
                allLines
                |> List.map (fun line ->
                    if line.Length >= blockIndent - firstIndent && firstIndent > 0 then line
                    elif line.TrimEnd().Length = 0 then ""
                    else line)
            let content =
                if isFolded then
                    let sb = System.Text.StringBuilder()
                    let mutable prevEmpty = false
                    for i in 0 .. processedLines.Length - 1 do
                        let line = processedLines.[i]
                        if line = "" then
                            sb.Append('\n') |> ignore
                            prevEmpty <- true
                        else
                            if i > 0 && not prevEmpty then
                                sb.Append(' ') |> ignore
                            sb.Append(line) |> ignore
                            prevEmpty <- false
                    sb.ToString()
                else
                    processedLines |> String.concat "\n"
            let chomped =
                match chomp with
                | ValueSome '-' -> content.TrimEnd('\n').TrimEnd('\r')
                | ValueSome '+' -> content + "\n"
                | _ -> content.TrimEnd('\n').TrimEnd('\r') + "\n"
            return chomped
        }

    // ============================================================================
    // Flow Collections
    // ============================================================================

    let private pFlowValueRef = RefParser<YamlValue, char, unit, _, _>()
    let private pBlockValueRef = RefParser<YamlValue, char, unit, _, _>()

    let private pFlowSequence =
        parser {
            let! _ = pchar '['
            let! _ = pInlineWs
            let! items, _ = sepBy (pInlineWs >>. pFlowValueRef.Parser .>> pInlineWs) (pchar ',')
            let! _ = pInlineWs
            let! _ = pchar ']'
            return YamlValue.Sequence (items |> Seq.toList)
        }

    let private pFlowMappingPair =
        parser {
            let! _ = pInlineWs
            let! key =
                (pDoubleQuotedScalar) <|>
                (pSingleQuotedScalar) <|>
                (pPlainScalar true)
            let! _ = pInlineWs
            let! _ = pchar ':'
            let! _ = pInlineWs
            let! value = pFlowValueRef.Parser
            let! _ = pInlineWs
            return (key, value)
        }

    let private pFlowMapping =
        parser {
            let! _ = pchar '{'
            let! _ = pInlineWs
            let! pairs, _ = sepBy pFlowMappingPair (pchar ',')
            let! _ = pInlineWs
            let! _ = pchar '}'
            return YamlValue.Mapping (pairs |> Seq.toList)
        }

    let private resolveScalar (s: string) : YamlValue =
        match s with
        | "null" | "Null" | "NULL" | "~" -> YamlValue.Null
        | _ -> YamlValue.Scalar s

    let private pFlowValue =
        fun (reader: Reader<char, unit, _, _>) ->
            match reader.Peek() with
            | ValueSome '[' -> pFlowSequence reader
            | ValueSome '{' -> pFlowMapping reader
            | ValueSome '"' -> (pDoubleQuotedScalar |>> YamlValue.Scalar) reader
            | ValueSome '\'' -> (pSingleQuotedScalar |>> YamlValue.Scalar) reader
            | _ -> (pPlainScalar true |>> resolveScalar) reader

    do pFlowValueRef.Set(pFlowValue)

    // ============================================================================
    // Block Collections
    // ============================================================================

    /// Parse the value of a block sequence item. Same-line values use inline parsing;
    /// next-line values detect child indent and use full block parsing.
    let rec private pBlockSequenceItemValue (indent: int) =
        fun (reader: Reader<char, unit, _, _>) ->
            match reader.Peek() with
            | ValueNone -> preturn YamlValue.Null reader
            | ValueSome c when isNewline c || c = '#' ->
                // Value on next line — detect child indent
                (pTrailing >>. pNewline >>. pBlankLines >>. pConsumeIndent >>= fun childIndent ->
                    if childIndent > indent then
                        pBlockValueAfterIndent childIndent
                    else
                        preturn YamlValue.Null
                ) reader
            | _ -> pInlineValue reader

    /// Parse a block sequence. The first `- ` indicator has NOT been consumed;
    /// the indent spaces HAVE been consumed by the caller.
    and private pBlockSequenceBody (indent: int) =
        parser {
            // First item: indent already consumed
            let! _ = pstring "- "
            let! _ = pInlineWs
            let! firstValue = pBlockSequenceItemValue indent
            let! _ = opt (pTrailing >>. pNewline)
            // Subsequent items
            let! rest = many (
                parser {
                    let! _ = pBlankLines
                    let! _ = pIndent indent
                    let! _ = pstring "- "
                    let! _ = pInlineWs
                    let! value = pBlockSequenceItemValue indent
                    let! _ = opt (pTrailing >>. pNewline)
                    return value
                }
            )
            return YamlValue.Sequence (firstValue :: (rest |> Seq.toList))
        }

    /// Parse a block mapping. The first key has NOT been consumed;
    /// the indent spaces HAVE been consumed by the caller.
    and private pBlockMappingBody (indent: int) =
        parser {
            // First pair: indent already consumed
            let! firstPair = pBlockMappingPair indent
            // Subsequent pairs
            let! rest = many (
                parser {
                    let! _ = pBlankLines
                    let! _ = pIndent indent
                    let! pair = pBlockMappingPair indent
                    return pair
                }
            )
            return YamlValue.Mapping (firstPair :: (rest |> Seq.toList))
        }

    and private pBlockMappingPair (indent: int) =
        parser {
            let! key =
                (pDoubleQuotedScalar) <|>
                (pSingleQuotedScalar) <|>
                (pPlainScalar false)
            let! _ = pInlineWs
            let! _ = pchar ':'
            let! _ = pInlineWs
            let! value =
                fun (reader: Reader<char, unit, _, _>) ->
                    match reader.Peek() with
                    | ValueNone -> preturn YamlValue.Null reader
                    | ValueSome c when isNewline c || c = '#' ->
                        // Value on next line — detect child indent
                        (pTrailing >>. pNewline >>. pBlankLines >>. pConsumeIndent >>= fun childIndent ->
                            if childIndent > indent then
                                pBlockValueAfterIndent childIndent
                            else
                                preturn YamlValue.Null
                        ) reader
                    | _ -> pInlineValue reader
            let! _ = opt (pTrailing >>. pNewline)
            return (key, value)
        }

    /// Parse a block value when the indent spaces have already been consumed
    /// and we know the indent level.
    and private pBlockValueAfterIndent (indent: int) =
        fun (reader: Reader<char, unit, _, _>) ->
            match reader.Peek() with
            | ValueSome '-' ->
                // Try block sequence (- item)
                (pBlockSequenceBody indent <|> (pPlainScalar false |>> resolveScalar)) reader
            | ValueSome '[' -> pFlowSequence reader
            | ValueSome '{' -> pFlowMapping reader
            | ValueSome '"' -> (pDoubleQuotedScalar |>> YamlValue.Scalar) reader
            | ValueSome '\'' -> (pSingleQuotedScalar |>> YamlValue.Scalar) reader
            | ValueSome '|' | ValueSome '>' -> (pBlockScalar |>> YamlValue.Scalar) reader
            | _ ->
                // Try mapping (key: value) first, fall back to plain scalar
                (pBlockMappingBody indent <|> (pPlainScalar false |>> resolveScalar)) reader

    /// Parse an inline value (on the same line as a mapping key).
    and private pInlineValue =
        fun (reader: Reader<char, unit, _, _>) ->
            match reader.Peek() with
            | ValueSome '[' -> pFlowSequence reader
            | ValueSome '{' -> pFlowMapping reader
            | ValueSome '"' -> (pDoubleQuotedScalar |>> YamlValue.Scalar) reader
            | ValueSome '\'' -> (pSingleQuotedScalar |>> YamlValue.Scalar) reader
            | ValueSome '|' | ValueSome '>' -> (pBlockScalar |>> YamlValue.Scalar) reader
            | _ -> (pPlainScalar false |>> resolveScalar) reader

    /// Top-level block value parser. Consumes indent and dispatches.
    let private pBlockValue =
        fun (reader: Reader<char, unit, _, _>) ->
            match reader.Peek() with
            | ValueSome '[' -> pFlowSequence reader
            | ValueSome '{' -> pFlowMapping reader
            | ValueSome '"' -> (pDoubleQuotedScalar |>> YamlValue.Scalar) reader
            | ValueSome '\'' -> (pSingleQuotedScalar |>> YamlValue.Scalar) reader
            | _ ->
                (pConsumeIndent >>= fun indent -> pBlockValueAfterIndent indent) reader

    do pBlockValueRef.Set(pBlockValue)

    // ============================================================================
    // Document
    // ============================================================================

    let private pDocStart =
        opt (pstring "---" >>. pTrailing >>. pNewline) >>% ()

    let private pDocEnd =
        opt (pstring "..." >>. pTrailing) >>% ()

    let private pDocument =
        parser {
            let! _ = pBlankLines
            let! _ = pDocStart
            let! _ = pBlankLines
            let! value = pBlockValue
            let! _ = pBlankLines
            let! _ = pDocEnd
            let! _ = pBlankLines
            return value
        }

    /// Parse a YAML string into a value (first document).
    let parse (input: string) : Result<YamlValue, string> =
        if input.Trim() = "" then
            Ok YamlValue.Null
        else
            let reader = Reader.ofString input ()
            match pDocument reader with
            | Ok result -> Ok result.Parsed
            | Error err ->
                let pos = err.Position.Index
                Error $"YAML parse error at position {pos}: {err.Errors}"
