/// XML 1.0 parser built with XParsec.
/// BCL-minimal design to support Clef dimensional types.
/// Handles elements, attributes, text, CDATA, comments, and processing instructions.
/// Entity references: &amp; &lt; &gt; &apos; &quot; plus &#NNN; and &#xHHH;.
/// Namespace prefixes are treated as part of element/attribute names (e.g. "xmlns:foo").
namespace Fidelity.Data.XML

open XParsec
open XParsec.Parsers
open XParsec.Combinators
open XParsec.CharParsers

#nowarn "40" // Recursive value definitions

/// Parser implementation for XML.
module XmlParser =

    // ============================================================================
    // Helpers
    // ============================================================================

    let private concatStrings (parts: string list) : string =
        parts |> List.fold (+) ""

    let private isWhitespace (c: char) =
        c = ' ' || c = '\t' || c = '\n' || c = '\r'

    // ============================================================================
    // Whitespace
    // ============================================================================

    /// Skip XML whitespace (space, tab, CR, LF).
    let private pWs =
        skipManySatisfies isWhitespace

    /// Require at least one whitespace character.
    let private pWs1 =
        skipMany1Satisfies isWhitespace

    // ============================================================================
    // Character and Entity References
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

    let private pDecDigit = satisfy (fun c -> c >= '0' && c <= '9')

    /// Decimal character reference: &#NNN;
    let private pDecCharRef =
        pstring "&#" >>. many1Chars pDecDigit .>> pchar ';'
        |>> fun digits ->
            let value = digits |> Seq.fold (fun acc c -> acc * 10 + (int c - int '0')) 0
            if value <= 0xFFFF then string (char value)
            else
                let adjusted = value - 0x10000
                let high = char (0xD800 + (adjusted >>> 10))
                let low = char (0xDC00 + (adjusted &&& 0x3FF))
                string high + string low

    /// Hex character reference: &#xHHH;
    let private pHexCharRef =
        pstring "&#x" >>. many1Chars pHexDigit .>> pchar ';'
        |>> fun digits ->
            let value = digits |> Seq.fold (fun acc c -> acc * 16 + hexValue c) 0
            if value <= 0xFFFF then string (char value)
            else
                let adjusted = value - 0x10000
                let high = char (0xD800 + (adjusted >>> 10))
                let low = char (0xDC00 + (adjusted &&& 0x3FF))
                string high + string low

    /// Named entity reference: &amp; &lt; &gt; &apos; &quot;
    let private pNamedEntity =
        pchar '&' >>. manyChars (satisfy (fun c -> c <> ';')) .>> pchar ';'
        |>> fun name ->
            match name with
            | "amp" -> "&"
            | "lt" -> "<"
            | "gt" -> ">"
            | "apos" -> "'"
            | "quot" -> "\""
            | _ -> $"&{name};"  // Preserve unknown entities

    /// Any entity or character reference.
    let private pReference =
        pHexCharRef <|> pDecCharRef <|> pNamedEntity

    // ============================================================================
    // Names (element and attribute names)
    // ============================================================================

    /// Valid XML name start character (letter, underscore, colon for namespaces).
    let private isNameStartChar (c: char) =
        (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c = '_' || c = ':'

    /// Valid XML name character (name start + digit, hyphen, dot).
    let private isNameChar (c: char) =
        isNameStartChar c || (c >= '0' && c <= '9') || c = '-' || c = '.'

    /// XML name: starts with letter/underscore/colon, followed by name chars.
    let private pName =
        parser {
            let! first = satisfy isNameStartChar
            let! rest = manyChars (satisfy isNameChar)
            return string first + rest
        }

    // ============================================================================
    // Attribute Values
    // ============================================================================

    /// Attribute value character (not the quote char, not &, not <).
    let private pAttrValueChar (quote: char) =
        satisfy (fun c -> c <> quote && c <> '&' && c <> '<') |>> string

    /// Attribute value content: chars and entity references.
    let private pAttrValueContent (quote: char) =
        many (pReference <|> pAttrValueChar quote)
        |>> fun parts -> parts |> Seq.toList |> concatStrings

    /// Double-quoted attribute value.
    let private pDoubleQuotedValue =
        pchar '"' >>. pAttrValueContent '"' .>> pchar '"'

    /// Single-quoted attribute value.
    let private pSingleQuotedValue =
        pchar '\'' >>. pAttrValueContent '\'' .>> pchar '\''

    /// Any attribute value (double or single quoted).
    let private pAttrValue =
        pDoubleQuotedValue <|> pSingleQuotedValue

    /// A single attribute: name = "value"
    let private pAttribute =
        parser {
            let! name = pName
            let! _ = pWs
            let! _ = pchar '='
            let! _ = pWs
            let! value = pAttrValue
            return { Name = name; Value = value } : XmlAttribute
        }

    /// Zero or more attributes (each preceded by whitespace).
    let private pAttributes =
        many (pWs1 >>. pAttribute) |>> fun attrs -> attrs |> Seq.toList

    // ============================================================================
    // Comments
    // ============================================================================

    /// XML comment: <!-- ... -->
    let private pComment =
        parser {
            let! _ = pstring "<!--"
            let! content, _ = manyCharsTill anyChar (pstring "-->")
            return XmlNode.Comment content
        }

    // ============================================================================
    // CDATA
    // ============================================================================

    /// CDATA section: <![CDATA[ ... ]]>
    let private pCData =
        parser {
            let! _ = pstring "<![CDATA["
            let! content, _ = manyCharsTill anyChar (pstring "]]>")
            return XmlNode.CData content
        }

    // ============================================================================
    // Processing Instructions
    // ============================================================================

    /// Processing instruction: <?target data?>
    /// Excludes the XML declaration (handled separately).
    let private pPI =
        parser {
            let! _ = pstring "<?"
            let! target = pName
            let! content, _ = manyCharsTill anyChar (pstring "?>")
            let data = content.Trim()
            return XmlNode.ProcessingInstruction (target, data)
        }

    // ============================================================================
    // Text Content
    // ============================================================================

    /// Text character (not < and not &).
    let private pTextChar =
        satisfy (fun c -> c <> '<' && c <> '&') |>> string

    /// Text content: chars and entity references, at least one segment.
    let private pText =
        many1 (pReference <|> pTextChar) |>> fun parts ->
            let text = parts |> Seq.toList |> concatStrings
            XmlNode.Text text

    // ============================================================================
    // Elements (recursive)
    // ============================================================================

    let private pNodeRef = RefParser<XmlNode, char, unit, _, _>()

    /// Content inside an element: mix of child elements, text, comments, CDATA, PIs.
    let private pContent =
        many pNodeRef.Parser |>> fun nodes -> nodes |> Seq.toList

    /// Self-closing element: <name attrs/>
    let private pSelfClosingElement =
        parser {
            let! _ = pchar '<'
            let! elName = pName
            let! attrs = pAttributes
            let! _ = pWs
            let! _ = pstring "/>"
            return XmlNode.Element (elName, attrs, [])
        }

    /// Open/close element: <name attrs>content</name>
    let private pOpenCloseElement =
        parser {
            let! _ = pchar '<'
            let! elName = pName
            let! attrs = pAttributes
            let! _ = pWs
            let! _ = pchar '>'
            let! children = pContent
            let! _ = pstring "</"
            let! closeName = pName
            let! _ = pWs
            let! _ = pchar '>'
            if elName <> closeName then
                return! fail (Message $"Mismatched tags: <{elName}> closed by </{closeName}>")
            else
                return XmlNode.Element (elName, attrs, children)
        }

    /// Any element (self-closing or open/close).
    let private pElement =
        pSelfClosingElement <|> pOpenCloseElement

    /// Comment or CDATA (both start with "<!")
    let private pBangNode =
        pComment <|> pCData

    /// Any XML node (used as the recursive content parser).
    /// Uses combinator-level backtracking — no manual reader manipulation.
    let private pNode =
        fun (reader: Reader<char, unit, _, _>) ->
            match reader.Peek() with
            | ValueSome '<' ->
                // Try each structural node type via combinator backtracking
                choice [
                    pBangNode       // <!-- comment --> or <![CDATA[...]]>
                    pPI             // <?target ...?>
                    pElement        // <name .../> or <name ...>...</name>
                ] reader
            | ValueSome _ ->
                // Text content (including entity refs starting with &)
                pText reader
            | ValueNone ->
                fail (Message "Unexpected end of input") reader

    do pNodeRef.Set(pNode)

    // ============================================================================
    // XML Declaration
    // ============================================================================

    /// Parse the version attribute in XML declaration.
    let private pVersionAttr =
        pstring "version" >>. pWs >>. pchar '=' >>. pWs >>. pAttrValue

    /// Parse the encoding attribute in XML declaration.
    let private pEncodingAttr =
        pWs1 >>. pstring "encoding" >>. pWs >>. pchar '=' >>. pWs >>. pAttrValue

    /// Parse the standalone attribute in XML declaration.
    let private pStandaloneAttr =
        pWs1 >>. pstring "standalone" >>. pWs >>. pchar '=' >>. pWs >>. pAttrValue

    /// XML declaration: <?xml version="1.0" encoding="UTF-8" standalone="yes"?>
    let private pXmlDecl =
        parser {
            let! _ = pstring "<?xml"
            let! _ = pWs1
            let! version = pVersionAttr
            let! encoding = opt pEncodingAttr
            let! standalone = opt pStandaloneAttr
            let! _ = pWs
            let! _ = pstring "?>"
            return {
                Version = version
                Encoding = match encoding with ValueSome e -> Some e | ValueNone -> None
                Standalone = match standalone with ValueSome s -> Some s | ValueNone -> None
            }
        }

    // ============================================================================
    // Miscellaneous (comments, PIs, whitespace between prolog items)
    // ============================================================================

    /// Skip miscellaneous content (whitespace, comments, PIs) in prolog/epilog.
    let private pMisc =
        skipMany (
            (pWs1 >>% ()) <|>
            (pComment >>% ()) <|>
            (pPI >>% ())
        )

    // ============================================================================
    // Document
    // ============================================================================

    /// Parse a complete XML document.
    let private pDocument =
        parser {
            let! _ = pWs
            let! decl = opt pXmlDecl
            let! _ = pMisc
            let! root = pElement
            let! _ = pMisc
            let! _ = pWs
            let! _ = eof
            return {
                Declaration =
                    match decl with
                    | ValueSome d -> Some d
                    | ValueNone -> None
                Root = root
            }
        }

    /// Parse an XML string into a document.
    let parse (input: string) : Result<XmlDocument, string> =
        let reader = Reader.ofString input ()
        match pDocument reader with
        | Ok result -> Ok result.Parsed
        | Error err ->
            let pos = err.Position.Index
            Error $"XML parse error at position {pos}: {err.Errors}"
