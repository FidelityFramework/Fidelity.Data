/// XML Serializer — converts XmlDocument/XmlNode back to valid XML text.
///
/// Follows the DU->text pattern: match each XmlNode case, emit the appropriate
/// XML representation. Design: Pure F#, no dependencies beyond the types in XmlTypes.fs.
namespace Fidelity.Data.XML

open System.Text

module XmlSerializer =

    // =========================================================================
    // Text Escaping
    // =========================================================================

    /// Escape text content for element body (escape &, <, >).
    let private escapeText (s: string) : string =
        let sb = StringBuilder(s.Length)
        for c in s do
            match c with
            | '&' -> sb.Append("&amp;") |> ignore
            | '<' -> sb.Append("&lt;") |> ignore
            | '>' -> sb.Append("&gt;") |> ignore
            | c -> sb.Append(c) |> ignore
        sb.ToString()

    /// Escape attribute value (escape &, <, ", ').
    let private escapeAttrValue (s: string) : string =
        let sb = StringBuilder(s.Length)
        for c in s do
            match c with
            | '&' -> sb.Append("&amp;") |> ignore
            | '<' -> sb.Append("&lt;") |> ignore
            | '"' -> sb.Append("&quot;") |> ignore
            | c -> sb.Append(c) |> ignore
        sb.ToString()

    // =========================================================================
    // Serialization
    // =========================================================================

    /// Serialize an XML declaration.
    let private serializeDeclaration (sb: StringBuilder) (decl: XmlDeclaration) =
        sb.Append("<?xml version=\"").Append(decl.Version).Append('"') |> ignore
        match decl.Encoding with
        | Some enc -> sb.Append(" encoding=\"").Append(enc).Append('"') |> ignore
        | None -> ()
        match decl.Standalone with
        | Some sa -> sb.Append(" standalone=\"").Append(sa).Append('"') |> ignore
        | None -> ()
        sb.Append("?>") |> ignore

    /// Serialize attributes onto the StringBuilder.
    let private serializeAttributes (sb: StringBuilder) (attrs: XmlAttribute list) =
        for attr in attrs do
            sb.Append(' ')
              .Append(attr.Name)
              .Append("=\"")
              .Append(escapeAttrValue attr.Value)
              .Append('"') |> ignore

    /// Serialize an XML node.
    let rec serializeNode (sb: StringBuilder) (node: XmlNode) =
        match node with
        | XmlNode.Element (name, attrs, children) ->
            sb.Append('<').Append(name) |> ignore
            serializeAttributes sb attrs
            if children.IsEmpty then
                sb.Append("/>") |> ignore
            else
                sb.Append('>') |> ignore
                for child in children do
                    serializeNode sb child
                sb.Append("</").Append(name).Append('>') |> ignore
        | XmlNode.Text text ->
            sb.Append(escapeText text) |> ignore
        | XmlNode.CData text ->
            sb.Append("<![CDATA[").Append(text).Append("]]>") |> ignore
        | XmlNode.Comment text ->
            sb.Append("<!--").Append(text).Append("-->") |> ignore
        | XmlNode.ProcessingInstruction (target, data) ->
            sb.Append("<?").Append(target) |> ignore
            if data.Length > 0 then
                sb.Append(' ').Append(data) |> ignore
            sb.Append("?>") |> ignore

    /// Serialize a complete XML document to a string.
    let serializeDocument (doc: XmlDocument) : string =
        let sb = StringBuilder()
        match doc.Declaration with
        | Some decl ->
            serializeDeclaration sb decl
            sb.AppendLine() |> ignore
        | None -> ()
        serializeNode sb doc.Root
        sb.ToString()
