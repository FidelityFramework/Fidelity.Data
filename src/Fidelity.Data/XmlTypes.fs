/// XML document types for representing parsed XML.
/// Covers elements, attributes, text, CDATA, comments, and processing instructions.
/// Designed to be BCL-minimal, to support Clef dimensional types.
namespace Fidelity.Data.XML

/// An XML attribute (name-value pair on an element).
[<Struct>]
type XmlAttribute = {
    Name: string
    Value: string
}

/// An XML node in the document tree.
[<RequireQualifiedAccess>]
type XmlNode =
    /// An element with name, attributes, and child nodes.
    | Element of name: string * attributes: XmlAttribute list * children: XmlNode list
    /// Text content within an element.
    | Text of string
    /// A CDATA section: <![CDATA[...]]>
    | CData of string
    /// A comment: <!-- ... -->
    | Comment of string
    /// A processing instruction: <?target data?>
    | ProcessingInstruction of target: string * data: string

/// The XML declaration: <?xml version="1.0" encoding="UTF-8"?>
type XmlDeclaration = {
    Version: string
    Encoding: string option
    Standalone: string option
}

/// A complete XML document.
type XmlDocument = {
    Declaration: XmlDeclaration option
    Root: XmlNode
}

/// Module for navigating and querying XML nodes.
module XmlNode =
    /// Get the element name, or None for non-element nodes.
    let name (node: XmlNode) : string option =
        match node with
        | XmlNode.Element (n, _, _) -> Some n
        | _ -> None

    /// Get the attribute list of an element.
    let attributes (node: XmlNode) : XmlAttribute list =
        match node with
        | XmlNode.Element (_, attrs, _) -> attrs
        | _ -> []

    /// Get the child nodes of an element.
    let children (node: XmlNode) : XmlNode list =
        match node with
        | XmlNode.Element (_, _, ch) -> ch
        | _ -> []

    /// Get only element children.
    let elements (node: XmlNode) : XmlNode list =
        children node |> List.filter (fun n ->
            match n with XmlNode.Element _ -> true | _ -> false)

    /// Find the first child element with the given name.
    let element (elName: string) (node: XmlNode) : XmlNode option =
        elements node |> List.tryFind (fun n ->
            match n with
            | XmlNode.Element (n, _, _) -> n = elName
            | _ -> false)

    /// Find all child elements with the given name.
    let elementsNamed (elName: string) (node: XmlNode) : XmlNode list =
        elements node |> List.filter (fun n ->
            match n with
            | XmlNode.Element (n, _, _) -> n = elName
            | _ -> false)

    /// Get the value of a named attribute.
    let attr (attrName: string) (node: XmlNode) : string option =
        attributes node
        |> List.tryFind (fun a -> a.Name = attrName)
        |> Option.map (fun a -> a.Value)

    /// Get the value of a named attribute, with a default.
    let attrDefault (attrName: string) (defaultValue: string) (node: XmlNode) : string =
        match attr attrName node with
        | Some v -> v
        | None -> defaultValue

    /// Get the direct text content of a node.
    let text (node: XmlNode) : string option =
        match node with
        | XmlNode.Text t -> Some t
        | XmlNode.CData t -> Some t
        | XmlNode.Element (_, _, ch) ->
            ch |> List.tryPick (fun n ->
                match n with
                | XmlNode.Text t -> Some t
                | XmlNode.CData t -> Some t
                | _ -> None)
        | _ -> None

    /// Collect all text content recursively (depth-first).
    let rec innerText (node: XmlNode) : string =
        match node with
        | XmlNode.Text t -> t
        | XmlNode.CData t -> t
        | XmlNode.Element (_, _, ch) ->
            ch |> List.map innerText |> String.concat ""
        | _ -> ""

/// Module for working with XML documents.
module XmlDocument =
    /// Get the root element node.
    let root (doc: XmlDocument) : XmlNode = doc.Root

    /// Get the root element name.
    let rootName (doc: XmlDocument) : string option =
        XmlNode.name doc.Root
