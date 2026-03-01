/// Public API for Fidelity.Data.XML.
namespace Fidelity.Data.XML

/// Main entry point for XML parsing, serialization, and navigation.
[<RequireQualifiedAccess>]
module Xml =

    /// Parse an XML string into a document.
    let parse (input: string) : Result<XmlDocument, string> =
        XmlParser.parse input

    /// Parse an XML string, throwing an exception on failure.
    let parseOrFail (input: string) : XmlDocument =
        match parse input with
        | Ok doc -> doc
        | Error msg -> failwith msg

    /// Serialize an XML document to a string.
    let serialize (doc: XmlDocument) : string =
        XmlSerializer.serializeDocument doc

    /// Get the root element node.
    let root (doc: XmlDocument) : XmlNode =
        doc.Root

    /// Get the root element name.
    let rootName (doc: XmlDocument) : string option =
        XmlNode.name doc.Root
