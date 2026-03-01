/// Tests for XML document-level parsing.
/// Covers XML declarations, processing instructions, comments, and full documents.
module Fidelity.Data.XML.Tests.XmlDocumentTests

open Expecto
open Fidelity.Data.XML

[<Tests>]
let declarationTests =
    testList "XML Declaration" [
        test "minimal declaration" {
            let doc = Xml.parseOrFail """<?xml version="1.0"?><root/>"""
            Expect.isSome doc.Declaration "Should have declaration"
            Expect.equal doc.Declaration.Value.Version "1.0" "Version"
            Expect.isNone doc.Declaration.Value.Encoding "No encoding"
            Expect.isNone doc.Declaration.Value.Standalone "No standalone"
        }

        test "declaration with encoding" {
            let doc = Xml.parseOrFail """<?xml version="1.0" encoding="UTF-8"?><root/>"""
            Expect.equal doc.Declaration.Value.Encoding (Some "UTF-8") "Encoding"
        }

        test "declaration with standalone" {
            let doc = Xml.parseOrFail """<?xml version="1.0" standalone="yes"?><root/>"""
            Expect.equal doc.Declaration.Value.Standalone (Some "yes") "Standalone"
        }

        test "full declaration" {
            let doc = Xml.parseOrFail """<?xml version="1.0" encoding="UTF-8" standalone="no"?><root/>"""
            Expect.equal doc.Declaration.Value.Version "1.0" "Version"
            Expect.equal doc.Declaration.Value.Encoding (Some "UTF-8") "Encoding"
            Expect.equal doc.Declaration.Value.Standalone (Some "no") "Standalone"
        }

        test "no declaration" {
            let doc = Xml.parseOrFail "<root/>"
            Expect.isNone doc.Declaration "No declaration"
        }

        test "declaration with single-quoted values" {
            let doc = Xml.parseOrFail "<?xml version='1.0' encoding='UTF-8'?><root/>"
            Expect.equal doc.Declaration.Value.Version "1.0" "Version"
            Expect.equal doc.Declaration.Value.Encoding (Some "UTF-8") "Encoding"
        }
    ]

[<Tests>]
let commentTests =
    testList "XML Comments" [
        test "comment before root" {
            let doc = Xml.parseOrFail "<!-- header comment --><root/>"
            Expect.equal (XmlNode.name doc.Root) (Some "root") "Root after comment"
        }

        test "comment inside element" {
            let doc = Xml.parseOrFail "<root><!-- inner --></root>"
            let children = XmlNode.children doc.Root
            Expect.equal children.Length 1 "One child"
            match children.[0] with
            | XmlNode.Comment c -> Expect.equal c " inner " "Comment content"
            | _ -> failtest "Expected comment node"
        }

        test "comment with dashes and content" {
            let _doc = Xml.parseOrFail "<root><!-- This is a multi-word comment --></root>"
            ()
        }

        test "multiple comments in prolog" {
            let _doc = Xml.parseOrFail "<!-- first --><!-- second --><root/>"
            ()
        }
    ]

[<Tests>]
let processingInstructionTests =
    testList "XML Processing Instructions" [
        test "PI before root" {
            let _doc = Xml.parseOrFail """<?style type="text/xsl"?><root/>"""
            ()
        }

        test "PI inside element" {
            let doc = Xml.parseOrFail "<root><?php echo 'hello'; ?></root>"
            let children = XmlNode.children doc.Root
            Expect.equal children.Length 1 "One child"
            match children.[0] with
            | XmlNode.ProcessingInstruction (target, data) ->
                Expect.equal target "php" "PI target"
                Expect.equal data "echo 'hello';" "PI data"
            | _ -> failtest "Expected PI node"
        }
    ]

[<Tests>]
let whitespaceTests =
    testList "XML Document Whitespace" [
        test "whitespace before root" {
            let _doc = Xml.parseOrFail "  \n  <root/>"
            ()
        }

        test "whitespace after root" {
            let _doc = Xml.parseOrFail "<root/>  \n  "
            ()
        }

        test "whitespace around declaration" {
            let _doc = Xml.parseOrFail "  <?xml version=\"1.0\"?>  \n  <root/>"
            ()
        }

        test "newlines between prolog items" {
            let _doc = Xml.parseOrFail "<?xml version=\"1.0\"?>\n<!-- comment -->\n<root/>"
            ()
        }
    ]

[<Tests>]
let fullDocumentTests =
    testList "XML Full Documents" [
        test "minimal document" {
            let _doc = Xml.parseOrFail "<root/>"
            ()
        }

        test "document with declaration and content" {
            let doc = Xml.parseOrFail """<?xml version="1.0" encoding="UTF-8"?>
<config>
  <setting name="debug" value="true"/>
  <setting name="level" value="5"/>
</config>"""
            let settings = XmlNode.elementsNamed "setting" doc.Root
            Expect.equal settings.Length 2 "Two settings"
            Expect.equal (XmlNode.attr "name" settings.[0]) (Some "debug") "First setting"
            Expect.equal (XmlNode.attr "value" settings.[1]) (Some "5") "Second setting value"
        }

        test "Wayland-style protocol XML" {
            let doc = Xml.parseOrFail """<?xml version="1.0" encoding="UTF-8"?>
<protocol name="wayland">
  <interface name="wl_display" version="1">
    <description summary="core global object">
      The core global object.
    </description>
    <request name="sync">
      <arg name="callback" type="new_id" interface="wl_callback"/>
    </request>
    <event name="error">
      <arg name="object_id" type="object"/>
      <arg name="code" type="uint"/>
      <arg name="message" type="string"/>
    </event>
    <enum name="error">
      <entry name="invalid_object" value="0" summary="server couldn't find object"/>
      <entry name="invalid_method" value="1" summary="method doesn't exist"/>
      <entry name="no_memory" value="2" summary="server is out of memory"/>
    </enum>
  </interface>
</protocol>"""
            Expect.equal (XmlNode.attr "name" doc.Root) (Some "wayland") "Protocol name"

            let iface = (XmlNode.element "interface" doc.Root).Value
            Expect.equal (XmlNode.attr "name" iface) (Some "wl_display") "Interface name"
            Expect.equal (XmlNode.attr "version" iface) (Some "1") "Interface version"

            let request = (XmlNode.element "request" iface).Value
            Expect.equal (XmlNode.attr "name" request) (Some "sync") "Request name"
            let arg = (XmlNode.element "arg" request).Value
            Expect.equal (XmlNode.attr "type" arg) (Some "new_id") "Arg type"
            Expect.equal (XmlNode.attr "interface" arg) (Some "wl_callback") "Arg interface"

            let event = (XmlNode.element "event" iface).Value
            Expect.equal (XmlNode.attr "name" event) (Some "error") "Event name"
            let eventArgs = XmlNode.elementsNamed "arg" event
            Expect.equal eventArgs.Length 3 "Three event args"

            let enum = (XmlNode.element "enum" iface).Value
            let entries = XmlNode.elementsNamed "entry" enum
            Expect.equal entries.Length 3 "Three enum entries"
            Expect.equal (XmlNode.attr "name" entries.[0]) (Some "invalid_object") "First entry"
            Expect.equal (XmlNode.attr "value" entries.[2]) (Some "2") "Third entry value"
        }

        test "empty root with attributes only" {
            let doc = Xml.parseOrFail """<?xml version="1.0"?><root xmlns="http://example.com" version="2"/>"""
            Expect.equal (XmlNode.attr "xmlns" doc.Root) (Some "http://example.com") "xmlns"
            Expect.equal (XmlNode.attr "version" doc.Root) (Some "2") "version attr"
        }
    ]

[<Tests>]
let errorTests =
    testList "XML Parse Errors" [
        test "empty input" {
            Expect.isError (Xml.parse "") "Should fail on empty input"
        }

        test "no root element" {
            Expect.isError (Xml.parse "just text") "Should fail without root element"
        }

        test "multiple root elements" {
            Expect.isError (Xml.parse "<a/><b/>") "Should fail with multiple roots"
        }

        test "unclosed element" {
            Expect.isError (Xml.parse "<root><unclosed>") "Should fail on unclosed element"
        }
    ]
