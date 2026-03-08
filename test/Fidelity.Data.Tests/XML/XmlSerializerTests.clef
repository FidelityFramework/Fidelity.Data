/// Tests for XML serialization (document -> string) and round-trip consistency.
module Fidelity.Data.XML.Tests.XmlSerializerTests

open Expecto
open Fidelity.Data.XML

[<Tests>]
let serializationTests =
    testList "XML Serialization" [
        test "serialize self-closing element" {
            let doc = { Declaration = None; Root = XmlNode.Element ("br", [], []) }
            Expect.equal (Xml.serialize doc) "<br/>" "Self-closing"
        }

        test "serialize element with attributes" {
            let attrs = [ { Name = "a"; Value = "1" }; { Name = "b"; Value = "2" } ]
            let doc = { Declaration = None; Root = XmlNode.Element ("el", attrs, []) }
            Expect.equal (Xml.serialize doc) """<el a="1" b="2"/>""" "With attributes"
        }

        test "serialize element with text child" {
            let doc = { Declaration = None; Root = XmlNode.Element ("p", [], [XmlNode.Text "hello"]) }
            Expect.equal (Xml.serialize doc) "<p>hello</p>" "With text"
        }

        test "serialize escapes text content" {
            let doc = { Declaration = None; Root = XmlNode.Element ("t", [], [XmlNode.Text "a & b < c"]) }
            Expect.equal (Xml.serialize doc) "<t>a &amp; b &lt; c</t>" "Escaped text"
        }

        test "serialize escapes attribute values" {
            let attrs = [ { Name = "v"; Value = "a & \"b\"" } ]
            let doc = { Declaration = None; Root = XmlNode.Element ("t", attrs, []) }
            Expect.equal (Xml.serialize doc) """<t v="a &amp; &quot;b&quot;"/>""" "Escaped attr"
        }

        test "serialize comment" {
            let doc = { Declaration = None; Root = XmlNode.Element ("r", [], [XmlNode.Comment " test "]) }
            Expect.equal (Xml.serialize doc) "<r><!-- test --></r>" "Comment"
        }

        test "serialize CDATA" {
            let doc = { Declaration = None; Root = XmlNode.Element ("r", [], [XmlNode.CData "<raw>"]) }
            Expect.equal (Xml.serialize doc) "<r><![CDATA[<raw>]]></r>" "CDATA"
        }

        test "serialize processing instruction" {
            let doc = { Declaration = None; Root = XmlNode.Element ("r", [], [XmlNode.ProcessingInstruction ("php", "echo 1;")]) }
            Expect.equal (Xml.serialize doc) "<r><?php echo 1;?></r>" "PI"
        }

        test "serialize with declaration" {
            let decl = { Version = "1.0"; Encoding = Some "UTF-8"; Standalone = None }
            let doc = { Declaration = Some decl; Root = XmlNode.Element ("root", [], []) }
            let output = Xml.serialize doc
            Expect.stringStarts output "<?xml version=\"1.0\" encoding=\"UTF-8\"?>" "Declaration"
            Expect.stringContains output "<root/>" "Root element"
        }

        test "serialize nested elements" {
            let inner = XmlNode.Element ("b", [], [XmlNode.Text "hi"])
            let doc = { Declaration = None; Root = XmlNode.Element ("a", [], [inner]) }
            Expect.equal (Xml.serialize doc) "<a><b>hi</b></a>" "Nested"
        }
    ]

[<Tests>]
let roundTripTests =
    testList "XML Round-Trip" [
        test "simple element round-trips" {
            let input = "<root/>"
            Expect.equal (Xml.serialize (Xml.parseOrFail input)) input "Round-trip"
        }

        test "element with attributes round-trips" {
            let input = """<el a="1" b="2"/>"""
            Expect.equal (Xml.serialize (Xml.parseOrFail input)) input "Round-trip attrs"
        }

        test "element with text round-trips" {
            let input = "<p>Hello, world!</p>"
            Expect.equal (Xml.serialize (Xml.parseOrFail input)) input "Round-trip text"
        }

        test "nested elements round-trip" {
            let input = "<a><b><c>text</c></b></a>"
            Expect.equal (Xml.serialize (Xml.parseOrFail input)) input "Round-trip nested"
        }

        test "entities round-trip through escape" {
            let input = "<t>a &amp; b</t>"
            Expect.equal (Xml.serialize (Xml.parseOrFail input)) input "Round-trip entities"
        }
    ]
