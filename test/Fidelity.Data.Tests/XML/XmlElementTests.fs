/// Tests for XML element parsing.
/// Covers self-closing elements, open/close elements, nesting, and attributes.
module Fidelity.Data.XML.Tests.XmlElementTests

open Expecto
open Fidelity.Data.XML

[<Tests>]
let selfClosingTests =
    testList "XML Self-Closing Elements" [
        test "simple self-closing element" {
            let doc = Xml.parseOrFail "<br/>"
            Expect.equal (XmlNode.name doc.Root) (Some "br") "Element name"
            Expect.isEmpty (XmlNode.children doc.Root) "No children"
        }

        test "self-closing with space before />" {
            let doc = Xml.parseOrFail "<br />"
            Expect.equal (XmlNode.name doc.Root) (Some "br") "Element name"
        }

        test "self-closing with attributes" {
            let doc = Xml.parseOrFail """<img src="logo.png" width="100"/>"""
            let root = doc.Root
            Expect.equal (XmlNode.attr "src" root) (Some "logo.png") "src attr"
            Expect.equal (XmlNode.attr "width" root) (Some "100") "width attr"
        }

        test "self-closing with single-quoted attributes" {
            let doc = Xml.parseOrFail "<input type='text' value='hello'/>"
            let root = doc.Root
            Expect.equal (XmlNode.attr "type" root) (Some "text") "type attr"
            Expect.equal (XmlNode.attr "value" root) (Some "hello") "value attr"
        }
    ]

[<Tests>]
let openCloseTests =
    testList "XML Open/Close Elements" [
        test "empty element" {
            let doc = Xml.parseOrFail "<div></div>"
            Expect.equal (XmlNode.name doc.Root) (Some "div") "Element name"
            Expect.isEmpty (XmlNode.children doc.Root) "No children"
        }

        test "element with text content" {
            let doc = Xml.parseOrFail "<p>Hello, world!</p>"
            Expect.equal (XmlNode.innerText doc.Root) "Hello, world!" "Text content"
        }

        test "element with attributes and text" {
            let doc = Xml.parseOrFail """<span class="bold">text</span>"""
            let root = doc.Root
            Expect.equal (XmlNode.attr "class" root) (Some "bold") "class attr"
            Expect.equal (XmlNode.innerText root) "text" "Text content"
        }

        test "mismatched tags produce error" {
            let result = Xml.parse "<div></span>"
            Expect.isError result "Should fail on mismatched tags"
        }
    ]

[<Tests>]
let nestingTests =
    testList "XML Nested Elements" [
        test "single level nesting" {
            let doc = Xml.parseOrFail "<root><child/></root>"
            let root = doc.Root
            Expect.equal (XmlNode.elements root |> List.length) 1 "One child"
            Expect.equal (XmlNode.element "child" root |> Option.bind XmlNode.name) (Some "child") "Child name"
        }

        test "multiple children" {
            let doc = Xml.parseOrFail "<root><a/><b/><c/></root>"
            let names = XmlNode.elements doc.Root |> List.choose XmlNode.name
            Expect.equal names ["a"; "b"; "c"] "Three children"
        }

        test "deeply nested" {
            let doc = Xml.parseOrFail "<a><b><c><d>deep</d></c></b></a>"
            let d =
                doc.Root
                |> XmlNode.element "b"
                |> Option.bind (XmlNode.element "c")
                |> Option.bind (XmlNode.element "d")
            Expect.isSome d "Should find deeply nested element"
            Expect.equal (XmlNode.innerText d.Value) "deep" "Deep text"
        }

        test "mixed children and text" {
            let doc = Xml.parseOrFail "<p>Hello <b>world</b>!</p>"
            Expect.equal (XmlNode.innerText doc.Root) "Hello world!" "Inner text"
            Expect.equal (XmlNode.children doc.Root |> List.length) 3 "Three child nodes"
        }

        test "sibling elements with text between" {
            let doc = Xml.parseOrFail "<root><a/>text<b/></root>"
            Expect.equal (XmlNode.elements doc.Root |> List.length) 2 "Two element children"
            Expect.equal (XmlNode.innerText doc.Root) "text" "Text between elements"
        }
    ]

[<Tests>]
let attributeTests =
    testList "XML Attributes" [
        test "multiple attributes" {
            let doc = Xml.parseOrFail """<el a="1" b="2" c="3"/>"""
            let root = doc.Root
            Expect.equal (XmlNode.attr "a" root) (Some "1") "a"
            Expect.equal (XmlNode.attr "b" root) (Some "2") "b"
            Expect.equal (XmlNode.attr "c" root) (Some "3") "c"
        }

        test "attribute with entity in value" {
            let doc = Xml.parseOrFail """<el title="a &amp; b"/>"""
            Expect.equal (XmlNode.attr "title" doc.Root) (Some "a & b") "Decoded entity"
        }

        test "attribute with quotes in value" {
            let doc = Xml.parseOrFail """<el say="he said &quot;hi&quot;"/>"""
            Expect.equal (XmlNode.attr "say" doc.Root) (Some "he said \"hi\"") "Decoded quotes"
        }

        test "namespace-prefixed attribute" {
            let doc = Xml.parseOrFail """<root xmlns:foo="http://example.com"/>"""
            Expect.equal (XmlNode.attr "xmlns:foo" doc.Root) (Some "http://example.com") "Namespace attr"
        }

        test "missing attribute returns None" {
            let doc = Xml.parseOrFail """<el a="1"/>"""
            Expect.isNone (XmlNode.attr "missing" doc.Root) "Missing attr"
        }

        test "attrDefault returns default for missing" {
            let doc = Xml.parseOrFail """<el a="1"/>"""
            Expect.equal (XmlNode.attrDefault "missing" "fallback" doc.Root) "fallback" "Default value"
        }

        test "attrDefault returns value when present" {
            let doc = Xml.parseOrFail """<el a="1"/>"""
            Expect.equal (XmlNode.attrDefault "a" "fallback" doc.Root) "1" "Actual value"
        }
    ]

[<Tests>]
let namespacedElementTests =
    testList "XML Namespaced Elements" [
        test "element with namespace prefix" {
            let doc = Xml.parseOrFail """<soap:Envelope xmlns:soap="http://schemas.xmlsoap.org/soap/envelope/"><soap:Body/></soap:Envelope>"""
            Expect.equal (XmlNode.name doc.Root) (Some "soap:Envelope") "Namespaced root"
            Expect.equal (XmlNode.element "soap:Body" doc.Root |> Option.bind XmlNode.name) (Some "soap:Body") "Namespaced child"
        }
    ]
