/// Tests for XML text content, entity references, character references, and CDATA.
module Fidelity.Data.XML.Tests.XmlTextTests

open Expecto
open Fidelity.Data.XML

[<Tests>]
let entityReferenceTests =
    testList "XML Entity References" [
        test "amp entity in text" {
            let doc = Xml.parseOrFail "<t>A &amp; B</t>"
            Expect.equal (XmlNode.innerText doc.Root) "A & B" "Decoded &amp;"
        }

        test "lt entity in text" {
            let doc = Xml.parseOrFail "<t>1 &lt; 2</t>"
            Expect.equal (XmlNode.innerText doc.Root) "1 < 2" "Decoded &lt;"
        }

        test "gt entity in text" {
            let doc = Xml.parseOrFail "<t>2 &gt; 1</t>"
            Expect.equal (XmlNode.innerText doc.Root) "2 > 1" "Decoded &gt;"
        }

        test "apos entity in text" {
            let doc = Xml.parseOrFail "<t>it&apos;s</t>"
            Expect.equal (XmlNode.innerText doc.Root) "it's" "Decoded &apos;"
        }

        test "quot entity in text" {
            let doc = Xml.parseOrFail "<t>say &quot;hello&quot;</t>"
            Expect.equal (XmlNode.innerText doc.Root) "say \"hello\"" "Decoded &quot;"
        }

        test "multiple entities in text" {
            let doc = Xml.parseOrFail "<t>&lt;tag attr=&quot;val&quot;&gt;</t>"
            Expect.equal (XmlNode.innerText doc.Root) "<tag attr=\"val\">" "Multiple entities"
        }
    ]

[<Tests>]
let charReferenceTests =
    testList "XML Character References" [
        test "decimal character reference" {
            let doc = Xml.parseOrFail "<t>&#65;</t>"
            Expect.equal (XmlNode.innerText doc.Root) "A" "&#65; = A"
        }

        test "hex character reference" {
            let doc = Xml.parseOrFail "<t>&#x41;</t>"
            Expect.equal (XmlNode.innerText doc.Root) "A" "&#x41; = A"
        }

        test "hex character reference uppercase" {
            let doc = Xml.parseOrFail "<t>&#x2603;</t>"
            Expect.equal (XmlNode.innerText doc.Root) "\u2603" "Snowman character"
        }

        test "character reference in attribute value" {
            let doc = Xml.parseOrFail """<el v="&#65;"/>"""
            Expect.equal (XmlNode.attr "v" doc.Root) (Some "A") "Char ref in attr"
        }
    ]

[<Tests>]
let cdataTests =
    testList "XML CDATA Sections" [
        test "simple CDATA" {
            let doc = Xml.parseOrFail "<t><![CDATA[raw text]]></t>"
            Expect.equal (XmlNode.innerText doc.Root) "raw text" "CDATA content"
        }

        test "CDATA with special characters" {
            let doc = Xml.parseOrFail "<t><![CDATA[<not> & <xml>]]></t>"
            Expect.equal (XmlNode.innerText doc.Root) "<not> & <xml>" "Special chars preserved"
        }

        test "CDATA with newlines" {
            let doc = Xml.parseOrFail "<t><![CDATA[line1\nline2\nline3]]></t>"
            Expect.equal (XmlNode.innerText doc.Root) "line1\nline2\nline3" "Newlines preserved"
        }

        test "CDATA among text and elements" {
            let doc = Xml.parseOrFail "<t>before<![CDATA[middle]]>after</t>"
            Expect.equal (XmlNode.innerText doc.Root) "beforemiddleafter" "Mixed content"
        }
    ]

[<Tests>]
let textContentTests =
    testList "XML Text Content" [
        test "whitespace preservation in text" {
            let doc = Xml.parseOrFail "<t>  hello  world  </t>"
            Expect.equal (XmlNode.innerText doc.Root) "  hello  world  " "Whitespace preserved"
        }

        test "multiline text" {
            let doc = Xml.parseOrFail "<t>line1\nline2\nline3</t>"
            Expect.equal (XmlNode.innerText doc.Root) "line1\nline2\nline3" "Multiline text"
        }

        test "empty text element" {
            let doc = Xml.parseOrFail "<t></t>"
            Expect.equal (XmlNode.innerText doc.Root) "" "Empty text"
        }
    ]
