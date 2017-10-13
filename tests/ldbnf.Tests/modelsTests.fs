module modelsTests
open NUnit.Framework
open Bnf.InteracitonParser
open Bnf.Interaction
open Bnf.Shared

let parseInteractionFrom xml =
  xml
  |> inProvider.Parse
  |> InteractsWith.from 

[<Test>]
let ``Should build interaction message from topic with single ph`` () =
    let xml = """
	<topic>
<title>Carbamazepine</title>
<body><p><ph>Interaction</ph><ph class="moderate" outputclass="int-severity">Moderate</ph></p></body>
</topic>"""
    let interaction = parseInteractionFrom xml
    let message = interaction.messages |> Seq.head
    Assert.AreEqual("Interaction", message.pElem.XElement.Value)

[<Test>]
let ``Should build interaction message from topic with muliple ph`` () =
    let xml = """
<topic>
<title>Carbamazepine</title>
<body><p><ph>Interaction one</ph> <ph>Interaction two</ph>.<ph class="moderate" outputclass="int-severity">Moderate</ph></p></body>
</topic>"""
    let interaction = parseInteractionFrom xml
    let message = interaction.messages |> Seq.head
    Assert.AreEqual("Interaction one Interaction two.", message.pElem.XElement.Value)

[<Test>]
let ``Should build interaction message from topic and exclude severity text`` () =
    let xml = """
<topic>
<title>Carbamazepine</title>
<body><p><ph>Interaction one</ph>.<ph class="moderate" outputclass="int-severity">Moderate</ph></p><p><ph>Interaction two</ph>.<ph class="moderate" outputclass="int-severity">Moderate</ph></p></body>
</topic>"""
    let interaction = parseInteractionFrom xml
    let message1 = interaction.messages |> Seq.head
    let message2 = interaction.messages |> Seq.last
    Assert.AreEqual("Interaction one.", message1.pElem.XElement.Value)
    Assert.AreEqual("Interaction two.", message2.pElem.XElement.Value)


[<Test>]
let ``Should build interaction message from topic and exclude evidence`` () =
    let xml = """
<topic>
<title>Carbamazepine</title>
<body><p><ph>Interaction one</ph>.<ph class="moderate" outputclass="int-severity">Moderate</ph><ph outputclass="int-evidence">Theoretical</ph></p></body>
</topic>"""
    let interaction = parseInteractionFrom xml
    let message = interaction.messages |> Seq.head
    Assert.AreEqual("Interaction one.", message.pElem.XElement.Value)


[<TestCase("moderate","Moderate")>]
[<TestCase("mild","Mild")>]
[<TestCase("severe","Severe")>]
[<TestCase("unknown","Unknown")>]
let ``Should build interaction importance from topic`` (severityClass, expectedImportance) =
    let xml = sprintf """<topic><title>Carbamazepine</title><body><p><ph class="%s" outputclass="int-severity">Not used</ph></p></body></topic>""" severityClass
    let interaction = parseInteractionFrom xml
    let message = interaction.messages |> Seq.head
    Assert.AreEqual(expectedImportance, message.importance.ToString())

[<Test>]
let ``When there is no severity in the feed, severity should be unknown`` () =
    let xml = """<topic><title>Carbamazepine</title><body><p></p></body></topic>"""
    let interaction = parseInteractionFrom xml
    let message = interaction.messages |> Seq.head
    Assert.AreEqual("Unknown", message.importance.ToString())


[<Test>]
let ``Should build interaction message list from multiple paragraphs`` () =
    let xml = """
	<topic>
			<title>Asenapine</title>
			<body><p>This <ph>is</ph> a <ph>test</ph>.</p><p>This <ph>is</ph> a second <ph>test</ph>.</p></body>
		</topic>"""
    let interaction = parseInteractionFrom xml
    let message1 = interaction.messages |> Seq.head
    let message2 = interaction.messages |> Seq.last
    Assert.AreEqual("This is a test.", message1.pElem.XElement.Value)
    Assert.AreEqual("This is a second test.", message2.pElem.XElement.Value)


[<Test>]
let ``Should build interaction importance from topic with multiple paragraphs`` () =
    let xml = """
	<topic>
			<title>Asenapine</title>
<body><p><ph>Interaction one</ph>.<ph class="moderate" outputclass="int-severity">Moderate</ph></p><p><ph>Interaction two</ph>.<ph class="severe" outputclass="int-severity">Severe</ph></p></body>
	</topic>"""
    let interaction = parseInteractionFrom xml
    let message1 = interaction.messages |> Seq.head
    let message2 = interaction.messages |> Seq.last
    Assert.AreEqual("Moderate", message1.importance.ToString())
    Assert.AreEqual("Severe", message2.importance.ToString())
   
