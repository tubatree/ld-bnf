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
<body>
<p><ph>Interaction</ph></p>
</body>
</topic>"""
    let interaction = parseInteractionFrom xml
    Assert.AreEqual("Interaction", interaction.messageString)

[<Test>]
let ``Should build interaction message from topic with muliple ph`` () =
    let xml = """
<topic>
<title>Carbamazepine</title>
<body>
<p><ph>Interaction one</ph> <ph>Interaction two</ph>.</p>
</body>
</topic>"""
    let interaction = parseInteractionFrom xml
    Assert.AreEqual("Interaction one Interaction two.", interaction.messageString)

[<Test>]
let ``Should build interaction message from topic and exclude severity`` () =
    let xml = """
<topic>
<title>Carbamazepine</title>
<body>
<p><ph>Interaction one</ph>.<ph outputclass="int-severity">Moderate</ph></p>
</body>
</topic>"""
    let interaction = parseInteractionFrom xml
    Assert.AreEqual("Interaction one.", interaction.messageString)

[<Test>]
let ``Should build interaction message from topic and exclude evidence`` () =
    let xml = """
<topic>
<title>Carbamazepine</title>
<body>
<p><ph>Interaction one</ph>.<ph outputclass="int-evidence">Theoretical</ph></p>
</body>
</topic>"""
    let interaction = parseInteractionFrom xml
    Assert.AreEqual("Interaction one.", interaction.messageString)