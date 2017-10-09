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
<p><ph>Interaction</ph><ph class="moderate" outputclass="int-severity">Moderate</ph></p>
</body>
</topic>"""
    let interaction = parseInteractionFrom xml
    Assert.AreEqual("Interaction", interaction.message.XElement.Value)

[<Test>]
let ``Should build interaction message from topic with muliple ph`` () =
    let xml = """
<topic>
<title>Carbamazepine</title>
<body>
<p><ph>Interaction one</ph> <ph>Interaction two</ph>.<ph class="moderate" outputclass="int-severity">Moderate</ph></p>
</body>
</topic>"""
    let interaction = parseInteractionFrom xml
    Assert.AreEqual("Interaction one Interaction two.", interaction.message.XElement.Value)

[<Test>]
let ``Should build interaction message from topic and exclude severity`` () =
    let xml = """
<topic>
<title>Carbamazepine</title>
<body>
<p><ph>Interaction one</ph>.<ph class="moderate" outputclass="int-severity">Moderate</ph></p>
</body>
</topic>"""
    let interaction = parseInteractionFrom xml
    Assert.AreEqual("Interaction one.", interaction.message.XElement.Value)

[<Test>]
let ``Should build interaction message from topic and exclude evidence`` () =
    let xml = """
<topic>
<title>Carbamazepine</title>
<body>
<p><ph>Interaction one</ph>.<ph class="moderate" outputclass="int-severity">Moderate</ph><ph outputclass="int-evidence">Theoretical</ph></p>
</body>
</topic>"""
    let interaction = parseInteractionFrom xml
    Assert.AreEqual("Interaction one.", interaction.message.XElement.Value)


[<TestCase("moderate","Moderate")>]
[<TestCase("mild","Mild")>]
[<TestCase("severe","Severe")>]
[<TestCase("unknown","Unknown")>]
let ``Should build interaction importance from topic`` (severityClass, expectedImportance) =
    let xml = sprintf """<topic><title>Carbamazepine</title><body><p><ph class="%s" outputclass="int-severity">Not used</ph></p></body></topic>""" severityClass
    let interaction = parseInteractionFrom xml
    Assert.AreEqual(expectedImportance, interaction.importance.ToString())

[<Test; ExpectedException(typeof<System.ArgumentNullException>)>]
let ``When there is no severity in the feed, the build should fail`` () =
    let xml = """<topic><title>Carbamazepine</title><body><p></p></body></topic>"""
    try
        parseInteractionFrom xml |> ignore
    with 
    | e -> Assert.IsTrue(e.Message.Contains("No severity found for this interaction.")); raise e  
   
