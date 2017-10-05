module modelsTests
open NUnit.Framework
open Bnf.InteracitonParser
open Bnf.Interaction
open Bnf.Shared

[<Test>]
let ``Should build interaction message from topic with single ph`` () =
    let xml = """
	<topic>
      <title>Fosphenytoin</title>
      <body>
		<p>
			<ph>Interaction</ph>
		</p>
	  </body>
   </topic>"""
    let topic = inProvider.Parse xml
    let actual = Bnf.Interaction.InteractsWith.from topic
    let expected = "Interaction"
    Assert.AreEqual(expected, actual.messageString)