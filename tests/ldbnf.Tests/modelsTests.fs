module modelsTests
open NUnit.Framework
open Bnf.InteracitonParser
open Bnf.Interaction
open Bnf.Shared

[<Test>]
let ``test`` () =
    let xml = """
        <samples>
	        <topic id="bnf_d1e4">
		        <title>Adalimumab</title>
		        <body outputclass="int-list">
			        <p outputclass="int-message">
				        <ph outputclass="int-message-begin">increased risk of side-effects when </ph>
				        <ph outputclass="drug">
					        <xref href="#bnf_int_1095">abatacept</xref>
				        </ph>
				        <ph outputclass="int-message-middle">given with</ph>
				        <ph outputclass="drug">
					        <xref href="#bnf_int_988">adalimumab</xref>
				        </ph>
				        <ph outputclass="int-message-end"/>
			        </p>
		        </body>
	        </topic>
        </samples>"""
    let topic = inProvider.Parse xml
    let actual = Bnf.Interaction.InteractsWith.from topic
    let expected = {
        id = Id("id");
        title = topic.Title;
        importance = High;
        message = Option.get topic.Body.P;
        interactswith = {href = Href "guff"; label = "label"}
    }
    Assert.AreEqual(expected, actual)