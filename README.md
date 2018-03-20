# LD-BNF
  
 > This application transforms the BNF/c DITA feed into triples.
 
<details>
<summary><strong>Table of contents</strong></summary>
<!-- START doctoc -->
<!-- END doctoc -->
</details>
  
## What is it?
Overview
- This is a package used by BNF-vNext for the first stage of the BNF conversion process.  


  
## Stack
- The system contains two console applications which are the Splitter and LDBNF.
- This service is a dependency of the BNF-vNext. 

### Splitter 
This is a single file console app written in csharp that uses recursion to split out all of the nodes that have an id and a type that match a predefined list. Each node is written out to a separate xml file in a directory corresponding to the type. The links between the files are all self consistent. Some small changes are made to the content e.g. changing some of the data elements into href’s.

### LDBNF
This fsharp console application parses the xml into an internal model of the feed. That model is then used to create triples to represent the feed. The internal model and the serialisation are completely separate, it would be totally reasonable to use the same model to output another file format.

Type providers are used for the parsing of the xml, the samples are in the directory of the same name. If the feed xml changes the samples need to be updated or there will be runtime errors.

There are four main files in the project. Two relate to the Monograph content (monograph.fs and monographrdf.fs). There is some shared functionality that could potentially be extracted to another file. The rest of the model can be found in models.fs and modelsrdf.fs.

The two final files are the prelude that contains some extensions to the base libraries and some helper functions and xml2rdf that performs all of the file management.
  
## Set up
- Clone the git repository 
- Shove a copy of the feed xml into the process directory, if it doesn’t exist create one (ld-bnf/process)
- Open in Visual Studio (VS2015 and Up) 
- Go into Splitter properties, on the debug tab add "../../../process/feed.xml ../../../process/xml" to the command line arguments. 
- Once the splitter has ran, go to the ldbnf properties. Go to the debug tab and make sure the command line arguments are "--xmldirectory process/xml --outputdirectory process/ttl" and that the working directory is the path to your ld-bnf project. 
- Run ldbnf.
- Your xml and ttl outputs are located in ld-bnf/process.
-  
### Gotchas
    - If you're not sure where to get an xml feed from you can use the live feed from the api or you can use the smaller feed stored in BNF-vNext https://github.com/nhsevidence/BNF-vNext/blob/master/tools/smaller_feed.xml.
  
## How to use
- Run the Splitter application first (to split the xml feed)
- Run the LDBNF application (To convert the xml to ttl)
 