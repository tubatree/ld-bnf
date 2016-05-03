# Transform the BNF DITA feed into triples

The new versions of the BNF/BNFc feeds are self referential so can be processed independently. Previously the BNF/BNFc feeds had to be processed at as a single item to resolve references. Any references to “the feed” should be taken to mean either the BNF or BNFc DITA feed.

## Getting Started

This runs best in a unix environment with mono installed. It is currently building against mono:4.2.1.102 in docker on team city.

* Shove a copy of the feed xml into the process directory, if it doesn’t exist create one (ld-bnf/process)
* <code>./build.sh</code>
* <code>mono splitter/bin/Release/splitter.exe process/feed.xml process/xml</code> should create some files in process/xml
* <code>./run.sh</code>

There will be some information about any ignored files and some .ttl files should have appeared in the process/ttl dir

## Process

There are two main parts to the process with the second being the most complex. Firstly the feed is split from one large file and written out as a set of individual files by splitter. Once the feed is in manageable chunks of xml it is transformed by ldbnf into turtle (.ttl) files.

### Splitter

This is a relatively simple single file console app written in csharp that uses recursion to split out all of the nodes that have an id and a type that match a predefined list.

Each node is written out to a separate xml file in a directory corresponding to the type.

The links between the files are all self consistent. Some small changes are made to the content e.g. changing some of the <code>data</code> elements into <code>href</code>’s.

Splatter makes as few changes as possible to the feed.

### LDBNF

This fsharp console application parses the xml into an internal model of the feed. That model is then used to create triples to represent the feed. The internal model and the serialisation are completely separate, it would be totally reasonable to use the same model to output another file format.

Type providers are used for the parsing of the xml, the samples are in the directory of the same name. If the feed xml changes the samples need to be updated or there will be runtime errors.

There are four main files in the project. Two relate to the Monograph content (monograph.fs and monographrdf.fs). There is some shared functionality that could potentially be extracted to another file. The rest of the model can be found in models.fs and modelsrdf.fs.

I was learning fsharp as I went in this project. The code style at the end of models.fs is cleaner than the code in Monograph.fs.

Ideally the code reading in the xml should be similar in appearance to the xml and the code writing triples should be similar in shape to the triples.

The two final files are the prelude that contains some extensions to the base libraries and some helper functions and xml2rdf that performs all of the file management.
