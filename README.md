# Transform the BNF DITA feed into triples

The new versions of the BNF/BNFc feeds are self referential so can be processed independently. Previously the BNF/BNFc feeds had to be processed at as a single item to resolve references. Any references to “the feed” should be taken to mean either the BNF or BNFc DITA feed.

## Process

There are two main parts to the process with the second being the most complex. Firstly the feed is split from one large file and written out as a set of individual files by splitter. Once the feed is in manageable chunks of xml it is transformed by ldbnf into turtle (.ttl) files.

### Splitter

This is a relatively simple single file console app that uses recursion to split out all of the nodes that have an id and a type that match a predefined list.

Each node is written out to a separate xml file in a directory corresponding to the type.

The links between the files are all self consistent. Some small changes are made to the content e.g. changing some of the <code>data</code> elements into <code>href</code>’s.

Splatter makes as few changes as possible to the feed.

### LDBNF

This parses the xml 