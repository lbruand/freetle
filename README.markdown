
FREETLE XML TRANSFORMATION FRAMEWORK
====================================

# Copyright
Freetle is copyrighted by Lucas Bruand.

# What is Freetle ?

Freetle is a Open sourced toolbox to write high performance Data transformations.
It can be used as an ETL, using specifically developed connectors.
(For instance database connectors)
Freetle is written in the [Scala language](http://www.scala-lang.org/) and compiles to Java bytecode.

# What does Freetle mean ?

Freetle is a contraction of Free ETL. An [ETL](http://en.wikipedia.org/wiki/Extract,_transform,_load) for Extract Transform and Load is a software tool used to transfer/transform datas (usually large chunks of it) from one system and one form to another system and another form.

# What is the audience ?

Those who are dissatisfied with existing transformation tools like XSLT.
Freetle hopes to bring both simplicity of programming with advanced performance and modularity.
Freetle favors Agile programming : It put emphases on Testing, Refactoring, Tooling.
Java programmers will feel at home and be immediately productive.
Rather than selling bells and whistle in the form of a visual IDE and _intuition_,
Freetle leverages existing knowledge, tools, methodology daily used by Java developers.
It promotes simplicity, modularity, openness and _established programming methodology_ to achieve your goals.

# Vision

Because it relies on Scala, Freetle's design is oriented toward a tried and true approach of making software :

* Freetle boasts seamless integration with Java. Thus it is easy to package and reuse.
* Freetle transformations are developed in a Textfile oriented vanilla programming language rather than exotic binary/XML programming language (Compare XSLT).
* Built-in plugin IDE support both for Eclipse and Intellij Idea with navigation between Freetle transformations
      and Java code.

While bringing in major new ideas from the functional programming world :

* Combinators and DeSL
* Lazyness and Streaming

# Governance

Ultimately, Freetle's goal is to stay forever :

* Free
* Open-Sourced (Apache 2.0 license)
* Scalable
* Robust (in the sense that it will be easy to debug, refactorable and change-proof)
 
# Technical Overview

## Acknowledgement
Freetle is deeply indebted to giants:

* The [Scala language](http://www.scala-lang.org/) being the most evident
* The [Haskell language](http://www.haskell.org/) that triggered the scala effort
* [HaXML](http://www.cs.york.ac.uk/fp/HaXml/) for proposing XML transformation combinators.

## Combinators : bottom-top approach
Freetle Transformations are expressed in the Scala language with syntactic sugar from the Freetle library.
In effect, transformations are expressed as algebra expressions of unitary transformations and operators.
This bottom-top approach, called _combinators_, gives almost infinite expressivity while retaining simplicity.
With Freetle, the basic concepts to understand are few, simple, already well established in the functional
software industry.
Moreover, this approach gives the ability to write meta processors that transform completely a transformation into
another.

## Stream-oriented
Freetle transformations are stream-oriented programming :
Meaning that the XML document is viewed as a stream of (STAX) XML events.
This goes contrary to most transformation systems in which Documents are viewed as trees.
Stream oriented programming is theorized as being faster but harder to program.
Thanks to Freetle's use of combinators, Freetle transformations are not harder to program than typically tree-oriented transformations such as XSL-T.

## Connectors
Freetle is bent to develop a set of connectors to technical systems such as :

* Databases (via JDBC)
* Message Oriented Middleware (via JMS)
* Content Management Systems.

## Leveraging existing tooling and standards.
Rather than redeveloping everything from scratch, Freetle is standing on the shoulders of giants :
the Scala language.
 As a result, much of the tooling for development is pre existing:
### IDE Support
#### Eclipse
Eclipse has scala-support in the form of the [Scala IDE](http://www.assembla.com/wiki/show/scala-ide)

#### Intellij Idea
Idea has scala-support in the form of the
    [Scala Plugin for Intellij IDEA](http://confluence.jetbrains.net/display/SCA/Scala+Plugin+for+Intellij+IDEA)

# Related work and links.

## HaXML
[HaXML](http://www.cs.york.ac.uk/fp/HaXml/) is a collection of many utilities for parsing, filtering, transforming, and generating XML documents using Haskell. Freetle heavily draws on HaXML and combinators from Haskell.

## XSLT
[XSLT](http://en.wikipedia.org/wiki/XSLT) is the w3c initiative to standardise a XML transformation langage. There are a number of quality and opensource implementations in the java world. The main problem of XSLT is that it is tree-oriented and thus it implies that one cannot transform large documents easily. The input document is effectively first loaded into memory. XSLT suffers from the use of XML programming language which makes develop awkward and difficult (it lacks a proper IDE etc...).

## STX/Joost
[STX/Joost](http://joost.sourceforge.net/) is an open source initiative to go beyond the large document problem inherent to XSLT. It is relatively slower than XSLT and lacks a number of features.(mainly a sort)

# Performance
Freetle is still behind XML transformation leader XSLTC when transforming small documents.
But XSLTC does not transform documents of arbitrary size whereas Freetle does.
Freetle compares well with [STX/Joost](http://joost.sourceforge.net/).

[Benchmark](http://yquem.inria.fr/~frisch/xstream/bench.html)

# Getting Started

See the document Getting Started.

# Licensing
Freetle is licensed under the [Apache License 2.0](http://www.apache.org/licenses/LICENSE-2.0) (See attached).
Commercial licensing and support can be obtained, please contact TODO.

# Disclaimer
Freetle is provided on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
implied, including, without limitation, any warranties or conditions
of TITLE, NON-INFRINGEMENT, MERCHANTABILITY, or FITNESS FOR A
PARTICULAR PURPOSE. You are solely responsible for determining the
appropriateness of using or redistributing the Work and assume any
risks associated with Your exercise of permissions under this License.
