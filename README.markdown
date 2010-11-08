
FREETLE XML TRANSFORMATION FRAMEWORK
====================================

# What is Freetle ?

Freetle is a Open sourced toolbox to write high performance XML transformations.
It can be used as an ETL, using specifically developed connectors.
(For instance database connectors)
Freetle is written in the [Scala language](http://www.scala-lang.org/) and compiles to Java bytecode.

# What does Freetle mean ?

Freetle is a contraction of Free ETL.

# What is the audience ?

Those who are dissatisfied with existing transformation tools like XSLT.
Freetle hopes to bring both simplicity of programming with advanced performance and modularity.

# Vision

Because it relies on Scala, Freetle's design is oriented toward tried and true approach of making software :

    * Freetle boasts seamless integration with Java.
    * Textfile oriented programming rather than exotic binary/XML programming language.
    * Built-in plugin IDE support both for Eclipse and Intellij Idea with navigation between Freetle transformations
      and Java code.

While bringing in major new ideas from the functional programming world :
    * Combinators and DeSL
    * Lazyness and Streaming

Ultimately, Freetle's goal to stay forever :
    * Open-Sourced (Apache 2.0 license)
    * Scalable
    * Refactorable (???)
 
# Technical Overview

## Acknowledgement
Freetle is deeply endebted to giants:
    * The [Scala language](http://www.scala-lang.org/) being the most evident
    * TODO ...

## Combinators
Freetle Transformations are expressed in the Scala language with syntactic sugar from the Freetle library.
In effect, transformations are expressed as algebra expressions of unitary transformations and operators.
This approach, called combinators, gives almost infinite expressivity while retaining simplicity.

## Stream-oriented
Freetle transformations are stream-oriented programming :
Meaning that the XML document is viewed as a stream of (SAX) XML events.
This goes contrary to most transformation systems in which XML Documents are viewed as trees.
Stream oriented programming is theorized as being faster but harder to program.
We believe that thanks to the use of combinators, Freetle transformations are not harder to program than typically tree-
oriented transformation such as XSL-T.

## Leveraging existing tooling and standards.
Rather than redevelopping everything from scratch, Freetle is standing on the shoulders of giants :
the Scala language.
 As a result, much of the preexisting tooling for development is pre existing:


# Related work and links.

Freetle heavily draws on HaXML and combinators from Haskell.

## HaXML
## XSLT
## STX/Joost

# Licensing
Freetle is licensed under the Apache License 2.0 (See attached).


# Copyright
Freetle is copyrighted by Lucas Bruand.

# Disclaimer

