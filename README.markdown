
FREETLE XML TRANSFORMATION FRAMEWORK
====================================

# What is Freetle ?

Freetle is a Open sourced toolbox to write high performance Data transformations.
It can be used as an ETL, using specifically developed connectors.
(For instance database connectors)
Freetle is written in the [Scala language](http://www.scala-lang.org/) and compiles to Java bytecode.

# What does Freetle mean ?

Freetle is a contraction of Free ETL.

# What is the audience ?

Those who are dissatisfied with existing transformation tools like XSLT.
Freetle hopes to bring both simplicity of programming with advanced performance and modularity.
Freetle favors Agile programming : It put emphases on Testing, Refactoring, Tooling.
Java programmers will feel in known territory and be immediately productive.
Rather than selling bells and whistle in the form of a visual IDE and _intuition_,
Freetle leverages existing knowledge, tools, methodology daily used by Java developers.
It promotes established programming methodology, simplicity, modularity and openness to achieve your goals.

# Vision

Because it relies on Scala, Freetle's design is oriented toward tried and true approach of making software :

    * Freetle boasts seamless integration with Java.
    * Textfile oriented vanilla programming language rather than exotic binary/XML programming language.
    * Built-in plugin IDE support both for Eclipse and Intellij Idea with navigation between Freetle transformations
      and Java code.

While bringing in major new ideas from the functional programming world :
    * Combinators and DeSL
    * Lazyness and Streaming

Ultimately, Freetle's goal is to stay forever :
    * Open-Sourced (Apache 2.0 license)
    * Scalable
    * Robust (in the sense that it will be easy to debug, refactorable and change-proof)
 
# Technical Overview

## Acknowledgement
Freetle is deeply indebted to giants:
    * The [Scala language](http://www.scala-lang.org/) being the most evident
    * TODO ...

## Combinators : bottom-top approach
Freetle Transformations are expressed in the Scala language with syntactic sugar from the Freetle library.
In effect, transformations are expressed as algebra expressions of unitary transformations and operators.
This approach, called _combinators_, gives almost infinite expressivity while retaining simplicity.
It is a reaction to the top-down approach of many ETLs which hide the great complexity of their code behind
with complex concepts like Cards, Maps, etc...
With Freetle, the basic concepts to understand are few, simple, already well established in the functional
software industry.
(See Architecture)

## Stream-oriented
Freetle transformations are stream-oriented programming :
Meaning that the XML document is viewed as a stream of (SAX) XML events.
This goes contrary to most transformation systems in which Documents are viewed as trees.
Stream oriented programming is theorized as being faster but harder to program.
Thanks to Freetle's use of combinators, Freetle transformations are not harder to program than typically tree-
oriented transformation such as XSL-T.

## Connectors
TODO

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

Freetle heavily draws on HaXML and combinators from Haskell.

## HaXML
## XSLT
## STX/Joost

# Licensing
Freetle is licensed under the Apache License 2.0 (See attached).


# Copyright
Freetle is copyrighted by Lucas Bruand.

# Disclaimer

