
FREETLE XML TRANSFORMATION FRAMEWORK
====================================

# Copyright
Freetle is copyrighted by Lucas Bruand.

README
------

# What is Freetle ?

Freetle is a Open sourced toolbox to write high performance Data transformations.
It can be used as an ETL, using specifically developed connectors.
(For instance database connectors)
Freetle is written in the [Scala language](http://www.scala-lang.org/) and compiles to Java bytecode.

# Current status

Freetle is production-ready.

# What does Freetle mean ?

Freetle is a contraction of Free ETL. An [ETL](http://en.wikipedia.org/wiki/Extract,_transform,_load) for Extract Transform and Load is a software tool used to transfer/transform datas (usually large chunks of it) from one system and one form to another system and another form.

# Vision

Because it relies on Scala, Freetle's design is oriented toward a tried and true approach of making software :

* Freetle boasts seamless integration with Java. Thus it is easy to package and reuse.
* Freetle transformations are developed in a Textfile oriented vanilla programming language rather than an exotic binary/XML programming language (Compare XSLT).
* Built-in plugin IDE support both for Eclipse and Intellij Idea with navigation between Freetle transformations and Java code.

While bringing in major new ideas from the functional programming world :

* Combinators and DeSL
* Lazyness and Streaming


# Where to go next ?

* [Discover the library itself](./freetlelib/)
* [Discover A sample transformation](./freetle-sample/)
