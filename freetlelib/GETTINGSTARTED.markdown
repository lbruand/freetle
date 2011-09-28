FREETLE XML TRANSFORMATION FRAMEWORK
====================================

# Copyright
Freetle is copyrighted by Lucas Bruand.

GETTING STARTED
---------------

# Pre-requisite

No previous knowledge of Scala is pre-supposed here - Through it is advisable for one to further one's knowledge in Scala, once the basics of Freetle are acquired.

# Choosing an IDE.

While it is possible to use no IDE, it is greatly advised to use one.
Amongst credible IDE with scala support, you can pick either Eclipse or Intellij IDEA.

# Introduction to freetle concepts.
## XML Events.

Freetle uses a Stax pull parser to read sequences of events from XML Files.

## Streams

Freetle relies heavily on [Streams](http://www.scala-lang.org/api/current/scala/collection/immutabe/Stream.html) of XML events.
Streams are basic scala datastructures which are used as input and output of freetle transformations.
Streams are variations on Lists : They contain a finite, ordered suit of XML Events.
But they differ from Lists on their ability to be created on demand. They are said to be 'lazy'.
For example, when a XML file is parsed in to a Stream of `XMLEvents`, the XML file is _not_ loaded at once into memory :
The Stream summons `XMLEvents` as needed.
Thus Streams are a very useful abstraction to limit memory usage while preserving the ability to write programs in an imperative manner.

## Context

During transformations, a Context is used to store information for later use. 
A Context is a scala class that may contain variables.

## Context-Free Transformations

Freetle provides various types of transformations.
They come in two kinds :

 * Context-free transformations (which are derived from ContextFreeTransform).
 * Context-using or Context modifying transformations (which are derived from ContextWritingTransform or ContextReadingTransform).

Transformation can be combined using operators :

## Operators
In the freetle library, there are two types of operators:

### Binary operators
These are operators that take two operands, the left hand operand and the right hand operand.

#### Sequence Operator.
Shortcut Symbol : `~`

This is the most frequent (binary) operator. It call first the left hand operand, and then call the right hand operand
on what left from the first call (this is call the Tail).

Example of usage : `<("order") ~ </("order")`

Instance of matching XML : `<order></order>`

#### Compose Operator.
Shortcut Symbol : `->`

Call the left hand operand and then call the right hand operand on the result returned from the first action.



#### Choice Operator
Shortcut Symbol : `|`

Call the left hand operand. If the result is positive, return it, else call the right hand operand.
NB : There is no backtracking coded so beware of factoring anything on the left.

Example of usage : `(<("string") ~ takeText ~ </("string"))|(<("integer") ~ takeInteger ~ </("integer"))`

Instance of matching XML : `<integer>1224</integer>`


### Unary operators.
These are operators that take only one operand, named the underlying operand.
These are mainly constituted by cardinality operators. These operators are used to describe that the underlying
operand can be repeated.

#### Zero Or One Operator
Shortcut Symbol : `?`

Example of usage : `(<("order") ~ </("order"))?`

Instance of matching XML : `<order/>`

#### Zero Or More Operator
Shortcut Symbol : `*`

Example of usage : (<("order") ~ </("order"))*

Instance of matching XML : `<order/><order/>`

#### One Or More Operator
Shortcut Symbol : `+`

Example of usage : `(<("order") ~ </("order"))+`

Instance of matching XML : `<order/><order/>`

### Element Matchers

#### Matching an opening tag
Shortcut Symbol : `<`

Example of usage : `<("order")`

Instance of matching XML : `<order>`

#### Matching a closing tag
Shortcut Symbol : `</`

Example of usage : `</("order")`

Instance of matching XML : `</order>`


# Creating a project using a maven artifact.

	mvn archetype:generate -DarchetypeGroupId=org.freetle -DarchetypeArtifactId=freetle-archetype -DarchetypeVersion=1.0-SNAPSHOT 
			       -DinteractiveMode=false -DgroupId=com.test -DartifactId=hello -Dpackage=com.test

# Proposed Structure of your transformations.

Freetle proposes a standard structure for transformations. This option is strongly encouraged but is not
the only option, Freetle does not impose it. 

	Parser 
	   ^
	   |
	Transformer

## Parser

A parser does not transform a stream, it merely matches the language grammar. The Parser is often generated from an other grammar description such as XML Schema.

The parser comprehend a set of named methods (called often rules) :


    class TransformSampleParser extends CPSXMLModel[TransformSampleContext] with CPSMeta[TransformSampleContext] {
        def header : ChainedTransformRoot = <("catalog")
        def footer : ChainedTransformRoot = </("catalog")
        def element : ChainedTransformRoot = <("cd") ~ </("cd")
        def document :ChainedTransformRoot = header ~ ((element)+) ~ footer
        def transform : ChainedTransformRoot = (document).metaProcess(new SpaceSkipingMetaProcessor())
    }

Each rule has a name and a rule body which is a combination of operators and unite transformations.

## Transformer

The Transformer inherites from the Parser class. It overrides specific methods in order to accomplish transformations in
the parsed stream. For example :

    class TransformSampleTransformer extends TransformSampleParser {
        /**
         * The element rule is overloaded with a drop which suppress all content
         * recognized in the element rule of the Parser
         */
        override def element : ChainedTransformRoot = (super.element) -> !>
    }

The element rule is overloaded with a drop.

# A more detailed view of the architecture.

## CPSStream

Freetle defines a result streams as `XMLEvent` streams decorated with specific 'result' markers (Boolean).

Thus, the Freetle defines :

```scala
type CPSElementOrPositive = Option[Element]
type CPSTupleElement = (CPSElementOrPositive, Boolean)
```

`CPSTupleElement` is the most basic type appearing in the freetle transformations.

 
The second position Boolean indicates whether the element is marked as a result or a tail. (`true` = result, `false` = tail)

`CPSStream` are streams of `CPSTupleElement`

	type CPSStream = Stream[CPSTupleElement]

Two properties of `CPSStream` that is not enforced by the type system (But should always be verified) :

 * At a certain rank, every element is a tail. 
 * Before this first tail element, all elements are results. 

There can possibly be no result elements at all.

# Licensing
Freetle is licensed under the [Apache License 2.0](http://www.apache.org/licenses/LICENSE-2.0) (See attached).

# Disclaimer
Freetle is provided on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
implied, including, without limitation, any warranties or conditions
of TITLE, NON-INFRINGEMENT, MERCHANTABILITY, or FITNESS FOR A
PARTICULAR PURPOSE. You are solely responsible for determining the
appropriateness of using or redistributing the Work and assume any
risks associated with Your exercise of permissions under this License.
