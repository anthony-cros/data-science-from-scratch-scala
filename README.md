## Introduction

This repository aims to reproduce in Scala the Python examples found in [data-science-from-scratch](https://github.com/joelgrus/data-science-from-scratch)
by Joel Grus (see the book itself: [2nd edition](https://www.oreilly.com/library/view/data-science-from/9781492041122/) from 2019)

## Table of Contents

Not all chapters have been ported as I'm going through them in the order I need them most. It all started with me having to cluster some data for a project.

1. Introduction
2. A Crash Course in Python
3. Visualizing Data
4. [Linear Algebra](./src/main/scala/scratchscala/04_LinearAlgebra.scala)
5. [Statistics](./src/main/scala/scratchscala/05_Statistics.scala)
6. Probability
7. Hypothesis and Inference
8. [Gradient Descent](./src/main/scala/scratchscala/08_GradientDescent.scala)
9. Getting Data
10. Working With Data
11. [Machine Learning](./src/main/scala/scratchscala/11_MachineLearning.scala)
12. [k-Nearest Neighbors](./src/main/scala/scratchscala/12_KNearestNeighbors.scala)
13. Naive Bayes
14. Simple Linear Regression
15. Multiple Regression
16. Logistic Regression
17. [Decision Trees](./src/main/scala/scratchscala/17_DecisionTrees.scala)
18. Neural Networks
19. Deep Learning
20. [Clustering](./src/main/scala/scratchscala/20_Clustering.scala)
21. Natural Language Processing
22. Network Analysis
23. Recommender Systems
24. Databases and SQL
25. MapReduce
26. Data Ethics
27. Go Forth And Do Data Science


## Python integration

The code uses the amazing [ScalaPy](https://scalapy.dev/) library developed by [Shadaj Laddad](https://www.shadaj.me),
which offers excellent interoperability between Python and Scala.

It's notably used to call _matplotlib_ in the same way the book does, since reproducing a plotting library is not the point of the book
(and finding exact equivalents probably not a trivial task).


## Scala version

The code uses Scala 2.12, which is very unfortunate since 2.13 has been out for a while now, and Scala just released the outstanding [3.0 version](https://www.scala-lang.org/blog/2021/05/14/scala3-is-here.html).

The main reason is that aforementioned ScalaPy does not have


## Utilities

The code makes use of some [utilities](./src/main/scala/scratchscala/utils/MiscUtils.scala) and [extension](./src/main/scala/scratchscala/utils/ExtensionMethods.scala) methods for improved readability.
Many of these can also be found in my own utilities library [Aptus](https://github.com/aptusproject/aptus-core) (not included here),
as they correspond to features often missed in the standard library.


## Python vs Scala

The scope of the book does not really allow comparing the two languages in general, and neither does the code in this repository. My personal take on "static vs dynamic" is best articulated in [this article](https://existentialtype.wordpress.com/2011/03/19/dynamic-languages-are-static-languages/) by Robert Harper. The Scala vs Python debate specifically is much more involved since their respective ecosystems matter tremendously.

A few things do stand out from the current repo:
- Python offers some nice abstractions such as collections.Counter that Scala would benefit from porting.
- Python oddly misses a built-in group-by mechanism, at least looking something like [this method](https://github.com/joelgrus/data-science-from-scratch/blob/d85ad71/scratch/decision_trees.py#L69-L73)
- Scala has nice a great group-by mechanism but misses group-by-key, count-bys and ListMap-counterparts (ListMap is insertion-order preserving)

## Style

### Case

I opted to preserve the snake case used in the book, which is not idiosyncratic in Scala. For instance I use `vector_means` instead of `vectorMeans`. I did not follow this rule for class and file names. I just couldn't.

### Code size

I tried as much as possible to maintain the original style, and not necessarily shorten/expand code blocks, so long as the resulting Scala code looked sufficiently idiosyncratic.


## Miscellaneous

- I am also rewriting some examples in more idiosyncratic Scala, or at least in a format that I find easier to comprehend. I will port those soon as well.
- Some of the code examples need a memory boost to run to completion, e.g. [KNearestNeighbor](./src/main/scala/scratchscala/12_KNearestNeighbors.scala) needs `-Xmx8g -Xms8g`
- I have found at least two similar efforts using alternative technologies:
  - in [Swift](https://github.com/melling/data-science-from-scratch-swift)
  - in [PyRx](https://github.com/thomasnield/data_science_from_scratch_rx) (quite bare)

