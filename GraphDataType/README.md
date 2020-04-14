# graph-data-type

In the meatgrinder (and in ArchiAnalyze too) we parse stuff in a rather straightforward way.
Then we make a Haskell data structure and fill it with the stuff that has been parsed.
The code to fill the data structure feels kinda redundant because it only heaves
data from the parser into a data structure. That code impedes the maintainability, because every change made to the parser requires a change to the conversion code as well.

A graph data type solves this problem by generating the data structure straight out of the Ampersand code and generating the conversion software as well. 

## What is exciting about this example?

The code uses an ontology to describe the data.
The ontology is described as a set of binary relations.
We can therefore generate the conversions between Haskell data-types.
To specify Haskell data-types (only product-types so far), we use "INTERFACE" syntax.
This is inspired on Ampersand.

## To run the example:

There is an example piece of code that:

1. parses a file with articles and reviewers, and a number of desired reviewers per paper
2. prints an assignment of reviewers to those papers

To run the code, install 'stack' and type:

```sh
cat example.txt | stack run grdt-example-reviews
```

The output will be an assignment of reviewers to papers,
and possibly some warnings

## Can I try this myself?

Sure, here's an exercise:
Suppose we want the warnings to show the paper numbers too.
That is, say we want the warnings to look as follows:

```txt
Warning about paper #1, titled "Fun with GRDTs"
An author 1 (常乐) is listed as a potential reviewer

Warning about paper #2, titled "GRDTs in Ampersand"
2 reviewer(s) required per paper, this paper can get at most 1
```

You'll need to make three changes:

1. Change the PaperInfo data-type:
   Extend the interface 'PaperInfo' with a line that says "articleId : articleId"
2. Change the way the Haskell data-type is used:
   the function warnAboutPaper contains the pattern "PaperInfo",
   it will need to get an extra argument, and you'll want to use it in the 'warn' function
3. Change the conversion to PaperInfo ...
   Where is the code in which we generate things of type 'PaperInfo'?
   The saying goes: "er is geen stap 3"

## What is GRDT?

GRaph-based Data Type, and a pun on GADT (generalized algebraic datatypes).
