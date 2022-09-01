---
description: >-
  If you want to check your script, generate documentation, generate prototypes
  or use any other feature of Ampersand, you need to run the Ampersand compiler.
  This page tells you how.
---

# Running the Ampersand compiler

## Purpose

In many situations, you can avoid running the Ampersand compiler from the command line. For instance, by using a system such as [RAP](https://rap.cs.ou.nl). However, in situations that you run Ampersand from the command line, this page is the place to be.&#x20;

## Prerequisites

We assume you have `docker` running on your computer and that you are connected to the internet. In that case you don't have to install anything.

## COMMAND

The following is the base command on Linux (works for bash as well as zsh)

```bash
docker run -it -v $(pwd):/scripts ampersandtarski/ampersand
```

On the Windows-10 command-line this looks slightly different (todo: check this!)

```
docker run -it -v $PWD:/scripts ampersandtarski/ampersand
```

This runs the Ampersand compiler (from your command prompt). The first time you do this, docker will take some time to download the images it needs.

## SYNOPSIS

```bash
docker run -it -v "$(pwd)":/scripts ampersandtarski/ampersand COMMAND [OPTIONS] FILE
```

In the sequel we will use `ampersand` as an alias for `docker run -it -v "$(pwd)":/scripts ampersandtarski/ampersand`

## DESCRIPTION

The command `ampersand` takes a file as input. It must contain a valid ampersand script, i.e. a script that complies to the [syntax](../the-language-ampersand/syntactical-conventions/) and semantics of ampersand. The compiler will not generate any output unless the script is valid.

## Environment variables

The standard behavior of `ampersand` is affected by the following environment variables.

* **CCdbName**  Sets the name of the database that is to be generated. Can be overruled by --dbName
* **CCdirOutput** Sets the default output directory for generated assets. Can be overruled by --outputDir&#x20;
* **CCdirPrototype** Sets the default directory where functional prototypes are being generated. Can be overruled by --proto

## EXAMPLES

```bash
ampersand documentation --format docx --verbose Hawaii.adl  # generates a specification document for Hawaii.adl
ampersand check Delivery.adl                                # just check Delivery.adl for errors
ampersand proto --proto-dir "D:\htdocs\CvM\" Contracts.adl  # generate a prototype from Contracts.adl and write the application to the specified directory.
ampersand --help
ampersand --help --verbose
ampersand --version
```

## BUGS

We are happy to receive [bug reports at AmpersandTarski](https://github.com/AmpersandTarski/ampersand/issues) . However, please try to help us by first checking that it has not been reported already. You can do so by searching the issues. When in doubt however, don't hesitate to [create a new issue](https://github.com/AmpersandTarski/ampersand/issues).

## Exit status

In case the Ampersand compiler is called by software and fails, it is useful to have an exit code to give some information about the nature of the failure. The Ampersand compiler [produces the following exit codes](https://github.com/AmpersandTarski/Ampersand/blob/main/src/Ampersand/Basics/Exit.hs):

| Code | Name                 | Meaning                                                                                                                                                                                 |
| ---- | -------------------- | --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| 0    | Success              | Compilation has terminated without failures                                                                                                                                             |
| 2    | Fatal                | This is a software error in the Ampersand compiler, that should never have occured in operational use. Please [report an issue](https://github.com/AmpersandTarski/ampersand/issues).   |
| 10   | Invalid              | The script is not valid and the compiler has produced error messages to help you diagnose your mistake(s).                                                                              |
| 20   | Inconsistent         | The population would violate invariants. The compiler cannot generate a violation-free database.                                                                                        |
| 30   | Invalid SQL          | An SQL query gives other results than the semantics of Ampersand prescribes. This should never occur, so please [report an issue](https://github.com/AmpersandTarski/ampersand/issues). |
| 50   | Ampersand violations | There are sanity checks on your script that have produced violations, so the compiler will not generate an application.                                                                 |
| 60   | Composer             | The installation of Composer failed, so the front-end application will not work. This is most likely a configuration error.                                                             |
| 70   | Wrong arguments      | The command-line arguments by which the compiler was called contain errors. Inspect the compiler output for a diagnosis.                                                                |
| 80   | Back-end             | The compiler failed to install the prototype framework. This is most likely a configuration error.                                                                                      |
