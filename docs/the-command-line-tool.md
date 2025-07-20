---
description: >-
  If you want to check your script, generate documentation, generate prototypes
  or use any other feature of Ampersand, you need to run the Ampersand compiler.
  This page tells you how.
---

# Running the Ampersand compiler (CLI)

## Purpose

In many situations, you can avoid running the Ampersand compiler from the command line. For instance, by using a system such as [RAP](https://rap.cs.ou.nl). However, in situations that you run Ampersand from the command line, this page is the place to be.

## Prerequisites

We assume you have `docker` running on your computer and that you are connected to the internet. In that case you don't have to install anything.

## Command

The following is the base command on Linux (works for bash as well as zsh)

```bash
docker run -it -v $(pwd):/scripts ampersandtarski/ampersand
```

On the Windows-10 command-line this looks slightly different (todo: check this!)

```
docker run -it -v $PWD:/scripts ampersandtarski/ampersand
```

This runs the Ampersand compiler (from your command prompt). The first time you do this, docker will take some time to download the images it needs.

## Synopsis

```bash
docker run -it -v "$(pwd)":/scripts ampersandtarski/ampersand COMMAND [OPTIONS] FILE
```

In the sequel we will use `ampersand` as an alias for `docker run -it -v "$(pwd)":/scripts ampersandtarski/ampersand`

## Description

The command `ampersand` takes a file as input. It must contain a valid ampersand script, i.e. a script that complies to the [syntax](./reference-material/how-to-read-syntax-statements) and semantics of ampersand. The compiler will not generate any output unless the script is valid.

## Examples

```bash
ampersand documentation --format docx --verbose Hawaii.adl  # generates a specification document for Hawaii.adl
ampersand check Delivery.adl                                # just check Delivery.adl for errors
ampersand proto --proto-dir "D:\htdocs\CvM\" Contracts.adl  # generate a prototype from Contracts.adl and write the application to the specified directory.
ampersand --help
ampersand --help --verbose
ampersand --version
```

## Help!

The help system of the command line interface shows help specific for the command you give. If you run `ampersand --help`, you wil get a list of all possible commands with their description. You can get help for each of these commands by asking for it. e.g. `ampersand documentation --help`.

## Bugs

We are happy to receive [bug reports at AmpersandTarski](https://github.com/AmpersandTarski/ampersand/issues) . However, please try to help us by first checking that it has not been reported already. You can do so by searching the issues. When in doubt however, don't hesitate to [create a new issue](https://github.com/AmpersandTarski/ampersand/issues).

## Exit status

In case the Ampersand compiler is called by software and fails, it is useful to have an exit code to give some information about the nature of the failure. The Ampersand compiler [produces the following exit codes](https://github.com/AmpersandTarski/Ampersand/blob/main/src/Ampersand/Basics/Exit.hs):

| Code | Name | Meaning                                                                           |
| --| --| -----------------------------------------------------------------------|
| 0  | Success  | Compilation has terminated without failures                                       |
| 2  | Fatal  | This is a software error in the Ampersand compiler, that should never have occured in operational use. Please [report an issue](https://github.com/AmpersandTarski/ampersand/issues). |
| 10 | Invalid  | The script is not valid and the compiler has produced error messages to help you diagnose your mistake(s).  |
| 20 | Inconsistent | The population would violate invariants. The compiler cannot generate a violation-free database.  |
| 30 | Invalid SQL  | An SQL query gives other results than the semantics of Ampersand prescribes. This should never occur, so please [report an issue](https://github.com/AmpersandTarski/ampersand/issues). |
| 50 | Ampersand violations | There are sanity checks on your script that have produced violations, so the compiler will not generate an application. |
| 60 | Composer | The installation of Composer failed, so the front-end application will not work. This is most likely a configuration error. |
| 70 | Wrong arguments  | The command-line arguments by which the compiler was called contain errors. Inspect the compiler output for a diagnosis.  |
| 80 | Back-end | The compiler failed to install the prototype framework. This is most likely a configuration error.  |


## Generating Documentation from Your Model

Ampersand is aimed at deriving software directly from a formal specification provided by the user. One of the outcomes of this process is a generated document that reflects the user's specification in a structured and analyzable format.

This document serves as an intermediate artifact in the software generation process. It is not written manually, but generated automatically based on the specification defined in the Ampersand language. It provides a concrete representation of the business rules, data structures, and constraints as interpreted by the Ampersand engine.

The purpose of this document is twofold:

1. **Transparency** – It helps users verify that their specification has been interpreted correctly.
2. **Foundation for Code Generation** – It acts as a foundation upon which further software artifacts—such as databases, user interfaces, and integrity checks—can be derived.

By aligning this document tightly with the user's formal specification, Ampersand ensures consistency, traceability, and correctness throughout the software development process.


### How to use

You can generate documentation from your model using the following command:

```bash
ampersand documentation MyModel.adl
```

This command produces several types of documentation derived from your specification. By default, it generates a <a href="https://fileinfo.com/extension/docx" target="_blank" rel="noopener noreferrer">.docx</a> file containing a number of structured chapters, each serving a different audience and purpose.

### Document Structure

The generated document typically includes the following chapters:

1. **Introduction**  
   Describes what the reader can expect from the rest of the document.  
   If your specification includes a `PURPOSE CONTEXT` statement, its content will appear here, allowing the modeler to guide the reader's understanding of the model's intent.

2. **Shared Language**  
   A chapter specifically intended for business stakeholders.  
   It focuses on the **business rules** in your model. If all rules are annotated with `PURPOSE` and `MEANING`, this chapter becomes an ideal tool for coordinating agreements and understanding with the business.

3. **Diagnosis**  
   Presents validation results regarding the **quality of the specification**.  
   This chapter is meant to assist the modeler in identifying inconsistencies or potential issues and improving the overall quality of the specification.

4. **Conceptual Analysis**  
   This chapter supports discussions on **functional requirements**.  
   It targets readers with experience in conceptual modeling.  
   - All **patterns** in the model are discussed sequentially.  
   - Concepts and relations defined within a pattern are discussed in that pattern's section.  
   - Remaining concepts and relations are discussed at the end of the chapter.

5. **Data Structure**  
   The most **technical** chapter, intended for software engineers.  
   It includes:
   - The **classification structure** (from `CLASSIFY` statements)  
   - All rules that must be implemented  
   - A **logical data model** with discussion  
   - A **technical data model** with discussion

### Visuals


The document includes **visuals** (diagrams) in multiple chapters and on various topics.


By default, visuals are generated just before the document itself is created. This ensures that the graphics are always up-to-date with the most recent state of the model. However, sometimes it is practical to generate the document and the visuals separately. Depending on your specific needs, the following options might help you in your specific case:

## Command Line Options for documentation

Generate a functional design document from your Ampersand model specification.

### Usage

```bash
ampersand documentation [OPTIONS] AMPERSAND_SCRIPT
```

### Available Options

:::note 

Only the most relevant options are listed here. For a full list, run `ampersand documentation --help`.

:::

| Option | Description | Default |
|---|---|---|
| `--[no-]Intro` | Include or exclude the **Intro** chapter. | *included*  |
| `--[no-]SharedLang`  | Include or exclude the **Shared Language** chapter. | *included*  |
| `--[no-]Diagnosis` | Include or exclude the **Diagnosis** chapter. | *included*  |
| `--[no-]ConceptualAnalysis`  | Include or exclude the **Conceptual Analysis** chapter. | *included*  |
| `--[no-]DataAnalysis`  | Include or exclude the **Data Analysis** chapter. | *included*  |
| `--datamodelOnly`  | Generate only datamodel images (implies `--no-text`). | *disabled*  |
| `--[no-]graphics` | Enable or disable generation of graphics before generating the document.  | *enabled* |
| `--graphicFormats FORMAT1,FORMAT2,...` | Comma-separated list of graphic formats to generate (no spaces). Possible values: bmp, gv, jpg, pdf, txt, png, svg, tif. Note that there must not be any spaces in this list. | `[gv,png]`  |
| `--focus-of-visuals FOCUS1,FOCUS2,...` | Comma-separated list of focuses for graphics (no spaces). Possible choices: context, pattern, rule, relation, concept. Note that there must not be any spaces in this list. | `[context,pattern]`  |
| `--[no-]text`  | Enable or disable generation of the document file containing text and graphics.  | *enabled* |
| `--format FORMAT`  | Output document format.  | `docx` |
| `--language OUTPUTLANGUAGE`  | Set output language: `NL` (Dutch) or `EN` (English). Defaults to your context language. | *context language*  |
| `--[no-]legal-refs` | Enable/disable generation of a table of legal references in the Natural Language chapter. | *disabled*  |
| `--output-dir DIR`  | Specify directory where output files will be written.  | *current working directory* |
| `--help` | Show help text. | |





### Example

```bash
ampersand documentation --no-Intro --format docx MyModel.adl