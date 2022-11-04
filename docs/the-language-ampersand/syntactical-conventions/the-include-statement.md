# The INCLUDE statement

## Purpose

To facilitate reusing code, Ampersand allows its user to divide code over different files.

## Description

The `INCLUDE`-statement includes the code of another Ampersand-script or the data of a .xlsx-file into the context.

## Examples

```text
INCLUDE "foo.adl"
INCLUDE "subdirectory/foo.adl"
INCLUDE "bar.xlsx"
```

## Syntax and meaning

```text
INCLUDE <filename>
```

This statement specifies files that need to be included before compiling. The filename is given in double quotes, including a path that is relative to the position of the main adl-file. The main adl-file is the file that is called with the command Ampersand.

Possible files to include are:

* other adl-files 
* xlsx-files to include population 

All code in the included adl-files will become part of the context of the main adl-file.

Make sure to include the adl-files before including xlsx-files.

Included files may contain `INCLUDE`statements themselves. The files mentioned there are treated as though they were included in the main file. So their code is also part of the same context. Nested adl-files can have their own xlsx-files included.

For formatting your excel-file see the text on [the Excel Importer](../../the-excel-importer.md).

