# WORKING DIRECTORY GUIDELINES

## Fixed Working Directory
- **Rule**: Always continue working from the main directory. That is the directory with which VS Code is opened.
- **Do not switch directories** Address files with their relative path from the working directory instead.
- Execute all commands from the main directory
- For subdirectories use relative paths: `project/`, `memorybank/`, `images/`, etc.
- For example: `ampersand check project/main.adl` instead of `cd project && ampersand check main.adl`

## Reasons
- Consistency in working method
- Prevents confusion about current location
- Maintains overview of project structure
- Avoids problems with relative path references

## Examples of Correct Commands
```bash
# Correct - from main directory
ampersand check project/main.adl
dot -Tsvg images/LogicalDataModel.gv -o images/LogicalDataModel.svg
python create_steekproef_excel.py

# Incorrect - no longer use
cd project && ampersand check main.adl
cd images && dot -Tsvg LogicalDataModel.gv -o LogicalDataModel.svg
```

**Date established**: August 1, 2025
**Status**: Active - always apply
