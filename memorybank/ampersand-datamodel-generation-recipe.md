# Ampersand Datamodel Generation Recipe

**Goal:** Generate a visual datamodel (SVG) of Ampersand script structure  
**Date added:** August 1, 2025

## 4-Step Recipe

### Step 1: Script Validation
```bash
ampersand check project/main.adl
```
**Goal:** Ensure that the Ampersand script is syntactically correct and translatable

### Step 2: Datamodel Generation  
```bash
ampersand documentation --datamodelOnly project/main.adl
```
**Result:** Generates `LogicalDataModel_Grouped_By_Pattern.gv` file
**Goal:** Creates GraphViz representation of logical datamodel

### Step 3: SVG Conversion
```bash
# Conversion from .gv to .svg (use dot, not pandoc)
dot -Tsvg LogicalDataModel_Grouped_By_Pattern.gv -o images/Landeneisen.svg
```
**Result:** SVG file in images/ directory
**Goal:** Creates browser-viewable vector graphics of datamodel

### Step 4: Browser Visualization
```bash
# Open SVG in Firefox
firefox images/Landeneisen.svg
# or via file path:
open -a Firefox images/Landeneisen.svg
```
**Goal:** Show visual datamodel for review and validation

## Important Notes

- **Script location:** Always project/main.adl as starting point
- **Output location:** images/ directory for SVG files  
- **Browser choice:** Firefox for best SVG support
- **Order matters:** Validation first, then generation, then conversion, then visualization

## Troubleshooting Tips

- If ampersand check fails: Solve syntax errors first
- If .gv file is not generated: Check if main.adl has all INCLUDE statements correct
- If SVG does not display properly: Try other browsers or GraphViz parameters

## Variations

- For other output formats: `dot -Tpng`, `dot -Tpdf`, etc.
- For specific patterns: Use pattern-specific ADL files
- For deployment: Integrate in CI/CD pipeline for automatic documentation updates

## Complete Workflow Example

```bash
# Step 1: Validate
ampersand check

# Step 2: Generate datamodel
ampersand documentation --datamodelOnly project/main.adl

# Step 3: Convert to SVG  
dot -Tsvg LogicalDataModel_Grouped_By_Pattern.gv -o images/Landeneisen.svg

# Step 4: Open in browser
firefox images/Landeneisen.svg
```

This recipe works for all Ampersand projects and always generates an up-to-date visual representation of the datamodel.
