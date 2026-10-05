# Module: Shiny

## Purpose
 
This module prepares and launches the interactive visualisation tools for
the North Atlantic albacore MSE results. It builds the input objects
required by two visualisation frameworks (shinyFLBEIA and Slick), combining
aggregated FLBEIA outputs across all management procedures and Operating
Model sets, computing performance indicators, and providing multilingual
metadata (EN, ES, FR). It also provides a utility function for translating
all descriptive text fields in the shinyFLBEIA object.

## Structure

```text
Shiny/   (no subdirectories)
```

## Active scripts

### `create_shiny_object.R`

Build the .flbeia input object for the shinyFLBEIA
interactive app. Loads and combines aggregated FLBEIA
outputs across all management procedures (MB 25% TAC var,
MB 10% TAC var, PCC, EMP) and Operating Model sets
(Reference + 3 Robustness), joins biological reference
points, calculates B/Bmsy and F/Fmsy, computes
performance indicators and TAC stability metrics, and
structures all results into the format expected by
shinyFLBEIA. Multilingual metadata (EN, ES, FR) is
included. The resulting object is saved as NALB.flbeia
and copied to the shinyFLBEIA repository.

### `create_slick_object.R`

Build the Slick input object for interactive MSE
visualisation of North Atlantic albacore results.
Loads aggregated FLBEIA outputs for three management
procedures (semi-constant catch, empirical and
model-based), joins biological reference points,
calculates B/Bmsy and F/Fmsy, and populates a Slick
object with time series, Kobe, boxplot, quilt and
spider plot components. Saves the object as ALB.slick
and launches the Slick app.

### `shiny_Emp.R`

Combine aggregated FLBEIA outputs across all management
procedure scenarios (ModelBased 25% TAC var, ModelBased
10% TAC var, PCC and EMP) and launch the FLBEIAshiny
interactive visualisation app.

### `translate_text.R`

Provide the translate_info() function, which adds
multilingual support (English, Spanish and French) to
the shinyFLBEIA input object. Replaces all descriptive
text fields with named lists containing the three
language versions, covering: MSE title and summary,
MP descriptions, OM factor and level descriptions,
time series labels, performance indicator descriptions,
and fleet descriptions.

