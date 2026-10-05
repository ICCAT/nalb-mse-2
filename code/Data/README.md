# Module: Data

## Purpose

This module prepares the CPUE input data used in the advice and MSE analyses.
It compares and normalises standardised CPUE series, constructs weighted
aggregate indices for North Atlantic albacore, and validates them against
historical stock status indicators (SSB/SSBmsy and F/Fmsy).

## Structure

```text
Data/   (no subdirectories)
```

## Active scripts

### `Comparison_CPUE_and_normalizedUpdated.R`

Compare the previous and updated standardized CPUE series,
normalize the updated series using the mean up to 2021,
and generate the CPUE file used in the 2025 advice analyses.

### `Estimate_CPUERef4JointIndexWeighted.R`

Calculate and explore weighted aggregate CPUE indices
for North Atlantic albacore and compare them with historical
stock status indicators (SSB/SSBMSY and F/FMSY).

