# Module: OEM

## Purpose

This module develops and validates the Observation Error Model (OEM) for the
North Atlantic albacore MSE. It estimates vulnerable biomass, projects CPUE
indices with residual error, and analyses residual autocorrelation across OEM
scenarios to ensure that the simulated indices realistically reproduce the
statistical properties of the observed data.

## Structure

```text
OEM/   (no subdirectories)
```

## Active scripts

### `OEM_CPUEdiagnostics.R`

Estimate vulnerable biomass for a selected CPUE fleet
and compare vulnerable biomass, CPUE and SSB indicators.

### `OEM_ProjectIndices.R`

Project CPUE indices including OEM residual error and
compare projected FLBEIA indices against SS3 expected
indices.

### `OEM_ResidualAutocorrelationAnalysis.R`

Analyse residual autocorrelation and partial
autocorrelation across OEM scenarios.

### `OEMFunctions.R`

Functions used for OEM residual analysis.

