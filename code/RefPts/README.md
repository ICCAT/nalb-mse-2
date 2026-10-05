# Module: RefPts

## Purpose

This module estimates and validates MSY biological reference points
(Fmsy, SSBmsy, MSY) for the North Atlantic albacore Operating Model.
It runs FLBRP and a custom R function across 400 Monte Carlo runs to
obtain reference point distributions, and compares them against the
SS3 estimates to assess consistency between methods.
Mostrar más líneas

## Structure

```text
RefPts/
└── FLBRP/
```

## Active scripts

### `FLBRP/compare_estimates.R`

Compare MSY biological reference points (Fmsy, SSBmsy,
MSY) estimated with FLR (FLBRP and a custom R function)
against those obtained directly from SS3, across all
Monte Carlo runs. Produces diagnostic violin plots to
assess consistency between estimation methods.

### `FLBRP/run_all.R`

Estimate MSY biological reference points (Fmsy, SSBmsy,
MSY) for the North Atlantic albacore Operating Model
across 400 Monte Carlo runs. For each run, sets the
parameters, loads the SS3 input objects, runs a short
FLBEIA projection to obtain the required biological
quantities, and calculates reference points using FLBRP
and a custom R function.

