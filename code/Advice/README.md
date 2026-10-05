# Module: Advice

## Purpose

This module calculates TAC advice for North Atlantic albacore under three
management procedures: an empirical MP based on the weighted joint CPUE index
(EMP), a model-based MP using a SPiCT surplus-production assessment, and a
pseudo-constant catch MP based on the joint CPUE index (PCC). It also provides
shared utility functions used across advice scripts and a comparison of OM and
MP trajectories using B/Bmsy and F/Fmsy indicators.

## Structure

```text
Advice/   (no subdirectories)
```

## Active scripts

### `Advice_EMP_CPUE2025.R`

Calculate TAC advice using the empirical management
procedure based on the weighted joint CPUE index.

### `Advice_ModelBasedMP_CPUE2025.R`

Fit the North Atlantic albacore SPiCT assessment using catch data
and standardised CPUE indices updated to 2025, and calculate
model-based management advice.

### `Advice_PCC_CPUE2025.R`

Calculate TAC advice using the pseudo-constant catch
management procedure based on the joint CPUE index.

### `AdviceFunctions.R`

Functions used by advice scripts.

### `Compare_OM_MB_MP_cpue2025.R`

Compare OM trajectories with MP trajectories using
B/Bmsy and F/Fmsy indicators up to 2025.

