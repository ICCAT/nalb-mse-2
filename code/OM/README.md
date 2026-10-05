# Module: OM

## Purpose

This module constructs the Operating Model (OM) for the North Atlantic
albacore MSE. It generates Monte Carlo realisations of the SS3 stock
assessment by sampling key biological parameters on the cluster, aggregates
and summarises the resulting outputs, and evaluates MCMC convergence
diagnostics to select the representative runs used to condition the OM.

## Structure

```text
OM/   (no subdirectories)
```

## Active scripts

### `Evaluate_MCMC_Convergence.r`

Evaluate MCMC convergence diagnostics, identify poor
model fits, and select representative runs for the
Atlantic albacore Operating Model.

### `Read_and_Summarize_MonteCarloRuns.R`

Aggregate and summarize the outputs of multiple SS3
Monte Carlo runs.

### `SS3_MonteCarlo.R`

Generate Monte Carlo realizations of the SS3 assessment
by sampling key biological parameters and running
independent SS3 model fits. (Run in the cluster)

