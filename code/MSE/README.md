# Module: MSE

## Purpose

This module contains the full management strategy evaluation framework for
North Atlantic albacore. It covers the complete MSE cycle: Operating Model
conditioning from SS3 outputs, generation of observation error model (OEM)
indices, projection of multiple management procedures (model-based MP using
SPiCT, empirical aggregate index MP and pseudo-constant catch MP) across
main and robustness scenarios (R0 up, R0 down, increased recruitment
variability), aggregation of FLBEIA outputs across runs and scenarios, and
extraction of biological reference points. Projection runs are designed to
execute as SLURM array jobs on the Hyperion cluster.
Mostrar más líneas

## Structure

```text
MSE/
├── Aggregate/
├── Conditioning/
├── hyperion/
├── Indices/
└── RefPts/
```

## Active scripts

### `Aggregate/AggregateOutput_AllScenarios.R`

Aggregate FLBEIA outputs across all scenario groups
(ModelBased 25% TAC var, ModelBased 10% TAC var, PCC and EMPW),
compute model-based reference points, generate quantile
summaries, and combine all scenarios for Shiny visualisation.

### `Aggregate/CurrentMP_95CI.R`

Load the aggregated FLBEIA output for the scenario with the
current MP,compute relative values of SSB/SSBmsy and F/Fmsy
and generate quantile summaries (90% CI and 95% CI) for the biological,
fleet, metier and advice components.

### `Conditioning/Conditioning_R1a.R`

Condition the North Atlantic albacore Operating Model and create
the FLBEIA input objects for the selected SS3 runs and OM
uncertainty scenarios. Without OEM.

### `Conditioning/Conditioning_R1b.R`

Add observation error to historical and projected CPUE
indices in the conditioned FLBEIA input objects for the
Northern albacore Operating Model.

### `Conditioning/Conditioning_R1b_NoOEM.R`

Create indices, but without error.

### `hyperion/code/Aggregate/Aggregate_indices_Emp.R`

Aggregate simulated CPUE indices across all runs for a
given empirical HCR scenario (EMPW3, EMPW5, EMPW8).
Produces multi-iteration FLQuant objects for each index,
ready for plotting and diagnostic analysis.
Designed to run as a SLURM array job on the cluster.

### `hyperion/code/Aggregate/Aggregate_indices_ModelBased.R`

Aggregate simulated CPUE indices across all runs for a
given model-based HCR scenario (sanity checks R0b and R3b,
AlbHCR scenarios 2S11-2S55, robustness trials and S13_Fmsy).
Produces multi-iteration FLQuant objects for each index,
ready for plotting and diagnostic analysis.
Designed to run as a SLURM array job on the cluster.

### `hyperion/code/Aggregate/Aggregate_Runs_Emp.R`

Aggregate FLBEIA outputs across all runs for a given
empirical HCR scenario (PCC, EMPW3, EMPW8 and their
robustness trials). Reference points are fixed (Ftarget,
Btarget, FFmsy = 1); BBmsy is extracted from the advice
covariate IvalGM. Designed to run as a SLURM array job.

### `hyperion/code/Aggregate/Aggregate_Runs_ModelBased.R`

Aggregate FLBEIA outputs across all runs for a given
scenario, extract SPiCT-based reference points per year,
and save combined summaries for downstream analysis.
Designed to run as a SLURM array job on the cluster.

### `hyperion/code/codeEmp/Run_EMPW3.R`

Run the FLBEIA projection stage (2026-2057) for scenario
EMPW3 using an empirical aggregate index HCR
(ALB_AggInd_HCR). No stock assessment model is used;
observation is set to perfectObs. Loads the Rinit output
as starting point and updates indices for 2025.
Designed to run as a SLURM array job.

### `hyperion/code/codeEmp/Run_EMPW3_R0dw.R`

Run the FLBEIA projection stage (2026-2057) for scenario
EMPW3_R0dw (R0 down robustness trial). Identical to
EMPW3 except that the stock-recruitment parameter a is
scaled down by 20% (R0 x 0.8). No stock assessment model
is used; observation is set to perfectObs.
Designed to run as a SLURM array job.

### `hyperion/code/codeEmp/Run_EMPW3_R0up.R`

Run the FLBEIA projection stage (2026-2057) for scenario
EMPW3_R0up (R0 up robustness trial). Identical to EMPW3
except that the stock-recruitment parameter a is scaled
up by 20% (R0 x 1.2). No stock assessment model is used;
observation is set to perfectObs.
Designed to run as a SLURM array job.

### `hyperion/code/codeEmp/Run_EMPW3_sigma.R`

Run the FLBEIA projection stage (2026-2057) for scenario
EMPW3_sigma (increased recruitment variability robustness
trial). Identical to EMPW3 except that stock-recruitment
uncertainty is increased by scaling sigma x 1.2 and
resampling the SRs uncertainty FLQuant. No stock
assessment model is used; observation is set to
perfectObs. Designed to run as a SLURM array job.

### `hyperion/code/codeEmp/Run_EMPW8_HCR.R`

Run the FLBEIA projection stage (2026-2057) for scenario
EMPW8 using an empirical aggregate index HCR
(ALB_AggInd_HCR). Identical to EMPW3 except that TAC
interannual change is constrained to ±10%
(maxRange = minRange = 0.1). No stock assessment model
is used; observation is set to perfectObs.
Designed to run as a SLURM array job.

### `hyperion/code/codeEmp/Run_EMPW8_R0dw.R`

Run the FLBEIA projection stage (2026-2057) for scenario
EMPW8_R0dw (R0 down robustness trial). Identical to
EMPW8 except that the stock-recruitment parameter a is
scaled down by 20% (R0 x 0.8). TAC interannual change
constrained to ±10% (maxRange = minRange = 0.1).
No stock assessment model is used; observation is set
to perfectObs. Designed to run as a SLURM array job.

### `hyperion/code/codeEmp/Run_EMPW8_R0up.R`

Run the FLBEIA projection stage (2026-2057) for scenario
EMPW8_R0up (R0 up robustness trial). Identical to EMPW8
except that the stock-recruitment parameter a is scaled
up by 20% (R0 x 1.2). TAC interannual change constrained
to ±10% (maxRange = minRange = 0.1). No stock assessment
model is used; observation is set to perfectObs.
Designed to run as a SLURM array job.

### `hyperion/code/codeEmp/Run_EMPW8_sigma.R`

Run the FLBEIA projection stage (2026-2057) for scenario
EMPW8_sigma (increased recruitment variability robustness
trial). Identical to EMPW8 except that stock-recruitment
uncertainty is increased by scaling sigma x 1.2 and
resampling the SRs uncertainty FLQuant. TAC interannual
change constrained to ±10% (maxRange = minRange = 0.1).
No stock assessment model is used; observation is set
to perfectObs. Designed to run as a SLURM array job.

### `hyperion/code/codeMB_10var/Run_2S31.R`

Run the FLBEIA projection stage (2026-2057) for scenario
2S31 under the MB_10var variability setting. Identical to
the MB_25var version except that TAC interannual change
is constrained to ±10% (maxRange = minRange = 0.1).
Designed to run as a SLURM array job.

### `hyperion/code/codeMB_10var/Run_2S31_R0dw.R`

Run the FLBEIA projection stage (2026-2057) for scenario
2S31_R0dw under the MB_10var variability setting.
TAC interannual change is constrained to ±10%
(maxRange = minRange = 0.1) and the stock-recruitment
parameter a is scaled down by 20% (R0 x 0.8).
Designed to run as a SLURM array job.

### `hyperion/code/codeMB_10var/Run_2S31_R0up.R`

Run the FLBEIA projection stage (2026-2057) for scenario
2S31_R0up under the MB_10var variability setting.
TAC interannual change is constrained to ±10%
(maxRange = minRange = 0.1) and the stock-recruitment
parameter a is scaled up by 20% (R0 x 1.2).
Designed to run as a SLURM array job.

### `hyperion/code/codeMB_10var/Run_2S31_sigma.R`

Run the FLBEIA projection stage (2026-2057) for scenario
2S31_sigma under the MB_10var variability setting.
TAC interannual change is constrained to ±10%
(maxRange = minRange = 0.1) and stock-recruitment
uncertainty is increased by scaling sigma x 1.2.
Designed to run as a SLURM array job.

### `hyperion/code/codeMB_10var/Run_2S33.R`

Run the FLBEIA projection stage (2026-2057) for scenario
2S33 under the MB_10var variability setting. Identical to
the MB_25var version except that TAC interannual change
is constrained to ±10% (maxRange = minRange = 0.1).
Designed to run as a SLURM array job.

### `hyperion/code/codeMB_10var/Run_2S33_R0dw.R`

Run the FLBEIA projection stage (2026-2057) for scenario
2S33_R0dw under the MB_10var variability setting.
TAC interannual change is constrained to ±10%
(maxRange = minRange = 0.1) and the stock-recruitment
parameter a is scaled down by 20% (R0 x 0.8).
Designed to run as a SLURM array job.

### `hyperion/code/codeMB_10var/Run_2S33_R0up.R`

Run the FLBEIA projection stage (2026-2057) for scenario
2S33_R0up under the MB_10var variability setting.
TAC interannual change is constrained to ±10%
(maxRange = minRange = 0.1) and the stock-recruitment
parameter a is scaled up by 20% (R0 x 1.2).
Designed to run as a SLURM array job.

### `hyperion/code/codeMB_10var/Run_2S33_sigma.R`

Run the FLBEIA projection stage (2026-2057) for scenario
2S33_sigma under the MB_10var variability setting.
TAC interannual change is constrained to ±10%
(maxRange = minRange = 0.1) and stock-recruitment
uncertainty is increased by scaling sigma x 1.2.
Designed to run as a SLURM array job.

### `hyperion/code/codeMB_25var/Run_2S31.R`

Run the FLBEIA projection stage (2026-2057) for scenario
2S31 (Ftg0.8) using AlbHCR and SPiCT assessment. Loads the Rinit
output as starting point, updates indices for 2025 using
VPB/VPN, and runs the full projection. Designed to run
as a SLURM array job, one job per iteration.

### `hyperion/code/codeMB_25var/Run_2S31_R0dw.R`

Run the FLBEIA projection stage (2026-2057) for scenario
2S31_R0dw (Ftg 0.8 and R0 down robustness trial). Identical to 2S31
except that the stock-recruitment parameter a is scaled
down by 20% (R0 x 0.8), reducing the carrying capacity
of the population. Designed to run as a SLURM array job.

### `hyperion/code/codeMB_25var/Run_2S31_R0up.R`

Run the FLBEIA projection stage (2026-2057) for scenario
2S31_R0up (Ftg 0.8, R0 up robustness trial). Identical to 2S31
except that the stock-recruitment parameter a is scaled
up by 20% (R0 x 1.2), increasing the carrying capacity
of the population. Designed to run as a SLURM array job.

### `hyperion/code/codeMB_25var/Run_2S31_sigma.R`

Run the FLBEIA projection stage (2026-2057) for scenario
2S31_sigma (increased recruitment variability robustness
trial). Identical to 2S31 except that stock-recruitment
uncertainty is increased by scaling sigma x 1.2 and
resampling the SRs uncertainty FLQuant.
Designed to run as a SLURM array job.

### `hyperion/code/codeMB_25var/Run_2S33.R`

Run the FLBEIA projection stage (2026-2057) for scenario
2S33 using AlbHCR and SPiCT assessment. Identical to 2S31
except that Ftar = 1 (higher fishing target). Loads the
Rinit output as starting point, updates indices for 2025
using VPB/VPN, and runs the full projection.
Designed to run as a SLURM array job.

### `hyperion/code/codeMB_25var/Run_2S33_R0dw.R`

Run the FLBEIA projection stage (2026-2057) for scenario
2S33_R0dw (R0 down robustness trial). Identical to 2S33
(Ftar = 1) except that the stock-recruitment parameter a
is scaled down by 20% (R0 x 0.8), reducing the carrying
capacity of the population.
Designed to run as a SLURM array job.

### `hyperion/code/codeMB_25var/Run_2S33_R0up.R`

Run the FLBEIA projection stage (2026-2057) for scenario
2S33_R0up (R0 up robustness trial). Identical to 2S33
(Ftar = 1) except that the stock-recruitment parameter a
is scaled up by 20% (R0 x 1.2), increasing the carrying
capacity of the population.
Designed to run as a SLURM array job.

### `hyperion/code/codeMB_25var/Run_2S33_sigma.R`

Run the FLBEIA projection stage (2026-2057) for scenario
2S33_sigma (increased recruitment variability robustness
trial). Identical to 2S33 (Ftar = 1) except that
stock-recruitment uncertainty is increased by scaling
sigma x 1.2 and resampling the SRs uncertainty FLQuant.
Designed to run as a SLURM array job.

### `hyperion/code/codePCC/Run_PCC2.R`

Run the FLBEIA projection stage (2026-2057) for scenario
PCC2 using a Pseudo Constant Catch (PCC) index-based HCR
(ALB_PCCatch_Jindex). No stock assessment model is used;
observation is set to perfectObs. Loads the Rinit output
as starting point and updates indices for 2025.
Designed to run as a SLURM array job.

### `hyperion/code/codePCC/Run_PCC2_R0dw.R`

Run the FLBEIA projection stage (2026-2057) for scenario
PCC2_R0dw (R0 down robustness trial). Identical to PCC2
except that the stock-recruitment parameter a is scaled
down by 20% (R0 x 0.8). No stock assessment model is
used; observation is set to perfectObs.
Designed to run as a SLURM array job.

### `hyperion/code/codePCC/Run_PCC2_R0up.R`

Run the FLBEIA projection stage (2026-2057) for scenario
PCC2_R0up (R0 up robustness trial). Identical to PCC2
except that the stock-recruitment parameter a is scaled
up by 20% (R0 x 1.2). No stock assessment model is
used; observation is set to perfectObs.
Designed to run as a SLURM array job.

### `hyperion/code/codePCC/Run_PCC2_sigma.R`

Run the FLBEIA projection stage (2026-2057) for scenario
PCC2_sigma (increased recruitment variability robustness
trial). Identical to PCC2 except that stock-recruitment
uncertainty is increased by scaling sigma x 1.2 and
resampling the SRs uncertainty FLQuant. No stock
assessment model is used; observation is set to
perfectObs. Designed to run as a SLURM array job.

### `hyperion/code/Others/ALB_Emp_AggInd_HCR.R`

Calculate TAC advice using an aggregate index harvest
control rule based on historical and recent index values.

### `hyperion/code/Others/ALB_PCCatch_Jindex.R`

Calculate TAC advice using a pseudo-constant catch harvest
control rule based on an aggregate biomass index.

### `hyperion/code/Others/AlbHCR.R`

Calculate TAC advice using a biomass-based harvest control
rule derived from SPiCT reference points.

### `hyperion/code/Others/spict2flbeiaALB.R`

FLBEIA-compatible wrapper for the SPiCT surplus production
model. Fits SPiCT to catch and CPUE index data for each
iteration and updates the stock and covariate objects with
the estimated biomass, fishing mortality and reference
points (Fmsy, Bmsy, F/Fmsy, B/Bmsy).

### `hyperion/code/Others/VPBInd.R`

Simulate an index based on vulnerable biomass.

### `hyperion/code/Others/VPNInd.R`

Simulate an index based on vulnerable abundance.

### `hyperion/code/Rinit/Run_init.R`

Run the FLBEIA initialisation stage (Rinit, 2022-2025)
for a single iteration using SPiCT as assessment model.
Designed to run as a SLURM array job on the cluster,
one job per iteration.

### `hyperion/code/SanityCheck/Comparison_SS3_FLBEIA_HistoricalOutputs.R`

Compare historical trajectories from SS3 and FLBEIA
to validate the Operating Model conditioning process.

### `hyperion/code/SanityCheck/Run_R0b.R`

Run FLBEIA with zero fishing effort (Ef0) as a reference
baseline scenario. SPiCT is used as assessment model with
fixed advice (no HCR applied). Designed to run as a SLURM
array job on the cluster, one job per iteration.

### `hyperion/code/SanityCheck/Run_R3b.R`

Run FLBEIA with fixed effort and fixed advice (TACF
scenario) using SPiCT as assessment model. Fleets operate
at their input effort level with no HCR applied. Designed
to run as a SLURM array job, one job per iteration.

### `hyperion/code/SanityCheck/Run_R4b.R`

Run FLBEIA with the AlbHCR management procedure and SPiCT
assessment for a single iteration on the cluster. This is
a direct projection run (2022-2050), without an explicit
initialisation stage. Designed to run as a SLURM array
job, one job per iteration.

### `Indices/Aggregate_Indices.R`

Aggregate FLBEIA index outputs across all simulation runs
and create FLQuant objects containing the full index
uncertainty distribution.

### `Indices/Compare_Index_BetweenScenarios.R`

Compare the behaviour of a selected aggregate index
between two management procedure scenarios.

### `Indices/Comparison_Jindex_Jrat_TAC.R`

Compare aggregate CPUE indicators (Jind and Jrat)
against projected TAC trajectories for a selected
management procedure.

### `Indices/Comparison_ssb_Jind_TAC.R`

Compare historical and projected trajectories of SSB,
Jind and TAC for a selected management procedure.

### `Indices/Plot_AggregatedIndices_byScenario.R`

Plot aggregated FLBEIA index projections and compare them
with individual-run and observed SS3 index time series.

### `Indices/Plot_OEM_Indices.R`

Compare historical observed CPUE indices with simulated
index trajectories from the Operating Model and visualise
index uncertainty through time. Supports two modes:
'projection' (pre-combined object) and 'historical'
(individual runs loaded and combined on the fly).

### `RefPts/Extract_SS3_ReferencePoints.R`

Extract biological reference points from the selected
SS3 realizations used to condition the Atlantic albacore
Operating Model.

