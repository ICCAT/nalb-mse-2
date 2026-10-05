# -------------------------------------------------------------------------
# Section 4: Simulation parameters related with time--------- ####
last.yr           <- proj_yr + 1 

# Create inputs:
source("code/RefPts/FLBRP/aux_functions/create_FLBEIA_inputs.R")

# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# Run FLBEIA:
Rinit_short <- FLBEIA(biols = biolsMOD, SRs = SRs, BDs = NULL, fleets=fleets, covars = NULL,
                indices = indices, advice = advice, main.ctrl = main.ctrl,
                biols.ctrl = biols.ctrl,
                fleets.ctrl = fleets.ctrl.SMFB,
                covars.ctrl = covars.ctrl,
                obs.ctrl = obs.ctrl,
                assess.ctrl = assess.ctrl,
                advice.ctrl = advice.ctrl
                )
