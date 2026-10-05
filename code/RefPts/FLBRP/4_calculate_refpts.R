# ============================================================
# Script: 4_calculate_refpts.R
#
# Purpose:
#   Calculate MSY biological reference points (Fmsy, SSBmsy,
#   MSY) for a single Operating Model run. Extracts biological
#   quantities from the FLBEIA output, back-calculates annual
#   F-at-age via the Baranov equation, fits a Beverton-Holt
#   stock-recruitment relationship, and estimates reference
#   points using two methods: FLBRP and a custom R function.
#
# Inputs:
#   - Rinit_short    : FLBEIA result object (biols, fleets,
#                      stocks) produced by 3_run_short_proj.R
#   - stock_name     : character name of the target stock
#                      (set in 1_set_params.R)
#   - first_yr, proj_yr, F_max, a1, b1, avg_years
#                    : parameters set in 1_set_params.R
#   - aux_functions/calc_msy_refpts.R : custom MSY function
#
# Outputs:
#   - estimates/est_<irun>.csv : data frame with Fmsy, SSBmsy
#     and MSY estimates from both methods for run <irun>
#
#
# Author: AZTI
# ============================================================
library(FLBRP)
source("code/RefPts/FLBRP/aux_functions/calc_msy_refpts.R")

# ══════════════════════════════════════════════════════════════════════════════
# SECTION 1: Extract biological quantities from the FLBEIA FLBiol object
# ══════════════════════════════════════════════════════════════════════════════
# Replace 'sim_output' with the name of your FLBEIA result object and
# 'stock_name' with the character name of your stock (e.g. "hake").
sim_output <- Rinit_short   # e.g. returned by FLBEIA()

# Extract the FLBiol for this stock
biol <- sim_output$biols[[stock_name]]
biol = biol[,ac(first_yr:(proj_yr-1)),,,,]

# Numbers-at-age [age, year, unit, season, area, iter]
N <- n(biol)

# Natural mortality-at-age (assumed constant across seasons here;
# if yours is seasonal, keep it as-is)
M <- m(biol)

# Weight-at-age in the stock (used for SSB and biomass)
Wt_stock <- wt(biol)

# Proportion mature-at-age (for SSB)
Mat <- mat(biol)

# Proportion of F before spawning  (spwn slot in FLBiol)
Spwn_F <- spwn(biol)   # fraction of F before spawning; often 0

# Proportion of M before spawning
Spwn_M <- spwn(biol)         # fraction of M before spawning; often 0.5


# ══════════════════════════════════════════════════════════════════════════════
# SECTION 2: Back-calculate total F-at-age from catch-in-numbers (Baranov)
# ══════════════════════════════════════════════════════════════════════════════

fleets <- sim_output$fleets  # FLFleetsExt object

# ── 2a. Sum catch-in-numbers across all 15 fleets (and their metiers) ─────────
# FLBEIA stores catches inside fleets[[fleet]][[metier]]@catches[[stock]]
# We iterate and accumulate.

# Initialise a template FLQuant from biol (age x year x unit x season x area x iter)
total_catch_n <- FLQuant(0, dimnames = dimnames(N))

for (flt_name in names(fleets)) {
  flt <- fleets[[flt_name]]
  flt_mets = flt@metiers
  for (met_name in names(flt_mets)) {
    met    <- flt_mets[[met_name]]
    # catches slot: a list keyed by stock name
    if (stock_name %in% names(met@catches)) {
      cat_obj  <- met@catches[[stock_name]]
      # landings.n + discards.n = total catch in numbers
      cat_n    <- landings.n(cat_obj) + discards.n(cat_obj)
      # Align dimensions before adding (guard against metier/fleet dimension mismatches)
      cat_n = cat_n[,ac(first_yr:(proj_yr-1)),,,,]
      total_catch_n <- total_catch_n + cat_n
    }
  }
}

# Wt for catches:
Wt_catch = sim_output$stocks[[stock_name]]@catch.wt
Wt_catch = Wt_catch[,ac(first_yr:(proj_yr-1)),,,,]

# ── 2b. Solve for F-at-age using the Baranov equation (Newton iterations) ─────
# Applied season by season; result is F per season, then summed to annual F.

baranov_F <- function(C, N_start, M_period) {
  F_a <- pmax(C / (N_start + 1e-10), 1e-6)   # initial guess
  for (i in seq_len(30)) {
    Z      <- F_a + M_period
    pred_C <- (F_a / Z) * N_start * (1 - exp(-Z))
    dC_dF  <- N_start * (exp(-Z) * (1 + F_a / Z) +
                           (1 - exp(-Z)) * (M_period / Z^2))
    delta  <- (pred_C - C) / (dC_dF + 1e-15)
    F_a    <- pmax(F_a - delta, 1e-9)
    if (max(abs(delta)) < 1e-12) break      # converged
  }
  return(F_a)
}

# Dimensions
n_ages  <- dim(N)[1]
n_years <- dim(N)[2]
n_seas  <- dim(N)[4]  
n_iter  <- dim(N)[6]

# ── 2c. Annual catch-in-numbers: sum over seasons ─────────────────────────────
C_annual <- apply(total_catch_n, c(1, 2, 3, 5, 6), sum)  # [age, yr, 1, 1, 1, iter]

# ── 2d. Annual N: start-of-year = start of season 1 (from FLBiol) ─────────────
# read directly from biol – no manual propagation.
N_annual <- N[, , 1, 1, 1, ]   # [age, yr, 1, 1, 1, iter]

# ── 2e. Annual M: sum of seasonal M (conventional annual M) ───────────────────
M_annual <- apply(M, c(1, 2, 3, 5, 6), sum)

# ── 2f. – Solve annual F directly from annual C and start-of-year N ─────
# This avoids the upward bias introduced by summing seasonal F values.
F_annual <- FLQuant(NA, dimnames = dimnames(N_annual))

for (yr in seq_len(n_years)) {
  for (it in seq_len(n_iter)) {
    F_annual[, yr, 1, 1, 1, it] <- baranov_F(
      C       = C_annual[, yr, 1, 1, 1, it, drop = TRUE],
      N_start = N_annual[, yr, 1, 1, 1, it, drop = TRUE],
      M_period = M_annual[, yr, 1, 1, 1, it, drop = TRUE]
    )
  }
}

# ── Diagnostic: compare old (summed) vs new (direct) annual F ─────────────────
# Run this block to verify the fix reduced F, then comment it out.
F_season_sum_check <- FLQuant(NA, dimnames = dimnames(N))
for (yr in seq_len(n_years)) {
  for (it in seq_len(n_iter)) {
    for (ss in seq_len(n_seas)) {
      # use N from FLBiol for this season directly
      N_seas_vec <- N[, yr, 1, ss, 1, it, drop = TRUE]
      C_seas_vec <- total_catch_n[, yr, 1, ss, 1, it, drop = TRUE]
      M_seas_vec <- M[, yr, 1, ss, 1, it, drop = TRUE]
      F_season_sum_check[, yr, 1, ss, 1, it] <- baranov_F(C_seas_vec, N_seas_vec, M_seas_vec)
    }
  }
}
F_seasonal_sum <- apply(F_season_sum_check, c(1, 2, 3, 5, 6), sum)

cat("\n--- Diagnostic: mean fbar, old (seasonal sum) vs new (annual direct) ---\n")
cat(sprintf("  Mean F old (seasonal sum) : %.4f\n",
            mean(apply(F_seasonal_sum,  2, mean), na.rm = TRUE)))
cat(sprintf("  Mean F new (annual direct): %.4f\n",
            mean(apply(F_annual,        2, mean), na.rm = TRUE)))
cat("  New F should be <= old F. A large difference confirms the fix was needed.\n\n")
# Remove the check object to keep the workspace tidy
rm(F_season_sum_check, F_seasonal_sum)

# Annual weight: mean across seasons (weighted average)
Wt_annual <- apply(Wt_stock, c(1, 2, 3, 5, 6), mean)
Wt_catch_annual <- apply(Wt_catch, c(1, 2, 3, 5, 6), mean)

# Annual maturity: use season 1 (spawning typically occurs in one season;
# adjust the season index if spawning occurs in a different season)
spawning_season <- 1   # <<< CHANGE if spawning happens in a different season
Mat_annual <- Mat[, , 1, spawning_season, 1, ]

# ══════════════════════════════════════════════════════════════════════════════
# SECTION 3: Build an FLStock object for FLBRP
# ══════════════════════════════════════════════════════════════════════════════
# FLBRP requires an FLStock or equivalent; we construct one from the aggregated
# annual quantities extracted above.

ages     <- dimnames(N)$age
years    <- dimnames(N)$year

stk_rp <- FLStock(
  name    = stock_name,
  stock.n = N_annual,
  harvest = F_annual,   # interpreted as F-at-age by FLBRP
  m       = M_annual,
  mat     = Mat_annual,
  stock.wt  = Wt_annual,
  catch.wt  = Wt_catch_annual,   # use stock weight if no separate catch weight
  landings.wt = Wt_catch_annual,
  discards.wt = Wt_catch_annual
)

# Set harvest units to "f" (fishing mortality, not harvest rate)
harvest(stk_rp)@units <- "f"

# Compute catch.n from F and N (consistent with Baranov)
catch.n(stk_rp)  <- (F_annual / (F_annual + M_annual)) *
                  N_annual * (1 - exp(-(F_annual + M_annual)))
landings.n(stk_rp) <- catch.n(stk_rp)   # assume all catch = landings; adjust if needed
discards.n(stk_rp) <- catch.n(stk_rp) * 0

# Spawning time:
m.spwn(stk_rp) = catch.n(stk_rp) * 0
harvest.spwn(stk_rp) = catch.n(stk_rp) * 0

# Range: set fbar ages (ages over which mean F is computed – usually the
# fully-selected ages; adjust to your stock)
range(stk_rp)["minfbar"] <- biol@range["minfbar"]
range(stk_rp)["maxfbar"] <- biol@range["maxfbar"]


# ══════════════════════════════════════════════════════════════════════════════
# SECTION 4: Fit a Stock-Recruitment Relationship (FLSR)
# ══════════════════════════════════════════════════════════════════════════════

# ── 4a. Compute SSB time series ───────────────────────────────────────────────
# SSB at spawning, accounting for mortality before spawning
SSB_ts <- quantSums(
  N_annual * Mat_annual * Wt_annual *
    exp(-(Spwn_F[, , 1, spawning_season, 1, ] * F_annual +
          Spwn_M[, , 1, spawning_season, 1, ] * M_annual))
)

# ── 4b. Recruitment time series ───────────────────────────────────────────────
# Recruitment: numbers at the minimum (recruit) age
rec_age  <- as.numeric(dimnames(N_annual)$age[1])
rec_ts   <- N_annual[ac(rec_age), ]

# ── 4c. Lag SSB by 1 year to match the recruit cohort ────────────────────────
# rec(y) is produced by SSB(y-1); align by trimming
nyrs     <- dim(SSB_ts)[2]
ssb_lagged <- SSB_ts[,ac(first_yr:(proj_yr-1)),,,,]
rec_aligned <- rec_ts[,ac(first_yr:(proj_yr-1)),,,,]

# ── 4d. Fit SR model ──────────────────────────────────────────────────────────
# Use Beverton-Holt by default
sr_model <- "bevholt"   

sr <- FLSR(
  rec   = rec_aligned,
  ssb   = ssb_lagged,
  model = sr_model,
  params = FLPar(c(a1, b1), params = c('a', 'b')) # use original params
)

# ══════════════════════════════════════════════════════════════════════════════
# SECTION 5: Calculate MSY Reference Points with FLBRP
# ══════════════════════════════════════════════════════════════════════════════
# FLBRP solves for equilibrium yield over a grid of F values and finds the F
# that maximises yield (F_MSY), then returns associated SSB_MSY and MSY.

# ── 5a. Build the FLBRP object ────────────────────────────────────────────────
# Use an average of the last N years of biological parameters for the
# equilibrium calculation (avoids transient dynamics in early years).
avg_years <- tail(years, 3)   # last 3 years; adjust as needed

brp_obj <- FLBRP(
  stk_rp,
  sr  = sr,
  fbar = FLQuant(seq(0, F_max, by = 0.005), quant = "age"),
  nyears = length(avg_years)   # number of years to average life-history over
)

brp_result <- brp(brp_obj)
rp <- refpts(brp_result)
est_1 = data.frame(
        MSY = c(rp["msy", "yield"]),
        F_MSY = c(rp["msy", "harvest"]),
        SSB_MSY = c(rp["msy", "ssb"]),
        type = "FLBRP"
)

# ── 5b. Method 2: R function ─────────────────────────────────────────────
brp_obj2 <- calc_msy_refpts(
  stk = stk_rp,
  fleets = fleets,
  stock_name = "ALB",
  sr  = sr,
  avg_years = avg_years,
  F_max = F_max,
  n_F = 200
)
est_2 = data.frame(
  MSY = brp_obj2$MSY,
  F_MSY = brp_obj2$Fmsy,
  SSB_MSY = brp_obj2$SSBmsy,
  type = "Rfun"
)

# Merge estimates:
est_df = rbind(est_1, est_2)
est_df$OM = irun

# F pattern
f_df_1 = data.frame(
  age = stk_rp@range["min"]:stk_rp@range["plusgroup"],
  Fval = as.vector(rowMeans(stk_rp@harvest[,avg_years])),
  type = 'refpts'
)

# Save
write.csv(est_df, file = file.path("estimates", paste0("est_", irun, ".csv")), row.names = FALSE)
