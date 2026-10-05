################################################################################
# calc_msy_refpts()
#
# Pure-R replacement for FLBRP::brp() for MSY reference point calculation.
#
# APPROACH – identical to FLBRP's equilibrium method:
#   1. Sweep an F grid from 0 to Fmax.
#   2. At each F, compute equilibrium per-recruit quantities:
#        - Spawners per recruit  (SPR)
#        - Yield per recruit     (YPR)
#        - Biomass per recruit   (BPR)
#   3. Scale SPR to absolute SSB using the inverse SR function:
#        SSB(F) = SR^-1( R ) ,  where R = SSB(F) / SPR(F)   [fixed point]
#      For Beverton-Holt and Ricker this has a closed-form solution.
#   4. Yield(F)  = YPR(F) * R(F)
#      SSB(F)    = SPR(F) * R(F)
#   5. MSY = max(Yield(F)) over the grid, with golden-section refinement.
#
# INPUTS – the exact objects already built in FLBEIA_MSY_reference_points.R:
#   stk       : FLStock  (annual, ages aggregated; needs stock.n, harvest, m,
#                         mat, stock.wt, harvest.spwn, m.spwn,
#                         range["minfbar"], range["maxfbar"])
#               NOTE: stk@catch.wt is NO LONGER used for YPR; fleet-specific
#                     catch weights from 'fleets' replace it.
#   fleets    : FLFleetsExt from your FLBEIA run (sim_output$fleets).
#               Each fleet/metier must have catches[[stock_name]] with
#               landings.wt, discards.wt, landings.n, and discards.n.
#   stock_name: character name of the stock (e.g. "hake")
#   sr_fit    : FLSR fitted with fmle() (bevholt, ricker, or segreg)
#   avg_years : character vector of years to average life-history parameters
#   n_F       : length of F grid (default 2000)
#   F_max     : upper bound of F grid (default 3.0)
#   n_iter    : number of iterations (NULL = all)
#
# OUTPUT – named list for FLBEIA's advice.ctrl:
#   $Fmsy, $MSY, $SSBmsy, $Bmsy  – numeric vectors of length n_iter
#   $curve – data.frame with full F-Yield-SSB equilibrium curves per iteration
#
# COMPATIBILITY
#   advice.ctrl[[stk_name]]$ref.pts["Fmsy", ] <- refpts$Fmsy
#   advice.ctrl[[stk_name]]$ref.pts["Bmsy", ] <- refpts$Bmsy
#   advice.ctrl[[stk_name]]$ref.pts["MSY",  ] <- refpts$MSY
#
# FLEET-WEIGHTED YPR – key change vs. the single-fleet version
#   Each fleet f has its own catch mean weight CWt_f[age] and its own share
#   of total F at age (prop_f_a = F_f_a / F_total_a).
#   YPR is computed as:
#     YPR(fbar) = sum_a { C_a(fbar) * CWt_eff_a }
#   where CWt_eff_a = sum_f { prop_f_a * CWt_f_a }
#   is the F-share-weighted mean catch weight at age a.
#   The fleet F-share proportions are held constant (historical mean);
#   only the overall fbar level is swept across the grid.
#
# NOTES
#   - harvest.spwn and m.spwn are used in SSB exactly as FLBRP does.
#   - For segreg SR, plateau parameter b is used as equilibrium R.
################################################################################

calc_msy_refpts <- function(stk,
                            fleets,
                            stock_name,
                            sr_fit,
                            avg_years  = NULL,
                            n_F        = 2000,
                            F_max      = 3.0,
                            n_iter     = NULL) {
  
  # ── 0. Packages ─────────────────────────────────────────────────────────────
  require(FLCore)
  
  # ── 1. Resolve dimensions ───────────────────────────────────────────────────
  all_years <- dimnames(stk)$year
  if (is.null(avg_years)) avg_years <- tail(all_years, 10)
  
  n_ages <- dim(stk)[1]
  ages   <- as.numeric(dimnames(stk)$age)
  
  if (is.null(n_iter)) n_iter <- dims(stk)$iter
  
  fbar_min <- range(stk)["minfbar"]
  fbar_max <- range(stk)["maxfbar"]
  
  # ── 2. Average life-history vectors over avg_years ─────────────────────────
  # Helper: extract a numeric matrix [age x iter] averaged over avg_years
  avg_flq <- function(flq) {
    sub <- flq[, avg_years, 1, 1, 1, ]
    apply(sub, c(1, 6), mean, na.rm = TRUE)   # [age, iter]
  }
  
  M_a      <- avg_flq(m(stk))            # natural mortality at age
  Wt_a     <- avg_flq(stock.wt(stk))     # stock weight at age (for SSB, BPR)
  Mat_a    <- avg_flq(mat(stk))          # maturity at age
  spwn_F_a <- avg_flq(harvest.spwn(stk)) # fraction of F before spawning
  spwn_M_a <- avg_flq(m.spwn(stk))       # fraction of M before spawning
  
  # ── 2a. Fleet-specific F-at-age and catch weight-at-age ───────────────────
  # For each fleet/metier that catches this stock we need:
  #   F_f_a   : fishing mortality at age from fleet f  [age x iter]
  #   CWt_f_a : catch mean weight at age from fleet f  [age x iter]
  #             (landings-weighted average of landings.wt and discards.wt)
  #
  # Both are averaged over avg_years before use.
  
  fleet_names <- names(fleets)
  n_fleets    <- length(fleet_names)
  
  # Collect per-fleet averaged F and catch weight into lists
  F_fleet_list   <- list()   # one [age x iter] matrix per active fleet
  CWt_fleet_list <- list()
  
  # Helper: sum catch-in-numbers and weighted catch weight over all metiers
  # within one fleet, then average over avg_years
  avg_fleet_catch <- function(flt) {
    # Initialise accumulators as FLQuants with correct dimensions
    # (use the first matching metier as template)
    land_n_acc <- NULL
    disc_n_acc <- NULL
    land_w_acc <- NULL   # landings.n * landings.wt  (value, not rate)
    disc_w_acc <- NULL
    flt_mets = flt@metiers
    
    for (met_name in names(flt_mets)) {
      met    <- flt_mets[[met_name]]
      if (!(stock_name %in% names(met@catches))) next
      co <- met@catches[[stock_name]]
      
      ln <- landings.n(co)
      dn <- discards.n(co)
      lw <- landings.wt(co)
      dw <- discards.wt(co)
      
      if (is.null(land_n_acc)) {
        land_n_acc <- ln * 0
        disc_n_acc <- dn * 0
        land_w_acc <- ln * 0
        disc_w_acc <- dn * 0
      }
      land_n_acc <- land_n_acc + ln
      disc_n_acc <- disc_n_acc + dn
      land_w_acc <- land_w_acc + ln * lw   # numerator for weighted mean
      disc_w_acc <- disc_w_acc + dn * dw
    }
    list(land_n = land_n_acc, disc_n = disc_n_acc,
         land_w = land_w_acc, disc_w = disc_w_acc)
  }
  
  for (flt_name in fleet_names) {
    fc <- avg_fleet_catch(fleets[[flt_name]])
    if (is.null(fc$land_n)) next   # fleet does not catch this stock
    
    # Total catch in numbers per fleet (landings + discards), averaged over years
    total_n_flt <- fc$land_n + fc$disc_n
    total_n_avg <- apply(total_n_flt[, avg_years, 1, 1, 1, ],
                         c(1, 6), sum, na.rm = TRUE)  # sum over seasons first
    total_n_avg <- apply(array(total_n_avg,
                               dim = c(n_ages, length(avg_years), n_iter)),
                         c(1, 3), mean, na.rm = TRUE)  # [age x iter]
    
    # Catch mean weight at age: (L_n*L_wt + D_n*D_wt) / (L_n + D_n)
    # Sum numerator and denominator over seasons then average over years
    num_w <- fc$land_w + fc$disc_w
    num_w_avg <- apply(num_w[, avg_years, 1, 1, 1, ],
                       c(1, 6), sum, na.rm = TRUE)
    num_w_avg <- apply(array(num_w_avg,
                             dim = c(n_ages, length(avg_years), n_iter)),
                       c(1, 3), mean, na.rm = TRUE)
    
    denom_avg <- total_n_avg
    cwt_flt   <- ifelse(denom_avg > 0, num_w_avg / denom_avg, 0)  # [age x iter]
    
    F_fleet_list[[flt_name]]   <- total_n_avg   # proxy for F share (see 2b)
    CWt_fleet_list[[flt_name]] <- cwt_flt
  }
  
  n_active_fleets <- length(F_fleet_list)
  if (n_active_fleets == 0)
    stop("No fleet catches stock '", stock_name, "'. Check fleet/metier names.")
  
  cat(sprintf("  Fleet-weighted YPR: %d fleets catching '%s'\n",
              n_active_fleets, stock_name))
  
  # ── 2b. Fleet F-share proportions and effective catch weight ──────────────
  # prop_f[fleet, age, iter] = catch_n_fleet / total_catch_n  (F share proxy)
  # CWt_eff[age, iter]       = sum_f { prop_f * CWt_f }
  #
  # We use catch-in-numbers as a proxy for F shares (proportional under the
  # same N; exact when N is the same for all fleets, which it is here since
  # fleets fish the same population).
  
  # Total catch across all active fleets
  total_n_all <- Reduce("+", F_fleet_list)   # [age x iter]
  
  CWt_eff_a <- matrix(0, n_ages, n_iter)
  for (flt_name in names(F_fleet_list)) {
    prop_f <- ifelse(total_n_all > 0,
                     F_fleet_list[[flt_name]] / total_n_all, 0)   # [age x iter]
    CWt_eff_a <- CWt_eff_a + prop_f * CWt_fleet_list[[flt_name]]
  }
  # CWt_eff_a is now the F-share-weighted mean catch weight at age [age x iter]
  
  # ── 2c. Total F-at-age selectivity (unchanged from single-fleet version) ───
  # Derived from the back-calculated total harvest in FLStock
  F_a_raw  <- avg_flq(harvest(stk))
  fbar_idx <- which(ages >= fbar_min & ages <= fbar_max)
  
  sel_a <- matrix(NA, n_ages, n_iter)
  for (it in seq_len(n_iter)) {
    fbar_it <- mean(F_a_raw[fbar_idx, it], na.rm = TRUE)
    # fbar_it <- max(F_a_raw[fbar_idx, it], na.rm = TRUE)
    if (fbar_it <= 0 || is.na(fbar_it)) fbar_it <- 1
    sel_a[, it] <- F_a_raw[, it] / fbar_it
  }
  
  # ── 3. SR parameters ────────────────────────────────────────────────────────
  sr_model  <- SRModelName(model(sr_fit))
  sr_params <- params(sr_fit)          # FLPar [param x iter]
  
  # Coerce params to a plain [param x iter] matrix
  if (dims(sr_params)$iter < n_iter) {
    # Expand scalar params to n_iter if needed
    sr_params <- propagate(sr_params, n_iter)
  }
  p_mat <- matrix(c(sr_params), nrow = dim(sr_params)[1],
                  dimnames = list(dimnames(sr_params)[[1]], NULL))
  
  # ── 4. Per-recruit engine ───────────────────────────────────────────────────
  # For a given fbar scalar and iteration index, compute:
  #   SPR  = spawners per recruit
  #   YPR  = yield per recruit
  #   BPR  = total biomass per recruit (at start of year)
  
  per_recruit <- function(fbar, it) {
    F_a <- sel_a[, it] * fbar
    Z_a <- F_a + M_a[, it]
    
    n_ages_loc <- length(ages)
    N <- numeric(n_ages_loc)
    N[1] <- 1.0                             # one recruit
    
    for (a in seq_len(n_ages_loc - 1)) {
      N[a + 1] <- N[a] * exp(-Z_a[a])
    }
    # Plus-group correction (geometric series)
    Z_plus <- Z_a[n_ages_loc]
    if (Z_plus > 1e-9) {
      N[n_ages_loc] <- N[n_ages_loc] / (1 - exp(-Z_plus))
    }
    
    # Catch in numbers per recruit (Baranov)
    C_a <- (F_a / Z_a) * N * (1 - exp(-Z_a))
    
    # Spawning N: apply pre-spawning mortality
    N_spawn <- N * exp(-(spwn_F_a[, it] * F_a + spwn_M_a[, it] * M_a[, it]))
    
    SPR <- sum(N_spawn * Mat_a[, it]   * Wt_a[, it],    na.rm = TRUE)
    BPR <- sum(N       * Wt_a[, it],                    na.rm = TRUE)
    
    # YPR: use fleet-weighted effective catch weight (replaces single CWt_a)
    # CWt_eff_a[, it] = sum_f { prop_f_a * CWt_f_a }
    YPR <- sum(C_a * CWt_eff_a[, it],                   na.rm = TRUE)
    
    c(SPR = SPR, YPR = YPR, BPR = BPR)
  }
  
  # ── 5. SR inverse: equilibrium R given SPR ──────────────────────────────────
  # The stock is at equilibrium when:
  #   SSB = SPR * R   AND   R = SR(SSB)
  # => R = SR(SPR * R)  [fixed point; solved analytically for BH and Ricker]
  #
  # Beverton-Holt:  R = a*SSB / (b + SSB)
  #   => R = a*SPR*R / (b + SPR*R)
  #   => b + SPR*R = a*SPR => R = (a*SPR - b) / SPR  (if > 0)
  #
  # Ricker:         R = a*SSB*exp(-b*SSB)
  #   => R = a*SPR*R*exp(-b*SPR*R)
  #   => 1 = a*SPR*exp(-b*SPR*R)
  #   => R = log(a*SPR) / (b*SPR)   (if a*SPR > 1)
  #
  # Segmented regression (segreg): R = min(a*SSB, b)
  #   => if SPR*R <= b/a: R = a*SPR*R => 1 = a*SPR => R = 1/(a*SPR) ... [not useful]
  #   More practically: R = b when SSB >= b/a; otherwise R = a*SSB.
  #   Equilibrium: SSB = SPR * R; R = a*SSB => R = a*SPR*R => R(1-a*SPR)=0 → only trivial
  #   => use the plateau: R = b (constant recruitment at b)
  #   => SSB_eq = SPR * b
  
  eq_recruitment <- function(SPR, it) {
    if (SPR <= 0) return(0)
    
    if (sr_model == "bevholt") {
      a <- p_mat["a", it]
      b <- p_mat["b", it]
      R <- (a * SPR - b) / SPR
      return(max(R, 0))
      
    } else if (sr_model == "ricker") {
      a <- p_mat["a", it]
      b <- p_mat["b", it]
      val <- log(a * SPR)
      if (val <= 0) return(0)
      return(val / (b * SPR))
      
    } else if (sr_model == "segreg") {
      # plateau parameter is 'b' (max recruitment)
      b <- p_mat["b", it]
      return(b)   # equilibrium on plateau
      
    } else {
      # Geomean / other: treat as constant recruitment
      a <- p_mat[1, it]
      return(a)
    }
  }
  
  # ── 6. Sweep F grid ─────────────────────────────────────────────────────────
  F_grid <- seq(0, F_max, length.out = n_F)
  
  # Output containers
  Fmsy_vec  <- numeric(n_iter)
  MSY_vec   <- numeric(n_iter)
  SSBmsy_vec<- numeric(n_iter)
  Bmsy_vec  <- numeric(n_iter)
  curve_list <- vector("list", n_iter)
  
  for (it in seq_len(n_iter)) {
    
    yield_vec <- numeric(n_F)
    ssb_vec   <- numeric(n_F)
    bm_vec    <- numeric(n_F)
    
    for (fi in seq_len(n_F)) {
      pr  <- per_recruit(F_grid[fi], it)
      R   <- eq_recruitment(pr["SPR"], it)
      yield_vec[fi] <- pr["YPR"] * R
      ssb_vec[fi]   <- pr["SPR"] * R
      bm_vec[fi]    <- pr["BPR"] * R
    }
    
    # ── 6a. Guard: check the curve is not flat/trivial ─────────────────────
    if (max(yield_vec, na.rm = TRUE) <= 0) {
      warning(sprintf("Iteration %d: yield curve is flat or negative. Check SR params and SPR.", it))
      Fmsy_vec[it]   <- NA
      MSY_vec[it]    <- NA
      SSBmsy_vec[it] <- NA
      Bmsy_vec[it]   <- NA
      next
    }
    
    # ── 6b. Coarse grid MSY ─────────────────────────────────────────────────
    best_fi   <- which.max(yield_vec)
    
    # ── 6c. Golden-section refinement around coarse optimum ─────────────────
    # Search in [F_grid[best_fi - 1], F_grid[best_fi + 1]]
    lo <- F_grid[max(best_fi - 2, 1)]
    hi <- F_grid[min(best_fi + 2, n_F)]
    
    yield_fun <- function(fbar) {
      pr <- per_recruit(fbar, it)
      R  <- eq_recruitment(pr["SPR"], it)
      pr["YPR"] * R
    }
    
    opt <- optimize(yield_fun, interval = c(lo, hi), maximum = TRUE,
                    tol = 1e-8)
    
    Fmsy_it <- opt$maximum
    pr_msy  <- per_recruit(Fmsy_it, it)
    R_msy   <- eq_recruitment(pr_msy["SPR"], it)
    
    Fmsy_vec[it]   <- Fmsy_it
    MSY_vec[it]    <- pr_msy["YPR"] * R_msy
    SSBmsy_vec[it] <- pr_msy["SPR"] * R_msy
    Bmsy_vec[it]   <- pr_msy["BPR"] * R_msy
    
    # Store full curve for diagnostics
    curve_list[[it]] <- data.frame(
      iter  = it,
      F     = F_grid,
      Yield = yield_vec,
      SSB   = ssb_vec,
      B     = bm_vec
    )
  } # end iter loop
  
  # ── 7. Combine curve data ───────────────────────────────────────────────────
  curve_df <- do.call(rbind, curve_list)
  
  # ── 8. Print summary ────────────────────────────────────────────────────────
  cat("\n========================================================\n")
  cat(" MSY Reference Points (per-recruit equilibrium method)\n")
  cat("========================================================\n")
  cat(sprintf("  Fmsy   : %.4f  (range: %.4f – %.4f)\n",
              median(Fmsy_vec,   na.rm=TRUE),
              min(Fmsy_vec,      na.rm=TRUE),
              max(Fmsy_vec,      na.rm=TRUE)))
  cat(sprintf("  MSY    : %.4f  (range: %.4f – %.4f)\n",
              median(MSY_vec,    na.rm=TRUE),
              min(MSY_vec,       na.rm=TRUE),
              max(MSY_vec,       na.rm=TRUE)))
  cat(sprintf("  SSBmsy : %.4f  (range: %.4f – %.4f)\n",
              median(SSBmsy_vec, na.rm=TRUE),
              min(SSBmsy_vec,    na.rm=TRUE),
              max(SSBmsy_vec,    na.rm=TRUE)))
  cat(sprintf("  Bmsy   : %.4f  (range: %.4f – %.4f)\n",
              median(Bmsy_vec,   na.rm=TRUE),
              min(Bmsy_vec,      na.rm=TRUE),
              max(Bmsy_vec,      na.rm=TRUE)))
  cat("========================================================\n\n")
  
  list(
    Fmsy   = Fmsy_vec,
    MSY    = MSY_vec,
    SSBmsy = SSBmsy_vec,
    Bmsy   = Bmsy_vec,
    curve  = curve_df
  )
}


################################################################################
# plot_msy_curve()
#
# Diagnostic plot of the equilibrium yield and SSB curves.
# Marks F_MSY and SSB_MSY on each panel.
# Works with the $curve data.frame returned by calc_msy_refpts().
################################################################################

plot_msy_curve <- function(refpts_obj, max_iter_plot = 9) {
  require(ggplot2)
  
  df   <- refpts_obj$curve
  iters_to_plot <- unique(df$iter)[seq_len(min(max_iter_plot, length(unique(df$iter))))]
  df   <- df[df$iter %in% iters_to_plot, ]
  
  # Reference point lines
  rp <- data.frame(
    iter   = seq_along(refpts_obj$Fmsy)[iters_to_plot],
    Fmsy   = refpts_obj$Fmsy[iters_to_plot],
    MSY    = refpts_obj$MSY[iters_to_plot],
    SSBmsy = refpts_obj$SSBmsy[iters_to_plot]
  )
  
  p1 <- ggplot(df, aes(x = F, y = Yield)) +
    geom_line(colour = "steelblue", linewidth = 0.8) +
    geom_vline(data = rp, aes(xintercept = Fmsy),
               linetype = "dashed", colour = "red") +
    geom_hline(data = rp, aes(yintercept = MSY),
               linetype = "dotted", colour = "darkgreen") +
    facet_wrap(~iter, scales = "free_y", labeller = label_both) +
    labs(title = "Equilibrium Yield vs F",
         subtitle = "Red dashed = F_MSY | Green dotted = MSY",
         x = "F (mean over fbar ages)", y = "Equilibrium Yield") +
    theme_bw(base_size = 10)
  
  p2 <- ggplot(df, aes(x = F, y = SSB)) +
    geom_line(colour = "darkorange", linewidth = 0.8) +
    geom_vline(data = rp, aes(xintercept = Fmsy),
               linetype = "dashed", colour = "red") +
    geom_hline(data = rp, aes(yintercept = SSBmsy),
               linetype = "dotted", colour = "purple") +
    facet_wrap(~iter, scales = "free_y", labeller = label_both) +
    labs(title = "Equilibrium SSB vs F",
         subtitle = "Red dashed = F_MSY | Purple dotted = SSB_MSY",
         x = "F (mean over fbar ages)", y = "Equilibrium SSB") +
    theme_bw(base_size = 10)
  
  print(p1)
  print(p2)
  invisible(list(yield_plot = p1, ssb_plot = p2))
}


################################################################################
# USAGE EXAMPLE
# (assumes objects from FLBEIA_MSY_reference_points.R are in the workspace)
################################################################################

# refpts <- calc_msy_refpts(
#   stk        = stk,            # FLStock built in Section 3 of the ref-pt script
#   fleets     = sim_output$fleets,  # FLFleetsExt – provides fleet catch weights
#   stock_name = stock_name,     # e.g. "hake"
#   sr_fit     = sr_fit,         # FLSR fitted in Section 4
#   avg_years  = avg_years,      # character vector, e.g. tail(years, 10)
#   n_F        = 2000,           # grid resolution; increase to 5000 if needed
#   F_max      = 2.0,            # raise if F_MSY is hitting the upper bound
#   n_iter     = n_iter
# )
#
# # Diagnostic plots
# plot_msy_curve(refpts)
#
# # Plug into F2CatchHCR advice.ctrl
# advice_ctrl[[stock_name]]$ref.pts["Fmsy", ] <- refpts$Fmsy
# advice_ctrl[[stock_name]]$ref.pts["Bmsy", ] <- refpts$Bmsy
#
# # Or for IcesHCR:
# advice_ctrl[[stock_name]]$ref.pts["Fmsy",     ] <- refpts$Fmsy
# advice_ctrl[[stock_name]]$ref.pts["Btrigger",  ] <- 0.5 * refpts$SSBmsy  # MSY Btrigger
# advice_ctrl[[stock_name]]$ref.pts["Blim",      ] <- 0.25 * refpts$SSBmsy # example Blim
