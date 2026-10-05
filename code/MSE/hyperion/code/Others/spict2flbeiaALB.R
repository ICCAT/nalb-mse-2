# ============================================================
# Function: spict2flbeiaALB
#
# Purpose:
#   FLBEIA-compatible wrapper for the SPiCT surplus production
#   model. Fits SPiCT to catch and CPUE index data for each
#   iteration and updates the stock and covariate objects with
#   the estimated biomass, fishing mortality and reference
#   points (Fmsy, Bmsy, F/Fmsy, B/Bmsy).
#
# Inputs:
#   - stock   : FLStock object containing catch time series
#   - indices : list of FLIndex objects with CPUE indices
#   - control : SPiCT control list (optional, not used)
#   - covars  : named list of FLQuant objects to store
#               SPiCT reference point estimates
#
# Outputs:
#   Returns a list with two elements:
#   - stock  : FLStock updated with SPiCT estimates of B
#              (stock@stock) and F (stock@harvest)
#   - covars : updated with spict_Bmsy, spict_Fmsy,
#              spict_BBmsy, spict_FFmsy at the last year
#
#
# Author: AZTI
# ============================================================

# ---------------------------------------------------------------------------
# SPiCT assessment wrapper for FLBEIA
#
# Objectives:
#
#   1. Initialise result containers (FLQuant templates) for
#      each SPiCT output slot: F, B, Fmsy, Bmsy, FFmsy, BBmsy.
#
#   2. Prepare or resize the covars object to match the stock
#      year range.
#
#   3. For each iteration:
#
#        a. Build the SPiCT input list (obsC, timeC, obsI,
#           timeI) from the stock and index objects.
#
#        b. Set priors and fix model parameters (n, logsdc).
#
#        c. Set CPUE uncertainty (stdevfacI = 0.2 per index).
#
#        d. Fit SPiCT and extract estimates of F, B, FFmsy,
#           BBmsy for all years, and Fmsy, Bmsy for the last
#           year only.
#
#   4. Update stock@stock (biomass) and stock@harvest (F)
#      with the SPiCT estimates.
#
#   5. Store Bmsy, Fmsy, BBmsy and FFmsy in covars at the
#      last simulated year.
#
#   6. Clean workspace and return updated stock and covars.
#
# Notes:
#
#   - The Fox model (n = 1) is used by fixing logn.
#
#   - Catch is assumed to be known without error (logsdc
#     fixed close to 0).
#
#   - Fmsy and Bmsy are stored only at the last year because
#     SPiCT estimates them as time-invariant scalars.
#
#   - logalpha and logbeta priors are removed (set to 0, 0, 0)
#     as they are not relevant for this application.
#
#   - getReportCovariance is set to FALSE to reduce memory
#     usage on the cluster.
#
# ---------------------------------------------------------------------------


spict2flbeiaALB <- function(stock, indices, control = NULL, covars = covars)
{
  slot_names <- c(paste0("spict_", c("F", "B", "Fmsy", "Bmsy", "FFmsy", "BBmsy")))
  
  st           <- name(stock)
  results      <- list()
  years        <- dimnames(stock@stock)$year
  res_template <- stock@stock[, years]
  res_template[] <- NA
  
  # Initialise result containers
  for (j in 1:length(slot_names)) {
    results[[st]][[j]] <- res_template
    print(j)
  }
  names(results[[st]]) <- slot_names
  
  # Prepare covars: resize existing slots or initialise from scratch
  if (any(slot_names %in% names(covars[[st]]))) {
    for (j in 1:4) {
      first.yr        <- as.numeric(years[1])
      last.yr         <- as.numeric(tail(years, n = 1))
      covars[[st]][[j]] <- window(covars[[st]][[j]], first.yr, last.yr)
    }
  } else {
    covars        <- list()
    covars[[st]]  <- list()
    for (j in 1:4) {
      covars[[st]][[j]] <- res_template
    }
    names(covars[[st]]) <- slot_names[c(3, 4, 5, 6)]
  }
  
  # Fit SPiCT for each iteration
  for (i in 1:dim(catch(stock))[6]) {
    print(i)
    
    # Build SPiCT input list
    ip_list        <- vector("list")
    ip_list$obsC   <- c(iter(catch(stock), i))
    ip_list$timeC  <- as.numeric(dimnames(catch(stock))$year)
    ip_list$timeI  <- lapply(indices, function(x) as.numeric(dimnames(index(x))$year))
    ip_list$obsI   <- lapply(indices, function(x) c(iter(index(x), i)))
    
    # Priors (logalpha and logbeta removed — not used in this application)
    ip_list$priors$logbeta    <- c(0, 0, 0)
    ip_list$priors$logalpha   <- c(0, 0, 0)
    ip_list$priors$logbkfrac  <- c(log(1), 0.01^2)
    ip_list$priors$logr       <- c(log(0.4), 0.5, 1)
    ip_list$priors$logK       <- c(log(1.2e6), 0.5, 1)
    
    # Fix model shape: Fox(n = 1), not estimated
    ip_list$ini$logn    <- log(1.001)
    ip_list$phases$logn <- -1
    
    # Fix catch uncertainty: catches assumed known without error
    ip_list$phases$logsdc <- -1
    ip_list$ini$logsdc    <- log(0.0001)
    
    # CPUE uncertainty: CV = 0.2 for all 8 indices
    ip_list$stdevfacI <- list()
    for (cv_index in 1:8) {
      ip_list$stdevfacI[[cv_index]] <- rep(0.2, length(ip_list$obsI[[cv_index]]))
    }
    
    # Numerical integration step and memory setting
    ip_list$dteuler              <- 1/8
    ip_list$getReportCovariance  <- FALSE
    
    # Fit SPiCT
    out <- fit.spict(ip_list)
    
    # Match years between stock and SPiCT output
    years_wanted    <- dimnames(catch(stock))$year
    years_available <- row.names(get.par("logB", out, exp = TRUE))
    years           <- intersect(years_wanted, years_available)
    
    pars_des <- c(paste0("log", c("F", "B", "Fmsy", "Bmsy", "FFmsy", "BBmsy")))
    
    # Extract F, B, FFmsy, BBmsy for all years
    for (j in seq_along(pars_des[c(1:2, 5:6)])) {
      res      <- get.par(pars_des[c(1:2, 5:6)][j], out, exp = TRUE)[, "est"]
      res      <- res[names(res) %in% years]
      iter(results[[st]][[slot_names[c(1:2, 5:6)[j]]]][, years], i)[] <- res
    }
    
    # Extract Fmsy, Bmsy at the last year only (time-invariant scalars)
    for (j in seq_along(pars_des[c(3:4)])) {
      res <- get.par(pars_des[c(3:4)][j], out, exp = TRUE)[, "est"]
      iter(results[[st]][[slot_names[c(3:4)[j]]]][,
                                                  tail(dimnames(stock@stock)$year, 1)], i)[] <- res
    }
  }
  
  # Update stock object with SPiCT estimates
  stock@stock[, years]   <- results[[st]][["spict_B"]]
  stock@stock.n[, years] <- stock@stock / stock@stock.wt
  stock@harvest[, years] <- results[[st]][["spict_F"]]
  
  # Store reference points in covars at the last year
  last_yr <- tail(years, n = 1)
  covars[[st]]$spict_Bmsy[,  last_yr] <- results[[st]][["spict_Bmsy"]][,  last_yr]
  covars[[st]]$spict_Fmsy[,  last_yr] <- results[[st]][["spict_Fmsy"]][,  last_yr]
  covars[[st]]$spict_BBmsy[, last_yr] <- results[[st]][["spict_BBmsy"]][, last_yr]
  covars[[st]]$spict_FFmsy[, last_yr] <- results[[st]][["spict_FFmsy"]][, last_yr]
  
  # Clean workspace and return
  rm(list = setdiff(ls(), c("stock", "covars")))
  return(list(stock = stock, covars = covars))
}