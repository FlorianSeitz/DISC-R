# ==========================================================================
# Setup: Cognitive models
# Author: Jana B. Jarecki
# ==========================================================================
# library(cognitivemodels) # from janajarecki/github


## Set up the models -------------------------------------------------------
# GCM: standard generalized context model (with Minkowski similarity)
# GCM_disc: generalized context model with the discrete similarity
# GCM with fixed bias (b0 = b1), metric exponent (r), and similarity exponent (q); first block discounted (dc)
GCM <- function(dt, comb, metr = "minkowski", unidim = FALSE, fix = list(r = 1, p = "r")) {
  discount <- dt[, which((type == "A" & type_trial %in% comb[, 1]) | 
                               (type == "B" & type_trial %in% comb[, 2]) |
                               (type == "C" & type_trial %in% comb[, 3]) |
                               (type == "D" & type_trial %in% comb[, 4]) | 
                               type %in% c("I", "V"))]
  
  if(!unidim) {
    gcm_sim(formula = response_s ~ feature1_l + feature2_l + feature3_l | feature1_r + feature2_r + feature3_r, 
            data = dt, 
            discount = discount, 
            metric = metr, 
            fixed = fix, 
            choicerule = NULL)
  } else {
    gcm_sim_unidim(formula = response_s ~ feature1_l + feature2_l + feature3_l | feature1_r + feature2_r + feature3_r, 
                   data = dt, 
                   discount = discount, 
                   metric = metr, 
                   fixed = fix,
                   choicerule = NULL)
  }
}

# GCM with discrete metric
GCM_disc <- function(dt, comb) {
  GCM(dt = dt, comb = comb, metr = "discrete")
}

# unidimensional GCM
GCM_unidim <- function(dt, comb) {
  GCM(dt = dt, comb = comb, unidim = TRUE)
}

# unidimensional GCM with discrete metric
GCM_disc_unidim <- function(dt, comb) {
  GCM(dt = dt, comb = comb, metr = "discrete", unidim = TRUE)
}
