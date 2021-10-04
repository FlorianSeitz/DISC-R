# ==========================================================================
# Setup: Cognitive models
# Author: Jana B. Jarecki
# ==========================================================================
# library(cognitivemodels) # from janajarecki/github


## Set up the models -------------------------------------------------------
# GCM: standard generalized context model (with Minkowski similarity)
# GCM_disc: generalized context model with the discrete similarity
# GCM with fixed bias (b0 = b1), metric exponent (r), and similarity exponent (q); first block discounted (dc)

GCM <- function(dt, metr = "minkowski", fix = list(r = 1, p = "r"), discount = nrow(dt) - 100) {
  gcm(formula = response ~ feature1 + feature2 + feature3, 
      cat = ~ true_cat, 
      data = dt, 
      discount = discount, 
      metric = metr, 
      fix = fix, 
      choicerule = "softmax"
  )
}

# GCM with discrete metric
GCM_disc <- function(dt) {
  GCM(dt = dt, metr = "discrete")
}

# unidimensional GCM
GCM_unidim <- function(dt, metr = "minkowski", fix) {
  gcm_unidim(formula = response ~ feature1 + feature2 + feature3, 
             cat = ~ true_cat, 
             data = dt, 
             discount = nrow(dt[block == "training"]), 
             metric = metr, 
             fixed = fix, 
             choicerule = "softmax"
  )
}

# unidimensional GCM with discrete metric
GCM_disc_unidim <- function(dt, fix) {
  GCM_unidim(dt = dt, metr = "discrete", fix = fix)
}

# GCM with Mnkowski metric and small discount (first block)
GCM_small <- function(dt) {
  GCM(dt = dt, discount = 8)
}

# GCM with discrete metric and small discount (first block)
GCM_disc_small <- function(dt) {
  GCM(dt = dt, metr = "discrete", discount = 8)
}

# GCM with Mnkowski metric and small discount (first block)
GCM_tau <- function(dt, fix) {
  GCM(dt = dt, discount = nrow(dt[block == "training"]), fix = fix)
}

# Decision tree (attend to second feature first, then to one of the remaining features)
RULE_seitz2021 <- function(dt, discount = nrow(dt) - 100) {
  rule_seitz2021(response ~ feature1 + feature2 + feature3, 
                 data = dt,
                 choicerule = "epsilon",
                 discount = discount
  )
}