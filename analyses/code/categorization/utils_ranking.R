ranking <- function(weights, models, crit, weighted = FALSE, exclude = FALSE) {
  pairs <- gtools::permutations(n = length(models), r = 2) # matrix of model pairs
  comparisons <- rbindlist(apply(pairs, 1, function(x) { # compares models pairwise
    pair <- models[x] # gets the names of the two models
    weights_x <- weights[, ..pair] # extracts the weights of the model pair
    models_x <- weights_x[, .SD/(rowSums(.SD) + .Machine$double.eps)] # normalizes the weights
    # Makes comparison and weights it with actual weight
    models_x <- models_x[, .(m1_better_m2 = ifelse(get(pair[1]) >= crit, 1,
                                                 ifelse(get(pair[2]) >= crit, -1, 0)),
                           m1 = pair[1],
                           m2 = pair[2])]
    if(exclude) models_x[, m1_better_m2 := m1_better_m2 * rowSums(weights_x > crit)]
    if(weighted) models_x[, m1_better_m2 := m1_better_m2 * weights_x[, abs(get(pair[1]) - get(pair[2]))]]
    models_x[, time_pressure_cond := weights$time_pressure_cond]
  }))
  comparisons <- comparisons[, .(sum = round(sum(m1_better_m2), 2)), by = list(time_pressure_cond, m1)]
  setkey(comparisons, time_pressure_cond, sum) # rank order (higher number = better)
  comparisons
}
