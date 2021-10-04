# Distances

# Compute Euclidean Distance
Dist.eucl <- function(Probe, Exemplar, r, w) { # r = distance metric; 2 = Euclidean, 1 = city-block
  Dis <- as.numeric(Probe - Exemplar)
  Dis <- sum( w*abs(Dis)^r )^(1/r)
  return(Dis)
}

# Compute Attributional Similarity
Dist.attr <- function(Probe, Exemplar, r, w) {
  Dis <- as.numeric(Probe - Exemplar)
  Dis <- w %*% as.numeric(Dis != 0) # Vector multiplication
  return(Dis)
}
