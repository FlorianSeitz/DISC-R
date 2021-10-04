# Simulation Master Thesis: Optimal Experimantal Design

# Load libraries
library(dplyr)

# Compute predicted category of probe based on euclidean distance/attributional distance
Similarity <- function(Stimuli, Metric, c, w, p, r) { # What do if both exemplars are equally distant? Percentage
  # Stimuli = Dataframe of stimulis to compare; column names: ID, Dim1, Dim2
  # Metric = string, either "Eucl", "Attr"
  # w = vector with attention weights, sum = 1
  # c = scalar, sensitivity that discriminate the items on the dimensions
  # p = scalar for decay function, 1 (exponential) for readily discriminable stimuli or 2 (gaussian) for highly confusable stimuli
  # r = scalar, distance metric, 1 = city-block for separable-dimension stimuli, 2 = euclidean for integral-dimension stimuli
  # Include stop rules to the function
  if(sum( grepl("^Dim", colnames( Stimuli ))) == 0) {
    stop("Columns Dim are missing in Learningset!")
  }
  N <- ncol(Stimuli)
  col.names <- colnames(Stimuli)
  n.dim <- sum(grepl("^Dim", col.names))
  dim.names <- col.names[grepl("^Dim", col.names)]
  if(length(w) != n.dim) {
    stop("Length of w must be equal to number of feautre dimensions!")
  }
  if(sum(round(w, 5)) != 1) {
    stop("Weights need to sum up to 1!")
  }
  if(!all(sapply(Stimuli[, dim.names], is.numeric))) {
    stop("Column ", colnames(Stimuli[!sapply(Stimuli, is.numeric)])," needs to be numeric!")
  }
  if(Metric != "Eucl" & Metric != "Attr") {
    stop("Metric needs to be either Eucl or Attr!")
  }
  # if(p != 1 & p != 2) {
  #   stop("p needs to be 1 or 2!")
  # }
  # if(r != 1 & r != 2) {
  #   stop("r needs to be 1 or 2!")
  # }
  Dist.sets <- NULL
  for(i in 1:(nrow(Stimuli)-1)) {
    Probe <- Stimuli[Stimuli$ID == i, ]
    Learningset <- Stimuli[Stimuli$ID > i, ]
    # Metric argument of the function
    if(n.dim == 2) {
      if(Metric == "Eucl") {
        Dist.set <- Learningset %>% 
          group_by(ID) %>%
          summarise(
            Dist = Dist.eucl(Probe = Probe[, grepl("^Dim", colnames(Probe))], Exemplar = c(unique(Dim1), unique(Dim2)), w = w, r = r)
          )
      }
      if(Metric == "Attr") {
        Dist.set <- Learningset %>%
          group_by(ID) %>%
          summarise(
            Dist = Dist.attr(Probe = Probe[, grepl("^Dim", colnames(Probe))], Exemplar = c(unique(Dim1), unique(Dim2)), w = w, r = r)
          )
      }
    }
    if(n.dim == 3) {
      if(Metric == "Eucl") {
        Dist.set <- Learningset %>%
          group_by(ID) %>%
          summarise(
            Dist = Dist.eucl(Probe = Probe[, grepl("^Dim", colnames(Probe))], Exemplar = c(unique(Dim1), unique(Dim2), unique(Dim3)), w = w, r = r)
          )
      }
      if(Metric == "Attr") {
        Dist.set <- Learningset %>%
          group_by(ID) %>%
          summarise(
            Dist = Dist.attr(Probe = Probe[, grepl("^Dim", colnames(Probe))], Exemplar = c(unique(Dim1), unique(Dim2), unique(Dim3)), w = w, r = r)
          )
      }
    }
    if(n.dim == 4) {
      if(Metric == "Eucl") {
        Dist.set <- Learningset %>%
          group_by(ID) %>%
          summarise(
            Dist = Dist.eucl(Probe = Probe[, grepl("^Dim", colnames(Probe))], Exemplar = c(unique(Dim1), unique(Dim2), unique(Dim3), unique(Dim4)), w = w, r = r)
          )
      }
      if(Metric == "Attr") {
        Dist.set <- Learningset %>%
          group_by(ID) %>%
          summarise(
            Dist = Dist.attr(Probe = Probe[, grepl("^Dim", colnames(Probe))], Exemplar = c(unique(Dim1), unique(Dim2), unique(Dim3), unique(Dim4)), w = w, r = r)
          )
      }
    }
    Dist.sets <- rbind(Dist.sets, Dist.set)
  }
  Dist.sets$Dist <- exp(-c * Dist.sets$Dist^p) # Nosofsky 1986, p. 40, psychological distance; rescale big distance to small choice probability 
  return(Dist.sets$Dist)
}
