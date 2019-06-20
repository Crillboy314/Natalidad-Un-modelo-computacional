## Assing characteristic to every person in the society provided they are randomdly choosed

sample.dynamic.matrix <- function(pop.symbols, probs, m, n) {
  samples <- sample(pop.symbols, m*n, prob = probs, replace=TRUE)
  return(matrix(samples, nrow=m))
}

## Function to normalize the vector between a range

normalize <- function(x) {
  return((x-min(x)) / (max(x)-min(x)))
}