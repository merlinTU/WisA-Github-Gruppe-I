
# expected_freq: betimmt erwartete Haeufikeiten fuer Chi^2-Test
expected_freq <- function(x){
  freq_table <- table(x)
  
  exp <- rep(sum(freq_table) / length(freq_table), length(freq_table))
  p <-  exp / sum(exp)
  return(p)
}

# gini: betimmt Gini-index
gini <- function(x){
  p = prop.table(table(x))
  g = 1 - sum((p)^2)
  
  return(g)
}