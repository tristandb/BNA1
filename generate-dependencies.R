library(dagitty)
library(bnlearn)

data <- mushroom_data[, sapply(mushroom_data, nlevels) > 1]

all_values <- colnames(data)
used_values <- c("population", "habitat", "class", "stalk.color.above.ring", "odor", "cap.color", "stalk.color.above.ring", "spore.print.color")
non_used_values <- all_values[! all_values %in% used_values]

# 1) Get non-implied conditional independencies by the network (combination of non_used_value _||_ used_value | used_value)
sink("cond.csv")
writeLines(c(paste("cond1, cond2, given, p-value, rmsea")), fileConn)
for (cond_0 in non_used_values) {
  for (cond_1 in used_values) {
    for (given in used_values) {
      if (cond_0 != "stalk.root" && cond_0 != "veil.type") {
        chi_square <- ci.test(data[,cond_0], data[,cond_1], data[,given], test="x2")
        rmsea <- sqrt(max((((chi_square$statistic / chi_square$parameter) - 1)/(nrow(data) - 1)), 0))
        if (chi_square$p.value < 0.05 && rmsea > 0.02 && cond_1 != given) {
          writeLines(c(paste(cond_0, ", ", cond_1, ", ", given, ", ", chi_square$p.value, ", ", rmsea)), fileConn)
        }
      }
    }
  }
}
sink()

# 2) Get implied conditional independencies by the network
graph <- dagitty("dag { X -> Y }")
impliedConditionalIndependencies(graph)


# 3) Perform X^2 test on 2
