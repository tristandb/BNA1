# Load bnlearn package 
library(bnlearn)

# Remove variables that only have one level (veil.type)
data <- mushroom_data[, sapply(mushroom_data, nlevels) > 1]

values <- "[population][habitat][stalk.color.above.ring][spore.print.color]"
# Create DAG
dag <- model2network(paste0("[odor|class][class|population:habitat:cap.color:stalk.surface.below.ring:stalk.color.above.ring:spore.print.color][stalk.surface.below.ring|stalk.color.above.ring][cap.color|population:habitat]",values))

# Plot DAG
graphviz.plot(dag, layout="neato", highlight=list(node = list(fontsize="80")))

# Get data that applies to this network 
used_values <- c("population", "habitat", "class", "stalk.surface.below.ring", "odor", "cap.color", "stalk.color.above.ring", "spore.print.color")
used_data <- mushroom_data[, used_values]


fitted <- bn.fit(dag, used_data)
bn.cv(used_data, bn = dag, loss = "pred-lw", loss.args = list(target = "class"))
