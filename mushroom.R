# Import gRain
library("gRain")

# Import Mushroom Dataset
mushroom_data <- data.frame(mushroom)


## 80% of the sample size
sample_size <- floor(0.80 * nrow(mushroom_data))

## Set the seed to make your partition reproductible
set.seed(1337)
train_ind <- sample(seq_len(nrow(mushroom_data)), size = sample_size)

# Get train and test set
train_set <- mushroom_data[train_ind, ]
test_set <- mushroom_data[-train_ind, ]

# Create DAG
dgf <- ~cap.shape:class + cap.surface:class + cap.color:class + bruises:class +
  odor:class + gill.attachment:class + gill.spacing:class + gill.size:class +
  gill.color:class + stalk.shape:class + stalk.root:class + stalk.surface.above.ring:class +
  stalk.surface.below.ring:class + stalk.color.above.ring:class + stalk.color.below.ring:class +
  veil.type:class + veil.color:class + ring.number:class + ring.type:class + spore.print.color:class +
  population:class + habitat:class

#dgf <- ~class:cap.shape + class:cap.surface + class:cap.color + class:bruises +
#        class:odor + class:gill.attachment + class:gill.spacing + class:gill.size +
#        class:gill.color + class:stalk.shape + class:stalk.root + class:stalk.surface.above.ring +
#        class:stalk.surface.below.ring + class:stalk.color.above.ring + class:stalk.color.below.ring +
#        class:veil.type + class:veil.color + class:ring.number + class:ring.type + class:spore.print.color +
#        class:population + class:habitat

dg <- dag(dgf)
pp <- extractCPT(train_set, dg)
cpp <- compileCPT(pp)
pn <- grain(cpp)

# Get test_set entry to determine evidence
testdf <- test_set[2,2:23]
# Create a proper list (not a list with one element)
test <- unlist(testdf)
test1 <- list(asia="hi")
#test1 <- list(test_set[4,2:23])
#test2 <- list(test_set[6,2:23])

querygrain(setEvidence(pn, evidence = list(test)))$class

