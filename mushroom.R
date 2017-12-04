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

# X -> M <- Y : X + Y + X * Y
# 
dgf <- ~cap.shape:class + cap.surface:class + cap.color:class + bruises:class
dG <- dag(dgf)
# pp <- extractCPT(train_set, dG)

bn <- grain(dG, train_set)
plot(bn)

# Compile network
bn <- compile(bn)

querygrain(bn, nodes=c("class"), type="joint")