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

dg <- dag(dgf)
# Added smooth parameter to enable dependencies between parameters
pp <- extractCPT(train_set, dg, smooth=0.5)
cpp <- compileCPT(pp)
pn <- grain(cpp)

plot(pn)

# Variable for counting the total amount of true values
TN = 0
FN = 0
TP = 0
FP = 0
for (j in 1:nrow(test_set)) {
  # Get test_set entry to determine evidence
  testdf <- test_set[j,2:23]
  testclass <- test_set[j, 1]
  # Not only the problem of one element list, we also need to change the factor class to character class to be read as evidence
  test <- as.list(vector(length = ncol(testdf)))
  for (i in 1:ncol(testdf)) {
    test[[i]] <- as.character(testdf[,i])
  }
  names(test) <- colnames(testdf)
  
  # Set evidence and get prediction
  evidence <- setEvidence(pn, evidence = test)
  result <- querygrain(evidence)$class
  maxresult <- which.max(result)
  
  # Compare true result to 
  if (as.character(maxresult) == "1") {
    maxresult <- "e"
    if (maxresult == as.character(testclass)) {
      TP <- TP + 1
    } else {
      FP <- FP + 1
    }
  } else {
    maxresult <- "p"
    if (maxresult == as.character(testclass)) {
      TN <- TN + 1
    } else {
      FN <- FN + 1
    }
  }
}

print(TP)
print(FP)
print(FN)
print(TN)
print(TN+TP+FP+FN)
