install.packages(c("mice", "lme4", "dplyr", "plyr", "mlmRev"))
library(plyr)

#the randomly generated numbers will be same each time we use this set.seed
set.seed(40917) 
df <- data.frame(subject = seq(1, 15, 1), 
                 mean = seq(10, 24, 1), 
                 sd = seq(2, 2.14, 0.01))

datasetA <- cbind(seq(1, 15,1), 
                  data.frame(matrix(rnorm(15*3), 15, 3) * df$sd + df$mean))
datasetA[, 2:4] <- round(datasetA[, 2:4], 2)
names(datasetA) <- c("subjectID", "X1", "X2", "X3")

df <- data.frame(subject = seq(1, 15, 1), 
                 mean = seq(110, 124, 1), 
                 sd = seq(2, 2.14, 0.01))
datasetB <- cbind(seq(8, 22,1), 
                  data.frame(matrix(rnorm(15*3), 15, 3) * df$sd + df$mean))
datasetB[, 2:4] <- round(datasetB[, 2:4], 2)
names(datasetB) <- c("subjectID", "X4", "X5", "X6")

datasetA #subjectIDs from 1 to 15
datasetB #subjectIDs from 8 tot 22.

AB.innerjoin <- join(datasetA, datasetB, by = "subjectID", type = "inner")
AB.innerjoin #keep subjects 8 to 15

AB.fullouterjoin <- join(datasetA, datasetB, by = "subjectID", type = "full")
AB.fullouterjoin #keep all subjects 

AB.leftjoin <- join(datasetA, datasetB, by = "subjectID", type = "left")
AB.leftjoin #keep all subjects from datasets A and match rows from B

AB.leftjoin <- join(datasetA, datasetB, by = "subjectID", type = "left")
AB.leftjoin #keep all subjects from datasets A and match rows from B

AB.rightjoin <- join(datasetA, datasetB, by = "subjectID", type = "right")
AB.rightjoin #keep all subjects from datasets A and match rows from B

df <- data.frame(subject = seq(1, 15, 1), 
                 mean = seq(10, 24, 1), 
                 sd = seq(2, 2.14, 0.01))
datasetC <- cbind(seq(1, 15,1), 
                  data.frame(matrix(rnorm(15*3), 15, 3) * df$sd + df$mean))
datasetC[, 2:4] <- round(datasetC[, 2:4], 2)
names(datasetC) <- c("subjectID", "X1", "X2", "X3")

df <- data.frame(subject = seq(1, 15, 1), 
                 mean = seq(10, 24, 1), 
                 sd = seq(2, 2.14, 0.01))
datasetD <- cbind(seq(16, 30,1), 
                  data.frame(matrix(rnorm(15*3), 15, 3) * df$sd + df$mean))
datasetD[, 2:4] <- round(datasetD[, 2:4], 2)
names(datasetD) <- c("subjectID", "X4", "X2", "X3")

datasetC
datasetD

datasetC.dropX1 <- subset(datasetC, select = c("subjectID", "X2", "X3"))
datasetD.dropX4 <- subset(datasetD, select = c("subjectID", "X2", "X3"))

datasetC.dropX1
datasetD.dropX4

add.CD.drop <- rbind(datasetC.dropX1, datasetD.dropX4)
add.CD.drop #subjectID are from 1 to 30.

datasetC.addX4 <- cbind(datasetC, rep("NA", 15))
names(datasetC.addX4) <- c("subjectID", "X1", "X2", "X3", "X4")

datasetD.addX1 <- as.data.frame(cbind(datasetD$subjectID, rep("NA", 15), 
                                      datasetD$X2, datasetD$X3, datasetD$X4))
names(datasetD.addX1) <- c("subjectID", "X1", "X2", "X3", "X4")

datasetC.addX4
datasetD.addX1

add.CD.keep <- rbind(datasetC.addX4, datasetD.addX1)
add.CD.keep
