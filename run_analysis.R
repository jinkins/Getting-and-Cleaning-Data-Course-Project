# Retrieve data from Test
read.table(file = "../../UCI HAR Dataset/test/X_test.txt") -> xTest;
read.table(file = "../../UCI HAR Dataset/test/y_test.txt") -> yTest;
read.table(file = "../../UCI HAR Dataset/test/subject_test.txt") -> sujetTest

# Retrieve data from Train
read.table(file="../../UCI HAR Dataset/train/X_train.txt") -> xTrain
read.table(file="../../UCI HAR Dataset/train/Y_train.txt") -> yTrain
read.table(file="../../UCI HAR Dataset/train/subject_train.txt") -> sujetTrain

# Merge datas
xFull <- rbind(xTest,xTrain);
yFull <- rbind(yTest,yTrain);
sujetFull <- rbind(sujetTest, sujetTrain);

# Use label for activities
activites <- read.table("../../UCI HAR Dataset/activity_labels.txt");
factor(x = yFull[,1],labels = activites[,2]);

# Affect right colnames
fonctions <- read.table(file = "../../UCI HAR Dataset/features.txt");
colnames(x = xFull) <- fonctions[,2];

# Extract only data required
mean <- c(1,2,3,41,42,43,81,82,83,121,122,123,161,162,163,201,214,227,240,253,266,267,268,294,295,296,345,346,347,373,374,375,424,425,426,452,453,454,503,513,516,526,529,539,542,552);
std <- c(4,5,6,44,45,46,84,85,86,124,126,127,164,165,166,202,215,228,241,254,269,270,271,348,349,350,427,428,429,504,517,530,543);
meanAndStd <- sort(c(mean,std));
xFullRequired <- xFull[,meanAndStd];

# find indices for activities
which(yFull$V1 == 1) -> one
which(yFull$V1 == 2) -> two
which(yFull$V1 == 3) -> three
which(yFull$V1 == 4) -> four
which(yFull$V1 == 5) -> five
which(yFull$V1 == 6) -> six

# Here is the mess
unique(sujetFull[,1]) -> uniqueSujet
sort(uniqueSujet) -> uniqueSujet

list() -> indices;
k <- 1;

intersect(x = one, y = which(sujetFull[,1] == uniqueSujet[i])) -> temp;
lapply(xFullRequired[temp,], mean) -> v;
v$Who <- 1
v$What <- activites[1,2]

df <- data.frame(t(unlist(v)));

for(i in 1:length(uniqueSujet))
{
     # Activity One
     intersect(x = one, y = which(sujetFull[,1] == uniqueSujet[i])) -> temp;
     lapply(xFullRequired[temp,], mean) -> v;
     v$Who <- i
     v$What <- activites[1,2]
     df[k,] <- t(unlist(v))
     k = k + 1;
     
     # Activity Two
     intersect(x = two, y = which(sujetFull[,1] == uniqueSujet[i])) -> temp;
     lapply(xFullRequired[temp,], mean) -> v;
     v$Who <- i
     v$What <- activites[2,2]
     df[k,] <- t(unlist(v))
     k = k + 1;
     
     # Activity Three
     intersect(x = three, y = which(sujetFull[,1] == uniqueSujet[i])) -> temp;
     lapply(xFullRequired[temp,], mean) -> v;
     v$Who <- i
     v$What <- activites[3,2]
     df[k,] <- t(unlist(v))
     k = k + 1;
     
     # Activity Four
     intersect(x = four, y = which(sujetFull[,1] == uniqueSujet[i])) -> temp;
     lapply(xFullRequired[temp,], mean) -> v;
     v$Who <- i
     v$What <- activites[4,2]
     df[k,] <- t(unlist(v))
     k = k + 1;
     
     # Activity Five
     intersect(x = five, y = which(sujetFull[,1] == uniqueSujet[i])) -> temp;
     lapply(xFullRequired[temp,], mean) -> v;
     v$Who <- i
     v$What <- activites[5,2]
     df[k,] <- t(unlist(v))
     k = k + 1;
     
     # Activity Six
     intersect(x = six, y = which(sujetFull[,1] == uniqueSujet[i])) -> temp;
     lapply(xFullRequired[temp,], mean) -> v;
     v$Who <- i
     v$What <- activites[6,2]
     df[k,] <- t(unlist(v))
     k = k + 1;
     
}


