install.packages("tidyverse")
# install.packages("readxl")
# install.packages("Deducer")
# install.packages("rJava")
# install.packages("xlsx")
install.packages("datarium")
install.packages("caret")
install.packages("animint2")

require(utils)

library(rJava)

library(tidyverse)
library(readxl)
library("rJava")
library("Deducer")
library(xlsx)
library(datarium)
library(caret)

# coachingdata <- data.table::fread("C:\\Users\\sh2742\\Downloads\\Coaching logs Fall 2017- Spring 2021.csv",header = TRUE,check.names=TRUE )
coachingdata <- read.csv("C:\\Users\\sh2742\\Documents\\py\\coaching.csv")

required.coaching.cols <- coachingdata[,c(
  "State.District.ID",
  "Duration.of.Event",
  "Interaction.Type",
  "Effective.teaching.learning.practices",
  "School.based.implementation.coaching"
)]

required.coaching.cols$Effective.teaching.learning.practices[required.coaching.cols$Effective.teaching.learning.practices == ""] <- 0
required.coaching.cols$Effective.teaching.learning.practices[required.coaching.cols$Effective.teaching.learning.practices == "No"] <- 0
required.coaching.cols$Effective.teaching.learning.practices[required.coaching.cols$Effective.teaching.learning.practices == "no"] <- 0
required.coaching.cols$Effective.teaching.learning.practices[required.coaching.cols$Effective.teaching.learning.practices == "Yes"] <- 1
required.coaching.cols$Effective.teaching.learning.practices[required.coaching.cols$Effective.teaching.learning.practices == "yes"] <- 1

required.coaching.cols$School.based.implementation.coaching[required.coaching.cols$School.based.implementation.coaching == ""] <- 0
required.coaching.cols$School.based.implementation.coaching[required.coaching.cols$School.based.implementation.coaching == "No"] <- 0
required.coaching.cols$School.based.implementation.coaching[required.coaching.cols$School.based.implementation.coaching == "no"] <- 0
required.coaching.cols$School.based.implementation.coaching[required.coaching.cols$School.based.implementation.coaching == "Yes"] <- 1
required.coaching.cols$School.based.implementation.coaching[required.coaching.cols$School.based.implementation.coaching == "yes"] <- 1

# 0 -> ""
# 1 -> "Conference Call"
# 2 -> "In-person", "In-Person"
# 3 -> "In-Person & Virtual", "In-person & Virtual", "In-Person and Virtual", "In-person & Virtual", 
# 3 -> "In-Person & virtual", "In-person & virtual", "In-Person and virtual", "In-person & virtual", 
# 4 -> "phone", "Phone"
# 5 -> "Phone/conference call", "Phone/Conference Call"
# 6 -> "Virtual w/ Video", "Virtual w/video"

required.coaching.cols$Interaction.Type[required.coaching.cols$Interaction.Type == ""] <- "0"
required.coaching.cols$Interaction.Type[required.coaching.cols$Interaction.Type == "Conference Call"] <- "1"
for (interaction in c("In-person", "In-Person")) {
  required.coaching.cols$Interaction.Type[required.coaching.cols$Interaction.Type == interaction] <- "2"
}
for (interaction in c("In-person and Virtual", "In-Person & Virtual")) {
  required.coaching.cols$Interaction.Type[tolower(required.coaching.cols$Interaction.Type) == tolower(interaction)] <- "3"
}
for (interaction in c("phone", "Phone")) {
  required.coaching.cols$Interaction.Type[required.coaching.cols$Interaction.Type == interaction] <- "4"
}
for (interaction in c("Phone/conference call", "Phone/Conference Call")) {
  required.coaching.cols$Interaction.Type[required.coaching.cols$Interaction.Type == interaction] <- "5"
}
for (interaction in c("Phone/conference call", "Phone/Conference Call")) {
  required.coaching.cols$Interaction.Type[required.coaching.cols$Interaction.Type == interaction] <- "6"
}
for (interaction in c("Virtual w/ Video", "Virtual w/video")) {
  required.coaching.cols$Interaction.Type[required.coaching.cols$Interaction.Type == interaction] <- "7"
}

# 0 -> ""
# 0 -> "7h"
# 0 -> "8 hrs (2 days)"
# 0 -> "8 hours"
# 0 -> "8.00 hours"
# 0 -> "9.0 hours"
# 0 -> "one hour", "l hour", "i hour", 
# 0 -> "90 minutes"
# 0 -> "Full Day", "all day"

required.coaching.cols$Duration.of.Event[required.coaching.cols$Duration.of.Event == ""] <- 0
for (duration in c()) {
  required.coaching.cols$Interaction.Type[required.coaching.cols$Interaction.Type == duration] <- "1"
}



cwisdata <- read.csv("C:\\Users\\sh2742\\Documents\\py\\cwis.csv")
required.cwis.cols <- cwisdata[,c(
  "State.District.ID",
  "experience",
  "role",
  "ETL.AVERAGE",
  "prof_learning_leader_teacher_observation",
  "prof_learning_self_dev_instructional_practices",
  "prof_learning_self_receive_feedback"
)]

for (col in c("prof_learning_leader_teacher_observation", "prof_learning_self_dev_instructional_practices", "prof_learning_self_receive_feedback")) {
  the.mean <- mean(required.cwis.cols[[col]][!is.na(required.cwis.cols[[col]])])
  print(the.mean)
  required.cwis.cols[[col]][is.na(required.cwis.cols[[col]])] <- mean(required.cwis.cols[[col]][! is.na(required.cwis.cols[[col]])])
}

required.cwis.cols <- required.cwis.cols[complete.cases(required.cwis.cols[ , "ETL.AVERAGE"]),]
required.cwis.cols$experience[is.na(required.cwis.cols$experience)] <- 0

vals <- c(
  list(""),
  list("Building Administrator"),
  list("Classroom Teacher"),
  list("District Administrator"),
  list("Instructional Coach"),
  list("Literacy Coach"),
  list("Pre-k Educator", "Pre-K Educator"),
  list("School Counselor"),
  list("School Psychologist"),
  list("Special Educator"),
  list("Other")
)
i <- 1
for (val in vals) {
  for (vali in val) {
    required.cwis.cols$role[required.cwis.cols$role == val] <- i
  }
  i <- i + 1
}


cwis.state.id <- required.cwis.cols$State.District.ID
length(cwis.state.id)
coaching.state.id <- coachingdata$State.District.ID
length(coaching.state.id )

unique.cwis.state.id <- unique(cwis.state.id)
length(unique.cwis.state.id)
unique.coaching.state.id <- unique(coaching.state.id)
length(unique.coaching.state.id)

# write.csv(required.cwis.cols,"CSVs/preprocessed_cwis_cols", row.names = FALSE)
# write.csv(required.coaching.cols,"CSVs/preprocessed_coaching_cols", row.names = FALSE)

------------------------------------------------------------------------------------------
#   
# if(.Platform$OS.type == "windows") withAutoprint({
#   memory.size()
#   memory.size(TRUE)
#   memory.limit()
#   gc()
# })

cwis.coaching <- merge(required.cwis.cols, required.coaching.cols, by = "State.District.ID")

write.csv(cwis.coaching,"C:\\Users\\sh2742\\Documents\\py\\merged_cwis_coaching.csv", row.names = TRUE)

------------------------------------------------------------------------------------------------

cwis.coaching = cwis.coaching[,!(names(cwis.coaching) %in% c("State.District.ID", "Duration.of.Event"))]
random_sample <- createDataPartition(cwis.coaching$ETL.AVERAGE, p = 0.8, list = FALSE)

cwis.coaching <- data.frame(Interaction.Type = as.numeric(cwis.coaching$`Interaction.Type`),
                          Effective.teaching.learning.practices = as.integer(cwis.coaching$`Effective.teaching.learning.practices`),
                          School.based.implementation.coaching = as.numeric(cwis.coaching$`School.based.implementation.coaching`),
                          experience = as.numeric(cwis.coaching$experience),
                          role =as.numeric(cwis.coaching$role),
                          prof_learning_leader_teacher_observation = as.numeric(cwis.coaching$prof_learning_leader_teacher_observation),
                          prof_learning_self_dev_instructional_practices= as.numeric(cwis.coaching$prof_learning_self_dev_instructional_practices),
                          prof_learning_self_receive_feedback = as.numeric(cwis.coaching$prof_learning_self_receive_feedback),
                          ETL.AVERAGE = cwis.coaching$`ETL.AVERAGE`,
                          stringsAsFactors = FALSE)


input.cols <- c("Interaction.Type","Effective.teaching.learning.practices","School.based.implementation.coaching",
                "experience","role",
                "prof_learning_leader_teacher_observation", "prof_learning_self_receive_feedback")

cwis.coaching <- cwis.coaching[,input.cols]

training_dataset <- cwis.coaching[random_sample, ]
testing_dataset <- cwis.coaching[-random_sample, ]

x <- as.matrix(training_dataset[,!(names(training_dataset) %in% c("ETL.AVERAGE"))])
head(x)
y <- as.matrix(training_dataset[,c("ETL.AVERAGE")])
head(y)

library(lars)
fit <- lars(x,y,type="lasso")
fit$lambda
fit


pred.nox <- predict(fit, type="coef")
beta <- scale(pred.nox$coefficients, FALSE, 1/fit$normx)
arclength <- rowSums(abs(beta))
path.list <- list()
for(variable in colnames(beta)){
  standardized.coef <- beta[, variable]
  path.list[[variable]] <- data.table::data.table(
    step=seq_along(standardized.coef),
    lambda=c(fit$lambda, 0),
    variable,
    standardized.coef,
    fraction=pred.nox$fraction,
    arclength)
}


path <- do.call(rbind, path.list)
variable.colors <- c(
  "#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00", "#FFFF33", 
  "#A65628", "#F781BF", "#999999","#FFF00F")

library(animint2)
library("ggplot2")
gg.lambda <- ggplot()+
  theme_bw()+
  theme(panel.margin=grid::unit(0, "lines"))+
  scale_color_manual(values=variable.colors)+
  geom_line(aes(
    lambda, standardized.coef, color=variable, group=variable),
    data=path)+
  ggtitle("LASSO path which influence the rate of improving instruction using LARS")

gg.lambda

x.scaled <- with(fit, scale(x, meanx, normx))
lfit <- lm.fit(x.scaled, y)
library(data.table)
lpoints <- data.table::data.table(
  variable=colnames(x),
  standardized.coef=lfit$coefficients,
  arclength=sum(abs(lfit$coefficients)))

gg.lambda+
  geom_point(aes(
    0, standardized.coef, color=variable),
    data=lpoints)



fraction <- sort(unique(c(
  seq(0, 1, l=21))))
pred.fraction <- predict(
  fit, cwis.coaching,
  type="coef", mode="fraction", s=fraction)
coef.grid.list <- list()
coef.grid.mat <- scale(pred.fraction$coefficients, FALSE, 1/fit$normx)
for(fraction.i in seq_along(fraction)){
  standardized.coef <- coef.grid.mat[fraction.i,]
  coef.grid.list[[fraction.i]] <- data.table::data.table(
    fraction=fraction[[fraction.i]],
    variable=colnames(x),
    standardized.coef,
    arclength=sum(abs(standardized.coef)))
}
coef.grid <- do.call(rbind, coef.grid.list)
ggplot()+
  ggtitle("LASSO path for ETLP learning calculated using the LARS")+
  theme_bw()+
  theme(panel.margin=grid::unit(0, "lines"))+
  scale_color_manual(values=variable.colors)+
  geom_line(aes(
    arclength, standardized.coef, color=variable, group=variable),
    data=path)+
  geom_point(aes(
    arclength, standardized.coef, color=variable),
    data=lpoints)+
  geom_point(aes(
    arclength, standardized.coef, color=variable),
    shape=21,
    fill=NA,
    size=3,
    data=coef.grid)

#----------------------------------------------------------------------------------------

## Test Train Error Plot


pred.list <- predict(
  fit, cwis.coaching,
  mode="fraction", s=fraction)


residual.mat <- pred.list$fit - cwis.coaching$`ETL.AVERAGE`
squares.mat <- residual.mat * residual.mat
mean.error.list <- list()

train.n <- 3692250 #20439
train.set <- rep(TRUE, train.n)
train.set

test.n <- 923061 #5108
validation.set <- rep(FALSE, test.n)
validation.set


set = "train"
mse <- colMeans(squares.mat[train.set, ])
mean.error.list[[paste(set)]] <- data.table::data.table(
  set, mse, fraction,
  arclength=rowSums(abs(coef.grid.mat)))

set = "validation"
mse <- colMeans(squares.mat[validation.set, ])
mean.error.list[[paste(set)]] <- data.table::data.table(
  set, mse, fraction,
  arclength=rowSums(abs(coef.grid.mat)))

mean.error <- do.call(rbind, mean.error.list)
rect.width <- diff(mean.error$arclength[1:2])/2
addY <- function(dt, y){
  data.table::data.table(dt, y.var=factor(y, c("error", "weights")))
}
tallrect.dt <- coef.grid[variable==variable[1],]
gg.path <- ggplot()+
  theme_bw()+
  theme(panel.margin=grid::unit(0, "lines"))+
  facet_grid(y.var ~ ., scales="free")+
  ylab("")+
  scale_color_manual(values=variable.colors)+
  geom_line(aes(
    arclength, standardized.coef, color=variable, group=variable),
    data=addY(path, "weights"))+
  geom_line(aes(
    arclength, mse, linetype=set, group=set),
    data=addY(mean.error, "error"))+
  geom_tallrect(aes(
    xmin=arclength-rect.width,
    xmax=arclength+rect.width),
    clickSelects="arclength",
    alpha=0.5,
    data=tallrect.dt)
print(gg.path)