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


install.packages("data.table")

library(data.table)

cwisdata <- read.csv("C:\\Users\\sh2742\\Documents\\py\\cwis.csv")

required.cwis.cols <- cwisdata[-c(1,2,5,6,8,12,13,14,15,21,22,23,24,25,26,27)]
str(cwisdata) #80267 obs. of  106 variables:
required.cwis.cols$experience[is.na(required.cwis.cols$experience)] <- 0
required.cwis.cols$member_grade_span_level[required.cwis.cols$member_grade_span_level == "TRUE" 
                                           | required.cwis.cols$member_grade_span_level == "Yes"
                                           | required.cwis.cols$member_grade_span_level == "t" 
                                           | required.cwis.cols$member_grade_span_level == "1"] <- 1
required.cwis.cols$member_grade_span_level[required.cwis.cols$member_grade_span_level == "FALSE" 
                                           | required.cwis.cols$member_grade_span_level == "No"
                                           | required.cwis.cols$member_grade_span_level == "f" 
                                           | required.cwis.cols$member_grade_span_level == "0"
                                           | required.cwis.cols$member_grade_span_level == "TK"
                                           | required.cwis.cols$member_grade_span_level == ""] <- 0
required.cwis.cols$admin_receive_coaching[is.na(required.cwis.cols$admin_receive_coaching)] <- -1
required.cwis.cols$admin_receive_coaching[required.cwis.cols$admin_receive_coaching == "TRUE"] <- 1
required.cwis.cols$admin_receive_coaching[required.cwis.cols$admin_receive_coaching == "FALSE"] <- 0
required.cwis.cols$district_accept_questions[is.na(required.cwis.cols$district_accept_questions)] <- 0
required.cwis.cols$district_accept_questions[required.cwis.cols$district_accept_questions == "TRUE"] <- 1
required.cwis.cols$ETL.AVERAGE[is.na(required.cwis.cols$ETL.AVERAGE)] <- 0
required.cwis.cols$CFA.AVERAGE[is.na(required.cwis.cols$CFA.AVERAGE)] <- 0
required.cwis.cols$DBDM.AVERAGE[is.na(required.cwis.cols$DBDM.AVERAGE)] <- 0
required.cwis.cols$LEAD.AVERAGE[is.na(required.cwis.cols$LEAD.AVERAGEE)] <- 0
required.cwis.cols$PD.AVERAGE[is.na(required.cwis.cols$PD.AVERAGE)] <- 0
for(j in 12:ncol(required.cwis.cols)){
  required.cwis.cols[,j][is.na(required.cwis.cols[,j])] <- 0
  required.cwis.cols[,j][required.cwis.cols[,j] == "TRUE"] <- 1 
  required.cwis.cols[,j][required.cwis.cols[,j] == "FALSE"] <- 0
  
}

cwis.dt <- as.data.table(required.cwis.cols)

aggregate.cwis <- cwis.dt[, .(
  experience=mean(experience), admin_receive_coaching=mean(admin_receive_coaching), district_accept_questions=mean(district_accept_questions), ETL.AVERAGE=mean(ETL.AVERAGE), CFA.AVERAGE=mean(CFA.AVERAGE), DBDM.AVERAGE=mean(DBDM.AVERAGE), 
  PD.AVERAGE=mean(PD.AVERAGE), common_practices_addresses_standard=mean(common_practices_addresses_standard), 
  common_practices_teacher_use_cfa=mean(common_practices_teacher_use_cfa), common_practices_student_use_cfa=mean(common_practices_student_use_cfa), common_practices_cfa_reteaching=mean(common_practices_cfa_reteaching),
  collab_teams_reviews_data =mean(collab_teams_reviews_data), collab_teams_positive_interaction= mean(collab_teams_positive_interaction), collab_teams_effective_teaming=mean(collab_teams_effective_teaming),
  collab_teams_data_collaboration=mean(collab_teams_data_collaboration), collab_teams_analyze_during_meeting_now=mean(collab_teams_analyze_during_meeting_now), collab_teams_use_data_analysis_system_now=mean(collab_teams_use_data_analysis_system_now),
  collab_teams_learning_analyzed_p1 = mean(collab_teams_learning_analyzed_p1), collab_teams_systematically_analyze_p2= mean(collab_teams_systematically_analyze_p2),collab_teams_modify_instruction_p3=mean(collab_teams_modify_instruction_p3),
  collab_teams_reflecting_instruction_p4=mean(collab_teams_reflecting_instruction_p4),  collab_teams_review_learning_targets_p5=mean(collab_teams_review_learning_targets_p5), 
  prof_learning_self_receive_feedback = mean(prof_learning_self_receive_feedback), admin_clarified_purpose=mean(admin_clarified_purpose), admin_conv_gone_well=mean(admin_conv_gone_well),
  admin_conv_relevant_data=mean(admin_conv_relevant_data), admin_add_suggestions=mean(admin_add_suggestions), admin_provide_rationales=mean(admin_provide_rationales), admin_provide_opportunity=mean(admin_provide_opportunity), 
  prof_learning_leader_manage_expectations =mean(prof_learning_leader_manage_expectations), prof_learning_leader_teacher_observation = mean(prof_learning_leader_teacher_observation), prof_learning_leader_committed_instruction=mean(prof_learning_leader_committed_instruction), prof_learning_leader_collab_teams=mean(prof_learning_leader_collab_teams), prof_learning_self_dev_instructional_practices=mean(prof_learning_self_dev_instructional_practices), prof_learning_self_receive_coaching=mean(prof_learning_self_receive_coaching), prof_learning_self_dev_monitor_student= mean(prof_learning_self_dev_monitor_student),  collab_teams_positive_interaction=mean(collab_teams_positive_interaction), 
  
  collab_teams_effective_teaming=mean(collab_teams_effective_teaming)
), by="State.District.ID"]

aggregate.cwis[, admin_receive_coaching:=NULL]

#prof_learning_self_receive_feedback
#aggregate.cwis <- cwis.dt[,.(mean(cwis.dt[,1:80])), by="State.District.ID"]
  
# tesit <-cwis.dt 
# nts <- cwis.dt
# str(cwis.dt) #80267 obs. of  98 variables
# cwis.df <- required.cwis.cols
# colnames(cwis.dt)
# message(sprintf("Uncompiled functions : %s\n",paste(colnames(cwis.dt), collapse=", ")))
# 
# 
# cwis_mean_cols <- c("experience","member_grade_span_level","admin_receive_coaching","district_accept_questions","ETL.AVERAGE","CFA.AVERAGE","DBDM.AVERAGE","LEAD.AVERAGE","PD.AVERAGE","common_practices_addresses_standard","common_practices_teacher_use_cfa",
#                     "common_practices_student_use_cfa","common_practices_cfa_reteaching","collab_teams_reviews_data",
#                     "collab_teams_positive_interaction","collab_teams_effective_teaming","collab_teams_data_collaboration",
#                     "collab_teams_analyze_during_meeting_now","collab_teams_use_data_analysis_system_now",
#                     "collab_teams_learning_analyzed_p1","collab_teams_systematically_analyze_p2",
#                     "collab_teams_modify_instruction_p3","collab_teams_reflecting_instruction_p4","collab_teams_review_learning_targets_p5","prof_learning_leader_manage_expectations","prof_learning_leader_teacher_observation","prof_learning_leader_committed_instruction","prof_learning_leader_collab_teams","prof_learning_self_dev_instructional_practices","prof_learning_self_receive_coaching","prof_learning_self_dev_monitor_student","prof_learning_self_receive_feedback","admin_clarified_purpose","admin_conv_gone_well","admin_conv_relevant_data","admin_add_suggestions","admin_provide_rationales","admin_provide_opportunity","admin_supported_suggestions","admin_guided_practice","admin_identify_next_steps","admin_paced_conversation","district_identified_strategies","district_deploy_central_office","district_deploy_principals","district_use_aligned_teams","district_using_technology","district_integrate_technology","district_utilize_virtual_learning","district_monitor_focused_improvement","collab_teams_analyze_during_meeting_historical","collab_teams_use_data_analysis_system_historical","employed_last_year","admin_expected_meet_during_covid","admin_collab_teams_reviews_data_now","admin_collab_teams_reviews_data_pre_covid","admin_collab_teams_positive_interaction_now","admin_collab_teams_positive_interaction_pre_covid","admin_collab_teams_effective_teaming_now","admin_collab_teams_effective_teaming_pre_covid","admin_collab_teams_analyze_during_meeting_now","admin_collab_teams_analyze_during_meeting_pre_covid","admin_collab_teams_use_data_analysis_system_now","admin_collab_teams_use_data_analysis_system_pre_covid","admin_prof_learning_self_dev_instructional_practices_now","admin_prof_learning_self_dev_instructional_practices_pre_covid","admin_prof_learning_self_receive_coaching_now","admin_prof_learning_self_receive_coaching_pre_covid","admin_prof_learning_self_dev_monitor_student_now","admin_prof_learning_self_dev_monitor_student_pre_covid","admin_prof_learning_self_receive_feedback_now","admin_prof_learning_self_receive_feedback_pre_covid","admin_common_practices_can_statements_now","admin_common_practices_can_statements_pre_covid","admin_common_practices_student_work_now","admin_common_practices_student_work_pre_covid","admin_common_practices_self_assessment_now","admin_common_practices_self_assessment_pre_covid","admin_common_practices_receive_feedback_now","admin_common_practices_receive_feedback_pre_covid","admin_common_practices_student_feedback_now","admin_common_practices_student_feedback_pre_covid","admin_common_practices_state_criteria_now","admin_common_practices_state_criteria_pre_covid",
#                     "admin_common_practices_student_review_cfa_now","admin_common_practices_student_review_cfa_pre_covid")

# dtnew <- dt[, lapply(.SD, as.character), by=ID]
# str(dtnew)
# 
# tesit[, convert_to_numeric] <- tesit[, lapply(.SD, as.numeric), .SDcols = convert_to_numeric]
# tesit[, convert_to_numeric] <- tesit[, lapply(.SD, mean), .SDcols = convert_to_numeric]


cwis.dt[, cwis_mean_cols] <- cwis.dt[, lapply(.SD, as.numeric), .SDcols = cwis_mean_cols]
cwis.dt[, cwis_mean_cols] <- cwis.dt[, lapply(.SD, mean), .SDcols = cwis_mean_cols]
#cwis.aggregate <- unique(cwis.dt, by = "State.District.ID")

#write.csv(cwis.aggregate[-c(2,3,4)],"/Users/akhilachowdarykolla/Documents/Coding/development/PredictiveModelling/cwis_aggregate_districts.csv", row.names = FALSE)

#cwis.aggregate <- cwis.aggregate[-c(2,3,4)] 


# cwis.districts.aggregates <- tesit[, lapply(.SD, mean), by=State.District.ID]
#cwis.aggregates <- unique(tesit, by = "State.District.ID")


  
  
#nces.cwis.dt <-nces.dt[cwis.dt,on=.(State.School.ID ),nomatch = NULL]
#nces+cwis+coaching
#cwis.aggregrate.districts <- districts.aggregate.dt[cwis.aggregate,on=.(State.District.ID ),nomatch = NULL]
#write.csv(cwis.aggregrate.districts,"/Users/akhilachowdarykolla/Documents/Coding/development/PredictiveModelling/coaching_cwis_aggregrate_districts.csv", row.names = FALSE)

 
#   
# cwis.aggregrate.districts[, admin_receive_coaching:=NULL] #prapti
# 
# cwis.dt[, cwis_required_cols] <- cwis.dt[, lapply(.SD, as.numeric), .SDcols = cwis_required_cols]
# 
# cwis.dt[, cwis_required_cols] <- cwis.dt[, lapply(.SD, mean), .SDcols = cwis_required_cols]




#====================================================================================================================

#coaching logs aggregate

coachingdata <- read.csv("C:\\Users\\sh2742\\Documents\\py\\coaching.csv")#,header = TRUE,check.names=TRUE )
required.coaching.cols <- coachingdata[,c(4,5,6,8,9,21,23,25,27,29,31,33,41,42,43,44,46)]

required.coaching.cols$Year <- as.numeric(paste0(sapply(strsplit(as.character(
  required.coaching.cols$Date.of.Event.Visit),'/'), "[", 3)))
required.coaching.cols$Month <- as.numeric(sapply(strsplit(as.character(required.coaching.cols$Date.of.Event.Visit),'/'), "[", 1))


required.coaching.cols$Day <- as.numeric(sapply(strsplit(as.character(required.coaching.cols$Date.of.Event.Visit),'/'), "[", 2))


setcolorder(required.coaching.cols, c("Date.of.Event.Visit", "Year", "Month","Day"))


required.coaching.cols$Duration.of.Event[required.coaching.cols$Duration.of.Event == ""] <- 0
required.coaching.cols$Interaction.Type[required.coaching.cols$Interaction.Type == ""] <- "None"


required.coaching.cols$Collaborative.teams[required.coaching.cols$Collaborative.teams == "Yes" |
                                             required.coaching.cols$Collaborative.teams == "yes" ] <- 1

required.coaching.cols$Collaborative.teams[required.coaching.cols$Collaborative.teams == "No" |
                                             required.coaching.cols$Collaborative.teams == "" ] <- 0

for(j in 9:ncol(required.coaching.cols)){
  required.coaching.cols[,j][is.na(required.coaching.cols[,j])] <- 0
  required.coaching.cols[,j][required.coaching.cols[,j] == "Yes" |
                               required.coaching.cols[,j] == "yes"  ] <- 1 
  required.coaching.cols[,j][required.coaching.cols[,j] == "No" |
                               required.coaching.cols[,j] == ""] <- 0
  
}

coaching.dt <-as.data.table(required.coaching.cols)

colnames(coaching.dt)[8] <- "Districts"

coaching.dt$Districts <- tolower(coaching.dt$Districts) #5924 obs. of  16 variables

# length(unique(coaching.dt$State.District.ID))

aggregate.dt.list <- list()
for(i in 1:nrow(coaching.dt)){
  row <- coaching.dt[i,]
  col_val <- paste0(row$State.District.ID,"_",row$Districts)
  aggregate.dt.list[[col_val]] <- data.table(
    Year = row$Year,
    Month = row$Month,
    Day = row$Day,
    Duration.of.Event = row$Duration.of.Event,
    Interaction.Type = row$Interaction.Type,
    State.District.ID = 0 , 
    Districts = 0,
    #Duration.of.Event = 0,
    Collaborative.teams = 0,
    Common.formative.assessment = 0,
    Data.based.decision.making = 0,
    Effective.teaching.learning.practices = 0,
    Instructional.Leadership = 0,
    School.based.implementation.coaching = 0, Collective.teacher.efficacy = 0,
    Practice.profiles = 0,Self.assessment.practice.profile...SAPP. = 0,
    Learning.module.materials..i.e..power.points..handouts. = 0,
    DESE.virtual.learning.platform = 0,
    CWIS = 0
  )
}
# 
# Year = row$Year,
# Month = row$Month,
# Day = row$Day,
# Duration.of.Event = row$Duration.of.Event,
# Interaction.Type = row$Interaction.Type,

for(i in 1:nrow(coaching.dt)){
  row <- coaching.dt[i,]
  col_val <- paste0(row$State.District.ID,"_",row$Districts)
  #print(col_val)
  #print(i)
  aggregate.dt.list[[col_val]] <- data.table(
    State.District.ID = row$State.District.ID , 
    Districts = row$Districts,
    Collaborative.teams = as.numeric(aggregate.dt.list[[col_val]]$Collaborative.teams | as.numeric(row$Collaborative.teams)),
    Common.formative.assessment = as.numeric(aggregate.dt.list[[col_val]]$Common.formative.assessment | as.numeric(row$Common.formative.assessment)),
    Data.based.decision.making = as.numeric(aggregate.dt.list[[col_val]]$Data.based.decision.making | as.numeric(row$Data.based.decision.making)),
    Effective.teaching.learning.practices = as.numeric(aggregate.dt.list[[col_val]]$Effective.teaching.learning.practices | as.numeric(row$Effective.teaching.learning.practices)),
    Instructional.Leadership = as.numeric(aggregate.dt.list[[col_val]]$Instructional.Leadership | as.numeric(row$Instructional.Leadership)),
    School.based.implementation.coaching =as.numeric(aggregate.dt.list[[col_val]]$School.based.implementation.coaching | as.numeric(row$School.based.implementation.coaching)),
    Collective.teacher.efficacy = as.numeric(aggregate.dt.list[[col_val]]$Collective.teacher.efficacy | as.numeric(row$Collective.teacher.efficacy)),
    Practice.profiles =as.numeric(aggregate.dt.list[[col_val]]$Collective.teacher.efficacy | as.numeric(row$Collective.teacher.efficacy)),
    Self.assessment.practice.profile...SAPP. = as.numeric(aggregate.dt.list[[col_val]]$Self.assessment.practice.profile...SAPP. | as.numeric(row$Self.assessment.practice.profile...SAPP.)),
    Learning.module.materials..i.e..power.points..handouts. = as.numeric(aggregate.dt.list[[col_val]]$Learning.module.materials..i.e..power.points..handouts. | as.numeric(row$Learning.module.materials..i.e..power.points..handouts.)),
    DESE.virtual.learning.platform = as.numeric(aggregate.dt.list[[col_val]]$DESE.virtual.learning.platform | as.numeric(row$DESE.virtual.learning.platform)),
    CWIS =as.numeric(aggregate.dt.list[[col_val]]$CWIS | as.numeric(row$CWIS))
    # Duration.of.Event = aggregate.dt.list[[col_val]]$Duration.of.Event + row$Date.of.Event.Visit,
  )
  
}

districts.aggregate.dt <- do.call(rbind, aggregate.dt.list)

#================================================================================================================

#cwis.coaching = merge(districts.aggregate.dt, aggregate.cwis, by="State.District.ID", allow.cartesian=TRUE)

cwis.aggregrate.coaching <- districts.aggregate.dt[aggregate.cwis,on=.(State.District.ID ),nomatch = NULL]


# nces.cwis.dt <-nces.dt[cwis.dt,on=.(State.School.ID ),nomatch = NULL]
# #nces+cwis+coaching
# cwis.aggregrate.districts <- districts.aggregate.dt[cwis.aggregate,on=.(State.District.ID ),nomatch = NULL]
# write.csv(cwis.aggregrate.districts,"/Users/akhilachowdarykolla/Documents/Coding/development/PredictiveModelling/coaching_cwis_aggregrate_districts.csv", row.names = FALSE)
# 
# 
# 
# 
# cwis.dt[, cwis_required_cols] <- cwis.dt[, lapply(.SD, as.numeric), .SDcols = cwis_required_cols]
# 
# cwis.dt[, cwis_required_cols] <- cwis.dt[, lapply(.SD, mean), .SDcols = cwis_required_cols]


#=================================================================================================================

nces <- read.csv("C:\\Users\\sh2742\\Documents\\py\\nces.csv")

nces <- unique(nces, by = "State.District.ID")

required.nces.cols <- nces[,c(4,5,6,18,19,20,21,23,25,27)]

required.nces.cols$Charter[required.nces.cols$Charter == "TRUE" 
                                           | required.nces.cols$Charter == "Yes"
                                           | required.nces.cols$Charter == "t" 
                                           | required.nces.cols$Charter == "1"] <- 1
required.nces.cols$Charter[required.nces.cols$Charter == "FALSE" 
                                           | required.nces.cols$Charter == "No"
                                           | required.nces.cols$Charter == "f" 
                                           | required.nces.cols$Charter == "0"
                                           | required.nces.cols$Charter == "TK"
                                           | required.nces.cols$Charter == ""] <- 0

required.nces.cols$Charter[is.na(required.nces.cols$Charter)] <- 0
required.nces.cols$Magnet.[is.na(required.nces.cols$Magnet.)] <- 0

required.nces.cols$Charter[required.nces.cols$Charter == "â???"] <- 0 

nces.dt <-as.data.table(required.nces.cols)

total.dt <-nces.dt[cwis.aggregrate.coaching,on=.(State.District.ID ),nomatch = NULL]

#cwis.nces <- aggregate.cwis[nces,on=.(State.District.ID ),nomatch = NULL]
