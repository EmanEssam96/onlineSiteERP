setwd("E:/FCi/second term/ERP/processingData")
# 1- read the file "workflow1.txt" as a table(padat table)
dataTable <- read.table("WorkFlow1.txt", sep="|", header=TRUE)

#########################
# 2- factorize variables highlighted in workflow1 
#########################
#the most important uses of factors is in statistical modeling;
# since categorical variables enter into statistical models differently than continuous variables, 
#storing data as factors insures that the modeling functions will treat such data correctly.
# and to visualize data
loanTypefctr=as.factor(dataTable$loan_type)
loanPurposefctr=as.factor(dataTable$loan_purpose)
actionTypefctr=as.factor(dataTable$action_type)
applicantSexfctr=as.factor(dataTable$applicant_sex)
applicantRacefctr=as.factor(dataTable$applicant_race_1)
apllicantetintfctr = as.factor(dataTable$applicant_ethnicity)
lienStatusfctr = as.factor(dataTable$lien_status)
preapproval = as.factor(dataTable$preapproval)
county = as.factor(dataTable$county_name)
plot(county)
plot(loanTypefctr)
plot(loanPurposefctr)
plot(actionTypefctr)
plot(applicantSexfctr)
plot(applicantRacefctr)
plot(apllicantetintfctr)
plot(lienStatusfctr)
plot(preapproval)

######################
# 3-How many people were accepted in the Adams county
######################
cat("number of people were accepted in the Adams county.= ", nrow(dataTable[dataTable$county_name == "Adams", ]))

######################
#4- we inserted the income and other variables as VARCHAR , where it should be numeric, convert the needed
#variables to numeric. Make sure that they are numeric using a function.
#######################

dataTable$applicant_income_ink = as.numeric(as.character(dataTable$applicant_income_ink)) 
class(dataTable$applicant_income_ink)

dataTable$rate_spread = as.numeric(as.character(dataTable$rate_spread)) 
class(dataTable$rate_spread)

dataTable$minority_population_pct = as.numeric(as.character(dataTable$minority_population_pct)) 
class(dataTable$minority_population_pct)

dataTable$hud_median_family_income = as.numeric(as.character(dataTable$hud_median_family_income))
class(dataTable$hud_median_family_income )

dataTable$tract_to_msamd_income_pct = as.numeric(as.character(dataTable$tract_to_msamd_income_pct))
class(dataTable$tract_to_msamd_income_pct)

dataTable$number_of_owner_occupied_units = as.numeric(as.character(dataTable$number_of_owner_occupied_units))
class(dataTable$number_of_owner_occupied_units)


###################
#5-Remove records without income info.
###################
dataTable=subset.data.frame(dataTable,!is.na(as.numeric(as.character(dataTable$applicant_income_ink)))==TRUE )
dataTable=subset.data.frame(dataTable,!is.na(as.numeric(as.character(dataTable$hud_median_family_income)))==TRUE)

###################
#6- Remove rows with nulls (NA) in Tract_To_MSAMD_Income_pct, Minority_Population_pct,Tract_To_MSAMD_Income_pct.
###################
dataTable=subset.data.frame(dataTable,!is.na(as.numeric(as.character(dataTable$tract_to_msamd_income_pct)))==TRUE)
dataTable=subset.data.frame(dataTable,!is.na(as.numeric(as.character(dataTable$minority_population_pct)))==TRUE)

##############################################
#Analysis
##############################################

####################
#1-visualize 5 variables
####################
plot(as.factor(dataTable$applicant_income_ink))  # applicant with low income are more
plot(as.factor(dataTable$loan_amount_ink)) 
plot(as.factor(dataTable$hud_median_family_income))
plot(as.factor(dataTable$number_of_owner_occupied_units)) 
plot(as.factor(dataTable$tract_to_msamd_income_pct))
plot(as.factor(dataTable$applicant_sex)) #male is more

####################
#2- relation between  the applicant income and the loan amount
####################
with(dataTable, cor(log(as.numeric(applicant_income_ink)),loan_amount_ink))
#there is a correlation between applicant income and the loan amount

#####################
#3- Check whether the loan amount has any odd, multi-modal 
#distribution. This may suggest to us that we might want to build 
#separate models for the different loan purposes.
#####################

#Two ways
#1st: remove outliers then plot
summary(dataTable$loan_amount_ink)
ds <- subset(dataTable, dataTable$loan_amount_ink < 500)
library(ggplot2)
ggplot(data=ds) + geom_density(aes(x=loan_amount_ink))

ggplot(data=ds) + geom_density(aes(x=loan_amount_ink, 
                                   colour=as.factor(loan_purpose)))

#2nd: plot log
ggplot(data=dataTable) + geom_density(aes(x=loan_amount_ink)) + scale_x_log10()

ggplot(data=ds) + geom_density(aes(x=loan_amount_ink, 
                                   colour=as.factor(loan_purpose))) + scale_x_log10()

#comment: yes, it's an odd dist. It has multimodal dist.
# Separating model for for different purposes is a better idea

#####################
#4- What is the loan purpose type that we need drop to make the experiment cleaner?
#dropping home improvement is 
#####################

#test using log
without_home_improvement <- subset(dataTable, dataTable$loan_purpose  != "Home improvement")
ggplot(data=without_home_improvement) + geom_density(aes(x=loan_amount_ink)) + scale_x_log10()

#test by removing outliers
without_improvement_outliers <- subset(without_home_improvement, without_home_improvement$loan_amount_ink < 500)
ggplot(data=without_improvement_outliers) + geom_density(aes(x=loan_amount_ink))

#comment: Removing home improvement make the dist more normal 
#with less spikes and as a result makes the experiment cleaner

