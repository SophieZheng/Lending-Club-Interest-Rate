dir = "/Users/SophieZheng/Google Drive/Stats Project"
setwd(dir)

#before you run the code, please run 1. install.packages("sampling"); 2. library("sampling", lib.loc="/Library/Frameworks/R.framework/Versions/3.3/Resources/library")

library(readxl)

loan <- read_excel("loan_final.xlsx",col_names = TRUE)
loan1 <- read_excel("LoanStats3b.xlsx",col_names = TRUE)
loan=na.omit(loan)
loan <- loan[!sapply(loan,function(var) (length(unique(var)) ==1) )]
clust_attr = loan[,c("emp_title","annual_inc")]
clust_attr_scaled=scale(clust_attr[,1])
k.means.fit <- kmeans(clust_attr_scaled,5)
loan$emp_title_cluster = k.means.fit$cluster
attach(loan)
#loan_10000=loan
#loandata=loan
#loan = read.csv("LoanStats3d.csv",header = TRUE)

#...randomly sample the raw data. Note: our sample size is 10,000.
#...using Stratified sampling strategy
#...Proportionate allocation

#0. handling the missing values on column "issue_d", by deleting them, for the purpose to use strata function below
naomitted_loan = loan #loan[!is.na(loan$issue_d),]
#nrow(naomitted_loan) =>  data size now becomes 52899

#1. get the proportion for each month in both 2012 and 2013; using 10001.5 in order to get total of proportionated size to be 10000
proportionated_strata_sample_size = round(c(table(naomitted_loan["issue_d"])/nrow(naomitted_loan["issue_d"])) * 10000)

#2. random sampling from each strata
#...Note: using strata function provided by sampling pakcage. 
sorted_loan = naomitted_loan[order(naomitted_loan$issue_d),]
stratas = strata(sorted_loan,stratanames=c("issue_d"),size=proportionated_strata_sample_size, method="srswor")
loan_10000 = getdata(sorted_loan, stratas)

#loan_10000 = loan[sample(nrow(loan),size=10000,replace = F),]

detach(loan)
attach(loan_10000)

library(xlsx)
write.csv( loan, file = "loan_final.csv")

#check if home_ownership has a relationship with int_rate
summary(aov(int_rate ~ home_ownership))
summary(aov(int_rate ~ purpose))

summary(m)


hist(int_rate)
loan_1000$purposeFF = factor(loan_1000$purpose)
attach(loan_1000)

loan_10000$tot_cur_bal[is.na(loan_10000$tot_cur_bal)] = mean(loan_10000$tot_cur_bal,na.rm=TRUE)
loan_10000$tot_hi_cred_lim[is.na(loan_10000$tot_hi_cred_lim)] = mean(loan_10000$tot_hi_cred_lim,na.rm=TRUE)
loan_10000$score = (loan_10000$tot_cur_bal+loan_10000$tot_hi_cred_lim+loan_10000$total_acc)
loan_10000$verification_statusF = factor(loan_10000$verification_status)
loan_10000$termF = factor(loan_10000$term)


attach(loan)
loan$emp_title_clusterF = factor(loan$emp_title_cluster)
loan = within(loan, emp_title_clusterF<-relevel(emp_title_clusterF,ref=8))
loan$purposeF = factor(loan$purpose)
loan = within(loan, purposeF<-relevel(purposeF,ref="credit_card"))
loan$percent_bc_gt_75_2 = loan$percent_bc_gt_75 / 100
model <- lm(formula= log(int_rate) ~  log(loan_amnt) + revol_util + dti + log(tot_hi_cred_lim) +  log(total_bc_limit) + term +percent_bc_gt_75_2+purposeF, data = loan)

summary(model)
