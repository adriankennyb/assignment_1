##IDS 572 - Assignment 1A
##Authors: Jinrong Qiu, Adrian Blamires, Mike Gannon
##Due date September 25, 2021

lcdf <- read.csv("~/Desktop/School/IDS 572/Assignment 1/lcData100K.csv")
library('tidyverse')
library('lubridate')
library('rpart')
library('dplyr')
library('knitr')
library('ggplot2')
library(pacman)
library(tidyr)
glimpse(lcdf)
summary(lcdf)

print("hello group")

##Question 2I
##Proportion of Defaults
lcdf %>% group_by(loan_status) %>% tally() %>% mutate(percent=n/sum(n)*100)
##Proportion of Defaults at grade level
lcdf %>% group_by(grade,loan_status) %>% tally() %>% mutate(percent=n/sum(n)*100)
##Proportion of Default/Fully Paid at grade/subgrade level
Q2i<- lcdf %>% group_by(grade,sub_grade,loan_status) %>% tally() %>% mutate(percent=n/sum(n)*100)
View(Q2i)
##Default rate increases as the grade level decreases (A to G). This relationship is consistent
##with sub grade too.  This makes sense since the grade is related to overall risk of the loan. Riskier loans are
##associated with higher rates of default

##Question 2II
##Number of Loans in each grade
lcdf %>% group_by(grade) %>% tally() %>% mutate(percent=n/sum(n)*100)
##Loan amounts (Total, avg, stdev, min, max) by loan grade
lcdf %>% group_by(grade) %>% summarize(TotalLoanAmt=sum(funded_amnt),AvgLoanAmt=mean(funded_amnt),stdevLoanAmt=sd(funded_amnt),MinLoanAmt=min(funded_amnt),MaxLoanAmt=max(funded_amnt))
##Loan amounts (Total, avg, stdev, min, max) by loan grade and sub grade
Q2ii_Amount<-lcdf %>% group_by(grade,sub_grade) %>% summarize(TotalLoanAmt=sum(funded_amnt),AvgLoanAmt=mean(funded_amnt),stdevLoanAmt=sd(funded_amnt),MinLoanAmt=min(funded_amnt),MaxLoanAmt=max(funded_amnt))
View(Q2ii_Amount)
##interest rates (avg, stdev, min,max) by loan grade
lcdf %>% group_by(grade) %>% summarize(Avginterestrate=mean(int_rate),stdevinterest=sd(int_rate),Mininterstrate=min(int_rate),Maxinterestrate=max(int_rate))
##interest rates (avg, stde, min, max) by loan grade and sub grade
Q2ii_Interestrate <-lcdf %>% group_by(grade, sub_grade) %>% summarize(Avginterestrate=mean(int_rate),stdevinterest=sd(int_rate),Mininterstrate=min(int_rate),Maxinterestrate=max(int_rate))
View(Q2ii_Interestrate)
##Generally the amount funded decreases as loan grade gets worse and interest rates increase as
##loan grades/sub-grades get worse. Stdev in interest rates and funded amount increases as the loan grades get worse
##This is consistent with what woudl be expected since higher risk loans need to have a higher potential return to the investor.  Therefore there would be more support for investors to 
##invest in less risky loans, and those that are risky shoudl have a higher interest rate.  

##Question2III
lcdf$annRet <- ((lcdf$total_pymnt -lcdf$funded_amnt)/lcdf$funded_amnt)*(12/36)*100
head(lcdf[, c("last_pymnt_d", "issue_d")])
lcdf$last_pymnt_d<-paste(sep = "",lcdf$last_pymnt_d, "-01")
head(lcdf[, c("last_pymnt_d", "issue_d")])
lcdf$last_pymnt_d<-parse_date_time(lcdf$last_pymnt_d,"myd")
head(lcdf[, c("last_pymnt_d", "issue_d")])
lcdf$actualTerm <- ifelse(lcdf$loan_status=="Fully Paid", as.duration(lcdf$issue_d  %--% lcdf$last_pymnt_d)/dyears(1), 3)
lcdf$actualReturn <- ifelse(lcdf$actualTerm>0, ((lcdf$total_pymnt -lcdf$funded_amnt)/lcdf$funded_amnt)*(1/lcdf$actualTerm)*100, 0)
lcdf %>% select(loan_status, int_rate, funded_amnt, total_pymnt, annRet, actualTerm, actualReturn) %>%  head()
boxplot(lcdf$actualTerm~lcdf$grade, data=lcdf, xlab("Loan Grade"), ylab("ActualTerm"))
lcdf%>%group_by(grade)%>%summarize(AvgTerm=mean(lcdf$actualTerm), MinTerm=min(lcdf$actualTerm), MaxTerm=max(lcdf$actualTerm))
summary(lcdf$actualTerm)

##Question2IV
lcdf %>% group_by(sub_grade, loan_status) %>% summarise(nLoans=n(), avgIntRate=mean(int_rate),  avgLoanAmt=mean(loan_amnt),  avgActRet = mean(actualReturn), avgActTerm=mean(actualTerm))
View(Q2Iv <-lcdf %>% group_by(sub_grade, loan_status) %>% summarise(nLoans=n(), avgIntRate=mean(int_rate),  avgLoanAmt=mean(loan_amnt),  avgActRet = mean(actualReturn), avgActTerm=mean(actualTerm)))

##Question2V
lcdf %>% group_by(purpose) %>% summarise(nLoans=n(), defaults=sum(loan_status=="Charged Off"), defaultRate=defaults/nLoans, avgLoanAmt=mean(loan_amnt))
table(lcdf$purpose, lcdf$grade)

##Question2VI
lcdf %>% group_by(emp_length) %>% summarise(nLoans=n(), defaults=sum(loan_status=="Charged Off"), defaultRate=defaults/nLoans, avgIntRate=mean(int_rate), avgLoanAmt=mean(loan_amnt), avgActRet=mean(actualReturn),avgActTerm=mean(actualTerm))
lcdf$emp_length <- factor(lcdf$emp_length, levels=c("n/a", "< 1 year","1 year","2 years", "3 years" ,  "4 years",   "5 years",   "6 years",   "7 years" ,  "8 years", "9 years", "10+ years" ))
lcdf %>% group_by(emp_length) %>% summarise(nLoans=n(), defaults=sum(loan_status=="Charged Off"), defaultRate=defaults/nLoans, avgIntRate=mean(int_rate), avgLoanAmt=mean(loan_amnt), avgActRet=mean(actualReturn),avgActTerm=mean(actualTerm))
lcdf %>% group_by(loan_status) %>% summarise(AnnualIncome=mean(annual_inc))

##Question2VII
#New Variable - DTI after loan origination
Monthly_Income <-lcdf$annual_inc/12
Monthly_Debt_Beforeloan <- Monthly_Income*lcdf$dti
lcdf$DTI_AfterLoan <- round(((Monthly_Debt_Beforeloan+lcdf$installment)/Monthly_Income),2)
Q2VIIA <- lcdf %>% select(c(DTI_AfterLoan,grade,loan_status))
Q2VIIA %>% group_by(grade,loan_status) %>% summarize(AvgDTI_AfterLoan=mean(DTI_AfterLoan),MedianDTI_AfterLoan=median(DTI_AfterLoan),stdev=sd(DTI_AfterLoan), Min=min(DTI_AfterLoan), Max=max(DTI_AfterLoan))
summary(lcdf$DTI_AfterLoan)
boxplot(lcdf$DTI_AfterLoan~lcdf$grade,lcdf,ylab=("DTI After Loan"),xlab = "Loan Grade")

#New Variable - Expected Interest as Percent of Annual Income
expected_interest <- lcdf$installment*36-lcdf$loan_amnt
lcdf$expint_perincome <-round(((expected_interest/lcdf$annual_inc)*100),2)
lcdf %>% group_by(grade,loan_status) %>% summarize(AVGexpint_perincome=mean(expint_perincome),Medianexpint_perincome=median(expint_perincome),stdev=sd(expint_perincome),Min=min(expint_perincome),Max=max(expint_perincome))
boxplot(lcdf$expint_perincome~lcdf$grade,lcdf,ylab = ("Expected Interest Per Income"),xlab = ("Loan Grade"))
View(filter(lcdf, lcdf$expint_perincome<0))
##New Variable - Percent of accounts still open
lcdf$per_accounts_open <-round((lcdf$open_acc/lcdf$total_acc)*100,2)
lcdf %>% group_by(grade,loan_status) %>% summarize(AVGPercentOpenAcc=mean(per_accounts_open),MedianPercentOpenAcc=median(per_accounts_open),stdev=sd(per_accounts_open),Min=min(per_accounts_open),Max=max(per_accounts_open))
boxplot(lcdf$per_accounts_open~lcdf$grade,lcdf,ylab = ("Percent of Accounts Open"), xlab = ("Loan Grade"))

##Question 2C - Missing Values
lcdf <- lcdf %>% select_if(function(x){!all(is.na(x))})
names(lcdf)[colSums(is.na(lcdf))>0]
colMeans(is.na(lcdf))
colMeans(is.na(lcdf))[colMeans(is.na(lcdf))>0]
nm<-names(lcdf)[colMeans(is.na(lcdf))>0.6]
lcdf <- lcdf %>% select(-nm)
colMeans(is.na(lcdf))[colMeans(is.na(lcdf))>0]
nm<- names(lcdf)[colSums(is.na(lcdf))>0]
summary(lcdf[, nm])
lcx<-lcdf[, c(nm)]
colMeans(is.na(lcx))[colMeans(is.na(lcx))>0]
lcx<- lcx %>% replace_na(list(mths_since_last_delinq = 500))
#For revol_util, suppose we want to replace the misisng values by the median
lcx<- lcx %>% replace_na(list(revol_util=median(lcx$revol_util, na.rm=TRUE)))
lcx$revol_util
summary(lcx[, nm])
lcdf<- lcdf %>% replace_na(list(mths_since_last_delinq=500, revol_util=median(lcdf$revol_util, na.rm=TRUE), bc_open_to_buy=median(lcdf$bc_open_to_buy, na.rm=TRUE), mo_sin_old_il_acct=1000, mths_since_recent_bc=1000, mths_since_recent_inq=50, num_tl_120dpd_2m = median(lcdf$num_tl_120dpd_2m, na.rm=TRUE),percent_bc_gt_75 = median(lcdf$percent_bc_gt_75, na.rm=TRUE), bc_util=median(lcdf$bc_util, na.rm=TRUE) ))

##Question 3 - Removing Leakage Variables
lcdf2 <- lcdf %>% select(-c("loan_amnt",delinq_2yrs,inq_last_6mths,revol_bal,revol_util,total_rec_late_fee,recoveries,collection_recovery_fee,collections_12_mths_ex_med,acc_now_delinq,tot_cur_bal,tot_coll_amt,acc_open_past_24mths,avg_cur_bal,bc_open_to_buy,chargeoff_within_12_mths,delinq_amnt,mo_sin_rcnt_rev_tl_op,mo_sin_rcnt_tl,mths_since_recent_bc,mths_since_recent_inq,num_actv_bc_tl,num_actv_rev_tl,num_tl_120dpd_2m,num_tl_30dpd,num_tl_90g_dpd_24m,num_tl_op_past_12m,pct_tl_nvr_dlq))
View(lcdf2)
##Question 4 - univariate analysis
library(pROC)
auc(response=lcdf2$loan_status, lcdf2$loan_amnt)
auc(response=lcdf2$loan_status, as.numeric(lcdf2$emp_length))
aucsNum<-sapply(lcdf2 %>% select_if(is.numeric), auc, response=lcdf2$loan_status)
auc(response=lcdf2$loan_status, as.numeric(lcdf2$emp_length))
aucAll<- sapply(lcdf2 %>% mutate_if(is.factor, as.numeric) %>% select_if(is.numeric), auc, response=lcdf2$loan_status)
library(broom)

tidy(aucAll[aucAll > 0.54]) %>% View() #TO determine which variables have auc > 0.54
tidy(aucAll[aucAll >=0.55 & aucAll < 0.59]) %>% View() #TO determine which variables have auc between 0.54 and 0.59

print("can i pull")
