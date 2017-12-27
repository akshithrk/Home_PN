getwd()
setwd("P:/R/Home_PN/Home_PN")
source("P:/R/Home_PN/Home_PN/ak_helperfunctions.r")
source("P:/R/Home_PN/Home_PN/ak_bmi_z.r")

# date <- Sys.Date()
# format(as.date(date), '%m/%d/%y')
# mdy.date(date)

# date.mdy(date)
# as.date(date)
# replacing the below to use format(Sys.Date(), "%d%b%Y") but error when converting to integer so leaving as is for now as data output not specific to dates
# error because when as.integet is used the output of the sys.date starts with quotes which is being read as a char

# today <- as.integer(mdy.date(3,1,2015))
today <- as.integer(mdy.date(12,31,2017))



# today <- as.integer(format(Sys.Date(), "%d%b%Y"))
# today
# mdy.date(11,14,2017)
#####
# Initial data import from REDCap file.
#####
#below variables were for old redcap hence replacing them with mrn as new design allows for only one mrn across all instruments


# test <- read.csv(fname)
# test
# write.csv(test, paste0("test_hpn_redcap_api_data_new_wo_na.strings",format(Sys.time(), "%Y %b %d %H.%M"),".csv"))
# test_with_na_strings <- read.csv(fname,na.strings=c("1/1/1901"))[,-1]
# write.csv(test_with_na_strings, paste0("test_hpn_redcap_api_data_new_with_na.strings",format(Sys.time(), "%Y %b %d %H.%M"),".csv"))

# replacing fname with individual instrument files instead of the raw.api file
# fname="hpn_redcap_api_alldata.csv"
# alldata="hpn_redcap_api_alldata.csv"
# alldata <- read.csv(alldata)

# below function reading redcap csv files & reading mrn from it so modifyiung it to read the instrument files and point to record
# all.dat$mrn

# readdata <- function(fname="hpn_redcap_api_data_new.csv")
# rm(all.dat)
readdata <- function(fname="NEWHomePNDatabaseALL_DATA_2017-12-19_1603.csv")
{
  read.csv
  # as tested above, the na.strings replaces the specified value within c as na or null and so removing it from this script as it seems to be removing values

  all.dat <- read.csv(fname)[,-1]
  all.dat <- read.csv(fname)
  # all.dat <- read.csv(fname,na.strings=c("1/1/1901"))[,-1]
  # all.dat$mrn <- as.integer(substr(all.dat$mrn,1,regexpr(",",all.dat$mrn)-1))
  # all.dat$active_mrn <- as.integer(substr(all.dat$active_mrn,1,regexpr(",",all.dat$active_mrn)-1))
  # all.dat$cvc_mrn <- as.integer(substr(all.dat$cvc_mrn,1,regexpr(",",all.dat$cvc_mrn)-1))
  # all.dat$inpt_mrn <- as.integer(substr(all.dat$inpt_mrn,1,regexpr(",",all.dat$inpt_mrn)-1))
  # all.dat$bld_mrn <- as.integer(substr(all.dat$bld_mrn,1,regexpr(",",all.dat$bld_mrn)-1))
  # all.dat$nutr_mrn <- as.integer(substr(all.dat$nutr_mrn,1,regexpr(",",all.dat$nutr_mrn)-1))
  # all.dat$growth_mrn <- as.integer(substr(all.dat$growth_mrn,1,regexpr(",",all.dat$growth_mrn)-1))
  # all.dat$liver_mrn <- as.integer(substr(all.dat$liver_mrn,1,regexpr(",",all.dat$liver_mrn)-1))
  # all.dat$outpt_mrn <- as.integer(substr(all.dat$outpt_mrn,1,regexpr(",",all.dat$outpt_mrn)-1))
  # all.dat <<- all.dat
}
# str(alldata)
# aos <- active_on_service_rawdata
# # aos$record same as as.integer yet sticking to as.integer
# # str(aos)
# as.integer(aos$record)
# bloodstream <- bloodstream_infections_rawdata
# central <- central_line_rawdata
# demog <- demographics_flat
# growth <- growth_data_rawdata
# inpatient <- inpatient_encounters_rawdata
# # iterventions <- interventions_rawdata
# liver <- liver_disease_rawdata
# # nutrition <- nut_rawdata
# outpatient <- outpatient_encounters_rawdata


#####trying cut function as the above is giving the error: relacement has 0 & data has xxx which could be because of knows issue of new variable needs to be created first before conditional statement can work 
# readdata <- function(fname="hpn_redcap_api_data_new.csv")
# {
#     df$valueBin <- cut(df$value, c(-Inf, 250, 500, 1000, 2000, Inf), 
#                        labels=c('<=250', '250-500', '500-1,000', '1,000-2,000', '>2,000'))
#     all.dat <<- all.dat
# }

####################

#####
# Prep data
#####
# below is appending all the ,mrn variables listed above and so replacing it with just one mrn available in the data set
# m1 <- 0
# m2=today
# prepdata <- function(mrnlist,m1=0,m2=today)

prepdata <- function(mrnlist,m1=0,m2=today)
{
  # changing all.dat to alldata in the following 2 lines
  # all.dat <- all.dat[all.dat$mrn %in% mrnlist,]
  all.dat <- all.dat
  
  # all.dat <- all.dat[all.dat$mrn %in% mrnlist | all.dat$active_mrn %in% mrnlist | all.dat$cvc_mrn %in% mrnlist
  #                         | all.dat$inpt_mrn %in% mrnlist | all.dat$bld_mrn %in% mrnlist | all.dat$nutr_mrn %in% mrnlist
  #                         | all.dat$growth_mrn %in% mrnlist | all.dat$liver_mrn %in% mrnlist | all.dat$outpt_mrn %in% mrnlist,]
  
  # below line tells to skip the first 34062 rows but get all the columns so removig this logic
  # all.dat <- all.dat[-34062,]
  
  # below is reading the individual files
  # ak: removing the _complete as its not reflected in new redcap design. But as all.dat has a lot of nulls switc
  # ak: the below values exist as columns so retaining hte _complete 
  # demogix <- which(names(all.dat)=="demographics_complete")
  
  # below gets the number of records in each of the instrument category
  demogix <- which(names(all.dat)=="demographics_complete")
  activeix <- which(names(all.dat)=="active_on_service_complete")
  clix <- which(names(all.dat)=="central_line_complete")
  hospix <- which(names(all.dat)=="inpatient_encounters_complete")
  bloodix <- which(names(all.dat)=="bloodstream_infections_complete")
  nutrix <- which(names(all.dat)=="nutrition_intake_complete")
  growthix <- which(names(all.dat)=="growth_data_complete")
  liverix <- which(names(all.dat)=="liver_disease_complete")
  outptix <- which(names(all.dat)=="outpatient_encounters_complete")
  
  # alldata$redcap_repeat_instrument=="%demogr%"
  # ak: rfom the following changing instrument to instance according to new redcap
  # demog.dat <- all.dat[all.dat$redcap_repeat_instrument=="demo_arm_1",1:demogix]
  
  # the below was designed to old redcap which had all the data in an order
  # demog.dat <- alldata[alldata$redcap_repeat_instrument=="demographics",1:demogix]
  # active.dat <- alldata[alldata$redcap_repeat_instrument=="active_on_service", c(1:3,(demogix+1):activeix)]
  # cl.dat <- alldata[alldata$redcap_repeat_instrument=="central_line",c(1:3,(activeix+1):clix)]
  # hosp.dat <- alldata[alldata$redcap_repeat_instrument=="inpatient_encounters",c(1:3,(clix+1):hospix)]
  # blood.dat <- alldata[alldata$redcap_repeat_instrument=="bloodstream_infections",c(1:3,(hospix+1):bloodix)]
  # nutr.dat <- alldata[alldata$redcap_repeat_instrument=="nutrition_intake",c(1:3,(bloodix+1):nutrix)]
  # growth.dat <- alldata[alldata$redcap_repeat_instrument=="growth_data",c(1:3,(nutrix+1):growthix)]
  # liver.dat <- alldata[alldata$redcap_repeat_instrument=="liver_disease",c(1:3,(growthix+1):liverix)]
  # outpt.dat <- alldata[alldata$redcap_repeat_instrument=="outpatient_encounters",c(1:3,(liverix+1):outptix)]
  # rm(demog.dat)

  # demog.dat <- all.dat[all.dat$redcap_repeat_instrument == is.na(all.dat$redcap_repeat_instrument),]
  # ?subset
  demog.dat <- subset(all.dat, is.na(redcap_repeat_instrument))
  active.dat <- all.dat[all.dat$redcap_repeat_instrument=="active_on_service",]
  cl.dat <- all.dat[all.dat$redcap_repeat_instrument=="central_line",]
  hosp.dat <- all.dat[all.dat$redcap_repeat_instrument=="inpatient_encounters",]
  blood.dat <- all.dat[all.dat$redcap_repeat_instrument=="bloodstream_infections",]
  nutr.dat <- all.dat[all.dat$redcap_repeat_instrument=="nutrition_intake",]
  growth.dat <- all.dat[all.dat$redcap_repeat_instrument=="growth_data",]
  liver.dat <- all.dat[all.dat$redcap_repeat_instrument=="liver_disease",]
  outpt.dat <- all.dat[all.dat$redcap_repeat_instrument=="outpatient_encounters",]
  
  active.dat$datein <- as.integer(split.date(active.dat$svc_start,char="-",ymd=T))
  active.dat$dateout <- as.integer(split.date(active.dat$svc_stop,char="-",ymd=T))
  active.dat$dateout <- replace(active.dat$dateout,is.na(active.dat$dateout),today)
  active.dat$servdays <- active.dat$dateout-active.dat$datein+1
  
  cl.dat$datein <- as.integer(split.date(cl.dat$insert_date,char="-",ymd=T))
  cl.dat$dateout <- as.integer(split.date(cl.dat$remove_date,char="-",ymd=T))
  cl.dat$dateout <- replace(cl.dat$dateout,is.na(cl.dat$dateout),today)
  cl.dat$cldays <- cl.dat$dateout-cl.dat$datein+1
  cl.dat <- cl.dat[-which(cl.dat$cldays < 0 | is.na(cl.dat$cldays)),]
  
  hosp.dat$datein <- as.integer(split.date(hosp.dat$hosp_admitdt,char="-",ymd=T))
  hosp.dat$dateout <- as.integer(split.date(hosp.dat$hosp_dischargedt,char="-",ymd=T))
  hosp.dat$dateout <- replace(hosp.dat$dateout,is.na(hosp.dat$dateout),today)
  hosp.dat$hospdays <- hosp.dat$dateout-hosp.dat$datein+1
  
  blood.dat$datein <- as.integer(split.date(blood.dat$bcx_date,char="-",ymd=T))
  nutr.dat$datein <- as.integer(split.date(nutr.dat$nutr_date,char="-",ymd=T))
  growth.dat$datein <- as.integer(split.date(growth.dat$growth_date,char="-",ymd=T))
  liver.dat$datein <- as.integer(split.date(liver.dat$liver_date,char="-",ymd=T))
  outpt.dat$datein <- as.integer(split.date(outpt.dat$outpt_date,char="-",ymd=T))
  
  blood.dat$datein <- replace(blood.dat$datein,blood.dat$datein>today,today)
  hosp.dat$dateout <- replace(hosp.dat$dateout,hosp.dat$dateout>today,today)
  cl.dat$datein <- replace(cl.dat$datein,cl.dat$datein>today,today)
  cl.dat$dateout <- replace(cl.dat$dateout,cl.dat$dateout>today,today)
  active.dat$datein <- replace(active.dat$datein,active.dat$datein>today,today)
  active.dat$dateout <- replace(active.dat$dateout,active.dat$dateout>today,today)
  nutr.dat$datein <- replace(nutr.dat$datein,nutr.dat$datein>today,today)
  growth.dat$datein <- replace(growth.dat$datein,growth.dat$datein>today,today)
  liver.dat$datein <- replace(liver.dat$datein,liver.dat$datein>today,today)
  outpt.dat$datein <- replace(outpt.dat$datein,outpt.dat$datein>today,today)
  
  # the original data was as factors and so converting into numeric vectors
  growth.dat$growth_wt_kg <- as.numeric(as.character(growth.dat$growth_wt_kg))
  growth.dat$growth_ht_cm <- as.numeric(as.character(growth.dat$growth_ht_cm))
  growth.dat$bmi <- (growth.dat$growth_wt_kg)/((growth.dat$growth_ht_cm/100)^2)
  
  # str(demog.dat)
  # demog.dat[,c(2,3,6)]
  # demog.dat[,c(2,3,6)] this is targeting 2:mrn, 3: dob, 6: gender_male
  
  # replacing x=growth_mrn to x=mrn  & then ignoring the merge as demog.data in null
  # merge <- 
  # demog.dat[c(1, 4,7)]
  # demog[,c(1,2,3)]
  # demog$dob


  # BMI calc missing because, demog data is missing and need to figure out how to join growth and demg data
  # growth.dat <- merge(growth.dat,demog[,c(1,2,3)],all.x=T,all.y=F,by.x="mrn",by.y="mrn")
  # growth.dat <- merge(growth.dat,demog[,c(1,2,3)],all.x=T, all.y = F, by.x="mrn", by.y = 'mrn')
  growth.dat <- merge(growth.dat, demog.dat[,c(1,4,7)], all.x=TRUE, all.y = F, by.x='mrn', by.y = 'mrn')
  
  # # ingnoring the below as dob data from demogs is null
  growth.dat$ageyrs <- round((growth.dat$datein - as.integer(split.date(growth.dat$dob.y,char="-", ymd=T))+1)/365,1)
  
  # length(growth.dat$bmi)
  # growth.dat$gender_male.x
  
  if (length(growth.dat$bmi)!=0)
  {
      for (i in 1:length(growth.dat$bmi)) 
        growth.dat$bmiz[i] <- bmiz(growth.dat$bmi[i],growth.dat$ageyrs[i],1-growth.dat$gender_male.x[i])
      growth.dat$bmiperc <- 100*round(pnorm(growth.dat$bmiz),3)
  }
  
  nowactive <- !(active.dat$datein > m2 | active.dat$dateout < m1)
  nowcl <- !(cl.dat$datein > m2 | cl.dat$dateout < m1)
  nowhosp <- !(hosp.dat$datein > m2 | hosp.dat$dateout < m1)
  nowblood <- !(blood.dat$datein > m2 | blood.dat$datein < m1)
  nowgrowth <- !(growth.dat$datein > m2 | growth.dat$datein < m1)
  nownutr <- !(nutr.dat$datein > m2 | nutr.dat$datein < m1)
  nowoutpt <- !(outpt.dat$datein > m2 | outpt.dat$datein < m1)
  
  # active.dat[nowactive,]
  # demog.dat <<- demog.dat
  # active.dat <<- active.dat[nowactive,]
  # cl.dat <<- cl.dat[nowcl,]
  # blood.dat <<- blood.dat[nowblood,]
  # hosp.dat <<- hosp.dat[nowhosp,]
  # outpt.dat <<- outpt.dat[nowoutpt,]
  # growth.dat <<- growth.dat[nowgrowth,]
  # nutr.dat <<- nutr.dat[nownutr,]
  
  return (cat("Demographic records:        ",length(demog.dat$mrn),"\n","Active service records:    ",length(active.dat$mrn),"\n",
             "Central line records:      ",length(cl.dat$mrn),"\n","Blood infection records:   ",length(blood.dat$mrn),"\n",
             "Hospitalization records:   ",length(hosp.dat$mrn),"\n","Outpatient records:        ",length(outpt.dat$mrn),"\n",
             "Growth records:            ",length(growth.dat$mrn),"\n","Nutrition records:         ",length(nutr.dat$mrn),"\n"))
}


#####
# Function to count CL days for given MRN
#####
# as new redcap has one mrn for all instruments, commenting variables for event specific mrn's out for the previous design

# 'bcx_site' %in% colnames(blood.dat)
# 'clabsi_commun' %in% colnames(blood.dat)
# from the following countcldays function other than active.dat, replacing the xxx_mrn to just mrn to reflect new redcap changes

countcldays <- function(targetmrn,mask1=0,mask2=today)
{
  # this.dat1 <- all.dat[all.dat$mrn==targetmrn,]
  
  this.dat1 <- active.dat[active.dat$mrn==targetmrn,]
  this.dat2 <- cl.dat[cl.dat$mrn==targetmrn,]
  this.dat3 <- hosp.dat[hosp.dat$mrn==targetmrn,]
  this.dat4 <- blood.dat[blood.dat$mrn==targetmrn & blood.dat$bcx_site==1 & blood.dat$clabsi_commun==1,]
  firstdate <- min(this.dat1$datein,this.dat2$datein,this.dat3$datein,this.dat4$datein,na.rm=T)

  # this.dat1 <- active.dat[active.dat$mrn==mrnlist,]
  # this.dat2 <- cl.dat[cl.dat$mrn==mrnlist,]
  # this.dat3 <- hosp.dat[hosp.dat$mrn==mrnlist,]
  # this.dat4 <- blood.dat[blood.dat$mrn==mrnlist & blood.dat$bcx_site==1 & blood.dat$clabsi_commun==1,]
  # firstdate <- min(this.dat1$datein,this.dat2$datein,this.dat3$datein,this.dat4$datein,na.rm=T)
  
  # length(this.dat1$mrn) 79
  ndays <- today - firstdate + 1
  # ndays
  
  if (ndays < 1) return(rep(0,5))
  firstdayhome <- NA
  
  # ?which
  # this.dat1[which(this.dat1$st_start == 1)]
  # 
  # this.dat1$st_start
  # this.dat1$st_start[1:100] == 1
  
  if (length(this.dat1$mrn)>0) 
    for (i in 1:length(this.dat1$mrn)) 
      if (is.na(this.dat1$st_start[i])) {
        firstdayhome <- NA
      } else {
        firstdayhome <- this.dat1$datein[i]
      }

      # if (this.dat1$st_start[i]==1) {
      #   firstdayhome <- this.dat1$datein[i]
      # } else {
      #   firstdayhome <- NA
      # }
  
  isactive <- rep(0,ndays)
  newhpn <- rep(0,ndays)
  admit <- rep(0,ndays)
  centline <- rep(0,ndays)
  nothosp <- rep(1,ndays)
  datemask <- rep(0,ndays)
  bloodinf <- rep(0,ndays)
  
  # rep(1,this.dat1$dateout[1]-this.dat1$datein[1]+1)
  # ?rep
  # this.dat1$dateout[1]
  # this.dat1$datein[1]+1
  # this.dat1$dateout[1]-this.dat1$datein[1]+1
  # rep(1, NA)
  
  # replaced cvc_mrn, inpt_mrn, bld_mrn from the following to just mrm to reflect new redcap
  if (length(this.dat1$mrn)>0)
    
    for (i in 1:length(this.dat1$mrn)) 
      if (is.na(this.dat1$dateout[i]-this.dat1$datein[i]+1)) next
      else isactive[(today-this.dat1$dateout[i]+1):(today-this.dat1$datein[i]+1)] <- rep(1,this.dat1$dateout[i]-this.dat1$datein[i]+1)
  
  if (length(this.dat1$mrn)>0) 
    for (i in 1:length(this.dat1$mrn)) for (j in 1:ndays) 
      if (is.na(firstdayhome)) next 
      else newhpn[j] <- as.numeric(((today-j+1) - firstdayhome) <= 30 & ((today-j+1) - firstdayhome) >= 0)
  
  if (length(this.dat2$mrn)>0) 
    for (i in 1:length(this.dat2$mrn))
      centline[(today-this.dat2$dateout[i]+1):(today-this.dat2$datein[i]+1)] <- rep(1,this.dat2$dateout[i]-this.dat2$datein[i]+1)
  
  # this.dat3$dateout[1]
  # this.dat3$datein[1]
  # this.dat3$datein[1]+1
  # this.dat3$dateout[1]-this.dat3$datein[1]+1
  # rep(0,NA)
  # is.na(this.dat3$dateout[i]-this.dat3$datein[i]+1)
  
  # if (length(this.dat3$mrn)>0) 
  #   for (i in 1:length(this.dat3$mrn))
  #     if (is.na(this.dat3$dateout[i]-this.dat3$datein[i]+1)) next
  #     nothosp[(today-this.dat3$dateout[i]+1):(today-this.dat3$datein[i]+1)] <- rep(0,this.dat3$dateout[i]-this.dat3$datein[i]+1)
  
  if (length(this.dat3$mrn)>0) 
    for (i in 1:length(this.dat3$mrn)) admit[(today-this.dat3$datein[i]+1)] <- 1
  
  if (length(this.dat4$bld_mrn)>0) 
    for (i in 1:length(this.dat4$bld_mrn)) bloodinf[(today-this.dat4$datein[i]+1)] <- 1
  
  if (mask2 < firstdate) mask2 <- firstdate
  if (mask1 > today) mask1 <- today
  datemask[(today-(min(mask2,today))+1):(today-(max(mask1,firstdate))+1)] <- rep(1,(min(mask2,today))-(max(mask1,firstdate))+1)
  cldays <- sum((isactive+centline+nothosp+datemask)==4)
  cldaysnew <- sum((isactive+centline+nothosp+datemask+newhpn)==5)
  clabsi <- sum((isactive+centline+bloodinf+datemask)==4)
  clabsinew <- sum((isactive+centline+bloodinf+datemask+newhpn)==5)
  readmitnew <- as.numeric(max(newhpn+admit+datemask)==3)
  return(data.frame(cldays,cldaysnew,clabsi,clabsinew,readmitnew))
}


######################

#####
# Calculate dashboard outcomes:
# 1.  Central line days #
# 2.  subjects being followed #
# 3.  30 day readmission <of first start on HPN> #/%
# 4a. CLABSI rate any
# 4b. CLABSI rate within 30 days <of first start on HPN>
# 5.  Unplanned hospitalizations (#) *
# 6.  Total hospitalizations (#) *
# 7.  Length of stay (median, IQR) *
# 8.  % days spent at home per month<#outpatient days/#total days per month>
# 9.  New HPN patients (#)
# 10. Mortality (#)
# 11. Transfers (#)
# 12. Weaned off HPN (#)
# 13. Outpatient encounters (total) @@@
# 14. Lines removed for CLABSI #/%
# 15. BMI less than 3rd percentile #/%
# 16. Direct bilirubin >= 2 #/%
#####

# m1=0
# m2 = today

calcdash <- function(m1=0,m2=today)
{
  nowactive <- !(active.dat$datein > m2 | active.dat$dateout < m1)
  nowcl <- !(cl.dat$datein > m2 | cl.dat$dateout < m1)
  nowhosp <- !(hosp.dat$datein > m2 | hosp.dat$dateout < m1)
  nowgrowth <- !(growth.dat$datein > m2 | growth.dat$datein < m1)
  nowoutpt <- !(outpt.dat$datein > m2 | outpt.dat$datein < m1)
  
  clnow <- 0
  clnownew <- 0
  clabsi <- 0
  clabsinew <- 0
  newhpn <- 0
  readmitnew <- 0
  
  # length(demog.dat$mrn) 78
  # demog.dat$mrn[2]
  # countcldays(demog.dat$mrn[1],m1,m2)
  # is.atomic(demog.dat$mrn)
  # is.atomic(tempcalc$cldays)
  # is.atomic(demog.dat$mrn)
  # is.atomic(demog.dat$mrn)
  # is.atomic(demog.dat$mrn)
  # is.atomic(demog.dat$mrn)
  # tempcalc['cldays']
  # is.atomic(clnow)

  # suppressWarnings(firstdate)
  for (k in 1:length(demog.dat$mrn))
  {
    tempcalc <- countcldays(demog.dat$mrn[k],m1,m2)
    clnow <- clnow + tempcalc['cldays']
    clnownew <- clnownew + tempcalc['cldaysnew']
    clabsi <- clabsi + tempcalc['clabsi']
    clabsinew <- clabsinew + tempcalc['clabsinew']
    newhpn <- newhpn + as.numeric(tempcalc['cldaysnew'] > 0)
    readmitnew <- readmitnew + tempcalc['readmitnew']
  }
  npatients <- length(unique(active.dat$mrn[nowactive]))
  clabsirate <- round(1000*clabsi/clnow,1)
  clabsiratenew <- round(1000*clabsinew/clnownew,1)
  unplanhosp <- sum(hosp.dat$hosp_status[nowhosp]==1,na.rm=T)
  totalhosp <- sum(nowhosp,na.rm=T)
  if (sum(nowhosp,na.rm=T)==0) los.median <- NA else los.median <- median(hosp.dat$hospdays[nowhosp],na.rm=T)
  los.iqr <- as.numeric(quantile(hosp.dat$hospdays[nowhosp],0.75,na.rm=T) - quantile(hosp.dat$hospdays[nowhosp],0.25,na.rm=T))
  percout <- 100*round(sum(active.dat$servdays[nowactive],na.rm=T) / (sum(hosp.dat$hospdays[nowhosp],na.rm=T) + sum(active.dat$servdays[nowactive],na.rm=T)),3)
  death <- sum((active.dat$end_type[nowactive]==4) & (active.dat$dateout[nowactive] >= m1) & active.dat$dateout[nowactive] <= m2,na.rm=T)
  transfer <- sum((active.dat$end_type[nowactive]==1) & (active.dat$dateout[nowactive] >= m1) & active.dat$dateout[nowactive] <= m2,na.rm=T)
  weanoff <- sum((active.dat$end_type[nowactive]==2) & (active.dat$dateout[nowactive] >= m1) & active.dat$dateout[nowactive] <= m2,na.rm=T)
  outptenc <- sum(nowoutpt,na.rm=T)
  remclabsi <- sum((cl.dat$remove[nowcl]==1) & (cl.dat$remove_type___clabsi[nowcl]==1) & (cl.dat$dateout[nowcl] >= m1) & cl.dat$dateout[nowcl] <= m2,na.rm=T)
  if (sum(nowgrowth,na.rm=T)==0) medbmi <- NA else medbmi <- median(growth.dat$bmiperc[nowgrowth],na.rm=T)
  # 16. Direct bilirubin >= 2 #/% (patient level not number of labs -- use maximum lab reading from month)
  
  
  # return (c(clnow,clnownew,clabsi,clabsinew,clabsirate,npatients,unplanhosp,los.median,percout,newhpn,death,transfer,weanoff,outptenc,remclabsi,medbmi))
  return  (t(c(clnow,clnownew,clabsi,clabsinew,clabsirate,npatients,unplanhosp,los.median,percout,newhpn,death,transfer,weanoff,outptenc,remclabsi,medbmi)))
  
}

#####
# Demographic table
#####
# m1=0
# m2=today
# freezedate=today

calcdemog <- function(m1=0,m2=today,freezedate=today,tabprint=FALSE)
{
  activelist <- unique(active.dat$mrn)
  nsub <- length(activelist)
  dob <- as.integer(split.date(demog.dat$dob,char="-",ymd=T))
  ageyrs <- round((freezedate - dob)/365,1)
  agemed <- univar(ageyrs)$median
  ageiqr <- univar(ageyrs)$iqr
  
  firstdate <- rep(NA,nsub)
  stateres <- rep(NA,nsub)
  for (i in 1:nsub) 
    if (!(demog.dat$mrn[i] %in% activelist)) 
      firstdate[i] <- NA  else firstdate[i] <- min(active.dat$datein[active.dat$mrn==demog.dat$mrn[i]],na.rm=T)
  
  for (i in 1:nsub) 
    if (!(demog.dat$mrn[i] %in% activelist)) 
      stateres[i] <- NA else 
      stateres[i] <- active.dat$state_res[active.dat$mrn==demog.dat$mrn[i] & active.dat$datein==firstdate[i]] =="MA"
  
  hpntime <- round((freezedate - firstdate)/365,1)
  hpntimemed <- univar(hpntime)$median
  hpntimeiqr <- univar(hpntime)$iqr
  
  # is.factor(demog.dat$gender_male)
  # is.factor(demog.dat$mrn) T
  # is.factor(active.dat$mrn) T
  
  if(is.factor(demog.dat$gender_male))
    demog.dat$gender_male <- as.numeric(as.character( demog.dat$gender_male ))
  
  if(is.factor(demog.dat$demog.dat$mrn))
    demog.dat$mrn <- as.numeric(as.character( demog.dat$mrn ))
  
  if(is.factor(active.dat$mrn))
    active.dat$mrn <- as.numeric(as.character( active.dat$mrn ))
  
  nmale <- sum(demog.dat$gender_male[demog.dat$mrn[i] %in% active.dat$mrn],na.rm=T)
  nstateres <- sum(stateres,na.rm=T)
  
  if(is.factor(demog.dat$diag_sbs))
    demog.dat$diag_sbs <- as.numeric(as.character( demog.dat$diag_sbs ))
  
  nsbs <- sum(demog.dat$diag_sbs[demog.dat$mrn[i] %in% activelist],na.rm=T)
  
  if(is.factor(demog.dat$diag_enterop))
    demog.dat$diag_enterop <- as.numeric(as.character( demog.dat$diag_enterop ))
  nenterop <- sum(demog.dat$diag_enterop[demog.dat$mrn[i] %in% activelist],na.rm=T)
  
  if(is.factor(demog.dat$diag_motility))
    demog.dat$diag_motility <- as.numeric(as.character( demog.dat$diag_motility ))
  nmotil <- sum(demog.dat$diag_motility[demog.dat$mrn[i] %in% activelist],na.rm=T)
  
  if(is.factor(demog.dat$diag_pn))
    demog.dat$diag_pn <- as.numeric(as.character( demog.dat$diag_pn ))
  nmisc <- sum(demog.dat$diag_pn[demog.dat$mrn[i] %in% activelist],na.rm=T)
  
  # replacing the cvc_mrn to mrn below because cl.dat is the active.data with the respective column contains cl related words
  ncl <- length(cl.dat$mrn[cl.dat$mrn[i] %in% activelist])
  nbrov <- sum(cl.dat$insert_type[cl.dat$mrn[i] %in% activelist]==1,na.rm=T)
  npicc <- sum(cl.dat$insert_type[cl.dat$mrn[i] %in% activelist]==2,na.rm=T)
  nportcath <- sum(cl.dat$insert_type==3[cl.dat$mrn[i] %in% activelist],na.rm=T)
  
  if (tabprint)
  {
    cat("PATIENT CHARACTERISTICS, N =",nsub,"\n",
        "Gender male, n (%): \t\t\t   ",nmale," (",round(100*nmale/nsub,1),")\n",
        "In-state residence, n (%): \t\t   ",nstateres," (",round(100*nstateres/nsub,1),")\n\n",
        "Age yrs on ",as.character(freezedate),", median (IQR): \t   ",agemed," (",round(ageiqr,1),")\n",
        "Yrs HPN program on",as.character(freezedate),", median (IQR):  ",hpntimemed," (",round(hpntimeiqr,1),")\n\n"
    )
    
    cat("PATIENT PRIMARY DIAGNOSIS, N =",nsub,"\n",
        "   SBS, n (%):             ",nsbs," (",round(100*nsbs/nsub,1),")\n",
        "   Enteropathy, n (%):     ",nenterop," (",round(100*nenterop/nsub,1),")\n",
        "   Motility, n (%):        ",nmotil," (",round(100*nmotil/nsub,1),")\n",
        "   Misc (PN temp), n (%):  ",nmisc," (",round(100*nmisc/nsub,1),")\n\n"
    )
    
    cat("CENTRAL LINE TYPE, N =",ncl,"\n",
        "   Broviac, n (%):        ",nbrov," (",round(100*nbrov/ncl,1),")\n",
        "   PICC, n (%):           ",npicc," (",round(100*npicc/ncl,1),")\n",
        "   Port-a-Cath, n (%):    ",nportcath," (",round(100*nportcath/ncl,1),")\n"
    )
  }
  
  return (list(nsub=nsub,nmale=nmale,nstateres=nstateres,
              agemed=agemed,ageiqr=ageiqr,hpntimemed=hpntimemed,hpntimeiqr=hpntimeiqr,
              nsbs=nsbs,nenterop=nenterop,nmotil=nmotil,nmisc=nmisc,
              ncl=ncl,nbrov=nbrov,npicc=npicc,nportcath=nportcath))
}


#
