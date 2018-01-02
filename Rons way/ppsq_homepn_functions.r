setwd("J:/Analytics Projects/Home Parenteral Nutrition QI/Data/Feb 2017 Processing")
source("J:/Analytics Projects/Home Parenteral Nutrition QI/Code/BI Data Prep Routines/helperfunctions.r")
source("J:/Analytics Projects/Home Parenteral Nutrition QI/Code/BI Data Prep Routines/bmi_z.r")


#####
# Initial data import from REDCap file.
#####

readdata <- function(fname="ppsq_homepn_alldata.csv")
{
        all.dat <- read.csv(fname,na.strings=c("1/1/1901"),stringsAsFactors = FALSE)[,-1]

        all.dat$active_mrn <- as.integer(substr(all.dat$active_mrn,1,regexpr(",",all.dat$active_mrn)-1))
        all.dat$cvc_mrn <- as.integer(substr(all.dat$cvc_mrn,1,regexpr(",",all.dat$cvc_mrn)-1))
        all.dat$inpt_mrn <- as.integer(substr(all.dat$inpt_mrn,1,regexpr(",",all.dat$inpt_mrn)-1))
        all.dat$bld_mrn <- as.integer(substr(all.dat$bld_mrn,1,regexpr(",",all.dat$bld_mrn)-1))
        all.dat$nutr_mrn <- as.integer(substr(all.dat$nutr_mrn,1,regexpr(",",all.dat$nutr_mrn)-1))
        all.dat$growth_mrn <- as.integer(substr(all.dat$growth_mrn,1,regexpr(",",all.dat$growth_mrn)-1))
        all.dat$liver_mrn <- as.integer(substr(all.dat$liver_mrn,1,regexpr(",",all.dat$liver_mrn)-1))
        all.dat$outpt_mrn <- as.integer(substr(all.dat$outpt_mrn,1,regexpr(",",all.dat$outpt_mrn)-1))
        all.dat <<- all.dat

        data_export_date <<- as.Date(file.mtime(fname))
}

####################

#####
# Prep data
#####

prepdata <- function()
{

    demogix <- which(names(all.dat)=="demographics_complete")
    activeix <- which(names(all.dat)=="active_on_service_complete")
    clix <- which(names(all.dat)=="central_line_complete")
    hospix <- which(names(all.dat)=="inpatient_encounters_complete")
    bloodix <- which(names(all.dat)=="bloodstream_infections_complete")
    nutrix <- which(names(all.dat)=="nutrition_intake_complete")
    growthix <- which(names(all.dat)=="growth_data_complete")
    liverix <- which(names(all.dat)=="liver_disease_complete")
    outptix <- which(names(all.dat)=="outpatient_encounters_complete")

    demog.dat <- all.dat[all.dat$redcap_event_name=="demo_arm_1",2:demogix]
    active.dat <- all.dat[all.dat$redcap_event_name=="active_arm_2",c((demogix+1):activeix)]
    cl.dat <- all.dat[all.dat$redcap_event_name=="cvc_arm_3",c((activeix+1):clix)]
    hosp.dat <- all.dat[all.dat$redcap_event_name=="inpatient_arm_4",c((clix+1):hospix)]
    blood.dat <- all.dat[all.dat$redcap_event_name=="blood_arm_5",c((hospix+1):bloodix)]
    nutr.dat <- all.dat[all.dat$redcap_event_name=="nutrition_arm_6",c((bloodix+1):nutrix)]
    growth.dat <- all.dat[all.dat$redcap_event_name=="growth_arm_7",c((nutrix+1):growthix)]
    liver.dat <- all.dat[all.dat$redcap_event_name=="liver_arm_8",c((growthix+1):liverix)]
    outpt.dat <- all.dat[all.dat$redcap_event_name=="outpatient_arm_9",c((liverix+1):outptix)]

    if( NROW(demog.dat) > 0) {
            demog.dat[demog.dat$dob=="", "dob"] <- NA
            demog.dat$dob <- as.Date(demog.dat$dob)
    }

    # active.dat$datein <- as.integer(split.date(active.dat$svc_start,char="-",ymd=T))
    # active.dat$dateout <- as.integer(split.date(active.dat$svc_stop,char="-",ymd=T))
    # active.dat$dateout <- replace(active.dat$dateout,is.na(active.dat$dateout),today)
    # active.dat$servdays <- active.dat$dateout-active.dat$datein+1

    if( NROW(active.dat) > 0) {
            active.dat[active.dat$svc_start=="","svc_start"] <- NA
            active.dat$svc_start <- as.Date(active.dat$svc_start)
            active.dat[active.dat$svc_stop=="","svc_stop"] <- NA
            active.dat$svc_stop <- as.Date(active.dat$svc_stop)
            active.dat$servdays <- with( active.dat, as.integer(svc_stop - svc_start + 1))
            if(!is.na(data_export_date)) {
                    active.dat[is.na(active.dat$servdays) & !is.na(active.dat$svc_start), "servdays"] <-
                            as.integer(data_export_date - active.dat[is.na(active.dat$servdays) & !is.na(active.dat$svc_start),"svc_start"] + 1)
            }
    }

    #
    # cl.dat$datein <- as.integer(split.date(cl.dat$insert_date,char="-",ymd=T))
    # cl.dat$dateout <- as.integer(split.date(cl.dat$remove_date,char="-",ymd=T))
    # cl.dat$dateout <- replace(cl.dat$dateout,is.na(cl.dat$dateout),today)
    # cl.dat$cldays <- cl.dat$dateout-cl.dat$datein+1
    # cl.dat <- cl.dat[-which(cl.dat$cldays < 0 | is.na(cl.dat$cldays)),]

    if( NROW(cl.dat) > 0) {
            cl.dat[cl.dat$insert_date=="","insert_date"] <- NA
            cl.dat$insert_date <- as.Date(cl.dat$insert_date)
            cl.dat[cl.dat$remove_date=="","remove_date"] <- NA
            cl.dat$remove_date <- as.Date(cl.dat$remove_date)
            cl.dat$cldays <- with(cl.dat,as.integer(remove_date - insert_date + 1))
            if(!is.na(data_export_date)) {
                    cl.dat[is.na(cl.dat$cldays) & !is.na(cl.dat$insert_date), "cldays"] <-
                            as.integer(data_export_date - cl.dat[is.na(cl.dat$cldays) & !is.na(cl.dat$insert_date),"insert_date"] + 1)
            }
    }

    #
    # hosp.dat$datein <- as.integer(split.date(hosp.dat$hosp_admitdt,char="-",ymd=T))
    # hosp.dat$dateout <- as.integer(split.date(hosp.dat$hosp_dischargedt,char="-",ymd=T))
    # hosp.dat$dateout <- replace(hosp.dat$dateout,is.na(hosp.dat$dateout),today)
    # hosp.dat$hospdays <- hosp.dat$dateout-hosp.dat$datein+1

    if( NROW(hosp.dat) > 0) {
            hosp.dat[hosp.dat$hosp_admitdt=="","hosp_admitdt"] <- NA
            hosp.dat$hosp_admitdt <- as.Date(hosp.dat$hosp_admitdt)
            hosp.dat[hosp.dat$hosp_dischargedt=="","hosp_dischargedt"] <- NA
            hosp.dat$hosp_dischargedt <- as.Date(hosp.dat$hosp_dischargedt)
            hosp.dat$hospdays <- with(hosp.dat,as.integer(hosp_dischargedt - hosp_admitdt + 1))
            if(!is.na(data_export_date)) {
                    hosp.dat[is.na(hosp.dat$hospdays) & !is.na(hosp.dat$hosp_admitdt), "hospdays"] <-
                            as.integer(data_export_date - hosp.dat[is.na(hosp.dat$hospdays) & !is.na(hosp.dat$hosp_admitdt),"hosp_admitdt"] + 1)
            }
    }

    #
    # blood.dat$datein <- as.integer(split.date(blood.dat$bcx_date,char="-",ymd=T))

    if( NROW(blood.dat) > 0) {
            blood.dat[blood.dat$bcx_date=="","bcx_date"] <- NA
            blood.dat$bcx_date <- as.Date(blood.dat$bcx_date)
    }

    # nutr.dat$datein <- as.integer(split.date(nutr.dat$nutr_date,char="-",ymd=T))

    if( NROW(nutr.dat) > 0) {
            nutr.dat[nutr.dat$nutr_date=="","nutr_date"] <- NA
            nutr.dat$nutr_date <- as.Date(nutr.dat$nutr_date)
    }

    # growth.dat$datein <- as.integer(split.date(growth.dat$growth_date,char="-",ymd=T))

    if( NROW(growth.dat) > 0) {
            growth.dat[growth.dat$growth_date=="","growth_date"] <- NA
            growth.dat$growth_date <- as.Date(growth.dat$growth_date)

            growth.dat$bmi <- (growth.dat$growth_wt_kg)/((growth.dat$growth_ht_cm/100)^2)
            growth.dat <- merge(growth.dat,demog.dat[,c("mrn","dob","gender_male")],all.x=T,all.y=F,by.x="growth_mrn",by.y="mrn")
            growth.dat$ageyrs <- round((as.numeric(growth.dat$growth_date-growth.dat$dob)+1)/365, 1)
            for (i in 1:length(growth.dat$bmi)) {
                    growth.dat$bmiz[i] <- bmiz(growth.dat$bmi[i],growth.dat$ageyrs[i],(1-growth.dat$gender_male[i]))
                    growth.dat$bmiperc[i] <- 100*round(pnorm(growth.dat$bmiz[i]),3)
            }
    }

    # liver.dat$datein <- as.integer(split.date(liver.dat$liver_date,char="-",ymd=T))

    if( NROW(liver.dat) > 0) {
            liver.dat[liver.dat$liver_date=="","liver_date"] <- NA
            liver.dat$liver_date <- as.Date(liver.dat$liver_date)
    }

    # outpt.dat$datein <- as.integer(split.date(outpt.dat$outpt_date,char="-",ymd=T))

    if( NROW(outpt.dat) > 0) {
            outpt.dat[outpt.dat$outpt_date=="","outpt_date"] <- NA
            outpt.dat$outpt_date <- as.Date(outpt.dat$outpt_date)
    }


    demog.dat <<- demog.dat
    active.dat <<- active.dat
    cl.dat <<- cl.dat
    blood.dat <<- blood.dat
    hosp.dat <<- hosp.dat
    outpt.dat <<- outpt.dat
    growth.dat <<- growth.dat
    nutr.dat <<- nutr.dat

    return(cat("Demographic records:        ",length(demog.dat$mrn),"\n","Active service records:    ",length(active.dat$active_mrn),"\n",
                        "Central line records:      ",length(cl.dat$cvc_mrn),"\n","Blood infection records:   ",length(blood.dat$bld_mrn),"\n",
                        "Hospitalization records:   ",length(hosp.dat$inpt_mrn),"\n","Outpatient records:        ",length(outpt.dat$outpt_mrn),"\n",
                        "Growth records:            ",length(growth.dat$growth_mrn),"\n","Nutrition records:         ",length(nutr.dat$nutr_mrn),"\n"))
}

#####
# Function to count CL days for given MRN
#####

countcldays <- function(targetmrn,mask1=0,mask2=today)
{
    this.dat1 <- active.dat[active.dat$active_mrn==targetmrn,]
    this.dat2 <- cl.dat[cl.dat$cvc_mrn==targetmrn,]
    this.dat3 <- hosp.dat[hosp.dat$inpt_mrn==targetmrn,]
    this.dat4 <- blood.dat[blood.dat$bld_mrn==targetmrn & blood.dat$bcx_site==1 & blood.dat$clabsi_commun==1,]
    firstdate <- min(this.dat1$datein,this.dat2$datein,this.dat3$datein,this.dat4$datein,na.rm=T)
    ndays <- today - firstdate + 1
    if (ndays < 1) return(rep(0,5))
    firstdayhome <- NA
    if (length(this.dat1$mrn)>0) for (i in 1:length(this.dat1$mrn)) if (this.dat1$st_start[i]==1) firstdayhome <- this.dat1$datein[i]

    isactive <- rep(0,ndays)
    newhpn <- rep(0,ndays)
    admit <- rep(0,ndays)
    centline <- rep(0,ndays)
    nothosp <- rep(1,ndays)
    datemask <- rep(0,ndays)
    bloodinf <- rep(0,ndays)

    if (length(this.dat1$mrn)>0) for (i in 1:length(this.dat1$mrn)) isactive[(today-this.dat1$dateout[i]+1):(today-this.dat1$datein[i]+1)] <- rep(1,this.dat1$dateout[i]-this.dat1$datein[i]+1)
    if (length(this.dat1$mrn)>0) for (i in 1:length(this.dat1$mrn)) for (j in 1:ndays) if (is.na(firstdayhome)) next else newhpn[j] <- as.numeric(((today-j+1) - firstdayhome) <= 30 & ((today-j+1) - firstdayhome) >= 0)
    if (length(this.dat2$cvc_mrn)>0) for (i in 1:length(this.dat2$cvc_mrn)) centline[(today-this.dat2$dateout[i]+1):(today-this.dat2$datein[i]+1)] <- rep(1,this.dat2$dateout[i]-this.dat2$datein[i]+1)
    if (length(this.dat3$inpt_mrn)>0) for (i in 1:length(this.dat3$inpt_mrn)) nothosp[(today-this.dat3$dateout[i]+1):(today-this.dat3$datein[i]+1)] <- rep(0,this.dat3$dateout[i]-this.dat3$datein[i]+1)
    if (length(this.dat3$inpt_mrn)>0) for (i in 1:length(this.dat3$inpt_mrn)) admit[(today-this.dat3$datein[i]+1)] <- 1
    if (length(this.dat4$bld_mrn)>0) for (i in 1:length(this.dat4$bld_mrn)) bloodinf[(today-this.dat4$datein[i]+1)] <- 1

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

    for (k in 1:length(demog.dat$mrn))
    {
        tempcalc <- countcldays(demog.dat$mrn[k],m1,m2)
        clnow <- clnow + tempcalc$cldays
        clnownew <- clnownew + tempcalc$cldaysnew
        clabsi <- clabsi + tempcalc$clabsi
        clabsinew <- clabsinew + tempcalc$clabsinew
        newhpn <- newhpn + as.numeric(tempcalc$cldaysnew > 0)
        readmitnew <- readmitnew + tempcalc$readmitnew
    }
    npatients <- length(unique(active.dat$active_mrn[nowactive]))
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

    return(c(clnow,clnownew,clabsi,clabsinew,clabsirate,npatients,unplanhosp,los.median,percout,newhpn,death,transfer,weanoff,outptenc,remclabsi,medbmi))
}

#####
# Demographic table
#####

calcdemog <- function(m1=0,m2=today,freezedate=today,tabprint=FALSE)
{
    activelist <- unique(active.dat$active_mrn)
    nsub <- length(activelist)
    dob <- as.integer(split.date(demog.dat$dob,char="-",ymd=T))
    ageyrs <- round((freezedate - dob)/365,1)
    agemed <- univar(ageyrs)$median
    ageiqr <- univar(ageyrs)$iqr

    firstdate <- rep(NA,nsub)
    stateres <- rep(NA,nsub)
    for (i in 1:nsub) if (!(demog.dat$mrn[i] %in% activelist)) firstdate[i] <- NA else firstdate[i] <- min(active.dat$datein[active.dat$active_mrn==demog.dat$mrn[i]],na.rm=T)
    for (i in 1:nsub) if (!(demog.dat$mrn[i] %in% activelist)) stateres[i] <- NA else stateres[i] <- active.dat$state_res[active.dat$active_mrn==demog.dat$mrn[i] & active.dat$datein==firstdate[i]] =="MA"

    hpntime <- round((freezedate - firstdate)/365,1)
    hpntimemed <- univar(hpntime)$median
    hpntimeiqr <- univar(hpntime)$iqr

    nmale <- sum(demog.dat$gender_male[demog.dat$mrn[i] %in% active.dat$active_mrn],na.rm=T)
    nstateres <- sum(stateres,na.rm=T)

    nsbs <- sum(demog.dat$diag_sbs[demog.dat$mrn[i] %in% activelist],na.rm=T)
    nenterop <- sum(demog.dat$diag_enterop[demog.dat$mrn[i] %in% activelist],na.rm=T)
    nmotil <- sum(demog.dat$diag_motility[demog.dat$mrn[i] %in% activelist],na.rm=T)
    nmisc <- sum(demog.dat$diag_pn[demog.dat$mrn[i] %in% activelist],na.rm=T)

    ncl <- length(cl.dat$cvc_mrn[cl.dat$cvc_mrn[i] %in% activelist])
    nbrov <- sum(cl.dat$insert_type[cl.dat$cvc_mrn[i] %in% activelist]==1,na.rm=T)
    npicc <- sum(cl.dat$insert_type[cl.dat$cvc_mrn[i] %in% activelist]==2,na.rm=T)
    nportcath <- sum(cl.dat$insert_type==3[cl.dat$cvc_mrn[i] %in% activelist],na.rm=T)

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

    return(list(nsub=nsub,nmale=nmale,nstateres=nstateres,
                    agemed=agemed,ageiqr=ageiqr,hpntimemed=hpntimemed,hpntimeiqr=hpntimeiqr,
                    nsbs=nsbs,nenterop=nenterop,nmotil=nmotil,nmisc=nmisc,
                    ncl=ncl,nbrov=nbrov,npicc=npicc,nportcath=nportcath))
}


#
