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

        if( NROW(cl.dat) > 0) {
                cl.dat[cl.dat$insert_date=="","insert_date"] <- NA
                cl.dat$insert_date <- as.Date(cl.dat$insert_date)
                cl.dat[is.na(cl.dat$remove), "remove"] <- 0
                cl.dat[cl.dat$remove_date=="","remove_date"] <- NA
                cl.dat$remove_date <- as.Date(cl.dat$remove_date)
                cl.dat$cldays <- with(cl.dat,as.integer(remove_date - insert_date + 1))
                if(!is.na(data_export_date)) {
                        cl.dat[is.na(cl.dat$cldays) & !is.na(cl.dat$insert_date), "cldays"] <-
                                as.integer(data_export_date - cl.dat[is.na(cl.dat$cldays) & !is.na(cl.dat$insert_date),"insert_date"] + 1)
                }
        }

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


        if( NROW(blood.dat) > 0) {
                blood.dat[blood.dat$bcx_date=="","bcx_date"] <- NA
                blood.dat$bcx_date <- as.Date(blood.dat$bcx_date)
        }


        if( NROW(nutr.dat) > 0) {
                nutr.dat[nutr.dat$nutr_date=="","nutr_date"] <- NA
                nutr.dat$nutr_date <- as.Date(nutr.dat$nutr_date)
        }


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


        if( NROW(liver.dat) > 0) {
                liver.dat[liver.dat$liver_date=="","liver_date"] <- NA
                liver.dat$liver_date <- as.Date(liver.dat$liver_date)
        }


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
