make_patient_daily_dataframe<- function(targetmrn) {

        this.dat1 <- active.dat[active.dat$active_mrn==targetmrn,]
        this.dat2 <- cl.dat[cl.dat$cvc_mrn==targetmrn,]
        this.dat3 <- hosp.dat[hosp.dat$inpt_mrn==targetmrn,]
        this.dat4 <- blood.dat[blood.dat$bld_mrn==targetmrn & blood.dat$bcx_site==1 & blood.dat$clabsi_commun==1,]

        firstdate <- min(this.dat1$svc_start,this.dat2$insert_date,this.dat3$hosp_admitdt,this.dat4$bcx_date,na.rm=T)

        if(!exists("data_export_date"))   stop( "data_export_date does not exist.")
        else if (is.na(data_export_date)) stop("data_export_date is NA.")
        else lastdate <- data_export_date

        ndays <- lastdate - firstdate + 1
        if (ndays < 1) return( NULL )

        caldate <- firstdate:lastdate

        patient_daily.df <- data.frame(mrn=targetmrn,caldate=as.Date(caldate,"1970-01-01"),
                                       isactive=0,newhpn=0,admit=0,centline=0,nothosp=1,bloodinf=0,
                                       death=0,transfer=0,weanoff=0,remclabsi=0)

        if (length(this.dat1$active_mrn)>0) {
                for (svcstart in this.dat1$svc_start) {
                        n <- length(this.dat1[this.dat1$svc_start==svcstart,"active_current"]==0)
                        if (n > 1) {
                                warning(paste("OMITTING mrn",targetmrn, "because found", n, "records for same service start date:",
                                              as.Date(svcstart,"1970-01-01") ))
                                return( NULL )
                        }
                        if (this.dat1[this.dat1$svc_start==svcstart,"active_current"]==0) {

                                svcstop <- this.dat1[this.dat1$svc_start==svcstart,"svc_stop"]

                                if (!is.na(this.dat1[this.dat1$svc_start==svcstart, "end_type"])) {

                                if (this.dat1[this.dat1$svc_start==svcstart, "end_type"]==1)
                                        patient_daily.df[patient_daily.df$caldate==svcstop, "transfer"] <- 1

                                else if (this.dat1[this.dat1$svc_start==svcstart, "end_type"]==2)
                                        patient_daily.df[patient_daily.df$caldate==svcstop, "weanoff"] <- 1

                                else if (this.dat1[this.dat1$svc_start==svcstart, "end_type"]==4)
                                        patient_daily.df[patient_daily.df$caldate==svcstop, "death"] <- 1
                                }
                        }
                        else svcstop <- lastdate
                        patient_daily.df[patient_daily.df$caldate %in% svcstart:svcstop,"isactive"] <- 1
                }

                firstdayhome <- this.dat1[this.dat1$st_start==1,"svc_start"]
                lfdh <- length(firstdayhome)
                if (lfdh > 0)
                        patient_daily.df[patient_daily.df$caldate %in% firstdayhome[lfdh]:(min(firstdayhome[lfdh]+29,lastdate)), "newhpn"] <- 1
        }

        if (length(this.dat2$cvc_mrn)>0)
                for(cvcinsert in this.dat2$insert_date){
                        n <- length(this.dat2[this.dat2$insert_date==cvcinsert,"remove"]==1)
                        if (n > 1) {
                                warning(paste("OMITTING mrn",targetmrn, "because found", n, "records for same line insert date:",
                                              as.Date(cvcinsert,"1970-01-01") ))
                                return( NULL )
                        }
                        else if (this.dat2[this.dat2$insert_date==cvcinsert,"remove"]==1) {
                                cvcremove <- this.dat2[this.dat2$insert_date==cvcinsert,"remove_date"]

                                if (this.dat2[this.dat2$insert_date==cvcinsert, "remove_type___clabsi"]==1)
                                        patient_daily.df[patient_daily.df$caldate==cvcremove, "remclabsi"] <- 1
                        }
                        else cvcremove <- lastdate
                        patient_daily.df[patient_daily.df$caldate %in% cvcinsert:cvcremove,"centline"] <- 1
                }

        if (length(this.dat3$inpt_mrn)>0)
                for(admitdate in this.dat3$hosp_admitdt){
                        n <- length(this.dat3[this.dat3$hosp_admitdt==admitdate,"hosp_discharge"]==1)
                        if (n > 1) {
                                warning(paste("OMITTING mrn",targetmrn, "because found", n, "records for same hospital admit date:",
                                              as.Date(admitdate,"1970-01-01") ))
                                return( NULL )
                        }
                        else if (this.dat3[this.dat3$hosp_admitdt==admitdate,"hosp_discharge"]==1)
                                dischargedate <- this.dat3[this.dat3$hosp_admitdt==admitdate,"hosp_dischargedt"]
                        else dischargedate <- lastdate
                        patient_daily.df[patient_daily.df$caldate %in% admitdate:dischargedate,"nothosp"] <- 0
                        patient_daily.df[patient_daily.df$caldate==admitdate,"admit"] <- 1
                }

        if (length(this.dat4$bld_mrn)>0) patient_daily.df[patient_daily.df$caldate %in% this.dat4$bcx_date,"bloodinf"] <- 1


        return(patient_daily.df)

}


make_all_patients_daily_dataframe <- function() {

        if(!exists("data_export_date"))
                stop("data_export_date does not exist.")
        else if (is.na(data_export_date))
                stop("data_export_date is NA")

        all_patients_daily.df <- data.frame(mrn=numeric(),caldate=as.Date(integer(),"1970-01-01"),isactive=integer(),newhpn=integer(),
                                             admit=integer(),centline=integer(),nothosp=integer(),bloodinf=integer(),
                                             death=integer(),transfer=integer(),weanoff=integer(),remclabsi=integer())

        for (mrn in demog.dat$mrn) {
                x <- make_patient_daily_dataframe(mrn)
                if (is.null(x)) next
                else all_patients_daily.df <- rbind(all_patients_daily.df,x)
        }

        all_patients_daily.df <<- all_patients_daily.df
}

data_export_date <- as.Date("2017-01-30")
# write.csv(all_patients_daily.df,"PNdaily.csv")