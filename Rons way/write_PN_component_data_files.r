write_homepn_data_files <- function() {

        if (nrow(demog.dat) > 0) write.csv(demog.dat, "PNdemog.csv")
        else warning("No data in demog.dat.  Not writing file.")

        if (nrow(active.dat) > 0) write.csv(active.dat, "PNactive.csv")
        else warning("No data in active.dat.  Not writing file.")

        if (nrow(cl.dat) > 0) write.csv(cl.dat, "PNcl.csv")
        else warning("No data in cl.dat.  Not writing file.")

        if (nrow(hosp.dat) > 0) write.csv(hosp.dat, "PNhosp.csv")
        else warning("No data in hosp.dat.  Not writing file.")

        if (nrow(blood.dat) > 0) write.csv(blood.dat, "PNblood.csv")
        else warning("No data in blood.dat.  Not writing file.")

        if (nrow(growth.dat) > 0) write.csv(growth.dat, "PNgrowth.csv")
        else warning("No data in growth.dat.  Not writing file.")

        if (nrow(outpt.dat) > 0) write.csv(outpt.dat, "PNoutpt.csv")
        else warning("No data in outpt.dat.  Not writing file.")

        if (nrow(nutr.dat) > 0) write.csv(nutr.dat, "PNnutr.csv")
        else warning("No data in nutr.dat.  Not writing file.")

        if (nrow(all_patients_daily.df) > 0) write.csv(all_patients_daily.df, "PNdaily.csv")
        else warning("No data in all_patients_daily.dat.  Not writing file.")

        # if (nrow(all.dat) > 0) write.csv(all.dat, "PNall.csv")
        # else warning("No data in all.dat.  Not writing file.")
}