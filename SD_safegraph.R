#########################################################################
# SafeGraph Social Distancing Data
# Created by: Sahir Bhatnagar (McGill U)
# Date: May 10, 2020
# Description: This script reads in the daily social distancing data,
# summarizes it by county, and combines it into one data.table
# Notes:
# This script assumes you have downloaded the census fips codes and
# the social distancing data from safegraph
# There is an option to run the script in parallel using the
# doParallel package
###########################################################################



# load packages -----------------------------------------------------------

if (!requireNamespace("pacman")) install.packages("pacman")
pacman::p_load(data.table)
pacman::p_load(tidyverse)
pacman::p_load(here)
pacman::p_load(doParallel)
doParallel::registerDoParallel(cores = 8)


# file path ---------------------------------------------------------------

# adjust accordingly. here I assume the data is in a folder called data
# which is located in your R working directory
datain <- here::here("data")


# other inputs ------------------------------------------------------------

startM <- 1 # which month start
endM <- 5 # which month end


# get census block groups -------------------------------------------------

# download file from https://docs.safegraph.com/docs/open-census-data
cbg <- fread(here::here("data/cbg_fips_codes.csv"),
             keepLeadingZeros = TRUE)

# check that all state fips are 2 and county fips are 3 characters
nchar(cbg$state_fips) %>% table
nchar(cbg$county_fips) %>% table

# update some codes
# source: https://www.cdc.gov/nchs/nvss/bridged_race/county_geography-_changes2015.pdf
cbg[state_fips == "02" & county_fips == "270", `:=`(county_fips = 158,
                                                    county = "Kusilvak Census Area")]
cbg[state_fips == "46" & county_fips == "113", `:=`(county_fips = 102,
                                                    county = "Oglala Lakota County")]

cbg[, fips := stringr::str_c(cbg$state_fips, cbg$county_fips)]

# check all fips are 5 characters long
cbg[,table(nchar(fips))]

# for faster data.table joins
setkey(cbg, "fips")



# read SD data, merge with cbg, summarise by county -----------------------

# this return a list of length equal to the number of months
result <- lapply(startM:endM, function(m){

  # get all days of the month for which theres data
  f <- list.files(file.path(datain,"2020",paste("0", m, sep = "")))

  daily_combined <- foreach(d = f) %dopar% {

    # daily Social distancing data
    dat <- fread(file.path(datain,"2020", paste("0", m, "/", d, "/2020-0",m,"-",
                                                d,"-social-distancing.csv.gz",sep="")),
                 select = col_names,
                 keepLeadingZeros = TRUE)

    # check that all CBG are 12 characters
    # dat[, table(nchar(origin_census_block_group))]

    dat[, fips:=stringr::str_sub(origin_census_block_group,1,5)]
    setkey(dat,"fips")

    DT <- merge(x = dat, y = cbg, by = "fips")

    # see paper by http://www-personal.umich.edu/~yingfan/Fan_Orhun_Turjeman.pdf
    DT[, `:=`(adjusted_completely_home_device_count = completely_home_device_count + pmax(0, candidate_device_count - device_count))]
    DT[, `:=`(SD_deviceCount_sum = sum(device_count,na.rm=T),
              SD_candidateDeviceCount_sum = sum(candidate_device_count,na.rm=T),
              SD_distTraveledFromHome_median = median(distance_traveled_from_home,na.rm=T),
              SD_completelyHomeDevice_sum = sum(completely_home_device_count,na.rm=T),
              SD_adjustedCompletelyHomeDevice_sum = sum(adjusted_completely_home_device_count, na.rm = T),
              SD_dwellHome_median = median(median_home_dwell_time,na.rm=T),
              SD_dwellNotHome_median = median(median_non_home_dwell_time, na.rm = T),
              SD_partTimeWorkBehaviorDevice_sum = sum(part_time_work_behavior_devices,na.rm=T),
              SD_fullTimeWorkBehaviorDevice_sum = sum(full_time_work_behavior_devices,na.rm=T),
              SD_deliveryBehaviorDevice_sum = sum(delivery_behavior_devices, na.rm = T),
              SD_percentageTimeHome_median =  median(median_percentage_time_home,na.rm=T)), by = fips]

    DT[, `:=`(SD_partTimeWorkBehaviorDevice_share = SD_partTimeWorkBehaviorDevice_sum / SD_candidateDeviceCount_sum,
              SD_fullTimeWorkBehaviorDevice_share = SD_fullTimeWorkBehaviorDevice_sum / SD_candidateDeviceCount_sum,
              SD_devicesLeavingHome_share = 1 - SD_adjustedCompletelyHomeDevice_sum / SD_candidateDeviceCount_sum)]

    # take first row only of each fips
    DTf <- DT[, .SD[1], by = fips, .SDcols = c("state",grep("SD", colnames(DT), value = T))]

    DTf[, date := lubridate::as_date(DT$date_range_start[1])]
  }

  rbindlist(daily_combined)

})


# this combines all the months together
DT_final <- rbindlist(result)
data.table::fwrite(DT_final, file = here::here("data/aggregated_may06.csv"))







