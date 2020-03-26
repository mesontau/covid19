#' @title readDataCovid
#' @description reads data from Covid19 in tsv format. Required columns: country, detected, healed, deceased, date.
#'
#' @param dataFile full path to data file
#' @param col_types types of columns for reading the table
#'
#' @return data frame with the data
#'
#' @export
#'
readDataCovid <- function(dataFile, col_types = "ciiic"){

  covid19 <- read_tsv(dataFile, col_types = col_types)

  #Fix special characters
  covid19$country <- gsub("\xa7", "ß", covid19$country)
  covid19$country <- gsub("\x9f", "ü", covid19$country)
  covid19$country <- gsub("\x85", "Ö", covid19$country)

  #Dates in lubridate format
  covid19$dateF <- lubridate::parse_date_time(gsub("\\.", "-", covid19$date), "dmy" )
  covid19$weekday <- weekdays(covid19$dateF)

  covid19 <- covid19 %>% group_by(country) %>%
              arrange(dateF) %>%
              mutate(epidemy.days = n()
                     ,max.detected = max(detected)
                     ,delta.detected = order_by(dateF, detected - lag(detected))
                     ,delta.healed   = order_by(dateF, healed - lag(healed))
                     ,delta.deceased = order_by(dateF, deceased - lag(deceased)))

  #Remove days were no update was done
  covid19 <- covid19 %>% filter(delta.detected > 0 | delta.healed > 0 | delta.deceased > 0)

  covid19 <- covid19 %>% group_by(country) %>%
    arrange(dateF) %>%
    mutate( active = detected - healed - deceased
           ,delta.time = ymd(dateF) - ymd(lag(dateF))
           ,delta.detected = order_by(dateF, detected - lag(detected))
           ,delta.healed   = order_by(dateF, healed - lag(healed))
           ,delta.deceased = order_by(dateF, deceased - lag(deceased))
           ,delta.detected.rel = delta.detected / (detected - delta.detected)
           ,delta.healed.rel   = delta.healed / (healed - delta.healed)
           ,delta.deceased.rel = delta.deceased / (deceased - delta.deceased)
           ,timeToDouble.detected = delta.time / delta.detected.rel
           ,timeToDouble.healed   = delta.time / delta.healed.rel
           ,timeToDouble.deceased = delta.time / delta.deceased.rel) %>%
    mutate( delta.active = order_by(dateF, active - lag(active))
            ,delta.active.rel = delta.active / (active - delta.active)
            ,timeToDouble.active = delta.time / delta.active.rel)

  return(covid19)
}


