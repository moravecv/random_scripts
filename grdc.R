#' Funkce pro vypocet faktoru z GRDC
#'
#' @param dta vektor umisteni jednotlivych GRDC souboru (string)
#' @param metoda metoda vypoctu faktoru "FWUA" nebo "TENANT30" (string)
#' @param od datum "oriznuti" od (string)
#' @param do datum "oriznuti" do (string)
#' @param type typ casove rady "original" nebo "calculated" (string)
#' @param naplnenost parametr naplnenosti dennimi daty (num)
#' @param output cesta k vytupnimu souboru (string)
#' @param var ulozeni vypoctu do promenne R (TRUE/FALSE)
#'
#' @return data.frame()
#' @export textovy soubor
#'
#' @examples grdc(dta = "D:/R/6233401.day" "D:/R/6340050.day" "D:/R/6340070.day", 
#' metoda = "FWUA", od = "1965-01-01", do = "2000-01-01", type = "calculated", 
#' naplnenost = 0.7, output = "D:/R/export.txt", var = T)

grdc <- function(dta, metoda, od, do, type, naplnenost, output, var){
  #' Funkce pro vypocet faktoru z GRDC
  #'
  #' @param dta vektor umisteni jednotlivych GRDC souboru (string)
  #' @param metoda metoda vypoctu faktoru "FWUA" nebo "TENANT30" (string)
  #' @param od datum "oriznuti" od (string)
  #' @param do datum "oriznuti" do (string)
  #' @param type typ casove rady "original" nebo "calculated" (string)
  #' @param naplnenost parametr naplnenosti dennimi daty (num)
  #' @param output cesta k vytupnimu souboru (string)
  #' @param var ulozeni vypoctu do promenne R (TRUE/FALSE)
  #'
  #' @return data.frame()
  #' @export textovy soubor
  #'
  #' @examples grdc(dta = "D:/R/6233401.day" "D:/R/6340050.day" "D:/R/6340070.day", 
  #' metoda = "FWUA", od = "1965-01-01", do = "2000-01-01", type = "calculated", 
  #' naplnenost = 0.7, output = "D:/R/export.txt", var = T)
  
  library(data.table)
  library(lubridate)
  
  tab <-  c()
  for (i in dta){
    ##### PROMENNE ####
    
    od <- as.Date(od, format = "%Y-%m-%d")
    do <- as.Date(do, format = "%Y-%m-%d")
    
    ##### UDAJE ####
    
    header <- as.data.frame(read.delim(file = i, skip = 7))
    header <- header[c(1:10),]
    
    ##### GRDC NO ####
    grdc.no <- strsplit(x = as.character(header[1]), split = ":")
    grdc.no <- gsub(pattern = " ", replacement = "", x = grdc.no[[1]][2], fixed = T)
    ##### RIVER ####
    river <- strsplit(x = as.character(header[2]), split = ":")
    river <- gsub(pattern = " ", replacement = "", x = river[[1]][2], fixed = T)
    ##### STATION ####
    station <- strsplit(x = as.character(header[3]), split = ":")
    station <- gsub(pattern = " ", replacement = "", x = station[[1]][2], fixed = T)
    ##### COUNTRY ####
    country <- strsplit(x = as.character(header[4]), split = ":")
    country <- gsub(pattern = " ", replacement = "", x = country[[1]][2], fixed = T)
    ##### LAT ####
    lat <- strsplit(x = as.character(header[5]), split = ":")
    lat <- gsub(pattern = " ", replacement = "", x = lat[[1]][2], fixed = T)
    ##### LON ####
    lon <- strsplit(x = as.character(header[6]), split = ":")
    lon <- gsub(pattern = " ", replacement = "", x = lon[[1]][2], fixed = T)
    ##### AREA ####
    area <- strsplit(x = as.character(header[7]), split = ":")
    area <- as.numeric(gsub(pattern = " ", replacement = "", x = area[[1]][2], fixed = T))
    
    dta <- data.table(read.delim(file = i, header = T, sep = ";", skip = 40))
    dta$YYYY.MM.DD <- as.Date(as.character(dta$YYYY.MM.DD), format = "%Y-%m-%d")
    
    ##### ORIGINAL ####
    if (type == "original"){
      
      dta[Original < 0, Original:= NA]
      dta[Flag == 99, Original:= NA ] 
      dta <- dta[YYYY.MM.DD %between% c(od, do),]
      
      ##### NO DATA ####
      if(nrow(dta) < 1){
        
        message(paste0("Pozadovane obdobi neobsahuje data pro GRDC NO. ", grdc.no))
        zprava <- paste0("Pozadovane obdobi neobsahuje data pro GRDC NO. ", grdc.no)
        radek <- c(grdc.no, river, station, country, lat, lon, as.character(od), as.character(do),
                   rep(x = "NA", 39), zprava)
        
      } else {
        napln <- dta[,sum(is.na(Original)) / .N, by = .(year(YYYY.MM.DD), month(YYYY.MM.DD))]
        napln$V1 <- abs(napln$V1 - 1)
        napln_stat <- napln[, mean(V1), by = month]
        #print(napln$V1)
        
        ##### DATA ####
        if (all(napln_stat$V1 <= 1) & all(napln_stat$V1 > naplnenost)){
          if (all(napln_stat$V1 < 1)) {
            message(paste0("GRDC NO. ", grdc.no, " ma naplnenost < 100%"))
            zprava <- paste0("GRDC NO. ", grdc.no, " ma naplnenost < 100%")
          } else {
            message(paste0("GRDC NO. ", grdc.no, " OK"))
            zprava <- NA
          }
          
          Qmi <- dta[, mean(Original, na.rm = T), by = month(YYYY.MM.DD)]
          Qr <- dta[, mean(Original, na.rm = T), by = year(YYYY.MM.DD)]
          Qr <- Qr[, mean(V1)]
          
          ###### METODA FWUA #######
          if (metoda == "FWUA"){
            
            ref_year <- 1
            ref_mon <- 1 / 12
            
            FWUA_CFi <- c()
            for (i in 1:12){
              CFi <- ref_mon / ((86400 * days_in_month(i) * Qmi$V1[i]) / (1000000 * area))
              FWUA_CFi <- append(FWUA_CFi, CFi)
            }
            CFr <- ref_year / ((86400 * 365 * Qr) / (1000000 * area))
            radek <-  list(grdc.no, river, station, country, lat, lon, as.character(od), as.character(do), napln_stat$V1, Qmi$V1, Qr, metoda, FWUA_CFi, CFr, zprava)
            
            ####### METODA TENANT30 ######    
          } else {
            
            ref_year <- 0.1632
            ref_mon <- 0.0136
            
            TENANT30_CFi <- c()
            for (i in 1:12){
              CFi <- ref_mon / ((86400 * days_in_month(i) * (Qmi$V1[i] - (0.3 * Qr))) / (1000000 * area))
              TENANT30_CFi <- append(TENANT30_CFi, CFi)
            }
            CFr <- ref_year / ((86400 * 365 * 0.7 * Qr) / (1000000 * area))
            radek <-  list(grdc.no, river, station, country, lat, lon, as.character(od), as.character(do), napln_stat$V1, Qmi$V1, Qr, metoda, TENANT30_CFi, CFr, zprava)
          }
        } else {
          
          message(paste0("GRDC NO. ", grdc.no, " ma naplnenost mensi nez je pozadovany limit"))
          zprava <- paste0("GRDC NO. ", grdc.no, " ma naplnenost mensi nez je pozadovany limit")
          radek <- radek <- c(grdc.no, river, station, country, lat, lon, as.character(od), as.character(do),
                              rep(x = "NA", 39), zprava)
        }
      }
      
      radek <- unlist(radek)
      names(radek) <- c("GRDC-No", "River", "Station", "Country", "Latitude", "Longitude", "Od", "Do",
                        "Nap(1)", "Nap(2)", "Nap(3)", "Nap(4)", "Nap(5)", "Nap(6)", "Nap(7)", "Nap(8)",
                        "Nap(9)", "Nap(10)", "Nap(11)", "Nap(12)",
                        "Qm(1)", "Qm(2)", "Qm(3)", "Qm(4)", "Qm(5)", "Qm(6)", "Qm(7)", "Qm(8)", 
                        "Qm(9)", "Qm(10)", "Qm(11)", "Qm(12)", "Qr", "metoda vypoctu", "CF(1)", "CF(2)", 
                        "CF(3)", "CF(4)", "CF(5)", "CF(6)", "CF(7)", "CF(8)", "CF(9)", "CF(10)", "CF(11)", 
                        "CF(12)", "CF(r)", "Chyba")
      
    } else {
      
      ##### CALCULATED ####
      dta[Calculated < 0, Calculated:= NA]
      dta[Flag == 99, Calculated:= NA ] 
      dta <- dta[YYYY.MM.DD %between% c(od, do),]
      
      ##### NO DATA ####
      if(nrow(dta) < 1){
        
        message(paste0("Pozadovane obdobi neobsahuje data pro GRDC NO. ", grdc.no))
        zprava <- paste0("Pozadovane obdobi neobsahuje data pro GRDC NO. ", grdc.no)
        radek <- c(grdc.no, river, station, country, lat, lon, as.character(od), as.character(do),
                   rep(x = "NA", 39), zprava)
        
      } else {
        napln <- dta[,sum(is.na(Calculated)) / .N, by = .(year(YYYY.MM.DD), month(YYYY.MM.DD))]
        napln$V1 <- abs(napln$V1 - 1)
        napln_stat <- napln[, mean(V1), by = month]
        #print(napln$V1)
        
        ##### DATA ####
        if (all(napln_stat$V1 <= 1) & all(napln_stat$V1 > naplnenost)){
          if (all(napln_stat$V1 < 1)) {
            message(paste0("GRDC NO. ", grdc.no, " ma naplnenost < 100%"))
            zprava <- paste0("GRDC NO. ", grdc.no, " ma naplnenost < 100%")
          } else {
            message(paste0("GRDC NO. ", grdc.no, " OK"))
            zprava <- NA
          }
          
          Qmi <- dta[, mean(Calculated, na.rm = T), by = month(YYYY.MM.DD)]
          Qr <- dta[, mean(Calculated, na.rm = T), by = year(YYYY.MM.DD)]
          Qr <- Qr[, mean(V1)]
          
          ###### METODA FWUA #######
          if (metoda == "FWUA"){
            
            ref_year <- 1
            ref_mon <- 1 / 12
            
            FWUA_CFi <- c()
            for (i in 1:12){
              CFi <- ref_mon / ((86400 * days_in_month(i) * Qmi$V1[i]) / (1000000 * area))
              FWUA_CFi <- append(FWUA_CFi, CFi)
            }
            CFr <- ref_year / ((86400 * 365 * Qr) / (1000000 * area))
            radek <-  list(grdc.no, river, station, country, lat, lon, as.character(od), as.character(do), napln_stat$V1, Qmi$V1, Qr, metoda, FWUA_CFi, CFr, zprava)
            
            ####### METODA TENANT30 ######    
          } else {
            
            ref_year <- 0.1632
            ref_mon <- 0.0136
            
            TENANT30_CFi <- c()
            for (i in 1:12){
              CFi <- ref_mon / ((86400 * days_in_month(i) * (Qmi$V1[i] - (0.3 * Qr))) / (1000000 * area))
              TENANT30_CFi <- append(TENANT30_CFi, CFi)
            }
            CFr <- ref_year / ((86400 * 365 * 0.7 * Qr) / (1000000 * area))
            radek <-  list(grdc.no, river, station, country, lat, lon, as.character(od), as.character(do), napln_stat$V1, Qmi$V1, Qr, metoda, TENANT30_CFi, CFr, zprava)
          }
        } else {
          
          message(paste0("GRDC NO. ", grdc.no, " ma naplnenost mensi nez je pozadovany limit"))
          zprava <- paste0("GRDC NO. ", grdc.no, " ma naplnenost mensi nez je pozadovany limit")
          radek <- radek <- c(grdc.no, river, station, country, lat, lon, as.character(od), as.character(do),
                              rep(x = "NA", 39), zprava)
        }
      }
      
      radek <- unlist(radek)
      names(radek) <- c("GRDC-No", "River", "Station", "Country", "Latitude", "Longitude", "Od", "Do",
                        "Nap(1)", "Nap(2)", "Nap(3)", "Nap(4)", "Nap(5)", "Nap(6)", "Nap(7)", "Nap(8)",
                        "Nap(9)", "Nap(10)", "Nap(11)", "Nap(12)",
                        "Qm(1)", "Qm(2)", "Qm(3)", "Qm(4)", "Qm(5)", "Qm(6)", "Qm(7)", "Qm(8)", 
                        "Qm(9)", "Qm(10)", "Qm(11)", "Qm(12)", "Qr", "metoda vypoctu", "CF(1)", "CF(2)", 
                        "CF(3)", "CF(4)", "CF(5)", "CF(6)", "CF(7)", "CF(8)", "CF(9)", "CF(10)", "CF(11)", 
                        "CF(12)", "CF(r)", "Chyba")
      
    }
    tab <- rbind(tab, radek)
  }
  write.table(x = tab, sep = ";", file = output, quote = F, row.names = F)
  if (var == F) {message("Vysledky neulozeny v promenne")} else {return(data.frame(tab))} 
}
