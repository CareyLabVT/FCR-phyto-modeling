##### HELPER FUNCTIONS #####
#' Finds all the nitrogen entries that need converting & converts them. 
#' 
#' @param inflowFile The full set of dates that we have data for
#' @param coefficients The slope parameters for that particular data set
#' @param medianDOC The median DOC value being used to estimate
#' @param medianPOC The median POC value being used to estimate
#' @param staticFlag Whether POC should be interpreted from static median
#' @return The inflow file with POC & DOC columns
formPOCDOCFiles <- function(inflowFile, coefficients, medianDOC, medianPOC, staticFlag) {
  datesNeeded = inflowFile$time
  pocReturned = c(rep(medianPOC, length(datesNeeded)))
  docReturned = c(rep(medianDOC, length(datesNeeded))) # fill the file with an identical value 
  if (!staticFlag) {
    for (g in 1:length(datesNeeded)) {
      # relationship is in ft3 / s - 35.3145 * m3 = ft3
      pocReturned[g] = inflowFile$FLOW[g] * coefficients['x'] + coefficients['(Intercept)']
      #if (datesNeeded[g] >= pocData) {
      # we can add in observational? 
      #}
    }
  }
  inflowFile$POC = pocReturned
  inflowFile$DOC = docReturned
  return(inflowFile)
}

#' 
#' Finds all the nitrogen entries that need converting & converts them. 
#' 
#' @param inflowFile The full set of dates that we have data for
#' @param coefficients The slope parameters for that particular data set
#' @param nitrogenData The USGS nitrogen data for this particular site
#' @param logFlag A parameter to differentiate between the logged relationship
#'                for AMM vs unlogged for NIT, by default false
formNitrogenFiles <- function(inflowFile, coefficients, nitrogenData, logFlag) {
  if (missing(nitrogenData)) {
    nitrogenData = NA
  }
  if (missing(logFlag)) {
    logFlag = FALSE
  }
  if (ncol(nitrogenData) == 1 || is.null(ncol(nitrogenData))) {
    nitrogenValues = nitrogenData
  } else {
    nitrogenValues = nitrogenData$value
  }
  # The inflow file contains all the dates you need in 'time' column
  datesNeeded = inflowFile$time
  vectorReturned = c(rep(NA, length(datesNeeded)))
  counter = 1
  daysofyear = (as.POSIXlt(inflowFile$time, origin = '1970-01-01')$yday)
  for (j in 1:length(datesNeeded)) {
    if (is.na(nitrogenData) || 
        as.character(nitrogenData$datetime[counter]) != as.character(datesNeeded[j])) {
      if (length(coefficients) == 2) {
        coeff = coefficients['x'] 
        if (is.na(coeff)) {
          coeff = coefficients['xalt'] 
        }
        vectorReturned[j] = exp(log(inflowFile$FLOW[j] * 35.3147) * coeff
                                + coefficients['(Intercept)']) # convert m3 to ft3 
      } else {
        coeff = coefficients['x'] 
        coeff2 = coefficients['x2']
        if (is.na(coeff)) {
          coeff = coefficients['xalt'] 
          coeff2 = coefficients['x2alt']
        }
        vectorReturned[j] = ((daysofyear[j]) * coeff +
                               (daysofyear[j] * daysofyear[j]) * coeff2
                             + coefficients['(Intercept)']) # quadratics not logged!
        if (logFlag) {
          vectorReturned[j] = exp(vectorReturned[j])
        }
      }
    } else {
      vectorReturned[j] = nitrogenValues[counter] #nitrogenData$value[counter]
      counter = counter + 1
      # print('picked')
    }
  }
  return (vectorReturned)
}

#' 
#' Finds all the nitrogen entries that need converting & converts them. 
#' 
#' @param inflowFile The full set of dates that we have data for
#' @param coefficients The slope parameters for that particular data set
#' @param nitrogenData The USGS nitrogen data for this particular site
#' @param logFlag A parameter to differentiate between the logged relationship
#'                for AMM vs unlogged for NIT, by default false
validateNitrogenRelation <- function(inflowFile, coefficients, nitrogenData, logFlag) {
  # The inflow file contains all the dates you need in 'time' column
  if (missing(nitrogenData)) {
    
  } else {
    datesNeeded = nitrogenData$datetime
  }
  if (missing(logFlag)) {
    logFlag = FALSE
  }
  vectorReturned = c(rep(NA, length(datesNeeded)))
  counter = 1
  daysofyear = (as.POSIXlt(nitrogenData$datetime, origin = '1970-01-01')$yday)
  j = 1
  while (j <= length(datesNeeded) && counter <= nrow(inflowFile)) {
    if (as.character(datesNeeded[j]) == as.character(inflowFile$time[counter])) {
      if (length(coefficients) == 2) {
        coeff = coefficients['x'] 
        if (is.na(coeff)) {
          coeff = coefficients['xalt'] 
        }
        vectorReturned[j] = exp(log(inflowFile$FLOW[j] * 35.3147) * coeff
                                + coefficients['(Intercept)']) # convert m3 to ft3 
      } else {
        coeff = coefficients['x'] 
        coeff2 = coefficients['x2']
        if (is.na(coeff)) {
          coeff = coefficients['xalt'] 
          coeff2 = coefficients['x2alt']
        }
        vectorReturned[j] = ((daysofyear[j]) * coeff +
                               (daysofyear[j] * daysofyear[j]) * coeff2
                             + coefficients['(Intercept)']) # quadratics not logged!
        if (logFlag) {
          vectorReturned[j] = exp(vectorReturned[j])
        }
      }
    } else {
      j = j - 1
      counter = counter + 1
    }
    j = j + 1
  }
  return (vectorReturned)
}

#'
#'Splits the data into its constituent parts based on the starting and stopping
#'points of the siteIDs
#'
#'@param TPdata The raw USGS data 
#'              NOTE: MUST have column name for site_no
#'@param siteIDs The potential site IDs 
#'@return A matrix in the following format: 
#'             Starting Index (row)  |  Ending Index (row)
#'Site ID # 1
#'  ...
#'Site ID # n
#' For some (n) entries of site IDs in TP data
splitData <- function(TPdata, siteIDs) {
  lengthPossibilities = suppressWarnings(unique(na.exclude(as.numeric(as.character(TPdata$site_no)))))
  outputMatrix = matrix(0, ncol = 2, nrow = length(lengthPossibilities))
  j = 1;
  while (j < nrow(TPdata)) {
    currentSite = suppressWarnings(as.numeric(as.character(TPdata$site_no[j])))
    colTog = NA
    if (!is.na(currentSite)) {
      for (p in 1:length(lengthPossibilities)) {
        if (lengthPossibilities[p] == currentSite) {
          colTog = p; break; 
        }
      }
    }
    if (!is.na(colTog)) {
      outputMatrix[colTog, 1] = j; 
      checkSite = as.numeric(as.character(TPdata$site_no[j]))
      while (!is.na(currentSite) && !is.na(checkSite) && checkSite == currentSite) {
        j = j + 1; 
        checkSite = as.numeric(as.character(TPdata$site_no[j]))
      }
      outputMatrix[colTog, 2] = j; 
    }
    j = j + 1; 
  }
  return(outputMatrix)
}

#'
#' Fixes TP data so that it only has the time period needed
#' 
#' @param TPdata The original TP data 
#'               NOTE: should have 'datetime' col
#' @param startDate The starting date needed
#' @param endDate The ending date needed
#' @return The new TP data
#' 
narrowTimePeriod <- function(TPdata, startDate, endDate) {
  starterIndex = enderIndex = NA;
  for (g in 1:nrow(TPdata)) {
    if (!is.na(as.character(TPdata$datetime[g])) &&
        (as.character(TPdata$datetime[g]) != "") && 
        as.POSIXct(as.character(TPdata$datetime[g]), format = '%m/%d/%Y') == startDate) {
      starterIndex = g;
      break; 
    }
  }
  for (l in nrow(TPdata):1) {
    if (!is.na(as.character(TPdata$datetime[l])) && 
        (as.character(TPdata$datetime[l]) != "") &&
        as.POSIXct(as.character(TPdata$datetime[l]), format = '%m/%d/%Y') == endDate) {
      enderIndex = l;
      break; 
    }
  }
  if (!is.na(starterIndex) && !is.na(enderIndex)) {
    return (TPdata[g:l,])
  } else {
    return ('Error: either starting or ending date not located.')
  }
}

#'
#' Depending on whether user set calcRMSEs flag to
#' true, calculates and displays RMSEs for present 
#' data set (results in much plotting output).
#' 
#' @param real The observed data in matrix form
#' @param modeled The modeled data in matrix form
#'
calculateRMSEs <- function(real,modeled) {
  first = TRUE; firstfirst = TRUE
  #for (g in 3:ncol(modeled)) {
  RMSE = sqrt(((real - modeled)^2))
  assign(paste0("RMSE_", colnames(modeled)[g]), RMSE)
  if (first) {
    RMSEs = RMSE; first = FALSE
  } else {
    RMSEs = c(RMSEs, RMSE)
  }
  #}
  if (firstfirst) {
    plot(RMSEs, type = 'l', xlab = "time", ylab = "RMSE", main = paste0("RMSEs across time"), col = "red", ylim = c(1,200))
    firstfirst = FALSE
  } else if (d %% 2 == 0) {
    lines(RMSEs ~ c(1:9, 9.3), col = cols[d])
  }
}
##### IMPORTANT INFORMATION #####

# Relevant site IDs for the NWIS data 
siteIDs =  c('05427850', # Yahara at Highway 113
             '05427718', # Yahara at Windsor
             '05427948') # Pheasant Branch

# 625 is NH3 + organic N - is this just ammonia or ammonium + ammonia? 
# because 608 is ammonia + ammonium but labeled as ammonia. 

siteID <- c("5427850","5427718","5427948")
siteNames = c('highway', 'windsor', 'pheasant') # MUST correspond with provided siteIDs. 

# Relevant codes for nutrient data 
pCodes <- c('00665', # Total phosphorus (mg / L P) "Phosphorus, water, unfiltered, mg/L as phosphorus"
            '00608', # Ammonia + ammonium (NH3 + NH4) (mg / L N) 
            # "Ammonia (NH3 + NH4+), water, filtered, milligrams per liter as nitrogen" (may not say NH3 + NH4)
            '00631', # Nitrate + nitrite (mg / L N)
            # "Nitrate plus nitrite, water, filtered, milligrams per liter as nitrogen"
            '00625'  # Ammonia plus organic nitrogen (mg / L N)
            # "Ammonia plus organic nitrogen, water, unfiltered, milligrams per liter as nitrogen"
)
##### PHOSPHORUS FRACTIONATION CONSTANTS #####

# Phosphorus: these are decimal % of TP. 
# Note: these first set are divided by two to account for non-aqueous fraction. 
# need the P multiplier - multiply by 2 first because P_ads is half 
pMultiplier = 2 # Multiplied by 2 to accommodate for USGS data format (above)

FRPfrac_winds = 0.23485 * pMultiplier   # FRP fraction of TP (0.4743 of USGS TP)
DOPfrac_winds = 0.195   * pMultiplier   # OGM_dop fraction of TP (0.39 of USGS TP)
POPfrac_winds = 0.07015 * pMultiplier   # OGM_pop fraction of TP (0.1357 of USGS TP)
adsfrac_winds = 0.5     * pMultiplier   # PHS_frp_ads fraction of TP (1 of USGS TP)

FRPfrac_pheas = 0.15075 * pMultiplier   # FRP fraction of TP (0.3015 of USGS TP)
DOPfrac_pheas = 0.195   * pMultiplier   # OGM_dop fraction of TP (0.39 of USGS TP)
POPfrac_pheas = 0.15425 * pMultiplier   # OGM_pop fraction of TP (0.3085 of USGS TP)
adsfrac_pheas = 0.5     * pMultiplier   # PHS_frp_ads fraction of TP (1 of USGS TP)

##### PHOSPHORUS AND NITROGEN CONVERSION FACTORS ######
convertP = 1000 / 30.97 # This converts P from mg / L to mmol / m3
# There are 1000 m3 in 1 L and the molar mass
# of phosphorus is 30.97 g/mol.

convertN = 1000 / 14.01 # This converts N from mg / L to mmol / m3
# There are 1000 m3 in 1 L and the molar mass 
# of nitrogen is 14.01 g/mol. 

##### READ IN NWIS DATA #####
windsorData = read.csv('./NP_comparison/Mendota/USGS_data/NWIS_Windsor.csv')
pheasantData = read.csv('./NP_comparison/Mendota/USGS_data/NWIS_Pheasant.csv')

TPdata = cbind(c(as.character(windsorData$agency_cd), as.character(pheasantData$agency_cd)),
               c(windsorData$site_no, pheasantData$site_no),
               c(as.character(windsorData$datetime), as.character(pheasantData$datetime)))
for (j in 1:max(ncol(windsorData), ncol(pheasantData))) {
  if (length(grep(pCodes[1], colnames(windsorData)[j])) > 0 ) {
    colnames(windsorData)[j] = 'X154888_00665_00003'
  }
  if (length(grep(pCodes[1], colnames(pheasantData)[j])) > 0 ) {
    colnames(pheasantData)[j] = 'X154888_00665_00003'
  }
}
TPdata = cbind(TPdata, c(windsorData$X154888_00665_00003,
                         pheasantData$X154888_00665_00003))
colnames(TPdata) = c('agency_cd', 'site_no', 'datetime', 'X154888_00665_00003')
TPdata = data.frame(TPdata)

##### CHECK FOR GAPS IN PROVIDED TP DATA #####
startingDate = as.POSIXct('2003-11-08')# first date for model run
endingDate = as.POSIXct('2014-12-31')  # last date for model run 

neededData = splitData(TPdata = TPdata, siteIDs = siteID)
allGaps = matrix(NA, nrow = nrow(TPdata), ncol = nrow(neededData)) 
maxGaps = 0
for (c in 1:nrow(neededData)) {
  
  dates = as.POSIXct(as.character(TPdata$datetime[neededData[c,1]:neededData[c,2]]), format = '%m/%d/%Y')
  g = 1; starterIndex = NA;
  while (dates[g] <= startingDate) {
    if (dates[g] == startingDate) {
      starterIndex = g;
    }
    g = g + 1
  }
  
  #gaps = c(rep(0, ncol(TPdata))); 
  h = 1; z = starterIndex; numGaps = 0;
  y = starterIndex + neededData[c,1]; gaps = c(as.POSIXct('1900-01-01'));
  if (!is.na(starterIndex)) {
    while (z < length(dates) && dates[z] <= endingDate) {
      if (dates[z] > (startingDate + ((60 * 60 * 24) * (h - 1)))) {
        gaps = c(gaps, dates[z])
        numGaps = numGaps + 1
        h = h + 1;
      } else if (is.na(TPdata$X154888_00665_00003[y])) {
        gaps = c(gaps, dates[z])
        numGaps = numGaps + 1
      }
      h = h + 1; z = z + 1; y = y + 1;
    }
    if (numGaps > maxGaps) {
      maxGaps = numGaps
    }
    if (numGaps > 0) {
      allGaps[1:numGaps,c] = gaps[2:length(gaps)]
    }
  }
}
allGaps = allGaps[1:maxGaps,]

##### REPLACE DATA IN DRIVER FILE WITH NEW P DATA #####
# Any gaps less than 14 days filled by linear interpolation

neededData = splitData(TPdata = TPdata, siteIDs = siteID) # This splits your data by site; the
# rows of the returned dataframe are where 
# your site data is located. 
startingDate = as.POSIXct('2003-11-08')# first data date you seek
endingDate = as.POSIXct('2014-12-31')  # last date needed 
siteNums = as.numeric(as.character(unique(TPdata$site_no)))
siteNamesData = c(rep(0, length(siteNums))) # these are sites we have data from, using their numbers in USGS spreadsheet
for (i in 1:length(siteNums)) {
  for (k in 1:length(siteIDs)) {
    if (as.numeric(siteIDs[k]) == siteNums[i]) {
      siteNamesData[i] = siteNames[k] # siteNames and siteIDs should match up, from above delineation
    }
  }
}
for (j in 1:nrow(neededData)) {
  assign(paste0(siteNamesData[j], '_TPdata'), TPdata[neededData[j,1]:neededData[j,2],]) # all columns, needed rows
  assign(paste0(siteNamesData[j], '_TPdata'), 
         narrowTimePeriod(eval(as.name(paste0(siteNamesData[j], '_TPdata'))), startingDate, endingDate))
  # can replace w/ siteIDs vector for number instead
}

pheasant = read.csv('./NP_comparison/Mendota/GLM/inflow_Pheasant.csv', header = TRUE) 

# Only run this once you know pheasant_TPdata exists from above loop
colnames(pheasant_TPdata)[4] = c('TP')
TPorig = as.numeric(as.character(pheasant_TPdata$TP))
pheasant$PHS_frp = c(TPorig * FRPfrac_pheas * convertP)
pheasant$OGM_dop = c(TPorig * DOPfrac_pheas * convertP)
pheasant$OGM_pop = c(TPorig * POPfrac_pheas * convertP)
pheasant$PHS_frp_ads = c(TPorig * adsfrac_pheas * convertP)

highway = read.csv('./NP_comparison/Mendota/GLM/inflow_YaharaHighway.csv', header = TRUE) 

# Only run this once you know windsor_TPdata exists from above loop
colnames(windsor_TPdata)[4] = c('TP')
TPorig = as.numeric(as.character(windsor_TPdata$TP))
highway$PHS_frp = c(TPorig * FRPfrac_winds * convertP) 
highway$OGM_dop = c(TPorig * DOPfrac_winds * convertP)
highway$OGM_pop = c(TPorig * POPfrac_winds * convertP)
highway$PHS_frp_ads = c(TPorig * adsfrac_winds * convertP)

###### NITROGEN SECTION #####
pacman::p_load(plyr, tidyverse) # Load packages

# Pull in USGS gauge data 
pheas <- read.csv('./NP_comparison/Mendota/USGS_data/NWIS_Pheasant.csv')
winds <- read.csv('./NP_comparison/Mendota/USGS_data/NWIS_Windsor.csv')

## Subset dataframes to retain gauge ID, datetime, and N variables and validity codes ####   
# Ammonia (00608); Ammonia + organic N (00625); Nitrate + nitrite (00631) 
neededEntries = c("site", "datetime", "00060", "00608", "00625", "00631")
neededEntriesHuman = c("site", "datetime", "flow", "amm", "ammorg", "nit")
first = TRUE; first2 = TRUE;
for (k in 1:length(neededEntries)) { # changing this to be generalizable - lost prior changes
  for (g in 1:max(ncol(pheas), ncol(winds))) {
    if (g <= ncol(pheas) &&
        length(grep(neededEntries[k], colnames(pheas)[g])) > 0 && 
        length(grep("cd", colnames(pheas)[g])) == 0) {
      if (first) {
        pheasNew = pheas[,g]
        colNamesPheas = (neededEntriesHuman)[k]
        first = FALSE
      } else {
        pheasNew = cbind(pheasNew, pheas[,g]) 
        colNamesPheas = c(colNamesPheas, (neededEntriesHuman)[k])
      }
    }
    
    if (g <= ncol(winds) &&
        length(grep(neededEntries[k], colnames(winds)[g])) > 0 && 
        length(grep("cd", colnames(winds)[g])) == 0) {
      if (first2) {
        windsNew = winds[,g]
        colNamesWinds = (neededEntriesHuman)[k]
        first2 = FALSE
      } else {
        windsNew = cbind(windsNew, winds[,g]) 
        colNamesWinds = c(colNamesWinds, (neededEntriesHuman)[k])
      }
    }
  }
}
windsNew = data.frame(windsNew)
pheasNew = data.frame(pheasNew)
colnames(windsNew) = colNamesWinds
colnames(pheasNew) = colNamesPheas
windsNew$datetime = winds$datetime; pheasNew$datetime = winds$datetime
# pheas <- select(pheas, matches('site_no|datetime|00060|00608|00625|00631'))
# names(pheas)[1:10] <- c('site','datetime','flow','flow_cd','nit','nit_cd','ammorg','ammorg_cd','amm','amm_cd')
# pheas <- select(pheas, -contains('_cd'))
# 
# windsor <- select(windsor, matches('site_no|datetime|00060|00608|00625|00631')) 
# names(windsor)[1:10] <- c('site','datetime','flow','flow_cd','ammorg','ammorg_cd','amm','amm_cd','nit','nit_cd')
# windsor <- select(windsor, -contains('_cd'))
namesSites = c("pheas", "winds")
pheas = pheasNew; winds = windsNew

flows <- full_join(pheas, winds)
flows$site <- factor(flows$site, levels=c('5427948', '5427718'), labels=c('Pheas','Winds'))
flows$datetime <- lubridate::mdy(flows$datetime)
flows$orgPON <- (flows$ammorg - flows$amm)
flows <- gather(flows, nutrient, value, -site, -datetime, -flow, na.rm=T)

# Plots for each nutrient Q vs. day of year; N vs. day of year; N vs. Q
par(mfrow = c(1,3))
# pleasant
plot(as.POSIXlt(flows$datetime[flows$site == "Pheas"], format = '%Y-%m-%d')$yday, 
     flows$flow[flows$site == "Pheas"], pch = 20, col = 'blue', 
     ylab = 'Q', xlab = 'day of year', main = 'Pheasant')
plot(as.POSIXlt(flows$datetime[flows$site == "Pheas" & flows$nutrient == 'nit'], format = '%Y-%m-%d', origin = "1970-01-01")$yday, 
     flows$value[flows$site == "Pheas" & flows$nutrient == 'nit'], pch = 20, col = 'blue', 
     ylab = 'Nitrate', xlab = 'day of year')
plot(flows$flow[flows$site == "Pheas" & flows$nutrient == 'nit'],
     flows$value[flows$site == "Pheas" & flows$nutrient == 'nit'], pch = 20, col = 'blue', 
     ylab = 'Nitrate', xlab = 'Q')


par(mfrow = c(1,3))
plot(as.POSIXlt(flows$datetime[flows$site == "Winds"], format = '%Y-%m-%d')$yday, 
     flows$flow[flows$site == "Winds"], pch = 20, col = 'red', 
     ylab = 'Q', xlab = 'day of year', main = 'Winds')
plot(as.POSIXlt(flows$datetime[flows$site == "Winds" & flows$nutrient == 'nit'], format = '%Y-%m-%d', origin = "1970-01-01")$yday, 
     flows$value[flows$site == "Winds" & flows$nutrient == 'nit'], pch = 20, col = 'red', 
     ylab = 'Nitrate', xlab = 'day of year')
plot(flows$flow[flows$site == "Winds" & flows$nutrient == 'nit'],
     flows$value[flows$site == "Winds" & flows$nutrient == 'nit'], pch = 20, col = 'red', 
     ylab = 'Nitrate', xlab = 'Q')

# Same plots with logged flow-nitrate relations
nutrients = unique(flows$nutrient)
for (g in 1:length(nutrients)) {
  par(mfrow = c(1,3))
  # pleasant
  plot(as.POSIXlt(flows$datetime[flows$site == "Pheas"], format = '%Y-%m-%d')$yday, 
       flows$flow[flows$site == "Pheas"], pch = 20, col = 'blue', 
       ylab = 'Q', xlab = 'day of year', main = 'Pheasant')
  plot(as.POSIXlt(flows$datetime[flows$site == "Pheas" & flows$nutrient == nutrients[g]], format = '%Y-%m-%d', origin = "1970-01-01")$yday, 
       flows$value[flows$site == "Pheas" & flows$nutrient == nutrients[g]], pch = 20, col = 'blue', 
       ylab = paste0(nutrients[g]), xlab = 'day of year')
  plot(log(flows$flow[flows$site == "Pheas" & flows$nutrient == nutrients[g]]),
       log(flows$value[flows$site == "Pheas" & flows$nutrient == nutrients[g]]), pch = 20, col = 'blue', 
       ylab = paste0("log ", nutrients[g]), xlab = 'Log Q')
  abline(lm(log(flows$value[flows$site == "Pheas" & flows$nutrient == nutrients[g]]) ~
              log(flows$flow[flows$site == "Pheas" & flows$nutrient == nutrients[g]])))
  
  
  par(mfrow = c(1,3))
  plot(as.POSIXlt(flows$datetime[flows$site == "Winds"], format = '%Y-%m-%d')$yday, 
       flows$flow[flows$site == "Winds"], pch = 20, col = 'red', 
       ylab = 'Q', xlab = 'day of year', main = 'Winds')
  plot(as.POSIXlt(flows$datetime[flows$site == "Winds" & flows$nutrient == nutrients[g]], format = '%Y-%m-%d', origin = "1970-01-01")$yday, 
       flows$value[flows$site == "Winds" & flows$nutrient == nutrients[g]], pch = 20, col = 'red', 
       ylab = nutrients[g], xlab = 'day of year')
  plot(log(flows$flow[flows$site == "Winds" & flows$nutrient == nutrients[g]]),
       log(flows$value[flows$site == "Winds" & flows$nutrient == nutrients[g]]), pch = 20, col = 'red', 
       ylab = paste0("log ", nutrients[g]), xlab = 'Log Q')
  abline(lm(log(flows$value[flows$site == "Winds" & flows$nutrient == nutrients[g]]) ~
              log(flows$flow[flows$site == "Winds" & flows$nutrient == nutrients[g]])))
}

# Compare Pheasant and Windsor nitrate data
pheasantDates = flows$datetime[flows$site == "Pheas" & flows$nutrient == nutrients[3]]
windsorDates = flows$datetime[flows$site == "Winds" & flows$nutrient == nutrients[3]]
matches = matrix(0, ncol = 3, nrow = min(flows$datetime[flows$site == "Winds" & flows$nutrient == nutrients[3]],
                                         flows$datetime[flows$site == "Pheas" & flows$nutrient == nutrients[3]]))
numMatches = 1
colnames(matches) = c("Date", "Pheas Nit", "Winds Nit")
# Do not run this unless you need it - clunky
for (g in 1:length(pheasantDates)) {
  for (f in 1:length(windsorDates)) {
    if (pheasantDates[g] == windsorDates[f]) {
      matches[numMatches, 2] = flows$value[flows$site == "Pheas" & flows$nutrient == nutrients[3]][g]
      matches[numMatches, 3] = flows$value[flows$site == "Winds" & flows$nutrient == nutrients[3]][f]
      matches[numMatches, 1] = pheasantDates[g]
      numMatches = numMatches + 1
      break
    }
  }
}
matches = data.frame(matches)
plot(matches$Pheas.Nit[1:numMatches-1], matches$Winds.Nit[1:numMatches-1], pch = 20, ylim = c(2,6), xlim = c(2,6), col = 'blue', ylab = 'Windsor Nitrate', xlab = 'Pheasant Nitrate')
abline(lm(matches$Winds.Nit[1:numMatches-1] ~ matches$Pheas.Nit[1:numMatches-1]))
summary(lm(matches$Winds.Nit[1:numMatches-1] ~ matches$Pheas.Nit[1:numMatches-1]))
# This is the linear relationship between the log of flow and the log of the
# concentration value for nitrate 
print(summary(lm(log(flows$value[flows$nutrient == 'nit']) 
                 ~ log(flows$flow[flows$nutrient == 'nit']))))

# Same for amm organic 
print(summary(lm(log(flows$value[flows$nutrient == 'ammorg']) 
                 ~ log(flows$flow[flows$nutrient == 'ammorg']))))

# Same for amm 
print(summary(lm(log(flows$value[flows$nutrient == 'amm']) 
                 ~ log(flows$flow[flows$nutrient == 'amm']))))

# Same for organic alone
print(summary(lm(log(flows$value[flows$nutrient == 'ammorg'] - (flows$value[flows$nutrient == 'amm']) )
                 ~ log(flows$flow[flows$nutrient == 'amm']))))

# Same relationships, Windsor & Pheasant separated

nutrients = unique(flows$nutrient)
for (g in 1:length(nutrients)) {
  for (k in 1:length(namesSites)) {
    if (nutrients[g] != 'orgPON') {
      print(summary(lm(log(flows$value[flows$nutrient == nutrients[g] & toupper(flows$site) == toupper(namesSites[k])] ) 
                       ~ log(flows$flow[flows$nutrient == nutrients[g] & toupper(flows$site) == toupper(namesSites[k])]))))
    }
  }
}

for (j in 1:length(nutrients)) {
  if (nutrients[j] != 'orgPON') {
    x = log(flows$flow[flows$nutrient == nutrients[j]])
    y = log(flows$value[flows$nutrient == nutrients[j]])
    assign(paste0(nutrients[j], '_model'), lm(y ~ x))
    assign(paste0(nutrients[j], '_coefficients'), lm(y ~ x)$coefficients)
    for (k in 1:length(namesSites)) {
      x = log(flows$flow[flows$nutrient == nutrients[j] & toupper(flows$site) == toupper(namesSites[k])])
      y = log(flows$value[flows$nutrient == nutrients[j] & toupper(flows$site) == toupper(namesSites[k])])
      assign(paste0(nutrients[j], '_', namesSites[k], '_model'), lm(y ~ x))
      assign(paste0(nutrients[j], '_', namesSites[k], '_coefficients'), lm(y ~ x)$coefficients)
      evaluateQuads = FALSE # no longer evaluating quadratics
      if (evaluateQuads && (namesSites[k] == 'pheas' && (nutrients[j] == 'nit' || nutrients[j] == 'amm'))) {
        x = (as.POSIXlt(flows$datetime[flows$nutrient == nutrients[j] & toupper(flows$site)
                                       == toupper(namesSites[k])], origin = '1970-01-01')$yday)
        x2 = x * x 
        y = (flows$value[flows$nutrient == nutrients[j] & 
                           toupper(flows$site) == toupper(namesSites[k])])
        if (nutrients[j] == "amm") {
          y = log(y)
        }
        assign(paste0(nutrients[j], '_', namesSites[k], '_model'), lm(y ~ x + x2))
        assign(paste0(nutrients[j], '_', namesSites[k], '_coefficients'), lm(y ~ x + x2)$coefficients)
        
      }
    }
  }
}

# Pull out relationship for org specifically 
for (k in 1:length(namesSites)) {
  y = log(flows$value[flows$nutrient == 'ammorg' & toupper(flows$site) == toupper(namesSites[k])] 
          - (flows$value[flows$nutrient == 'amm' & toupper(flows$site) == toupper(namesSites[k])]))
  x = (as.POSIXlt(flows$datetime[flows$nutrient == "amm" & toupper(flows$site) == toupper(namesSites[k])]
                  , origin = '1970-01-01')$yday)
  x2 = x * x
  xalt = log(flows$flow[flows$nutrient == "amm" & toupper(flows$site) == toupper(namesSites[k])])
  x2alt = xalt * xalt
  evaluateQuads = FALSE
  if (!evaluateQuads || (k == 2 || k == 1)) { # we are no longer evaluating quadratics 
    x = log(flows$flow[flows$nutrient == "amm" & toupper(flows$site) == toupper(namesSites[k])])
    assign(paste0("org", '_', namesSites[k], '_model'), lm(y ~ x))
    assign(paste0("org", '_', namesSites[k], '_coefficients'), lm(y ~ x)$coefficients)
  } else {
    assign(paste0("org", '_', namesSites[k], '_model'), lm(y ~ x + x2))
    assign(paste0("org", '_', namesSites[k], '_coefficients'), lm(y ~ x + x2)$coefficients)
  }
}

validatedNutrients = c(0)
for (g in 1:length(nutrients)) {
  if (nutrients[g] != 'orgPON') {
    logFlag = FALSE
    if (nutrients[g] == 'amm') {
      logFlag = FALSE #TRUE # we are interpreting logFlag to always be false. 
    }
    assign(paste0(nutrients[g], '_highway'), 
           formNitrogenFiles(highway, eval(as.name(paste0(nutrients[g], '_winds_coefficients'))), 
                             flows[flows$nutrient == nutrients[g] & flows$site == 'Winds',],
                             logFlag) * convertN)
    assign(paste0(nutrients[g], '_pheasant'), 
           formNitrogenFiles(pheasant, eval(as.name(paste0(nutrients[g], '_pheas_coefficients'))), 
                             flows[flows$nutrient == nutrients[g] & flows$site == 'Pheas',],
                             logFlag) * convertN)
    
    # Validate with observational data 
    assign(paste0(nutrients[g], '_highway_validate'), 
           validateNitrogenRelation(highway, eval(as.name(paste0(nutrients[g], '_winds_coefficients'))), 
                                    flows[flows$nutrient == nutrients[g] & flows$site == 'Winds',],
                                    logFlag) * convertN)
    assign(paste0(nutrients[g], '_pheasant_validate'), 
           validateNitrogenRelation(pheasant, eval(as.name(paste0(nutrients[g], '_pheas_coefficients'))), 
                                    flows[flows$nutrient == nutrients[g] & flows$site == 'Pheas',],
                                    logFlag) * convertN)
    validatedNutrients = c(validatedNutrients, nutrients[g])
  }
}

# do organic separate - don't ever run any of these lines independently 
nitrogenData =  data.frame(cbind((flows$value[flows$nutrient == "ammorg" & flows$site == 'Pheas']
                                  - flows$value[flows$nutrient == "amm" & flows$site == 'Pheas']), 
                                 flows$datetime[flows$nutrient == "amm" & flows$site == 'Pheas']))
colnames(nitrogenData) = c('value', 'datetime')
nitrogenData$datetime = as.Date(nitrogenData$datetime, origin = "1970-01-01")
assign(paste0("org", '_pheasant'), 
       formNitrogenFiles(pheasant, eval(as.name(paste0("org", '_pheas_coefficients'))), 
                         nitrogenData) * convertN)
assign(paste0('org', '_pheasant_validate'), 
       validateNitrogenRelation(pheasant, eval(as.name(paste0('org_', 'pheas_coefficients'))), 
                                nitrogenData) * convertN)

nitrogenData = data.frame(cbind((flows$value[flows$nutrient == "ammorg" & flows$site == 'Winds']
                                 - flows$value[flows$nutrient == "amm" & flows$site == 'Winds']), 
                                flows$datetime[flows$nutrient == "amm" & flows$site == 'Winds']))
colnames(nitrogenData) = c('value', 'datetime')
nitrogenData$datetime = as.Date(nitrogenData$datetime, origin = "1970-01-01")
assign(paste0("org", '_highway'), 
       formNitrogenFiles(highway, eval(as.name(paste0("org", '_winds_coefficients'))), 
                         nitrogenData) * convertN)
assign(paste0('org', '_highway_validate'), 
       validateNitrogenRelation(highway, eval(as.name(paste0('org_', 'winds_coefficients'))), 
                                nitrogenData) * convertN)
validatedNutrients = c(validatedNutrients, 'org')

## PLOT VALIDATION DATA ## 
# nutrients = c(nutrients[1:(length(nutrients)-1)], "org")
par(mfrow = c(2,2))
for (g in 2:length(validatedNutrients)) {
  if (validatedNutrients[g] == "ammorg") {
    g = g + 1
  }
  # par(mfrow = c(1,1))
  nit_pheas_obs = (flows$value[flows$nutrient == validatedNutrients[g] & flows$site == 'Winds'] * convertN)
  if (validatedNutrients[g] == "org") {
    nit_pheas_obs = ((flows$value[flows$nutrient == "ammorg" & flows$site == 'Winds'] -
                        flows$value[flows$nutrient == "amm" & flows$site == 'Winds']) * convertN)
  }
  plot(eval(as.name(paste0(validatedNutrients[g], '_highway_validate'))), pch = 20, col = 'blue', 
       main = paste0('Validation Highway ', validatedNutrients[g]), 
       ylab = 'nutrient', xlab = 'Time')
  points(nit_pheas_obs, pch = 20, col = "red")
  legend("topright", c("Modeled", "Observed"), fill = c("blue", "red"))
  # calculateRMSEs(nit_pheas_obs, eval(as.name(paste0(validatedNutrients[g], '_highway_validate'))))
  
  # par(mfrow = c(1,1))
  nit_pheas_obs = (flows$value[flows$nutrient == validatedNutrients[g] & flows$site == 'Pheas'] * convertN)
  if (validatedNutrients[g] == "org") { # if org you need special combo
    nit_pheas_obs = ((flows$value[flows$nutrient == "ammorg" & flows$site == 'Pheas'] -
                        flows$value[flows$nutrient == "amm" & flows$site == 'Pheas']) * convertN)
  }
  plot(eval(as.name(paste0(validatedNutrients[g], '_pheasant_validate'))), pch = 20, col = 'blue', 
       main = paste0('Validation Pheasant ', validatedNutrients[g]), 
       ylab = 'nutrient', xlab = 'Time')
  points(nit_pheas_obs, pch = 20, col = "red")
  legend("topright", c("Modeled", "Observed"), fill = c("blue", "red"))
  # calculateRMSEs(nit_pheas_obs, eval(as.name(paste0(validatedNutrients[g], '_pheasant_validate'))))
}

## PLOTS OF RESULTING VALUES FOR FULL TIME PERIOD ####
par(mfrow = c(1,3))
plot(as.POSIXct(pheasant$time, format = '%Y-%m-%d'), nit_pheasant, type = 'p', pch = 20, 
     col = 'blue', xlab = 'Time', ylab = 'Nitrate Concentration (mmol/m3)', main = 'Pheasant Branch Nitrate')

plot(as.POSIXct(pheasant$time, format = '%Y-%m-%d'), amm_pheasant, type = 'p', pch = 20, 
     col = 'red', xlab = 'Time', ylab = 'Ammonium Concentration (mmol/m3)', main = 'Pheasant Branch Ammonium')

plot(as.POSIXct(pheasant$time, format = '%Y-%m-%d'), ammorg_pheasant, type = 'p', pch = 20, 
     col = 'green', xlab = 'Time', ylab = 'Amm Organic Concentration (mmol/m3)', main = 'Pheasant Branch Ammonium Org')

## SAME FOR HIGHWAY ##
par(mfrow = c(1,3))
plot(as.POSIXct(highway$time, format = '%Y-%m-%d'), nit_highway, type = 'p', pch = 20, 
     col = 'blue', xlab = 'Time', ylab = 'Nitrate Concentration (mmol/m3)', main = 'Highway Nitrate')

plot(as.POSIXct(highway$time, format = '%Y-%m-%d'), amm_highway, type = 'p', pch = 20, 
     col = 'red', xlab = 'Time', ylab = 'Ammonium Concentration (mmol/m3)', main = 'Highway Ammonium')

plot(as.POSIXct(highway$time, format = '%Y-%m-%d'), ammorg_highway, type = 'p', pch = 20, 
     col = 'green', ylab = 'Time', xlab = 'Amm Organic Concentration (mmol/m3)', main = 'Highway Ammonium Org')

plot(flows$datetime[flows$nutrient == nutrients[3] & toupper(flows$site) == toupper(namesSites[1])], 
     flows$value[flows$nutrient == nutrients[3] & toupper(flows$site) == toupper(namesSites[1])] * convertN, type = 'p', 
     col = 'red', ylab = 'Nitrate Conc', xlab = 'Flow', pch = 20)

## WRITE FINAL nitrate values #####
highway$NIT_nit = nit_highway
highway$NIT_amm = amm_highway
highway$OGM_pon = ammorg_highway
highway$OGM_don = ammorg_highway

##### CARBON ESTIMATION #####
carbonMeasurements = read.csv('./NP_comparison/Mendota/USGS_data/stream_weekly_carbon_ysi_v1.csv')
carbonMeasurements$CRatio <- carbonMeasurements$doc / carbonMeasurements$poc # DOC:POC ratio
pocModel = summary(lm(carbonMeasurements$poc ~ x))
DOCpheas <- median(subset(carbonMeasurements, sample_site =="Pheasant Branch")$doc)
DOChighway <- median(subset(carbonMeasurements, sample_site =="Yahara")$doc)
POCpheas <- median(subset(carbonMeasurements, sample_site =="Pheasant Branch")$poc)
POChighway <- median(subset(carbonMeasurements, sample_site =="Yahara")$poc)
x = carbonMeasurements$discharge[carbonMeasurements$sample_site == "Pheasant Branch"]
POCcoeffpheas = lm(carbonMeasurements$poc[carbonMeasurements$sample_site == "Pheasant Branch"] ~ x)$coefficients
x = carbonMeasurements$discharge[carbonMeasurements$sample_site == "Yahara"]
POCcoeffyahara = lm(carbonMeasurements$poc[carbonMeasurements$sample_site == "Yahara"] ~ x)$coefficients
newInflowPhea = formPOCDOCFiles(pheasant, POCcoeff, DOCpheas, POCpheas, staticFlag = TRUE)
newInflowYaha = formPOCDOCFiles(highway, POCcoeff, DOChighway, POChighway, staticFlag = TRUE)
newInflowPhea$time = as.POSIXct(strptime(newInflowPhea$time, format = '%Y-%m-%d'), format = '%Y-%m-%d %H:%M:%s')
newInflowYaha$time = as.POSIXct(strptime(newInflowYaha$time, format = '%Y-%m-%d'), format = '%Y-%m-%d %H:%M:%s')
write.csv(newInflowPhea, 'pheasant_inflow_experimental.csv', row.names = FALSE, quote = FALSE)
write.csv(newInflowYaha, 'highway_inflow_experimental.csv', row.names = FALSE, quote = FALSE)
# Investigate carbon relationship #####
# Preliminary tests indicate no information in DOC ~ Q relationship; will use static median for each gauge

# Preliminary tests indicate no information in DOC ~ Q relationship; using static median for each gauge
DOCpheas <- median(subset(carbonMeasurements, sample_site =="Pheasant Branch")$doc) # static median for Pheasant
DOChighway <- median(subset(carbonMeasurements, sample_site =="Yahara")$doc) # static median for Yahara

POCpheas <- median(subset(carbonMeasurements, sample_site =="Pheasant Branch")$poc) # static median for Pheasant
POChwy <- median(subset(carbonMeasurements, sample_site =="Yahara")$poc) # static median for Pheasant

# Plot POC ~ Discharge (used to establish relation) ####
PhRatio <- DOCpheas / POCpheas
HwyRatio <- DOChighway / POChwy

# Visualize DOC:POC ratio for Pheasant, Yahara inflows over time
pacman::p_load(cowplot, ggplot2)

logQratio <- ggplot(subset(carbonMeasurements, sample_site == "Pheasant Branch" | sample_site == "Yahara"), 
                    aes(y = log(CRatio), x = log(discharge), fill = sample_site)) + 
  geom_point(pch=22, size=4)+ geom_smooth(aes(group=sample_site), method='lm', color='black') +
  scale_y_continuous("log DOC:POC",limits=c(-1,4)) +
  scale_x_continuous("logQ (cfs)") + theme(legend.position='none')

logpoc <- ggplot(subset(carbonMeasurements, sample_site == "Pheasant Branch" | sample_site == "Yahara"), 
                 aes(y = log(poc), x = log(discharge), fill = sample_site)) + 
  geom_point(pch=22, size=4)+ scale_y_continuous("logPOC, mg/L",limits=c(-1,3)) +
  geom_smooth (aes(group=sample_site), method='lm', color='black') +
  theme(legend.position='none')

logdoc <- ggplot(subset(carbonMeasurements, sample_site == "Pheasant Branch" | sample_site == "Yahara"), 
                 aes(y = log(doc), x = log(discharge), fill = sample_site)) + 
  geom_point(pch=22, size=4)+ scale_y_continuous("logDOC, mg/L", limits=c(-1,3)) +
  geom_smooth (aes(group=sample_site), method='lm', color='black') +
  theme(legend.position=c(0,1), legend.justification=c(0,1), legend.title=element_blank())

plot_grid(logdoc, logpoc, logQratio, ncol=3, align='h')

ph <- ggplot(subset(carbonMeasurements, sample_site == "Pheasant Branch"), 
             aes(y = log(DOCpheas/poc), x = log(discharge), fill = sample_site)) + 
  geom_hline(yintercept= log(PhRatio), lty=2) +
  geom_point(aes(y=log(CRatio)), pch=22, fill='black', size=3.5) +
  geom_point(pch=22, size=4)+ geom_smooth(aes(group=sample_site), method='lm', color='black') +
  scale_y_continuous("log DOC:POC",limits=c(-1,4)) + ggtitle("Pheasant") +
  scale_x_continuous("logQ (cfs)") + theme(legend.position='none')

hwy <- ggplot(subset(carbonMeasurements, sample_site == "Yahara"), 
              aes(y = log(DOChighway/poc), x = log(discharge))) + 
  geom_hline(yintercept= log(HwyRatio), lty=2) +
  geom_point(aes(y=log(CRatio)), pch=22, fill='black', size=3.5) +
  geom_point(pch=22, size=4, fill = '#00BFC4')+ geom_smooth(aes(group=sample_site), method='lm', color='black') +
  scale_y_continuous("log DOC:POC",limits=c(-1,4)) + ggtitle("Yahara") +
  scale_x_continuous("logQ (cfs)") + theme(legend.position='none')

plot_grid(ph, hwy, ncol=2)

# AIK's Plots POC ~ Discharge ####
par(mfrow = c(1,1))
plot((carbonMeasurements$discharge[carbonMeasurements$sample_site == "Pheasant Branch"]), 
     (carbonMeasurements$poc[carbonMeasurements$sample_site == "Pheasant Branch"]), 
     pch = 20, col = 'dark green', 
     ylab = 'log POC', xlab = 'log Discharge ft3/s', main = 'POC relationship')
plot((carbonMeasurements$discharge[carbonMeasurements$sample_site == "Yahara"]), 
     (carbonMeasurements$poc[carbonMeasurements$sample_site == "Yahara"]), 
     pch = 20, col = 'dark green', 
     ylab = 'log POC', xlab = 'log Discharge ft3/s', main = 'POC relationship')
summary(lm(log(carbonMeasurements$poc[carbonMeasurements$sample_site == "Pheasant Branch"]) ~ 
             log(carbonMeasurements$discharge[carbonMeasurements$sample_site == "Pheasant Branch"])))
abline(lm(log(carbonMeasurements$poc) ~ log(carbonMeasurements$discharge)))
summary(lm(log(carbonMeasurements$poc) ~ log(carbonMeasurements$discharge)))

plot(as.POSIXct(carbonMeasurements$sampledate[carbonMeasurements$sample_site == "Pheasant Branch"], format = "%Y-%m-%d %H:%M:%S"),
     carbonMeasurements$CRatio[carbonMeasurements$sample_site == "Pheasant Branch"],
     ylab = "Carbon Ratio DOC / POC", xlab = "Date", col = "dark green", pch = 20)
medpheas = median(carbonMeasurements$CRatio[carbonMeasurements$sample_site == "Pheasant Branch"])
medyahara = median(carbonMeasurements$CRatio[carbonMeasurements$sample_site == "Yahara"])
points(as.POSIXct(carbonMeasurements$sampledate[carbonMeasurements$sample_site == "Yahara"], format = "%Y-%m-%d %H:%M:%S"),
       carbonMeasurements$CRatio[carbonMeasurements$sample_site == "Yahara"], col = 'red', pch = 20)

plot(as.POSIXct(carbonMeasurements$sampledate[carbonMeasurements$sample_site == "Pheasant Branch"], format = "%Y-%m-%d %H:%M:%S"),
     carbonMeasurements$doc[carbonMeasurements$sample_site == "Pheasant Branch"],
     ylab = "DOC", xlab = "Date", col = "dark green", pch = 20)
abline(lm(carbonMeasurements$doc[carbonMeasurements$sample_site == "Pheasant Branch"] ~ as.POSIXct(carbonMeasurements$sampledate[carbonMeasurements$sample_site == "Pheasant Branch"], format = "%Y-%m-%d %H:%M:%S")),
       col = "dark green")
points(as.POSIXct(carbonMeasurements$sampledate[carbonMeasurements$sample_site == "Pheasant Branch"], format = "%Y-%m-%d %H:%M:%S"),
       carbonMeasurements$poc[carbonMeasurements$sample_site == "Pheasant Branch"] * medpheas, col = "red", pch = 20)
abline(lm(carbonMeasurements$poc[carbonMeasurements$sample_site == "Pheasant Branch"] * medpheas ~ as.POSIXct(carbonMeasurements$sampledate[carbonMeasurements$sample_site == "Pheasant Branch"], format = "%Y-%m-%d %H:%M:%S")),
       col = "red")
points(as.POSIXct(carbonMeasurements$sampledate[carbonMeasurements$sample_site == "Yahara"], format = "%Y-%m-%d %H:%M:%S"),
       carbonMeasurements$doc[carbonMeasurements$sample_site == "Yahara"], col = "blue", pch = 20)
points(as.POSIXct(carbonMeasurements$sampledate[carbonMeasurements$sample_site == "Yahara"], format = "%Y-%m-%d %H:%M:%S"),
       carbonMeasurements$poc[carbonMeasurements$sample_site == "Yahara"] * medyahara, col = "orange", pch = 20)
legend("topleft", c("Pheasant DOC", "Pheasant Predicted DOC", "Yahara DOC", "Yahara Predicted DOC"),
       fill = c("dark green", "red", "blue", "orange"), cex = 0.8)

plot(as.POSIXct(carbonMeasurements$sampledate[carbonMeasurements$sample_site == "Pheasant Branch"], format = "%Y-%m-%d %H:%M:%S"),
     carbonMeasurements$doc[carbonMeasurements$sample_site == "Pheasant Branch"] - carbonMeasurements$poc[carbonMeasurements$sample_site == 
                                                                                                            "Pheasant Branch"] * medpheas, col = "red", pch = 20,
     ylab = "Real - Predicted", xlab = "Date")

lines(rep(mean(carbonMeasurements$doc[carbonMeasurements$sample_site == "Pheasant Branch"] - carbonMeasurements$poc[carbonMeasurements$sample_site == "Pheasant Branch"] * medpheas), 
          length(carbonMeasurements$doc[carbonMeasurements$sample_site == "Pheasant Branch"] - carbonMeasurements$poc[carbonMeasurements$sample_site == "Pheasant Branch"] * medpheas), col = "red"))

# Make plots of raw data nitrate vs. Q, nitrate vs. day of year!!

##### PLOTTING #####
# VALIDATION 
par(mfrow = c(1,2))
plot(flows$flow[flows$nutrient == nutrients[3] & toupper(flows$site) == toupper(namesSites[1])], 
     flows$value[flows$nutrient == nutrients[3] & toupper(flows$site) == toupper(namesSites[1])], type = 'p', 
     col = 'red', ylab = 'Nitrate Conc', xlab = 'Flow', pch = 20, main = 'Unlogged')
plot(log(flows$flow[flows$nutrient == nutrients[3] & toupper(flows$site) == toupper(namesSites[1])]), 
     log(flows$value[flows$nutrient == nutrients[3] & toupper(flows$site) == toupper(namesSites[1])]), 
     type = 'p', col = 'blue', ylab = 'Nitrate Conc', xlab = 'Flow', pch = 20, main = 'Logged')
values = nit_pheas_coefficients['x'] * c(0:5) + (nit_pheas_coefficients['(Intercept)'])
lines(c(0:5), values, col = 'black', lwd = 2)

par(mfrow = c(1,2))
plot(flows$flow[flows$nutrient == nutrients[3] & toupper(flows$site) == toupper(namesSites[2])], 
     flows$value[flows$nutrient == nutrients[3] & toupper(flows$site) == toupper(namesSites[2])], type = 'p', 
     col = 'red', ylab = 'Nitrate Conc', xlab = 'Flow', pch = 20, main = 'Unlogged')
plot(log(flows$flow[flows$nutrient == nutrients[3] & toupper(flows$site) == toupper(namesSites[2])]), 
     log(flows$value[flows$nutrient == nutrients[3] & toupper(flows$site) == toupper(namesSites[2])]), 
     type = 'p', col = 'blue', ylab = 'Nitrate Conc', xlab = 'Flow', pch = 20, main = 'Logged')
values = nit_winds_coefficients['x'] * c(0:5) + (nit_winds_coefficients['(Intercept)'])
lines(c(0:5), values, col = 'black', lwd = 2)

par(mfrow = c(1,1))
plot((as.POSIXlt(flows$datetime[flows$nutrient == nutrients[3] & toupper(flows$site) == toupper(namesSites[1])], 
                 origin = '1970-01-01')$yday), 
     (flows$value[flows$nutrient == nutrients[3] & toupper(flows$site) == toupper(namesSites[1])]), pch = 20,
     ylab = 'Nitrate concentration', xlab = 'Day of year', main = 'Pheasant')


summary(    lm ( exp(flows$value[flows$nutrient == nutrients[3] & 
                                   toupper(flows$site) == toupper(namesSites[1])]) ~ 
                   (as.POSIXlt(flows$datetime[flows$nutrient == nutrients[3] & toupper(flows$site)
                                              == toupper(namesSites[1])], origin = '1970-01-01')$yday)))

plot((as.POSIXlt(flows$datetime[flows$nutrient == nutrients[3] & toupper(flows$site) == toupper(namesSites[1])], 
                 origin = '1970-01-01')$yday), 
     (flows$value[flows$nutrient == nutrients[3] & toupper(flows$site) == toupper(namesSites[1])]), pch = 20,
     ylab = 'Nitrate concentration', xlab = 'Day of year', main = 'Pheasant')
daysofyear = (as.POSIXlt(flows$datetime[flows$nutrient == nutrients[3] & toupper(flows$site)
                                        == toupper(namesSites[1])], origin = '1970-01-01')$yday)
daysofyear2 = daysofyear * daysofyear
loggednit = log(flows$value[flows$nutrient == nutrients[3] & 
                              toupper(flows$site) == toupper(namesSites[1])]) # nevermind, not logging is better!
summary(lm(loggednit ~ daysofyear + daysofyear2))

loggedorg = log(flows$value[flows$nutrient == 'ammorg' & toupper(flows$site) == toupper(namesSites[2])] 
                - (flows$value[flows$nutrient == 'amm' & toupper(flows$site) == toupper(namesSites[2])]))
daysofyear = (as.POSIXlt(flows$datetime[flows$nutrient == "amm" & toupper(flows$site) == toupper(namesSites[2])]
                         , origin = '1970-01-01')$yday)
daysofyear2 = daysofyear * daysofyear
summary(lm(loggedorg ~ daysofyear + daysofyear2))
summary(lm(loggedorg ~ log(flows$flow[flows$nutrient == "amm" & toupper(flows$site) == toupper(namesSites[2])])))

modeled = (lm(loggednit ~ daysofyear + daysofyear2))$coefficients
xvals = c(0:365)
yvals = modeled['(Intercept)'] + modeled['daysofyear'] * xvals + modeled['daysofyear2'] * xvals * xvals
plot(daysofyear, loggednit, pch = 20, main = 'Pheasant', ylab = 'Nitrate concentration', xlab = 'Day of year')
lines( (xvals), (yvals), col = 'red', lwd = 2)