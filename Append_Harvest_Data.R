############### ---- Appending years of harvest data to existing data file ---- ##############

### This code will append new harvest data to an existing file.  It will copy existing ratio/proportion values from the previous last year; therefore, 
#     if you wish to alter those values you will need to do so outside of this code.  



### -- Libraries -- ###
library(tidyverse)
library(writexl)
library(openxlsx)  

### -- Constants -- ###
ORIG.DATA.FILE <- "Oregon_Inputs_HWP_Model_alt.xlsx"    # Original data file
NEW.DATA.FILE <- "Oregon_MBF_2019.xlsx"            # Excel file with harvest data (must be Excel, not CSV)
OUT.DATA.FILE <- "Oregon_Inputs_HWP_Modelv2.xlsx"  # The output file name

ORIG.DATA.FOLDER <- "HWP Data/ExistingData/"       # Original data location
NEW.DATA.FOLDER <- "HWP Data/ModelExploration/"    # New data file location
OUT.DATA.FOLDER <- "HWP Data/ModelExploration/"    # Location for updated output file (could be anywhere)


### -- Functions -- ###

# This function takes a data frame with years for columns and duplicates the last column to match the number of new years added to the data set.
append.fcn <- function(target.df, ex.cols){    # Target data frame, extra columns (the number of non-year columns on the left hand side of the data frame).
  append.val <- target.df[, n.orig + ex.cols]   # The rightmost (most recent) column
  v.names <- paste0("a", 1:n.new)                               # Generating a variable list of temporary column names.
  append.df <- eval(parse( text = paste0("data.frame(", paste(v.names, " =  append.val", collapse = ","), ")")))  # Creates a data frame of the needed numbers of repeated columns.
  names(append.df) <- as.character(yrs.new$Year)                # Name the new columns after the new years.
  cbind(target.df, append.df)                   # Combine the new with the original.
}




# Load new data
new.dat <- read.xlsx(paste0(NEW.DATA.FOLDER, NEW.DATA.FILE))

# Original data file 
hwp.model.data <- paste0(ORIG.DATA.FOLDER, ORIG.DATA.FILE)

SheetNames <- getSheetNames(hwp.model.data)
hwp.data <- hwp.model.data %>%           # Creating a list of the Excel spreadsheets from the original data set.
  getSheetNames() %>%
  set_names() %>%
  map(read.xlsx, xlsxFile = hwp.model.data)


if (dim(hwp.data$BFCF)[2] > 3) {           # Just in case.... some template files have additional information for users on the BFCF Excel worksheet. This code 
  hwp.data$BFCF <- hwp.data$BFCF[, 1:3]    #    reduces the table to the desired values.
  hwp.data$BFCF <- hwp.data$BFCF %>% dplyr::filter(is.na(Conversion) == FALSE) 
}


# This code assumes that the data file is constructed correctly.  If there are questions, open the data in the app and conduct a QA test or perform the test 
#  using HWP_Stand_Alone_Code.R . 

issues.var <- 0       # Indicator variable that will stop things if issues are found with the data.

# Ensure columns are properly formatted
if (all(apply(new.dat, 2, is.numeric)) == TRUE) {
  print("New data are numeric") } else{ 
    issues.var <- 1
    print("WARNING: New data are not numeric.  Remove characters from data columns")}


# Reduce new data to just the new values
yrs.orig <- hwp.data$Harvest_MBF$Year
n.orig <- length(yrs.orig)                          # Number of years in original data set
yrs.new <- new.dat[!(new.dat$Year %in% yrs.orig),]   # Selecting years not in the original data set


# Verify year number is correct (orig + new = sequential, no gaps)
if (length(c(yrs.orig, yrs.new$Year)) == length(min(yrs.orig):max(yrs.new$Year))) {
  print("New and original data years are seqential") } else { 
    issues.var <- 1
    print("WARNING: a gap exists between new and original data years")  }



# Verify that the ownership names are correct
if (all(names(yrs.new) == names(hwp.data$Harvest_MBF))) {
  print("Column names for new and original data match") } else { 
    issues.var <- 1
    print("WARNING: Column names differ between new and original data") }

# For original data with ownership data...
if (ncol(hwp.data$Harvest_MBF) > 2){
  own.var <- 0  # Indicator variable that ownership data do (0) / do not (1) exist.
  # Verify that ownership data are provided.   
  own.sum <- apply(yrs.new[, 2:(ncol(yrs.new) - 1)], 1, sum)
  if(all(is.na(own.sum) == FALSE)) {
    print("New ownership data exists") } else {
      own.var <- 1
      issues.var <- 1
      print("WARNING: New ownership data is missing and needs to be included")}
  
  
  # Verify that Total = sum of other columns.  
  if(all(own.sum == yrs.new$Total) == TRUE & own.var != 1) {
    print("Column sums equal Total values") } else { 
      issues.var <- 1
      print("WARNING: Column sums DO NOT equal Total values.  Check ownership values or include ownership values.") }
}


if (issues.var == 1) {
  print("PROBLEMS WITH COMPATIBILITY OF NEW DATA. PLEASE REVISE")
} else {
  hwp.data$Harvest_MBF <- hwp.data$Harvest_MBF %>% bind_rows(yrs.new)            # Appending new harvest data to original.
  n.new <- nrow(yrs.new)                                                       # Number of new years.
  hwp.data$BFCF[nrow(hwp.data$BFCF), ncol(hwp.data$BFCF)] <- max(yrs.new$Year) # Changing EndYear value to last year of data set
  hwp.data$TimberProdRatios <- append.fcn(hwp.data$TimberProdRatios, 1)        # Adding on copies of the last year's proportion values.
  hwp.data$PrimaryProdRatios <- append.fcn(hwp.data$PrimaryProdRatios, 1)        # Adding on copies of the last year's proportion values.
  hwp.data$EndUseRatios <- append.fcn(hwp.data$EndUseRatios, 1)        # Adding on copies of the last year's proportion values.
  hwp.data$DiscardFates <- append.fcn(hwp.data$DiscardFates, 2)        # Adding on copies of the last year's proportion values.
  if(max(hwp.data$MonteCarloValues$Last_Year, na.rm = TRUE) <= max(yrs.new$Year)) {  # Setting the last year of the Monte Carlo to something way in the future if necessary.
    max.last.yr <- max(hwp.data$MonteCarloValues$Last_Year, na.rm = TRUE)            #   Note that this adjustment assumes that the final year for each of the three parameters
    hwp.data$MonteCarloValues$Last_Year[which(hwp.data$MonteCarloValues$Last_Year == max.last.yr)] <- 2100  # with years are equal to one another.
  }
  
  writexl::write_xlsx(hwp.data, paste0(OUT.DATA.FOLDER, OUT.DATA.FILE))  
}
























