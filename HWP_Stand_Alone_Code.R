### HWP Carbon Fate Modeling in R
### Author: Jeremy Groom, Groom Analytics LLC
###  April 2022
###
###  This code seeks to reproduce the modeling effort of Stockman et al. (2012).
###  The code is designed to accept input files from any region and produce interactive output graphics and tables. 
###
###  To run the Shiny version of the model go to the project HWP_Shiny_App/HWP_Shiny_App.Rproj and open the app.R file.
###
###   This is the primary code that runs the HWP model and generates tables and arrays. It sources other code in the HWP_Stand_Alone_Files
###     folder and the HWP_Shiny_App folder.  
###
###   The file Inputs_HWP_Model.xlsx is the source of data and instructions for the code.  See instructions.
###
###########################################################################################################



###########################################
## R libraries

renv::restore()   # Begin here to download appropriate package versions


# Loading libraries
library(tidyverse)
library(writexl)
library(openxlsx)  # faster than readxl
library(data.table)
library(abind)      # Combine matrices into an array
library(triangle)   # for generating random variables from triangular distributions for the Monte Carlo
library(lhs)        # Latin Hypercube Sampling
library(networkD3)   # Sankey diagram


##########################################################################
###  Constants specific to the stand-alone version of the model
GENERATE.SANKEY <- TRUE       # Set to FALSE if you do not want the code to generate a Sankey diagram.
SANKEY.HARVEST.YEAR <- 1960   # Set to any year within your data range.
SANKEY.YEARS.OF.DECAY <- 30    # Set to any number between 3 and 100.

### Folder locations
SHINY.CODE <- "HWP_Shiny_App/R_code_data/"              # Code chunks that both Shiny and the stand-alone model depend upon. They are stored in the Shiny app folder.
IMPORT.DATA.FOLDER <- "HWP Data/ExistingData/"
IMPORT.DATA.FILE <- "CA_Inputs_HWP_Model_alt.xlsx"    # Change this to select other files from the "HWP Data" folder.
QAQC.FOLDER <- "HWP_Stand_Alone_Files/QAQC_Reports/"
SA.CODE <- "HWP_Stand_Alone_Files/Standalone_R_files/"   # Stand-alone HWP model code

############################################################################
## Loading the data, either using the QA code or bypassing it
hwp.model.data <- paste0(IMPORT.DATA.FOLDER, IMPORT.DATA.FILE)

SheetNames <- getSheetNames(hwp.model.data)
hwp.data <- hwp.model.data %>%
  getSheetNames() %>%
  set_names() %>%
  map(read.xlsx, xlsxFile = hwp.model.data)

if (dim(hwp.data$BFCF)[2] > 3) {           # Some template files have additional information for users on the BFCF Excel worksheet. This code 
  hwp.data$BFCF <- hwp.data$BFCF[, 1:3]    #    reduces the table to the desired values.
  hwp.data$BFCF <- hwp.data$BFCF %>% dplyr::filter(is.na(Conversion) == FALSE) 
}
  
hwp.model.options <- hwp.data$HWP_MODEL_OPTIONS


##         The code will run a QA test of incoming tables, if desired. If the user sets the QA_TEST option in HWP_MODEL_OPTIONS.csv to FALSE, 
###         the program will attempt to load the files regardless.
if (hwp.data$HWP_MODEL_OPTIONS$QA_TEST[1] == TRUE) {      # Should QA test be run?  
  source(paste0(SHINY.CODE, "QA_Code_Shiny.r"), local = TRUE)  # Using QAQC tests from Shiny app
  write_csv(joint.err.term.rept, paste0(QAQC.FOLDER, "Error_Report.csv"))  # If QAQC detects errors or files fail to load, R terminates
  .Last <- function() {
    graphics.off()
    print("Terminating HWP script. HWP files incorrectly loaded or contained formatting errors.  See QA Error Report.")
  }
  if (QA_PASS == TRUE) print("Data file passed QA tests") else quit(save = "ask", runLast = TRUE)  # If any terminate = 1, quit R
  
} else {
  harv.hwp <- hwp.data$Harvest_MBF   # harvest data, units = 1000 board feet. Used to obtain number of years (N.YEARS) and ownership names
  bfcf.hwp <- hwp.data$BFCF  # Thousand board feet to hundred cubic feet
  tpr.hwp <- hwp.data$TimberProdRatios     # Timber product ratios  (n = 40)
  ppr.hwp <- hwp.data$PrimaryProdRatio      # Primary product ratios (n = 64) 
  eur.hwp <- hwp.data$EndUseRatios       # Loading End Use Ratios (n = N.EUR)
  ratio_cat.hwp <- hwp.data$RatioCategories    # Associates TPR, EUR, and PPR values
  ccf_conversion.hwp <- hwp.data$CCF_MT_Conversion     # getting hundred cubic feet (CCF) to metric ton (MT) carbon conversion factors
  eu_half.lives.hwp <- hwp.data$EU_HalfLives    # Half-lives of End Use Products
  discard.fates.hwp <- hwp.data$DiscardFates    # Fraction of discarded C to different fates (dumps, compost, landfill, burned with energy capture, etc.)
  discard.hl.hwp <- hwp.data$Discard_HalfLives  # Half-lives of discards in different SWDS categories (dumps, landfills) plus fraction of landfill discard that doesn't decay
  mc.adj.hwp <- hwp.data$MonteCarloValues 
  }



#######################################################
### User Defined Options and CONSTANTS

source(paste0(SHINY.CODE, "HWP_Model_Prep.R"), local = TRUE)

opt.list <- list()
for (i in 1:ncol(hwp.model.options)) {             # Using for-loop to add columns to list
  opt.list[[i]] <- hwp.model.options[ , i]
}
names(opt.list) <- colnames(hwp.model.options)

OUTPUT_ARRAYS <- unlist(opt.list$OUTPUT_ARRAYS, use.names = F)[1]  # Save HWP model arrays in Arrays folder?
#OUTPUT_FIGURES <- unlist(opt.list$OUTPUT_FIGURES, use.names = F)[1]  # Save HWP model figures in Figures folder?
OUTPUT_TABLES <- unlist(opt.list$OUTPUT_TABLES, use.names = F)[1]  # Save HWP model tables in Tables folder?

#FIGURELOC <- unlist(opt.list$FIGURELOC, use.names = F)[1]   # Figure storage folder 
ARRAYLOC <- unlist(opt.list$ARRAYLOC, use.names = F)[1]     # Array storage folder
TABLELOC <- unlist(opt.list$TABLELOC, use.names = F)[1]    # Table storage folder


############# RUN HWP BASE MODEL ###############
source(paste0(SHINY.CODE, "PlotFunctions1.r"), local = TRUE)     # Loading functions used by the HWP model.
source(paste0(SHINY.CODE, "HWP_Model_Function.r"), local = TRUE) # The HWP model (a function) itself.

hwp.output <- HwpModel.fcn(harv = harv.hwp,  
                           bfcf = bfcf.hwp,
                           tpr = tpr.hwp,
                           ppr = ppr.hwp,
                           ratio_cat = ratio_cat.hwp,
                           ccf_conversion = ccf_conversion.hwp,
                           eur = eur.hwp,
                           eu_half.lives = eu_half.lives.hwp,
                           discard.fates = discard.fates.hwp,
                           discard.hl = discard.hl.hwp, 
                           ownership.names = ownership.names,
                           N.EUR = N.EUR, 
                           N.OWNERSHIP = N.OWNERSHIP, 
                           N.YEARS = N.YEARS, 
                           PIU.WOOD.LOSS = PIU.WOOD.LOSS,
                           PIU.PAPER.LOSS = PIU.PAPER.LOSS)

# Prepare outputs for use in tables and arrays
model.outputs <- hwp.output  
source(paste0(SHINY.CODE, "HWP_Output_Prep.R"), local = TRUE)


################################################

### Selection of year shift ###
##   To shift years, set SHIFTYEAR to TRUE in the file HWP_MODEL_OPTIONS, otherwise FALSE
##   This will change output for tables, arrays, and figures                

yearsTPO <- years                             ## For TPO, use years as-is.
yearsSE <- years  + 1                         ##  For years for Stocks and Emitted values shift to one year later (unless not shifted, see next code section)

if (SHIFTYEAR) {           # Shifted variables
  yearsUse <- yearsSE
  OwnershipStartYear <- OWNERSHIP_STARTYEAR + 1
} else {                   # Unshifted variables
  yearsUse <- yearsTPO
  OwnershipStartYear <- OWNERSHIP_STARTYEAR
}
###############


# Code determines, from HWP_MODEL_OPTIONS.csv file, whether to generate figures, tables, and run the Monte Carlo.

# The following code generates the same tables as the Shiny app
if (OUTPUT_TABLES == TRUE) {   
  source(paste0(SHINY.CODE, "HWP_Tables_Code.R"), local = TRUE)
  write_csv(t1, paste0(TABLELOC, "T1.0.Annual_Harvest.csv"))
  write_csv(t2, paste0(TABLELOC, "T2.0.Harvest_Halflives.csv"))
  write_csv(t3, paste0(TABLELOC, "T3.0.Cumulative.Ownership.Storage.Emissions.csv"))
  write_csv(t3.5, paste0(TABLELOC, "T3.5.Cumulative.Ownership.Storage.Emissions_CO2e.csv"))
  write_csv(t4, paste0(TABLELOC, "T4.0.CumulativeStorageEmissions_summary.csv"))
  write_csv(t4.5, paste0(TABLELOC, "T4.5.CumulativeStorageEmissions_detail.csv"))
  write_csv(t4.8, paste0(TABLELOC, "T4.8.CumulativeStorageEmissions_halflives.csv"))
  write_csv(t5, paste0(TABLELOC, "T5.0.AnnualStorageEmissionsChange.csv"))
  }


# Now, if indicated in the Excel file, the code will produce and save the model's arrays. 
ArraySave.fcn <- function() {    
  # Data frame list population function - writes Excel files out of 3-d arrays, first turning them into a list of 2-d data frames
  DFL_Pop.fcn <- function(arrayX, array_name) {
    x <- lapply(seq(dim(arrayX)[2]), function(x) data.frame(arrayX[ , x, ]))  # Creates a list of data frames, one per ownership
    names(x) <- ownership.names
    write_xlsx(x, path = paste0(ARRAYLOC, array_name))
  }
  
  DFL_Pop.fcn(eu_array, "eu_array.xlsx")
  DFL_Pop.fcn(eu.reduced_array, "eu.reduced_array.xlsx")
  DFL_Pop.fcn(eec_array, "eec_array.xlsx")
  DFL_Pop.fcn(fuel_array, "fuel_array.xlsx")
  DFL_Pop.fcn(dec.input_array, "dec_array.xlsx")
  
  DFL_Pop.fcn(dp.total_array, "dp.total_array.xlsx")
  DFL_Pop.fcn(ewoec_array, "ewoec_array.xlsx")
  DFL_Pop.fcn(dumps.discard_array, "dumps.discard_array.xlsx")
  DFL_Pop.fcn(landfill.discard_array, "lf.discard_array.xlsx")
  DFL_Pop.fcn(recov.discard_array, "recov.discard_array.xlsx")
  DFL_Pop.fcn(compost.input_array, "compost_array.xlsx")
  DFL_Pop.fcn(bwoec.input_array, "bwoec_array.xlsx")
  
  DFL_Pop.fcn(swdsCtotal_array, "swds.total_array.xlsx")
  DFL_Pop.fcn(hwp.output$lf.fixed_array, "lf.fixed.input_array.xlsx")
  DFL_Pop.fcn(lf.fixed.cumsum_array, "lf.fixed.cumsum_array.xlsx")
  DFL_Pop.fcn(landfill_array, "lf.decaying_array.xlsx")
  
  DFL_Pop.fcn(dumps_array, "dumps_array.xlsx")
  
  DFL_Pop.fcn(pu.final_array, "pu.final_array.xlsx")
  DFL_Pop.fcn(pu_array, "pu_array.xlsx")
  DFL_Pop.fcn(recov_array, "recov_array.xlsx")
}
if (OUTPUT_ARRAYS == TRUE) ArraySave.fcn()



## Optional: Run the Monte Carlo? 
if (hwp.model.options$RUN.MC == TRUE) {
  source(paste0(SHINY.CODE, "MC_Code.R"), local = TRUE)
  source(paste0(SA.CODE, "MC_Tables.R"), local = TRUE)
  if (MC.ARRAY.OUT) mc.save.array.fcn()
  
}

## Produce a savable version of the Sankey diagram ("Fate of Harvest Carbon").  To save from RStudio, run following code,
#   click "export" in the viewer window, select "Save as Image...", and modify the position of Sankey objects within the 
#    Save window before saving.
if (GENERATE.SANKEY == TRUE) {
  hwp.yr <- SANKEY.HARVEST.YEAR
  d.yrs <- SANKEY.YEARS.OF.DECAY
  ownr.sel <- "Total"
  ownr.index <- which(ownership.names == ownr.sel)
  yr.index <- which(years == hwp.yr)
  source(paste0(SHINY.CODE, "Sankey_Code.r"), local = TRUE)
  sankeyNetwork(Links = links, Nodes = nodes2,
                Source = "IDsource", Target = "IDtarget",
                Value = "value", NodeID = "name",
                fontSize = 14,
                sinksRight = FALSE)
}




















