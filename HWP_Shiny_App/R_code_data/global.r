## This global script is all processing that occurs prior to the app being called.
##  For shinyapps.io, it is important that the file extension for this be ".r".  
##  This reorg is not necessary except for deployment


source("./HWP_Shiny_App/R_code_data/PlotFunctions1.r", local = TRUE)
source("./HWP_Shiny_App/R_code_data/HWP_Model_Function.r", local = TRUE)


hwp.CA <- readRDS("./HWP_Shiny_App/R_code_data/HWP.CA.output.rds")    
hwp.OR <- readRDS("./HWP_Shiny_App/R_code_data/HWP.OR.output.rds")
#write_rds(hwp.CA, "./HWP_Shiny_App/R_code_data/HWP.CA.output.rds")
#write_rds(hwp.OR, "./HWP_Shiny_App/R_code_data/HWP.OR.output.rds")
state.choices <- c("California", "Oregon")
state.data <- list(hwp.CA = hwp.CA, hwp.OR = hwp.OR)

