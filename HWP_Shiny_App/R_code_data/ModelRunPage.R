############ Page for entering own data ##################

input1UI <- function(id) {
  tagList(
    fluidRow(box(
      #status = "success",   # makes the top of the box green.  
      column(8, offset = 1, h1(id = "main-header", "Upload Data")))),
    fluidRow(column(6, offset = 1, h4(tags$p("Note: New data file requirements 6/20/2024.  See ", style = "color:#22a783", a("Version Changes",  
                                                  href = "https://jeremygroom.github.io/HWP-vR-Documentation/vc.html"), " section in the documentation.")))),
    shinyjs::useShinyjs(),     # For enable/disable buttons to work, MUST have this call here.
    fluidRow(column(6, offset = 1, fileInput(NS(id, "file1"), label = h3("Select data file for input"), accept = ".xlsx", multiple = FALSE, buttonLabel = "Browse...", placeholder = "No file selected"))),
    fluidRow(column(4, offset = 1, actionButton(NS(id, "runQA"), label = "Run data file quality assurance")), 
             column(3, tableOutput(NS(id, "head"))),
             column(3, uiOutput(NS(id, "qa.download")))),
    fluidRow(column(4, offset = 1, actionButton(NS(id, "RunHWP"), label = "Run HWP Model")),
             column(3, verbatimTextOutput(NS(id, "QAvalTest"))),
             column(4, uiOutput(NS(id, "hwp.tables")))),        # This button appears after HWP model runs.
    fluidRow(column(4, offset = 1, actionButton(NS(id, "RunMC"), label = "Run Monte Carlo")),
             column(4, offset = 3, uiOutput(NS(id, "mc.tables")))),        # This button appears after HWP model runs.),
    fluidRow(column(5, offset = 1,  textInput(NS(id, "HWPcomplete"), label = h3("Model Status"), value = "Model Not Run"))),
    fluidRow(column(width = 12, offset = 0, style = 'padding-left:0px; padding-right:1px; padding-top:15px; padding-bottom:15px')), # Adding a blank row.
    fluidRow(box(width = 9,
      column(10, offset = 1, 
             h3("Steps for uploading your own data into the app"),
             p("Upload your HWP data here!  For complete instructions on how to provide your own data or how to upload other data sets, see " , 
               a("this chapter", href = "https://jeremygroom.github.io/HWP-vR-Documentation/own.html#own"), " of the documentation."),
             HTML("<ol>
                    <li> Obtain the template files.  This may be done from the Templates link on the left, by  
                  <a href=https://jeremygroom.github.io/HWP-vR-Documentation/dnld.html#dnld-git-git>cloning the GitHub repository </a>, or by
                  <a href=https://jeremygroom.github.io/HWP-vR-Documentation/dnld.html#dnld-git-zip>downloading a compressed file of the repository </a>.  
                  Each of these options will provide you with the HWP Data folder.   </li>
                    <li> If you wish to upload an existing data set: </li>
                    <ol  type=a>
                      <li> Adjust <a href=https://jeremygroom.github.io/HWP-vR-Documentation/own.html#own-prov-input-options> analysis options </a> 
                  in the first worksheet of the data set prior to uploading to suit your needs. </li> 
                      <li> Click the “Browse…” button above.  Navigate to the file of interest and select it.  </li>                      
                      <li> If the file uploads correctly, click the “Run data file quality control” button.  If that step fails, download the QA Output 
                      Table for review and <a href=https://jeremygroom.github.io/HWP-vR-Documentation/own.html#own-qa> see this section </a> of the documentation.  
                      Fix the issue and try again.   </li>
                      <li> If step 3.c works, click “Run HWP Model”.  If the model run succeeds (Model Status will say “MODEL RUN SUCCESSFUL!”) then all figures except those 
                      for the Monte Carlo will provide visualizations of your data.  You may download tables of those same outputs (button: “Download HWP Tables”). </li>
                      <li> Click “Run Monte Carlo” to do so.  The process may take a few minutes.  The MC figures will update, although 
                      <a href=https://jeremygroom.github.io/HWP-vR-Documentation/own.html#own-shiny> see this section </a>  if they do not appear to be updating. </li>                      
                    </ol>
                  <li> If you wish to upload your own data, we recommend the following: </li>
                    <ol type =a>
                      <li> Understand the <a href=https://jeremygroom.github.io/HWP-vR-Documentation/own.html#own-prov-temp> template files and their limitations </a>.  </li>
                      <li> Select the template for your region if it exists <a href=https://jeremygroom.github.io/HWP-vR-Documentation/own.html#own-prov-temp> (see map) </a>.  </li>
                      <li>  Users will need to provide their <a href=https://jeremygroom.github.io/HWP-vR-Documentation/own.html#own-prov-input-harvest> annual timber harvest </a> 
                      volumes in thousand board feet (MBF). </li>
                      <li> Users will need to provide their region's <a href=https://jeremygroom.github.io/HWP-vR-Documentation/own.html#own-prov-input-tpr> Timber Product Ratios</a>.  </li>
                      <li> Users should alter other tables (see <a href=https://jeremygroom.github.io/HWP-vR-Documentation/own.html#own-over-inputSum> a summary of tables to alter here </a>) as they are able given available information.  </li>
                      <li> Once your data file is ready, upload it and run the model by following the steps in 3, above.  </li>
                    </ol>
                  <li> 	If you wish to run the app from your own computer or run a sparse stand-alone version of the HWP model, then we recommend following the steps provided in 
                   <a href=https://jeremygroom.github.io/HWP-vR-Documentation/dnld.html#dnld> this chapter.</a>  You will need to have 
                   <a href=https://cran.r-project.org/> R </a> and <a href=https://www.rstudio.com/products/rstudio/> RStudio </a> installed on your machine. </li>
                  </ol>

                  ")
             
    )
    )
  )  )
  
}



input1Server <- function(id, hwp.dt, file.loc) {
  
  moduleServer(id, function(input, output, session) {
    
    
    # Counter for determining whether QA has been run for current uploaded file. Becomes 1 in qa.out  .
    counterQA <- reactiveValues(countervalue = 0)  # Setting a counter to register if plots are switched. If they are, the titles revert to original titles
    counterHWP <- reactiveValues(countervalue = 0) 
    counterMC <- reactiveValues(countervalue = 0) 
    
    ### First, resetting text default if a model has already been run
    observeEvent(input$file1, {
      if (input$HWPcomplete != "Model Not Run") {
        updateTextInput(session, "HWPcomplete", value = "Model Not Run")         # Updating text to signify HWP model worked.
      }
      # New data = disable all visible download buttons
      counterQA$countervalue <- counterHWP$countervalue <- counterMC$countervalue <- 0
    })
    
    
    
    ## -- Opens and prepares HWP data -- #
    data.hwp <- reactive({
      
      req(input$file1) 
      
      ext <- tools::file_ext(input$file1$name)
      switch(ext,
             xlsx = read.xlsx(input$file1$datapath, 1),
             validate("Invalid file. Please upload an .xlsx file."))
      
      hwp <- read.xlsx(input$file1$datapath, 1)
      SheetNames <- getSheetNames(input$file1$datapath)
      hwp.data <- input$file1$datapath %>%
        getSheetNames() %>%
        set_names() %>%
        map(read.xlsx, xlsxFile = input$file1$datapath)
      
      if (dim(hwp.data$BFCF)[2] > 3) {           # Some template files have additional information for users on the BFCF Excel worksheet. This code 
        hwp.data$BFCF <- hwp.data$BFCF[, 1:3]    #    reduces the table to the desired values.
        hwp.data$BFCF <- hwp.data$BFCF %>% dplyr::filter(is.na(Conversion) == FALSE) 
      }
      
      # Check for sheet names = correct
      sheet.names <- names(hwp.data)
      s.n.check <- all(sheet.names == c("HWP_MODEL_OPTIONS", "Harvest_MBF","BFCF", "TimberProdRatios",  "PrimaryProdRatios", "EndUseRatios", 
                                        "RatioCategories","CCF_MT_Conversion",
                                        "EU_HalfLives",  "DiscardFates",  "Discard_HalfLives", "MonteCarloValues")) 
      
      
      s.n.check <- if (s.n.check) 1 else 2
      switch(s.n.check,
             1, 
             validate("Incorrect worksheet names.  Check worksheet names and structure against those listed in the manual."))
      
      
      # Check to make sure harvest columns contain data and are not missing or equal to zero.
      harv.col.dat.check <- any( apply(hwp.data$Harvest_MBF, 2, sum, na.rm = TRUE) == 0)
      h.c.d.c <- if (harv.col.dat.check) 2 else 1
      switch(h.c.d.c,
             1,
      validate(
       # need(s.n.check, "Incorrect worksheet names.  Check worksheet names and structure against those listed in the manual."),
        "Harvest data are incomplete.  Add data to columns missing data or delete ownership columns.")
      )
      
      # Return data
      hwp.data
      
    })
    
    # Returning just the name of the new data (to show that it loaded)
    output$head <- renderTable( data.frame(Uploaded.Data = data.hwp()$HWP_MODEL_OPTIONS[1, 1]))
    
    
    ##### Creating reactive event from the button-push: running QA code #####
    qa.out <- eventReactive(input$runQA, {
      hwp.data <- data.hwp()
      source(paste0(file.loc, "HWP_Model_Prep.R"), local = TRUE)
      #browser()
      
      source(paste0(file.loc, "QA_Code_Shiny.r"), local = TRUE )
 #     browser()
      
      # The QA code will separate the worksheets into separate tables then run diagnostics on those tables
      counterQA$countervalue <- 1
      list(pass.fail = QA_PASS, qa.table = joint.err.term.rept)
      
    })
    

    
    #### --------------------------------- ####
    ###   Download tables from QA check    ###
    #### --------------------------------- ####
    

    
    ### Changing the output text default to reflect completion of the model
    observeEvent(qa.out(), {
      output$qa.download = renderUI(                                          # Download button appears after HWP model ran.
        downloadButton(NS(id, "qa.download01"), label = "Download QA Output Table"))  # Had to separate output$hwp.cables from output$hwp.tables01 otherwise Shiny is confused.
    })
    
    
    
    observe({
      toggleState("qa.download01", condition = (counterQA$countervalue == 1))
    })
    
    
    
    output$qa.download01 <- downloadHandler(
      filename = "QA_Download.csv", # May need: function(){"HWP_tables.zip"}, 
      content = function(file) {
        write_csv(qa.out()$qa.table, file)
      },
      contentType = "text/csv"
    )     
    
    ####################################
    ###         HWP model run        ###
    ####################################
    
    hwp.out <- eventReactive(input$RunHWP, {
      
      output$QAvalTest <- renderText({
        value = shiny::validate(
          paste(
            need(input$runQA > 0, "Run QA test first."),
            need(qa.out()$pass.fail == TRUE, "File did not pass QA tests.\nDownload and check QA table.")
          )
        )
      })
      
      req(input$runQA > 0)                  
      req(qa.out()$pass.fail == TRUE)       # Can only carry forth if the QA returned no errors
      
      
      # Prep the HWP model:
      hwp.data <- data.hwp()
      
      ## Importing constants
      source(paste0(file.loc, "HWP_Model_Prep.R"), local = TRUE)
      
      
      ## Setting up tables  
      
      mc_iter_results <- hwp.model.options$mc_iter_results
      mc_plot <- NULL
      C.names <-  NULL
      mc_PoolsTotalPlot <- NULL
      #browser()
      #Run the HWP model
      hwp.output <- HwpModel.fcn(harv = harv.hwp,  # See the R code HWP_Model_Function.r, sourced from global.r
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
      
      # Create a new HWP file
      
      model.outputs <- hwp.output  
      source(paste0(file.loc, "HWP_Output_Prep.R"), local = TRUE)
      
      
      # Update the original controls to include the new data
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
  #browser()
      hwp.new <- hwp.output
      const.lst <- list(N.OWNERSHIP = N.OWNERSHIP, N.YEARS = N.YEARS, years = years, yearsUse = yearsUse, yearsTPO = yearsTPO, 
                        yearsSE = yearsSE, ownership.names = ownership.names, N.EUR = N.EUR, PIU.WOOD.LOSS = PIU.WOOD.LOSS,
                        PIU.PAPER.LOSS = PIU.PAPER.LOSS, 
                        OWNERSHIP_STARTYEAR = OWNERSHIP_STARTYEAR, MIN.PLOT.YR = MIN.PLOT.YR,
                        MAX.PLOT.YR = MAX.PLOT.YR, MIN.PLOT.OWNR.YR = MIN.PLOT.OWNR.YR, SWDS_COLOR = SWDS_COLOR, PIU_COLOR = PIU_COLOR,
                        harv.hwp = harv.hwp, bfcf.hwp = bfcf.hwp, tpr.hwp = tpr.hwp, ppr.hwp = ppr.hwp,  # data frames for Sankey
                        ratio_cat.hwp = ratio_cat.hwp, ccf_conversion.hwp = ccf_conversion.hwp, eur.hwp = eur.hwp,  # data frames for Sankey 
                        eu_half.lives.hwp = eu_half.lives.hwp, discard.fates.hwp = discard.fates.hwp, discard.hl.hwp = discard.hl.hwp,    # data frames for Sankey
                        N.ITER = N.ITER, mc_iter_results = mc_iter_results, mc_plot = mc_plot, # Monte Carlo values (null)
                        C.names  = C.names, mc_PoolsTotalPlot = mc_PoolsTotalPlot, MC.CI.REPORT = MC.CI.REPORT)                             
      
      hwp.new <- append(hwp.new, const.lst)
      
      ### Changing the data sets, adding the new data set and the new data set name.        
      state.data <<- append(list(hwp.CA = state.data[[1]], hwp.OR = state.data[[2]]), list(hwp.new))  # Note: '<<-' is a 'superassignment' operator, which travels up levels outside 
      names(state.data)[[3]] <<- "hwp.new"                                                            #   of the function until it finds a variable of the target name
      #browser()
      
      # This code removes all earlier versions of the HWP tables if around.  Likely unnecessary. 
      csv.to.remove <- c("Table_Harvest.csv", "Table_Cumulative.C.Disposition.csv", "Table_AnnualStockChange.csv",
                         "CumulativeCarbonStorageEmissions.csv",  "Ownership_PIU_SWDS.csv", "MC_ComponentsSummary.csv", "MC_PIU_Plus_SWDS.csv")
      del.names <- dir()[which(dir() %in% csv.to.remove)]      # Finds which of the csv.to.remove files are present from a previous run.
      if (length(del.names) > 0) sapply(del.names, file.remove) # If any of those files are present, they are deleted.
      
      counterHWP$countervalue <- 1  # Re-enable HWP table download button if button is disabled
      
      return(list(new.name = as.character(hwp.model.options[1, 1]), msg = "MODEL RUN SUCCESSFUL!"))
      
    })
    
    
    
    
    ### Changing the output text default to reflect completion of the model, adding download button
    
    observe({  # Disables download button if new data are loaded.
      toggleState("hwp.tables01", condition = (counterHWP$countervalue == 1))
    })
    
    
    observeEvent(hwp.out(), {
      updateTextInput(session, "HWPcomplete", value = hwp.out()$msg)         # Updating text to signify HWP model worked.
      output$hwp.tables = renderUI(                                          # Download button appears after HWP model ran.
        downloadButton(NS(id, "hwp.tables01"), label = "Download HWP Tables"))  # Had to separate output$hwp.cables from output$hwp.tables01 otherwise Shiny is confused.
      
    })
    
    # This piece is the only thing returned by the module - it is used in the master "app.R" code to add the new HWP data set name and make it a selectable option.
    return_new_name <- eventReactive( hwp.out(),{
      #print(paste0("module> New name is: ", hwp.out()$new.name)) # When activated, this code will print a result to demonstrate the code works to this point.
      hwp.out()$new.name
    }) 
    
    
    #### ---------------------------- ####
    ###   Download tables from HWP    ###
    #### ---------------------------- ####
    hwp.tables <- eventReactive(hwp.out(), {
      
      # Obtain the HWP data again, to glean constants
      hwp.data <- data.hwp()
      ## Importing constants
      source(paste0(file.loc, "HWP_Model_Prep.R"), local = TRUE)
      
      ## Output names
      model.outputs <- state.data[[3]]  
      source(paste0(file.loc, "HWP_Output_Prep.R"), local = TRUE)
      
      yearsTPO <- years                             ## For TPO, use years as-is.
      yearsSE <- years  + 1                         ##  For years for Stocks and Emitted values shift to one year later (unless not shifted, see next code section)
      
      if (SHIFTYEAR) {           # Shifted variables
        yearsUse <- yearsSE
        OwnershipStartYear <- OWNERSHIP_STARTYEAR + 1
      } else {                   # Unshifted variables
        yearsUse <- yearsTPO
        OwnershipStartYear <- OWNERSHIP_STARTYEAR
      }
      
      #browser()
      source(paste0(file.loc, "HWP_Tables_Code.R"), local = TRUE)
      
      
      list(t1 = t1, t2 = t2, t3 = t3, t3.5 = t3.5, 
           t4 = t4, t4.5 = t4.5, t4.8 = t4.8 ,t5 = t5)  #t2 is missing/removed on purpose
    })
    
    
    
    
    
    output$hwp.tables01 <- downloadHandler(
      
      filename = "HWP_tables.zip", # May need: function(){"HWP_tables.zip"}, 
      content = function(file) {
        fnames <- c("T1.0.Annual_Harvest.csv", "T2.0.Harvest_Halflives.csv", "T3.0.Cumulative.Ownership.Storage.Emissions.csv", 
                    "T3.5.Cumulative.Ownership.Storage.Emissions_CO2e.csv", "T4.0.CumulativeStorageEmissions_summary.csv", 
                    "T4.5.CumulativeStorageEmissions_detail.csv", "T4.8.CumulativeStorageEmissions_halflives.csv", "T5.0.AnnualStorageEmissionsChange.csv")
        
        
        vroom_write(hwp.tables()$t1, "T1.0.Annual_Harvest.csv", delim = ",")
        vroom_write(hwp.tables()$t2, "T2.0.Harvest_Halflives.csv", delim = ",")
        vroom_write(hwp.tables()$t3, "T3.0.Cumulative.Ownership.Storage.Emissions.csv", delim = ",")
        vroom_write(hwp.tables()$t3.5, "T3.5.Cumulative.Ownership.Storage.Emissions_CO2e.csv", delim = ",")
        vroom_write(hwp.tables()$t4, "T4.0.CumulativeStorageEmissions_summary.csv", delim = ",")
        vroom_write(hwp.tables()$t4.5, "T4.5.CumulativeStorageEmissions_detail.csv", delim = ",")
        vroom_write(hwp.tables()$t4.8, "T4.8.CumulativeStorageEmissions_halflives.csv", delim = ",")
        vroom_write(hwp.tables()$t5, "T5.0.AnnualStorageEmissionsChange.csv", delim = ",")
        zip(zipfile = file, files = fnames)
      },
      contentType = "application/zip"
    )
    
    
    
    
    
    #################################################
    ###         Monte Carlo simulation run        ###
    #################################################
    
    
    
    mc.out <- eventReactive(input$RunMC, {
      #browser()
      
      req(hwp.out()) 
      
      
      hwp.data <- data.hwp()
      
      
      ## Importing constants
      source(paste0(file.loc, "HWP_Model_Prep.R"), local = TRUE)
      
      ## Loading the new data for the MC run:
      new.state.data <- state.data[[3]]

      y <- Sys.time()  # At the end of this section the app reports (behind the scenes) how long the process takes.  
      show_modal_spinner(spin = "trinity-rings", text = paste0("The Monte Carlo process is underway.  This process is expected to take ", 
                                                               round(0.0044 * new.state.data$N.ITER * new.state.data$N.YEARS / 60, 1), " minutes")) # show the modal window
      
      
      ## For the MC, need to define HWP outputs from the new data:
      model.outputs <- new.state.data  
      source(paste0(file.loc, "HWP_Output_Prep.R"), local = TRUE)

      ## RUN MONTE CARLO SCRIPT ##
      
      source(paste0(file.loc, "MC_Code.R"), local = TRUE)
      
      ## MAKE USE OF MONTE CARLO OUTPUT
      
      ## Evaluating convergence of last year's results with the given number of iterations and burn-in period
      MCout.lastyr <- MCout[N.YEARS, , 3] + MCout[N.YEARS, , 4]
      
      lci.prob <- (1 - hwp.data$HWP_MODEL_OPTIONS$MC.CI.REPORT) / 2
      uci.prob <- 1 - lci.prob
      
      cum_mean <- cum_se <- cumLCI <- cumUCI <- rep(0, N.ITER)
      for (i in 1:N.ITER) {
        cum_mean[i] <- mean(MCout.lastyr[1:i])
        cum_se[i] <- sd(MCout.lastyr[1:i])/sqrt(i)
        cumLCI[i] <- quantile(MCout.lastyr[1:i], probs = lci.prob)
        cumUCI[i] <- quantile(MCout.lastyr[1:i], probs = uci.prob)
      }
      
      mc_iter_results <- tibble(iter = 1:N.ITER, mean = cum_mean, se = cum_se, ciLCI = cumLCI, ciUCI = cumUCI) %>%
        filter(iter != 1) %>%
        pivot_longer(cols = 2:5, names_to = "stat", values_to = "C")
      
      
      #####  Finding MC summary stats for the four categories of general output ###
      
      mean_MC <- ciLCI_MC <- ciUCI_MC <- data.frame(eec = rep(0, N.YEARS), ewoec = rep(0, N.YEARS), 
                                                  swdsC = rep(0, N.YEARS), pu = rep(0, N.YEARS))
      for (i in 1:4) {
        mean_MC[,i] <- apply(MCout[, , i], 1, mean)
        ciLCI_MC[, i] <- apply(MCout[, , i], 1, quantile, probs = lci.prob)  # Obtaining the CI empirically
        ciUCI_MC[, i] <- apply(MCout[, , i], 1, quantile, probs = uci.prob)
      }
      
      mean_MC$Year <- years
      mean_MC2 <- mean_MC %>% pivot_longer(cols = 1:4, names_to = "Type.M", values_to = "Means")
      ciLCI_MC2 <- ciLCI_MC %>% pivot_longer(cols = 1:4, names_to = "Type.lci", values_to = "lci")
      ciUCI_MC2 <- ciUCI_MC %>% pivot_longer(cols = 1:4, names_to = "Type.uci", values_to = "uci")
      
      
      mc_plot <- cbind(mean_MC2, ciLCI_MC2[, 2], ciUCI_MC2[, 2]) %>%
        mutate(
          pct_lci = lci/Means,
          pct_uci = uci/Means)
      
      C.names <- as_labeller(c(`eec` = "Emitted with Energy Capture" ,  `ewoec` = "Emitted Without Energy Capture",`swdsC` = "Solid Waste Disposal Sites", `pu` = "Products in Use"))
      
      #  write_csv(mc_plot, "MC_Output/MC_Tables/MC_ComponentsSummary.csv")
      
      ## MMC total
      MC_MMTC <- MCout[, , 3] + MCout[, , 4]  # Summing SWDS and Products in Use values for each iteration.
      
      mean_MMTC <- apply(MC_MMTC, 1, mean)
      ciLCI_MMTC <- apply(MC_MMTC, 1, quantile, probs = lci.prob)  # Obtaining the CI empirically.
      ciUCI_MMTC <- apply(MC_MMTC, 1, quantile, probs = uci.prob)
      
      mc_PoolsTotalPlot <- tibble(Year = START.YEAR:END.YEAR, Mean = mean_MMTC/1e6, lci = ciLCI_MMTC/1e6, uci = ciUCI_MMTC/1e6)
      #write_csv(mc_PoolsTotalPlot, "MC_Output/MC_Tables/MC_PIU_Plus_SWDS.csv")

      ## Now updating the MC values for the new data set ##
      
      state.data[[3]]$mc_iter_results <<- mc_iter_results 
      state.data[[3]]$mc_plot <<- mc_plot
      state.data[[3]]$mc_PoolsTotalPlot <<- mc_PoolsTotalPlot
      state.data[[3]]$C.names <<- C.names 
      
      counterMC$countervalue <- 1
      remove_modal_spinner() # remove modal spinner 
      print(Sys.time() - y)
      
    })    
    
    ## -- Downloading MC tables -- ##
    observeEvent(mc.out(), {
      updateTextInput(session, "HWPcomplete", value = "Monte Carlo is complete!")
     # browser()                     # Uncomment here when replacing CA or OR default files. 
      output$mc.tables = renderUI(                                          # Download button appears after HWP model ran.
        downloadButton(NS(id, "mc.tables01"), label = "Download Monte Carlo Tables"))  # Had to separate output$hwp.cables from output$hwp.tables01 otherwise Shiny is confused.
    }) 
    
    observe({  # Disables download button if new data are loaded.
      toggleState("mc.tables01", condition = (counterMC$countervalue == 1))
    })
    
    
    output$mc.tables01 <- downloadHandler(
      filename = "MC_tables.zip", 
      content = function(file) {
        fnames <- c("MC_ComponentsSummary.csv", "MC_PIU_Plus_SWDS.csv")
        vroom_write(state.data[[3]]$mc_plot, "MC_ComponentsSummary.csv", delim = ",")
        vroom_write(state.data[[3]]$mc_PoolsTotalPlot, "MC_PIU_Plus_SWDS.csv", delim = ",")
        zip(zipfile = file, files = fnames)
      },
      contentType = "application/zip"
    )
    
    
    
    
    
    return(return_new_name)
    
    
    
  })
  
}

