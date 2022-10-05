# Plot for the 'Fate of Harvested Carbon' Module - Sankey Diagram

## Initially making reactive by Year, then by Year + #Years

plot_FateHarvC_UI <- function(id) {
  tagList(
    fluidRow(box(
      status = "success",   # makes the top of the box green.  
      column(12, offset = 1, h1(id = "main-header", "Fate of Harvested Carbon"),
             tags$style(HTML("#main-header{color: blue;}"))))),
    fluidRow( 
      box(
        #h3("Sankey Diagram"),
        alt = "Sankey diagram of harvested carbon use and decay over time",
        title = textOutput(NS(id, "MainTitle")),   # Dynamic title
        label = h3("Sankey diagram of harvested carbon use and decay over time"),
        status = "primary", 
        width = 12, 
        sankeyNetworkOutput(NS(id, "Plot_fhc")))),
    fluidRow(
      box(
        solidHeader = TRUE,
        width = 5,
        fluidRow(column(width = 4, selectInput(NS(id, "year"), label = h4("Harvest Year"),
                                               choices = 1960:1990, selected = 1990)), #, width = '50%'
                 column(width = 4, offset = 4, radioButtons(NS(id, "metrictype"), label = h3("Select Metric"),
                                                            choices = list("Tg C" = 1, "Tg CO2e" = 2), selected = 1))),
        sliderInput(
          NS(id, "num"),
          label = h4("Enter number of years for decay (between 3 and 100)"),
          min = 3,
          max = 100,
          value = 3
        ),
        fluidRow(column(width = 3, actionButton(NS(id, "action1"), label = "Change Sankey title")),
                 column(width = 6, style = "margin-top: 5px;", uiOutput(NS(id, "change_title_fhc.1"))),
                 column(width = 3, style = "margin-top: 5px;", downloadButton(NS(id, "sankey_download"), label = "Save Sankey Plot"))),
        fluidRow(column(width = 3, actionButton(NS(id, "action2"), label = "Change Decay Plot Title")),
                 column(width = 6, style = "margin-top: 5px;", uiOutput(NS(id, "change_title_fhc.2"))),
                 column(width = 3, style = "margin-top: 5px;", dl_moduleUI(NS(id, "dl_figure_fhc.2"))))
      ), 
      column(width = 7, plotOutput(NS(id, "Plot_fhcB")))
    ),
    fluidRow(column(width = 12, offset = 0, style = 'padding-left:0px; padding-right:1px; padding-top:15px; padding-bottom:15px')), # Adding a blank row.
    fluidRow(
      column(8, offset = 2, 
             box(width = 12,
                 h3("Fate of Harvested Carbon"),
                 p("Users can explore the fate of harvested carbon from a single year of harvest (chosen in the \"harvest year\" 
          dropdown bar in the bottom left portion of the screen) for 3 up to 100 years after harvest (using the slider 
          bar), in either teragrams of carbon or carbon dioxide equivalent.", style = "font-size:20px"),
                 p("The Sankey diagram, in the upper portion of the screen, begins 3 years after harvest so there is time for the harvested 
          carbon to cycle from \"products in use\" into the various discard pathways. Hovering the mouse over the vertical bars displays 
          the quantity of the carbon in that pool or category, while hovering over the shaded gray areas displays the quantity of 
          carbon that leaves that pool or category, either as a transfer to a subsequent pool or category, or as an overall emission 
          (i.e, emitted with or without energy capture). For example, carbon leaves the \"products in use\" storage pool to enter a 
          general discard category, with differing amounts of carbon then being diverted to disposal storage pools (i.e., dumps, 
          permanently inert landfill, landfill subject to decay, or recovered products in use), or emission categories (i.e., 
          compost, burned). Carbon can later leave a disposal storage pool through decay, ultimately 
          resulting in an emission.", style = "font-size:20px"),
                 p("Depending on the year of the harvest selected, certain pathways may not exist.   For example, California carbon harvested 
          in 1952 that is later retired from use depending on the appropriate end-use half-life does not have a recovered disposal 
          pathway until paper recycling begins in 1960, or wood recycling begins in 1990. Current end-use half-life and disposal 
          parameters are projected for simulations that extend beyond the last year of the inventory (e.g., 1952 harvest simulated more 
          than 67 years into the future beyond 2019, or 2019 harvest simulated any number of years after harvest).", style = "font-size:20px"),
                 p("The decay plot in the bottom right portion of the screen allows users to see the cumulative quantity of carbon for a given 
          number of years after harvest that is stored in products in use, recovered products, permanently inert products in landfills, 
          products subject to decay in landfills, or products in dumps or carbon that is emitted with or without energy capture. 
          Waste incineration for energy production is captured by \"discard energy capture\" if present in the data set.", style = "font-size:20px")
                 
             ))
    )
      )

}






plot_FateHarvC_Server <- function(id, hwp.dt, file.loc) {
  
  moduleServer(id, function(input, output, session) {
    
    # Code for activating box that allows for a figure name change
    observeEvent(input$action1, {         # Text widget will not appear if use ' NS(id, input$action) '. Instead is already there        
      output$change_title_fhc.1 = renderUI(
        textInput(NS(id, "change_title_fhc.1"), label = NULL, #h5("Enter figure title"), 
                  value = paste("Sankey diagram of carbon fate from harvest in", input$year, "after", input$num, "years"),
                  width = "700px"))
    })
    
    plotForOutput1 <- reactive({    
      
      #browser()
      hwp.data <- state.data[[which(state.choices == hwp.dt())]]     # Note: This is passed to module server from main UI, not module UI
      
      
      years <- hwp.data$years
      
      observe({
        updateSelectInput(session = session, inputId = "year", choices = years, selected = input$year) # input$year necessary to keep it from reverting to default value
      })
      
      output$MainTitle <- renderText({
        title = if (input$action1 == 0) {
          paste("Sankey diagram of carbon fate from harvest in", input$year, "after", input$num, "years") } else {
            input$change_title_fhc.1}
      })
      
      
      hwp.yr <- as.numeric(input$year)
      d.yrs <- input$num
      ownership.names <- hwp.data$ownership.names
      harv.hwp <- hwp.data$harv.hwp
      bfcf.hwp <- hwp.data$bfcf.hwp
      tpr.hwp <- hwp.data$tpr.hwp
      ppr.hwp <- hwp.data$ppr.hwp
      ratio_cat.hwp <- hwp.data$ratio_cat.hwp
      ccf_conversion.hwp <- hwp.data$ccf_conversion.hwp
      eur.hwp <- hwp.data$eur.hwp
      eu_half.lives.hwp <- hwp.data$eu_half.lives.hwp
      discard.fates.hwp <- hwp.data$discard.fates.hwp
      discard.hl.hwp <- hwp.data$discard.hl.hwp
      PIU.WOOD.LOSS <- hwp.data$PIU.WOOD.LOSS
      PIU.PAPER.LOSS <- hwp.data$PIU.PAPER.LOSS
      N.EUR <- hwp.data$N.EUR

      ownr.sel <- "Total"
      yr.index <- which(years == hwp.yr)
      ownr.index <- which(ownership.names == ownr.sel)
      source(paste0(file.loc, "Sankey_Code.r"), local = TRUE)

      sankeyNetwork(Links = links, Nodes = nodes2,
                    Source = "IDsource", Target = "IDtarget",
                    Value = "value", NodeID = "name",
                    fontSize = 14,
                    sinksRight = FALSE)
      
      #htmlwidgets::appendContent(sankey, htmltools::tags$h1("Title"))
      #htmlwidgets::onRender(sankey)
      
    })
    
    
    output$Plot_fhc <- renderSankeyNetwork({
      plotForOutput1() %>% 
        htmlwidgets::onRender('function(el) { el.getElementsByTagName("svg")[0].removeAttribute("viewBox") }')  # This portion was added so that the browser Firefox would display the Sankey correctly.  
      # See: https://stackoverflow.com/questions/51145370/tiny-plot-output-from-sankeynetwork-networkd3-in-firefox

    })
    
    
    
    output$sankey_download <-  downloadHandler(
      filename = function() {
        'SankeyPlot.html'
      },
      content = function(file) {
        saveNetwork(plotForOutput1(), file)
      })
    
    
    
    
    
    
    #################### Decay Plot #########################    
    # Code for activating box that allows for a figure name change
    observeEvent(input$action2, {         # Text widget will not appear if use ' NS(id, input$action) '. Instead is already there        
      output$change_title_fhc.2 = renderUI(
        textInput(NS(id, "change_title_fhc.2"), label = NULL, #h5("Enter figure title"), 
                  value = paste("Carbon in pools and emissions from harvest in", as.numeric(input$year)),
                  width = "700px"))
    })
    
    plotForOutput2 <- reactive({    
      
      hwp.data <- state.data[[which(state.choices == hwp.dt())]]     # Note: This is passed to module server from main UI, not module UI
      
      years <- hwp.data$years
      
      observe({
        updateSelectInput(session = session, inputId = "year", choices = years, selected = input$year) # input$year necessary to keep it from reverting to default value
      })
      # browser()
      
      hwp.yr <- as.numeric(input$year)
      d.yrs <- 100                              # Fixed to 100 years, unlike above
      blueline.yr <- input$num                  # For the plot, the input$num is now used for the vertical blue line
      ownership.names <- hwp.data$ownership.names
      harv.hwp <- hwp.data$harv.hwp
      bfcf.hwp <- hwp.data$bfcf.hwp
      tpr.hwp <- hwp.data$tpr.hwp
      ppr.hwp <- hwp.data$ppr.hwp
      ratio_cat.hwp <- hwp.data$ratio_cat.hwp
      ccf_conversion.hwp <- hwp.data$ccf_conversion.hwp
      eur.hwp <- hwp.data$eur.hwp
      eu_half.lives.hwp <- hwp.data$eu_half.lives.hwp
      discard.fates.hwp <- hwp.data$discard.fates.hwp
      discard.hl.hwp <- hwp.data$discard.hl.hwp
      PIU.WOOD.LOSS <- hwp.data$PIU.WOOD.LOSS
      PIU.PAPER.LOSS <- hwp.data$PIU.PAPER.LOSS
      N.EUR <- hwp.data$N.EUR
      
      
      ownr.sel <- "Total"
      yr.index <- which(years == hwp.yr)
      ownr.index <- which(ownership.names == ownr.sel)
      
        # browser()
      
      harv.red.hwp <- data.frame(Year = (hwp.yr + (0:(d.yrs - 1))), name = 0)
      colnames(harv.red.hwp)[2] <- eval(ownr.sel)
      harv.red.hwp[1, 2] <- harv.hwp[harv.hwp$Year == hwp.yr, ownr.index + 1]
      
      tpr.red.hwp <- tpr.hwp[,c(1, yr.index + 1)]
      ppr.red.hwp <- ppr.hwp[,c(1, yr.index + 1)]
      eur.red.hwp <- if ("data.table" %in% class(eur.hwp)) eur.hwp[, c(1, yr.index + 1), with = FALSE] else eur.hwp[, c(1, yr.index + 1)]
      
      discard.fates.red.hwp <- if (length(hwp.yr:years[length(years)]) < d.yrs) {   # Takes all discard fate data starting at hwp.yr, adds repeats if runs out of info
        xtra.yrs <- d.yrs - length(hwp.yr:years[length(years)])
        extend.disc.fates <- data.frame(matrix(unlist(rep(discard.fates.hwp[,length(years) + 2], xtra.yrs)), ncol = xtra.yrs))
        names(extend.disc.fates) <- (hwp.yr + (d.yrs - xtra.yrs)):(hwp.yr + (d.yrs - 1))
        cbind(discard.fates.hwp[, c(1:2, (yr.index + 2):(length(years) + 2))], extend.disc.fates)
      } else {
        discard.fates.hwp[, c(1:2, (yr.index + 2):(yr.index + 2 + d.yrs - 1))]
      } 
      
      # Run the HWP model that is minimized to the selected state/year/ownership for the Sankey display
      hwp.sankey.output <- HwpModel.Sankey.fcn(harv = harv.red.hwp, 
                                               bfcf = bfcf.hwp,
                                               tpr = tpr.red.hwp,
                                               ppr = ppr.red.hwp,
                                               ratio_cat = ratio_cat.hwp,
                                               ccf_conversion = ccf_conversion.hwp,
                                               eur = eur.red.hwp,
                                               eu_half.lives = eu_half.lives.hwp,
                                               discard.fates = discard.fates.red.hwp,
                                               discard.hl = discard.hl.hwp, 
                                               hwp.yr = hwp.yr, 
                                               ownership.names = ownership.names, 
                                               N.EUR = N.EUR, PIU.WOOD.LOSS = PIU.WOOD.LOSS,
                                               PIU.PAPER.LOSS = PIU.PAPER.LOSS, years = years, yr.index = yr.index,
                                               ownr.index = ownr.index, d.yrs = d.yrs)
      
      csum.pu.discard <- cumsum(apply(hwp.sankey.output$pu.discard_matrix, 2, sum))
      
      C_yrs.df <- data.frame(Decay_Years = 1:100,
                             EEC =  sum(hwp.sankey.output$eec_matrix[, 1]),
                             DEC = cumsum(apply(hwp.sankey.output$dec.input_matrix, 2, sum)),   #Discard with Energy Capture, was BWEC - Burned with Energy Capture
                             EWOEC = cumsum(apply(hwp.sankey.output$ewoec_matrix, 2, sum)),
                             Dumps = apply(hwp.sankey.output$dumps_matrix, 2, sum),
                             Landfill_Decomp = apply(hwp.sankey.output$landfill_matrix, 2, sum),
                             Landfill_Perm = cumsum(apply(hwp.sankey.output$lf.fixed_matrix, 2, sum)),
                             Recovered = apply(hwp.sankey.output$recov_matrix, 2, sum), 
                             PIU = sum(hwp.sankey.output$eu.reduced_matrix[,1]) - csum.pu.discard
      )
      
      names(C_yrs.df) <- c("Years of Decay", "Emitted with Energy Capture", "Discard Energy Capture", "Emitted without Energy Capture", "Dumps", "Decomposing Landfill", "Permanent Landfill", "Recovered", "Products in Use")
      
      rem.cols <- any(apply(as.matrix(C_yrs.df), 2, sum) == 0) # This bit of code removes columns if any columns were only zero in value
      if (rem.cols) C_yrs.df <- {
        rem.cols.id <- which(apply(as.matrix(C_yrs.df), 2, sum) == 0)
        C_yrs.df[, -rem.cols.id]
      }
      
      C_yrs.df2 <- C_yrs.df %>% pivot_longer(cols = 2:ncol(C_yrs.df), names_to = "Pools_Emissions", values_to = "MMT C")
      C_yrs.df2$`MMT C` <- C_yrs.df2$`MMT C`/1e6
      
      if (input$metrictype == "2") {    # Changing MMT C / Tg C values to CO2e if Tg CO2e selected
        C_yrs.df2$`MMT C` <- C_yrs.df2$`MMT C` * 44/12
      }        
      
      y.vec <- C_yrs.df2 %>% group_by(`Years of Decay`) %>% summarize(sum.y = sum(`MMT C`)) %>% select(sum.y)
      
      C_yrs.y <- y.axis.fcn(c(0, y.vec$sum.y), F)
      
      y.lab.txt <- switch(as.numeric(input$metrictype), "Tg C", expression("Tg C"*O[2]*e))
      
      gg.x <- ggplot(C_yrs.df2, aes(`Years of Decay`, `MMT C`, fill = factor(Pools_Emissions, levels = names(C_yrs.df)))) + 
        geom_area(alpha = 0.6, color = "white") + 
        theme_bw() + 
        scale_fill_viridis(discrete = T, 
                           begin = 1, end = 0) + 
        scale_x_continuous(breaks = seq(0, 100, by = 10)) +
        scale_y_continuous(breaks = seq(0, C_yrs.y$max.y, by = C_yrs.y$breaks.y), limits = c(0, C_yrs.y$max.y), expand = c(0, 0)) +
        labs(fill = "", x = "Years of Decay", y = y.lab.txt,
             title = if (input$action2 == 0) paste("Carbon in pools and emissions from harvest in", hwp.yr) else input$change_title_fhc.2) + 
        geom_vline(xintercept = blueline.yr, color = "blue", alpha = 0.4, size = 2) +
        theme(text = element_text(size = 18))
      
      gg.y <- gg.x +  guides(fill = guide_legend(title = element_blank(),
                                                 nrow = 2, byrow = T, override.aes = list(size = 1), reverse = F)) +
        theme(legend.position = "bottom")
      
      list(gg.x, gg.y)
      
    })
    
    output$Plot_fhcB <- renderPlot(plotForOutput2()[[1]])
    
    # The output for the Plot4, when saved as a png file, makes the legend image and font size too large.  The gg.x2 version corrects this, and is called here.
    plot2.for.printing <- eventReactive(plotForOutput2(), {
      plotForOutput2()[[2]]})
    
    # Server module for saving the file.
    callModule(id = "dl_figure_fhc.2", module = dl_module_server, plot_output = plot2.for.printing, plot_name = "Plot_FateHarvC.2.png", plot_height = 5, plot_width = 8)
    
    
  })
}      








