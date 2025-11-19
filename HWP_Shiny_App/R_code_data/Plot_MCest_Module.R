################## MC estimates module ##########################


plot_MCest_UI <- function(id) {
  tagList(
    fluidRow(box(
      #status = "success",   # makes the top of the box green.  
      column(12, offset = 1, h1(id = "main-header", "Monte Carlo Estimates")))),
    fluidRow(box(width = 4,
                 selectInput(NS(id, "select"), label = h3("Select Monte Carlo Figure Option"), 
                             choices = list("Cumulative carbon in individual storage and emission pools" = 1,
                                            "Cumulative carbon in storage pools combined (Products in use + SWDS)" = 2,
                                            "Monte Carlo convergence evaluation" = 3
                             ), 
                             selected = 1,
                             width = "400px"))),
    fluidRow(box(width = 12, 
                 column(width = 8, plotOutput(NS(id, "Plot_mc.est"), height = 600)),
                 column(width = 4, 
                        fluidRow(width = 12, radioButtons(NS(id, "metrictype"), label = h4("Select Metric"),
                                                          choices = list("Tg C" = 1, "Tg CO2e" = 2), selected = 1)),
                        fluidRow(width = 12, actionButton(NS(id, "action"), label = "Change figure title")),
                        fluidRow(width = 12, dl_moduleUI(NS(id, "dl_figureMC")))
                 )
    )
    ),
    fluidRow(column(width = 12, offset = 0, style = 'padding-left:0px; padding-right:1px; padding-top:15px; padding-bottom:15px')), # Adding a blank row.
    fluidRow(
      column(8, offset = 2, 
             box(width = 12,
                 h3("Monte Carlo estimates"),
                 p("Using the dropdown menu in the top left portion of the screen, users can view Monte Carlo analysis of uncertainty 
                   in the estimates for the individual carbon storage pools and emissions categories. The yellow line represents the 
                   estimated cumulative mean, with the black shading representing a 90% confidence interval.", style = "font-size:20px"),
                 p("The figures show estimated pool values, either separate or combined into a single pool. For the figure \"Cumulative carbon in
                   storage pools combined\" the Monte Carlo added each iteration's value for Solid Waste Disposal Sites and Products in Use.", style = "font-size:20px"),
                 p("Lastly, using the dropdown menu, users can view the Monte Carlo convergence evaluation which shows, for the final year of data,
                 how the Monte Carlo's estimate settles as more iterations (up to a point) are included in the model.  This figure serves as a tool 
                 for users to determine how many iterations to use.  For more details, see ", a("this section", href = "https://jeremygroom.github.io/HWP-vR-Documentation/model.html#model-mc-res"), 
                   " of the documentation). ", style = "font-size:20px"),
                 p("A full description of the Monte Carlo analysis process can be ", a("found here", 
                                                                                       href = "https://jeremygroom.github.io/HWP-vR-Documentation/model.html#model-mc"), ".", style = "font-size:20px")
             )     
      ))
  )
}


plot_MCest_Server <- function(id, hwp.dt, file.loc, mc_trigger) {
  
  moduleServer(id, function(input, output, session) {

    state.data.sel <- reactive({
      mc_trigger()  # This is necessary to incorporate the changes to the dataset from the Monte Carlo run of new data.
      # If no MC output are available, the as-is data are passed on. 
      mc.dat <- state.data[[which(state.choices == hwp.dt())]]
      mc.dat
      }) 
        
    counter <- reactiveValues(countervalue = 0)  # Setting a counter to register if plots are switched. If they are, the titles revert to original titles
    
    observeEvent(input$action, {  # If the user wants to change the title the counter changes to one and the figure's title can be changed.
      #browser()
      if (counter$countervalue == 0) {
        counter$countervalue <- 1
        
        insertUI(                                    # NOT renderUI.  This is better for having added and then taken away again (see below).
          selector = paste0("#", NS(id, "action")),
          where = "afterEnd",
          textInput(NS(id, "change_titleMC"), label = h4("Enter figure title"), 
                    value = switch(as.numeric(input$select),
                                   paste0("Monte Carlo mean (yellow line) and ", 100 * state.data.sel()$MC.CI.REPORT, "% confidence intervals\n(black shading) for carbon in storage and emissions pools"),
                                          paste0("Monte Carlo mean (yellow line) and ", 100 * state.data.sel()$MC.CI.REPORT, "% confidence intervals (black shading) for carbon in storage pools combined (products in use and solid waste disposal sites)"),
                                   NULL
                    )))#, 
        #width = "700px"))
      }
    })
    
    
    observeEvent(input$select, {  # If the user changes figures the counter resets and the original title reappears
      #browser()
      if (counter$countervalue == 1) {
        removeUI(
          selector = paste0("div:has(> #", NS(id, "change_titleMC"), ")")  # If just "#change_titleMC" it fails to remove the title.  If "div:has(>> #change_titleMC)"
        )                                                                  #   it removes the UI permanently, button and all.
      }
      
      counter$countervalue <- 0
    })
    
    
 
    
    observe({  # If no ownership data present, the "plotselect" and download buttons are disabled. 
      #### NOTE: The ID used for disabling the download button required a combination of two IDs, one for the 
      ###       dl_moduleUI (dl_figure4) and one for the ID within the module (figure_download). 
      ##         See: https://stackoverflow.com/questions/64867941/rshiny-enable-disable-ui-modules
      toggleState("dl_figureMC-figure_download", condition = is.null(state.data.sel()$mc_iter_results) == FALSE)
      toggleState("action", condition = is.null(state.data.sel()$mc_iter_results) == FALSE)
    })  
    
    
    plotForOutput <- reactive({
      #browser()
      hwp.data <- state.data.sel()     # Note: This is passed to module server from main UI, not module UI
      
      
      plot.type <- as.numeric(input$select)
      #browser()
      y.lab.cc <- "Tg C"   # Setting the y-axis label 
      if (input$metrictype == "2") {  # Changes to the plots if metric is Tg CO2e
        hwp.data$mc_plot[, 3:5] <- hwp.data$mc_plot[, 3:5] * 44/12
        hwp.data$mc_PoolsTotalPlot[, 2:4] <- hwp.data$mc_PoolsTotalPlot[, 2:4] * 44/12
        hwp.data$mc_iter_results$C <- hwp.data$mc_iter_results$C * 44/12
        y.lab.cc <- expression("Tg C"*O[2]*e)
      }  
      
      #  browser()
      if (is.null(hwp.data$mc_iter_results) == TRUE) {
        img <- readPNG(paste0(file.loc, "LakePic_MC2.png"))  # length 1.9 * height
        # plot with picture as layer
        library(ggplot2)
        plot.mc <- ggplot_pic(img)  # see PlotFunctions1.r
        
      } else if (is.null(hwp.data$mc_iter_results) == FALSE & plot.type == 1) {
        title.use <- paste0("Monte Carlo mean (yellow line) and ", 100 * state.data.sel()$MC.CI.REPORT, "% confidence intervals (black shading) for carbon in storage and emission pools")
        
        plot.mc <- ggplot(hwp.data$mc_plot, aes(Year, Means/1e6)) +
          geom_ribbon(aes(ymin = lci/1e6, ymax = uci/1e6), ) +
          geom_line(color = "yellow") + 
          facet_wrap(vars(Type.M), labeller = hwp.data$C.names) + 
          labs(x = NULL, y = y.lab.cc, 
               title = if (counter$countervalue == 0) wrapper(title.use, 75) else wrapper(input$change_titleMC, 75)) + # See functions for 'wrapper'
          #theme_bw() +
          theme(text = element_text(size = 20),
                axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
        
      } else if (is.null(hwp.data$mc_iter_results) == FALSE & plot.type == 2) {
        title.use2 <- paste0("Monte Carlo mean (yellow line) and ", 100 * state.data.sel()$MC.CI.REPORT, "% confidence intervals (black shading) for\ncarbon in storage pools combined (products in use and solid waste disposal sites)")
        
        
        plot.mc <- ggplot(hwp.data$mc_PoolsTotalPlot, aes(Year, Mean)) +
          geom_ribbon(aes(ymin = lci, ymax = uci), ) +
          geom_line(color = "yellow") + 
          #          scale_x_continuous(breaks = seq(hwp.data$MIN.PLOT.YR, hwp.data$MAX.PLOT.YR, by = 10), limits = c(hwp.data$MIN.PLOT.YR, hwp.data$MAX.PLOT.YR), minor_breaks = NULL) +
          labs(x = NULL, y = y.lab.cc, 
               title = if (counter$countervalue == 0) wrapper(title.use2, 75) else wrapper(input$change_titleMC, 75)) + 
          theme_bw() +
          theme(text = element_text(size = 20))
        
      } else if (is.null(hwp.data$mc_iter_results) == FALSE & plot.type == 3) {    
        #browser()
        end.yr <- hwp.data$mc_PoolsTotalPlot$Year[nrow(hwp.data$mc_PoolsTotalPlot)]
        title.use3 <- paste0("Convergence evaluation with ", hwp.data$N.ITER, " iterations for products in use + solid waste disposal sites, ", end.yr)
        
        hwp.data$mc_iter_results$facet.labs <- as.character(sapply(hwp.data$mc_iter_results$stat, switch, 
               "mean" = "Mean", "se" = "Standard Error", 
               "ciUCI" = paste0(100 * state.data.sel()$MC.CI.REPORT, "% Confidence Interval, Upper Limit"), 
               "ciLCI" = paste0(100 * state.data.sel()$MC.CI.REPORT, "% Confidence Interval, Lower Limit")))
        
        hwp.data$mc_iter_results$C <- hwp.data$mc_iter_results$C/1e6
        plot.mc <- ggplot(hwp.data$mc_iter_results, aes(iter, C)) +
          geom_line() +
         # geom_vline(xintercept = hwp.data$BURN.IN, color = "magenta") +
          facet_wrap(~ facet.labs, scales = "free_y") +
          labs(x = "Iterations", y = y.lab.cc, title = if (counter$countervalue == 0) {wrapper(title.use3, 75)} else wrapper(input$change_titleMC, 75)) +
          theme(text = element_text(size = 20))
        
      }
      
      plot.mc
    })
    
    
    output$Plot_mc.est <- renderPlot(plotForOutput() )
    
    # Server module for saving the file.
    callModule(id = "dl_figureMC", module = dl_module_server, plot_output = plotForOutput, plot_name = "Plot_MC.png", plot_height = 5, plot_width = 8)
    
  })    
  
}

