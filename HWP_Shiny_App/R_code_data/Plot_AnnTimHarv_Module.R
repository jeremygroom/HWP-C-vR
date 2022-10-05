## Annual Timber Harvest: Using End Use values to summarize total carbon harvested in TgCO2/TgCO2e, which are the carbon product values 
#     prior to any loss or product in use calculations.

plot_AnnTimHarv_UI <- function(id) {
  tagList(
    
    useShinyjs(),
    fluidRow(box(
      #status = "success",   # makes the top of the box green.  
      column(12, offset = 1, h1(id = "main-header", "Annual Timber Harvest")))),
    box(width = 12,
        status = "primary", 
        fluidRow(column(width = 9, 
                        plotOutput(NS(id, "Plot_ath"), height = 600)),
                 column(width = 3,
                        fluidRow(radioButtons(NS(id, "plotselect"), label = h3("Select Detail"),
                                              choices = list("Ownership and Total" = 1, "Ownership" = 2, "Total" = 3), 
                                              selected = 1)),
                        fluidRow(radioButtons(NS(id, "metrictype"), label = h3("Select Metric"),
                                              choices = list("Tg C" = 1, "Tg CO2e" = 2, "BBF" = 3), #subscript: https://stackoverflow.com/questions/32169748/how-can-i-have-html-elements-as-labels-for-radio-buttons
                                              selected = 1)),
                        fluidRow(radioButtons(NS(id, "ann.cumu"), label = h3("Select Summary"),
                                              choices = list("Annual" = 1, "Cumulative" = 2), 
                                              selected = 1)),
                        fluidRow(actionButton(NS(id, "action"), label = "Change figure title")),
                        fluidRow(dl_moduleUI(NS(id, "dl_figure_ath"))),
                        fluidRow(column(width = 11, uiOutput(NS(id, "change_title")))))
        )),
    fluidRow(column(width = 12, offset = 0, style = 'padding-left:0px; padding-right:1px; padding-top:15px; padding-bottom:15px')), # Adding a blank row.
    fluidRow(
      column(8, offset = 2, 
             box(width = 12,
                 h3("Annual Timber Harvest"),
                 p("Users can explore annual or cumulative timber harvest (i.e., timber product output) by ownership in different units 
          (i.e., billion board feet = BBF, teragrams of carbon = Tg C, or teragrams of carbon dioxide equivalent = Tg CO", tags$sub(2), "e). ", 
                   style = "font-size:20px")
             )
      )
    )
  )
}




plot_AnnTimHarv_Server <- function(id, hwp.dt) {
  
  moduleServer(id, function(input, output, session) {
    # browser()
    
    title.adj <- reactive({
      Ann.or.cumu <- if (input$ann.cumu == "1") "Annual" else "Cumulative"
      Own.or.tot <- if (input$plotselect == "3") "" else "by ownership"
      paste0(Ann.or.cumu, " timber harvest output ", Own.or.tot)
    })
    

    # Code for activating box that allows for a figure name change
    observeEvent(input$action, {         # Text widget will not appear if use ' NS(id, input$action) '. Instead is already there        
      output$change_title = renderUI(
        textInput(NS(id, "change_title"), label = h3("Enter figure title"), value = title.adj() ))#"Timber harvest volume by year and ownership"))
    })

    # The selected data are used for plotting and for controlling the UI; thus, defined early.
    state.data.sel <- reactive(state.data[[which(state.choices == hwp.dt())]])   
    
    # This forces the "plotselect" radio button to "Total" if no ownership data are present
    observe({    
      if (ncol(state.data.sel()$harv.hwp) == 2) {
    #observeEvent(ncol(state.data.sel()$harv.hwp) == 2, ignoreInit = TRUE, {    
      #browser()
        #if(length(state.data.sel()$ownership.names) == 2)  browser()
        updateRadioButtons(session, inputId = "plotselect", selected = 3)
       # browser()
      }
    })
    
    observe({  # If no ownership data present, the "plotselect" radio buttons are disabled. 
      toggleState("plotselect", condition = ncol(state.data.sel()$harv.hwp) != 2)
    })      
    
    
    plotForOutput <- reactive({           # Provided as a reactive so that it may be used both for plotting on webpage and saving as file
      # Figure code
        hwp.data <- state.data.sel()#state.data[[which(state.choices == hwp.dt())]]   # Note: This is passed to module server from main UI, not module UI
      plot.type <- as.numeric(input$plotselect)  # This is disabled (see above) if no ownership data are available.
      plot.metric <- as.numeric(input$metrictype)
      
      # Tg C, total
      f1 <- apply(hwp.data$eu_array[, hwp.data$N.OWNERSHIP, ], 2, sum)   # sum of all Total values for End Uses across years
      f1.1 <- data.frame(years = hwp.data$yearsTPO, values = f1/1e6)
      f1.1$Ownership <- hwp.data$ownership.names[1]
      if (input$ann.cumu == "2") f1.1$values <- cumsum(f1.1$values) 
      y1.1 <- y.axis.fcn(f1.1$values, FALSE)
      
      # Tg C CO2e total
      f1.1$valuesCO2e <- f1.1$values * 44/12
      y1.1CO2e <- y.axis.fcn(f1.1$valuesCO2e, FALSE)
      
      
      #BBF, total
      f1.1.bbf <- f1.1
      f1.1.bbf$values <- hwp.data$harv.hwp$Total/1e6   # Harvest values originally in BBF. This is why f1 loaded eu_array (transformation was done)
      if (input$ann.cumu == "2") f1.1.bbf$values <- cumsum(f1.1.bbf$values)
      y1.1.bbf <- y.axis.fcn(f1.1.bbf$values, FALSE)
      
      y1.lims <- switch(plot.metric,
                        y1.1,
                        y1.1CO2e,
                        y1.1.bbf)

      use.value <- switch(plot.metric, # When plotting the data, determining which values column to use (pertains to annual data).
                          "values", 
                          "valuesCO2e", 
                          "values")
      
      y.lab.sel <- switch(plot.metric,   # Y axis label selection for plots.
                          "Timber Product Output (Tg C)",
                          expression("Timber Product Output (Tg C"*O[2]*e*")"),
                          "Timber Product Output (BBF)")
      #if (input$ann.cumu == "2") browser()
      
     if (ncol(hwp.data$harv.hwp) > 2) {  # If ownership = more than just Total
        f1.c <- t(apply(hwp.data$eu_array, MAR = c(2, 3), sum))  # Finding sum of EU categories by year and ownership
        f1.c <- data.frame(f1.1$years, f1.c)
        names(f1.c) <- c('years', hwp.data$ownership.names)
        f1.c <- f1.c %>% select(-Total)
        #names(f1.c) <- c("years", hwp.data$ownership.names[hwp.data$ownership.names != "Total"])
        if (input$ann.cumu == "2") {

          f1.c[, 2:ncol(f1.c)] <- cumu.fcn(as.matrix(f1.c[, 2:ncol(f1.c)]), f1.1, hwp.data$OWNERSHIP_STARTYEAR, hwp.data$years)  
        }        
        
              f1.c <- f1.c %>%
                #mutate(years = hwp.data$years) %>%
                pivot_longer(2:(all_of(hwp.data$N.OWNERSHIP)), names_to = "Ownership", values_to = "values")
              f1.c$Ownership <- factor(f1.c$Ownership, levels = hwp.data$ownership.names[1:(hwp.data$N.OWNERSHIP - 1)])
              f1.c$values <- f1.c$values/1e6
              f1.c$valuesCO2e <- f1.c$values * 44/12
        
        f1.c.bbf <- hwp.data$harv.hwp %>% select(-Total)
        f1.c.bbf[is.na(f1.c.bbf)] <- 0
        #f1.c.bbf$Total <- ifelse(sum(f1.c.bbf[, 2:(hwp.data$N.OWNERSHIP + 1)]) == 0, f1.c.bbf$Total, 0)
        
        if (input$ann.cumu == "2") {
          f1.c.bbf[, 2:ncol(f1.c.bbf)] <- cumu.fcn(as.matrix(f1.c.bbf[, 2:ncol(f1.c.bbf)]), f1.1.bbf, hwp.data$OWNERSHIP_STARTYEAR, hwp.data$years)  
        }
        f1.c.bbf <- f1.c.bbf %>% 
          pivot_longer(2:(hwp.data$N.OWNERSHIP), names_to = "Ownership", values_to = "values")
        f1.c.bbf$Ownership <- factor(f1.c.bbf$Ownership, levels = hwp.data$ownership.names)
        f1.c.bbf$values <- f1.c.bbf$values/1e6
        colnames(f1.c.bbf)[1] <- "years"
      #  if (input$ann.cumu == "2") browser()
        
        if (plot.metric %in% 1:2) {
          data1 <- f1.1
          data2 <- f1.c} else {
            data1 <- f1.1.bbf
            data2 <- f1.c.bbf
          }
        
        p <-  ggplot(data2, aes(years, get(use.value), fill = Ownership)) + 
          scale_fill_viridis(discrete = T, 
                             begin = 1, end = 0) +
          scale_x_continuous(breaks = seq(hwp.data$MIN.PLOT.YR, hwp.data$MAX.PLOT.YR, by = 10), limits = c(hwp.data$MIN.PLOT.YR, hwp.data$MAX.PLOT.YR)) +
          scale_y_continuous(breaks = seq(0, y1.lims$max.y, by = y1.lims$breaks.y), limits = c(0, y1.lims$max.y), expand = c(0, 0)) +
          labs(y = y.lab.sel, x = "Harvest Year", title =  if (input$action == 0) title.adj() else input$change_title) + 
          theme_bw() +
          theme(text = element_text(size = 20), legend.position = "bottom",
                panel.background = element_rect(fill = 'lightgrey'))

        p1 <- p + geom_area(alpha = 0.8, color = "white") + 
          geom_line(data = data1, aes(color = "black"), size = 0.75) + 
          scale_color_manual(values = "black", name = "Total", labels = NULL)
        p2 <- p + geom_area(alpha = 0.8, color = "white")   
        p3 <- p + geom_line(data = data1, size = 0.75) 

        switch(plot.type,
               p1,
               p2, 
               p3)
  #      if(plot.type == 1) p1 else {if(plot.type == 2) p2
  #        else p3}

                # Continuing if-else for totals-only data (no ownerships)
      } else {
        if (plot.metric %in% 1:2) {
          data1 <- f1.1
        } else {
          data1 <- f1.1.bbf
        }
#        y.lab.sel <- if(plot.metric == 1) "Timber Product Output (Tg C)" else "Timber Product Output (BBF)"

        p1 <- ggplot(data1, aes(years, get(use.value))) + 
          geom_line(size = 0.75) +
          scale_x_continuous(breaks = seq(hwp.data$MIN.PLOT.YR, hwp.data$MAX.PLOT.YR, by = 10), limits = c(hwp.data$MIN.PLOT.YR, hwp.data$MAX.PLOT.YR)) +
          scale_y_continuous(breaks = seq(0, y1.lims$max.y, by = y1.lims$breaks.y), limits = c(0, y1.lims$max.y), expand = c(0, 0)) +
          labs(y = y.lab.sel, x = "Harvest Year", title =  if (input$action == 0) title.adj() else input$change_title) + 
          theme_bw() +
          theme(text = element_text(size = 20),
                panel.background = element_rect(fill = 'lightgrey'))
        p1
      }
      
    })
    
    output$Plot_ath <- renderPlot(plotForOutput())
    
    callModule(id = "dl_figure_ath", module = dl_module_server, plot_output = plotForOutput, plot_name = "Plot_ath.png", plot_height = 5, plot_width = 8)
    #  I could not get the above module to function with a moduleServer() call.  Note that plotForOutput is not sent as a reactive, but called as a reactive from within 
    #   the module.
    #   See the website: https://community.rstudio.com/t/examples-of-putting-downloadhandler-in-a-function/3993/5
  }
  )
  
}







