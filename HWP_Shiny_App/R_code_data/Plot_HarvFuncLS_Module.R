### Distribution of timber harvested into different product half-life categories (Plot of Utilized Timber Half Lives)


plot_HarvFuncLS_UI <- function(id) {
  tagList(
    fluidRow(box(width = 7,
      status = "success",   # makes the top of the box green.  
      column(12, offset = 1, h1(id = "main-header", "Harvest by Functional Lifespan"),
             tags$style(HTML("#main-header{color: blue;}"))))),
    fluidRow(box(width = 4,
                 selectInput(NS(id, "select"), label = h3("Select Figure Type"), 
                             choices = list("Harvest by end-use half-life" = 1, 
                                            "Proportional harvest by end-use half-life" = 2
                             ), 
                             selected = 1))),
    box(width = 12,
        fluidRow(column(9, plotOutput(NS(id, "Plot_hfls"), height = 600)),
                 column(width = 2, radioButtons(NS(id, "metrictype"), label = h3("Select Metric"),
                                                choices = list("Tg C" = 1, "Tg CO2e" = 2), selected = 1), 
                        fluidRow(actionButton(NS(id, "action"), label = "Change figure title")),
                        fluidRow(uiOutput(NS(id, "change_title_hfls"))),
                        fluidRow(dl_moduleUI(NS(id, "dl_figure_hfls"))))
        )#   fluidRow(column(width = 6, offset = 3, uiOutput(NS(id, "change_title_hfls"))))
    ),
    fluidRow(column(width = 12, offset = 0, style = 'padding-left:0px; padding-right:1px; padding-top:15px; padding-bottom:15px')), # Adding a blank row.
    fluidRow(
      column(8, offset = 2, 
             box(width = 12,
                 h3("Harvest by Functional Lifespan"),
                 p("This figure shows the quantity of carbon or carbon 
                   dioxide equivalent harvested in each year that is allocated to end-uses that have short, medium, or long half-lives 
                   (i.e., functional lifespans), or harvested carbon that is instantly oxidized as fuelwood. For example, harvested 
                   carbon that becomes lumber can be used in a single family home end-use with a long functional lifespan, while lumber 
                   in a manufactured product end-use has a medium functional lifespan, while lumber used in the packaging and shipping 
                   end-use has a short functional lifespan. Fuelwood includes landscaping materials and unutilized mill residue.", style = "font-size:20px"),
                 p("Using the dropdown menu, users can also explore the proportion of carbon harvested each year that is allocated to 
                   end-uses that have short, medium, or long functional lifespans.", style = "font-size:20px")
             )     
      ))
  )
}


plot_HarvFuncLS_Server <- function(id, hwp.dt) {

  moduleServer(id, function(input, output, session) {
    
    
    # Code for activating box that allows for a figure name change
    observeEvent(input$action, {         # Text widget will not appear if use ' NS(id, input$action) '. Instead is already there 
      output$change_title_hfls = renderUI(
        textInput(NS(id, "change_title_hfls"), label = h4("Enter figure title"), 
                  value = if (input$select == "1") {
                    'Annual allocation of harvested carbon to instant oxidation or short, medium, and long-lived end-uses'} else {
                      "Proportional allocation of harvested carbon to instant oxidation or short, medium, and long-lived end-uses"
                    }))#, 
                  #width = "700px"))
    })
    
    
    
    plotForOutput <- reactive({
      
      hwp.data <- state.data[[which(state.choices == hwp.dt())]]     # Note: This is passed to module server from main UI, not module UI
      
      #browser()
      
      sml <- hwp.data$eu_half.lives.hwp %>%
        mutate(ShortMedLong = ifelse(EU_HalfLife > 0 & EU_HalfLife <= 6, "Short", 
                                     ifelse(EU_HalfLife > 6 & EU_HalfLife <= 30, "Medium", 
                                            ifelse(EU_HalfLife > 30, "Long", "Fuel"))))
      
      sml2 <-   cbind(sml, hwp.data$eu_array[, hwp.data$N.OWNERSHIP, ]) %>% 
        pivot_longer(cols = 4:(hwp.data$N.YEARS + 3), names_to = "years", values_to = "MMTC") %>%
        group_by(ShortMedLong, years) %>%
        summarize(sumMMTC = sum(MMTC), .groups = "drop_last")
      
      totals <- sml2 %>% group_by(years) %>%
        summarize(totalMMTC = sum(sumMMTC))
      
      sml3 <- left_join(sml2, totals, by = "years") %>%
        mutate(pct = sumMMTC / totalMMTC)
      sml3$ShortMedLong <- factor(sml3$ShortMedLong, levels = c("Fuel", "Short", "Medium", "Long"))
      if (input$metrictype == "2") {
        sml3$sumMMTC <- sml3$sumMMTC * 44/12 
        sml3$totalMMTC <- sml3$totalMMTC * 44/12
      }
      
      y.sml.mmtc <- y.axis.fcn(sml3$totalMMTC, TRUE)
      
      plot.type <- as.numeric(input$select)
      
      
      if (plot.type == 1) {
        y.sml3 <- sml3$sumMMTC / 1e6
        breaks.sml3 <- seq(0, y.sml.mmtc$max.y, by = y.sml.mmtc$breaks.y)
        limits.sml3 <- c(0, y.sml.mmtc$max.y)
        title.sml3 <- "Annual allocation of harvested carbon to instant oxidation or short, medium, and long-lived end-uses"
      } else {
        y.sml3 <- sml3$pct
        breaks.sml3 <- seq(0, 1, by = 0.2)
        limits.sml3 <- c(0, 1.01)
        title.sml3 <- "Proportional allocation of harvested carbon to instant oxidation or short, medium, and long-lived end-uses"
      }
      
      yaxis.lab <- if (input$select == "1" & input$metrictype == "1") "Tg C per year" else if (
        input$select == "1" & input$metrictype == "2") expression("Tg C"*O[2]*e) else "Annual proportion of harvest carbon"
      
      title.sml3 <- if (input$action == 0) title.sml3 else input$change_title_hfls
      
      
      ggplot(sml3, aes(as.numeric(years), y.sml3, fill = ShortMedLong)) +
        geom_area(alpha = 0.6, color = "white") + 
        theme_bw() + 
        scale_fill_viridis(discrete = T,begin = 1, end = 0, 
                           labels = c("Fuel (burned immediately", "Short (1-6 years)", "Medium (7-30 years)", "Long (31+ years)")) +
        scale_x_continuous(breaks = seq(hwp.data$MIN.PLOT.YR, hwp.data$MAX.PLOT.YR, by = 10), limits = c(hwp.data$MIN.PLOT.YR, hwp.data$MAX.PLOT.YR)) +
        scale_y_continuous(breaks = breaks.sml3, limits = limits.sml3, expand = c(0, 0)) +
        labs(x = "Harvest Year", y = yaxis.lab, title = wrapper(title.sml3, 70)) +  #, fill = "Product in\nuse halflife") +
        guides(fill = guide_legend(title = element_blank(), nrow = 1, byrow = T, override.aes = list(size = 1))) +
        theme(text = element_text(size = 20), legend.position = "bottom", #legend.text = element_text(size=15),
              panel.background = element_rect(fill = 'lightgrey'))
      
      
      
      
    })
    
    output$Plot_hfls <- renderPlot(plotForOutput()) 
    
    # Server module for saving the file.
    callModule(id = "dl_figure_hfls", module = dl_module_server, plot_output = plotForOutput, plot_name = "Plot_HarvFuncLS.png", plot_height = 5, plot_width = 8)
    
  })
}


