# The "Contact Us" page


plot_ContactUs_UI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      box(width = 8,
        #status = "success",   # makes the top of the box green.  
        column(width = 11, offset = 1, h1(id = "main-header", "For questions about this tool, datasets, or accessibility issues, please contact us")
        )
      )
    ),
    fluidRow(
      column(width = 3, offset = 1, align = "center", 
             box(width = 12,
               imageOutput(ns("ga_logo")),
             #  br(),
               p("Jeremy Groom, Ph.D."),
               p("Groom Analytics, LLC"),
               p("Jeremy@GroomAnalytics.com"),
               p("541.602.7436"),
             p(a("GroomAnalytics.com", href = 'http://www.groomanalytics.com')))
             
      ),
      column(width = 3, align = "center", 
             box(width = 12,
                 imageOutput(ns("ca_logo")),
                 #  br(),
                 p("Nadia Tase"),
                 p("Climate Change and Forest Inventory Specialist"),
                 p("Fire and Resource Assessment Program"),
                 p("California Department of Forestry and Fire Protection"),
                 p("Nadia.Tase@fire.ca.gov"),
                 p("530.573.2320"))
             ),
      column(width = 3,  align = "center", 
             box(width = 12,
                 imageOutput(ns("odf_logo")),
                 #  br(),
                 p("Andrew Yost"),
                 p("Forest Ecologist"),
                 p("Oregon Department of Forestry"),
                 p("Andrew.YOST@odf.oregon.gov"),
                 p("503.945.7410"))
             ),
    )
    
  )
  
}

plot_ContactUs_Server <- function(id, file.loc) {
  moduleServer(id, function(input, output, session) {
    
    output$ga_logo <- renderImage({
      return(list(
        src = paste0(file.loc, "GroomAnalyticsH.png"),
        width = "240px",
        height = "114px",
        contentType = "image/png",
        alt = "Groom Analytics"
      ))
      #logos <- readPNG(paste0(file.loc, "logo_strip.png")) 
      #ggplot_pic(logos)
    },
    deleteFile = FALSE)

    
    output$ca_logo <- renderImage({
      return(list(
        src = paste0(file.loc, "CalFIRE.jpg"),
        width = "160px",
        height = "192px",
        contentType = "image/jpg",
        alt = "Cal FIRE"
      ))
      #logos <- readPNG(paste0(file.loc, "logo_strip.png")) 
      #ggplot_pic(logos)
    },
    deleteFile = FALSE)
    
    output$odf_logo <- renderImage({
      return(list(
        src = paste0(file.loc, "ODFCOLOR nslog  small.jpg"),
        width = "180px",
        height = "177px",
        contentType = "image/jpg",
        alt = "Oregon Dept Forestry"
      ))
      #logos <- readPNG(paste0(file.loc, "logo_strip.png")) 
      #ggplot_pic(logos)
    },
    deleteFile = FALSE)
    
    
        
  })
}