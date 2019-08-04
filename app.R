
# https://appsilon.com/fast-data-lookups-in-r-dplyr-vs-data-table/
# https://stackoverflow.com/questions/20039335/what-is-the-purpose-of-setting-a-key-in-data-table/20057411#20057411


# Cleaning
rm(list = ls(all=TRUE))

suppressWarnings(# gives warning on Windows
  
  # Sys.setlocale('LC_CTYPE',"de_DE.UTF-8") 
  Sys.setlocale('LC_MONETARY',"de_DE.UTF-8") 
  )

## DEPENDENCIES -------------------------------------------------------------------------
library(shiny)
library(shinydashboard)
library(tidyverse)
library(glue)
library(DT)
library(readr)
library(colourpicker)


setwd("C:/darbo/kodas/R/Saulute/rstudio_driling_data/myDashAppX")

source("helpers.R")


## DATA IMPORT --------------------------------------------------------------------------
dat <- read_csv("www/budget.csv")


## UI -----------------------------------------------------------------------------------
## |__sidebar ---------------------------------------------------------------------------

years <- c(2015:2018)

months <- c("All Year", "January", "February", "March",
            "April", "May", "June", "July", "August", "September",
            "October", "November", "December")

sidebar <- dashboardSidebar(
  includeCSS("www/style.css"),
  sidebarMenu(id = "sidebar_menu",
              selectInput("year", "Year: ", years, 2017, selectize = FALSE),
              selectInput("month", "Month: ", months, selected = "All Year", selectize = FALSE),
              hr(), 
              menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
              menuItem("Studio", tabName = "studio", icon = icon("th")),
              hr(), 
              uiOutput("menuItem_specific_ui")
  )
)


body <- dashboardBody(
  fluidRow(
    div(valueBoxOutput("incoming"), class = "special"),
    div(valueBoxOutput("outgoing"), class = "special"),
    div(valueBoxOutput("left"),     class = "special")
  ),
  tabItems(
    ## |_____dashboard tab --------------------------------------------------------------
    tabItem("dashboard",
            tabBox(id = "tabs", width = 12,
                   tabPanel(title = "Cash flows", value = "main",
                            fluidRow(
                              box(width = 6, DT::dataTableOutput("main_table")),
                              box(width = 6, plotOutput("main_plot", click = "main_plot_click"))
                            )
                   )
            )
    ),
    ## |_____studio tab -----------------------------------------------------------------
    tabItem("studio",
            helpText("A space to dynamically create value boxes corresponding",
                     "to an aggregate summary of the data by subcategory for the selected", 
                     "period of time."),
            fluidRow(
              column(3, 
                     selectInput("studio_subcategory", "Choose a subcategory", unique(dat$subcategory))
              ),
              column(2, 
                     numericInput("studio_amount", "Choose an amount", 0)
              ),
              column(2, 
                     radioButtons("studio_amount_direction", "Amount qualifier", c("Over", "Under"))
              ),
              column(2, 
                     selectInput("studio_function", "Choose a function", 
                                 c("sum", "mean", "count", "min", "max"))
              ),
              column(3, 
                     colourInput("studio_box_color", "Choose a color", palette = "limited",
                                 allowedCols = c(
                                   "#db4c3f", "#f19b2c", "#20c1ed", "#1074b5", "#408eba", "#17a55d", "#02203e", "#42cccb", 
                                   "#419871", "#2afd77", "#fd852f", "#ed25bc", "#605ea6", "#d62161", "#111111"
                                 ))
              )
            ),
            fluidRow(
              tags$div(id = "placeholder")
            )
    )
  )
)



## |__page ------------------------------------------------------------------
ui <- dashboardPage(skin = "purple", dashboardHeader(title = "Personal Budget"), sidebar, body )


server <- function(input, output, session) {
  
  ## UTILITIES --------------------------------------------------------------------------
  tab_list <- NULL
  
  timeColumn <- reactive({ if (input$month == "All Year") "year" else "month" })
  timeValue <-  reactive({ if (input$month == "All Year") input$year else input$month })
  
  subsettedData <- reactive({
    dat %>% filter(rowMeans(is.na(.)) < 1) %>%
      group_by(category) %>%
      filter(year == input$year) %>% 
      filter(timeValue() == !!as.name(timeColumn()))
  })
  

  subsetData <- function(categoryValues) {
    subsettedData() %>% filter(category %in% categoryValues) %>% 
      mutate(total = sum(amount))
  }
  
  
  output$menuItem_specific_ui <- renderUI({
    if (input$sidebar_menu == "dashboard") {
      actionLink("remove_tabs", "Remove detail tabs")
    } else if (input$sidebar_menu == "studio") {
      tagList(
        actionButton("create_box", "Create new box", class = "color_btn"),
        actionLink("remove_boxes", "Delete dynamic boxes")
      )
    }
  })
  
  
  ## VALUE BOXES ------------------------------------------------------------------------
  output$incoming <- renderValueBox({
    sub_dat <- subsetData("income")
    prettifyValueBox(sub_dat$total[1], "Incoming", "maroon")
  })
  
  output$outgoing <- renderValueBox({
    sub_dat <- subsetData("expenses")
    prettifyValueBox(sub_dat$total[1], "Outgoing", "blue")
  })
  
  output$left <- renderValueBox({
    sub_dat <- subsetData(categories) %>% group_by(category) %>% summarise(total = mean(total))
    vals <- map(categories, function(c) {
      res <- filter(sub_dat, category == c)$total[1]
      names(res) <- c
      res
    }) %>% unlist
    val <- sum(vals["income"], - vals["savings"], - vals["expenses"], na.rm = TRUE)
    prettifyValueBox(val, "$$$ left!", "navy")
  })
  
  ## MAIN TABLE -------------------------------------------------------------------------
  output$main_table <- DT::renderDataTable({
    sub_data <- subsettedData() %>% select(year, month, day, amount, category)
    DT::datatable(sub_data, rownames = FALSE, options = list(dom = "tp"))
  }, server = TRUE)
  
  # ------- Transaction Info ------------------------------------------------------------
  
  # creates a proxy object that can be used to manipulate an existing DataTables instance in a Shiny app
  main_table_proxy <- dataTableProxy("main_table")
  
  showTransactionInfo <- function(id) {
    row <- subsettedData()[id, ]
    all <- wellPanel(
      p(tags$b("Date: "), glue("{row$day}, {row$month}, {row$year}")),
      p(tags$b("Amount: "), row$amount),
      p(tags$b("Category: "), row$category),
      p(tags$b("Subcategory: "), row$subcategory),
      if (!is.na(row$origin)) p(tags$b("Origin: "), row$origin),
      if (!is.na(row$description)) p(tags$b("Description: "), row$description)
    )
    return(list(id = row$id, all = all))
  }
  
  observeEvent(input$main_table_rows_selected, {
    info <- showTransactionInfo(input$main_table_rows_selected)
    showModal(modalDialog(
      title = div(tags$b(glue("Transaction #{info$id}")), style = "color: #605ea6;"),
      info$all,
      footer = actionButton("close_modal", label = "Close", class = "color_btn")
    ))
  }, ignoreInit = TRUE)
  
  observeEvent(input$close_modal, {
    selectRows(main_table_proxy, NULL)
    removeModal()
  })
  
  # ------- Transaction Info ------------------------------------------------------------ END
  
  
  ## MAIN PLOT --------------------------------------------------------------------------
  subcategory_dat <- reactive({
    subsettedData() %>% 
      group_by(category, subcategory) %>% 
      mutate(total = sum(amount)) %>% 
      summarise(total = mean(total))
  })
  
  output$main_plot <- renderPlot({
    base_ggplot <- subcategory_dat() %>% basePlot
    renderLandingPagePlot(base_ggplot)
  })
  

  observeEvent(input$main_plot_click, 
  {
    # browser()
    # print ( input$main_plot_click$x )
    # print ( input$main_plot_click$y )
    
    clicked <- list(x=input$main_plot_click$x, y=input$main_plot_click$y)

    if (clicked$x  > 1.0) clicked$x <- 1.0
    if (clicked$y  > 1.0) clicked$y <- 1.0

    tree_dat <- subcategory_dat() %>% treemapified_dat
    tree_dat[, c("ymax",  "ymin",  "xmin",  "xmax")] <- round(tree_dat[, c("ymax",  "ymin",  "xmin",  "xmax")], 5)
    
    clicked_square <- getClickedPoint(tree_dat, clicked)
    
    clicked_label <- as.character(clicked_square[1, "subcategory"])
    
    outputID <- glue("dt-{clicked_label}")
    btnID <- glue("hide-{outputID}")
    
    if (!(clicked_label %in% tab_list)) {
      appendTab(inputId = "tabs",
                tabPanel(clicked_label,
                         actionButton(btnID, "Hide this tab", class = "color_btn pull-right"),
                         DT::dataTableOutput(outputID)
                )
      )
      tab_list <<- c(tab_list, clicked_label)
    }
    
    output[[outputID]] <- DT::renderDataTable({
      details <- subsettedData() %>% filter(subcategory == clicked_label)
      DT::datatable(details, rownames = FALSE, options = list(dom = "tp"))
    })
    
    showTab(inputId = "tabs", target = clicked_label, select = TRUE)
    
    observeEvent(input[[btnID]],{
      hideTab("tabs", clicked_label)
    }, ignoreInit = TRUE)
    
  }, 
  ignoreInit = TRUE)
  
  # Remove added tabs  
  observeEvent(input$remove_tabs,{
    tab_list %>% walk(~removeTab("tabs", .x))
    tab_list <<- NULL
  }, ignoreInit = TRUE)

  
  ## STUDIO TAB -------------------------------------------------------------------------
  # actionButton("create_box", "Create new box", class = "create_box"),
  # actionLink("remove_boxes", "Delete dynamic boxes")
  
  studioTabData <- reactive({
    
    # browser()
    
    subsettedData() %>% 
      filter(subcategory %in% input$studio_subcategory) %>% 
      filter(studioTabAmountDirection()(amount, input$studio_amount)) %>% 
      mutate(total = studioTabFunction()(amount))
  })
  
    
  studioTabAmountDirection <- reactive({
    if (input$studio_amount_direction == "Over") `>`
    else if (input$studio_amount_direction == "Under") `<`
  })
  
  studioTabFunction <- reactive({
    if (input$studio_function == "sum") sum
    else if (input$studio_function == "mean") mean
    else if (input$studio_function == "count") count
    else if (input$studio_function == "min") min
    else if (input$studio_function == "max") max
  })
  
  studioBoxColor <- reactive({
    if (input$studio_box_color == "#DB4C3F") "red"
    else if (input$studio_box_color == "#F19B2C") "yellow"
    else if (input$studio_box_color == "#20C1ED") "aqua"
    else if (input$studio_box_color == "#1074B5") "blue"
    else if (input$studio_box_color == "#408EBA") "light-blue"
    else if (input$studio_box_color == "#17A55D") "green"
    else if (input$studio_box_color == "#02203E") "navy"
    else if (input$studio_box_color == "#42CCCB") "teal"
    else if (input$studio_box_color == "#419871") "olive"
    else if (input$studio_box_color == "#2AFD77") "lime"
    else if (input$studio_box_color == "#FD852F") "orange"
    else if (input$studio_box_color == "#ED25BC") "fuchsia"
    else if (input$studio_box_color == "#605EA6") "purple"
    else if (input$studio_box_color == "#D62161") "maroon"
    else if (input$studio_box_color == "#111111") "black"
  })
  
  observeEvent(input$create_box, {
    
    # browser()
    
    divID <- gsub("\\.", "", format(Sys.time(), "%H%M%OS3"))
    divClass <- glue("user-dynamic-box")
    btnID <- glue("remove-{divID}")
    
    sub_dat <- studioTabData()
    val <- prettyNum(sub_dat$total[1], big.mark = ",")
    
    direction <- if (input$studio_amount_direction == "Over") ">" else "<"
    
    insertUI(
      selector = "#placeholder",
      ui = div(id = divID, class = divClass,
               column(4, 
                      actionButton(btnID, "X", class = "grey_btn pull-right"),
                      valueBox(width = NULL,
                               value = glue("{val}  \u20AC"), 
                               subtitle = HTML( paste(glue("{input$studio_function} over {input$studio_subcategory} ", 
                                               "for {direction} {input$studio_amount} "), "&#8364;") ),
                               
                               color = studioBoxColor()
                      )
               )
      )
    )
    
    observeEvent(input[[btnID]], {
      removeUI(glue("#{divID}"))
    }, ignoreInit = TRUE, once = TRUE)
    
  }, ignoreInit = TRUE)
  
  
  observeEvent(input$remove_boxes,{
    removeUI(".user-dynamic-box", multiple = TRUE)
  }, ignoreInit = TRUE)
  
}

shinyApp(ui, server)



