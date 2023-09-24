library(shinythemes)
library(shinyBS)
library(DT)
library(dplyr)
library(shinycssloaders)
library(tibble)
library(rvest)
library(jsonlite)
source("functions.R")

# TO DO
## Code validator
## Align lists better

# Some panel defaults
invitae_ibd <- "08122"
invitae_agam <- "08111"
invitae_pid <- "08100"
invitae_bc <- "01202"

# Switchable inputs for Panel 2
parameter_tabs <- tabsetPanel(
  id = "params",
  type = "hidden",
  tabPanel("invitae_panel",
           textAreaInput("panel_2_invitae", label = NULL, width = "100%", value = invitae_agam),
           p(em("Invitae panel code"), 
             style="text-align: right; font-size:12px;"),
  ),
  tabPanel("gene_list",
           textAreaInput("panel_2_custom", label = NULL, width = "100%", value = "IL10, RAG1"),
           p(em("Custom list of genes separated by ',' or ' '"), 
             style="text-align: right; font-size:12px;"),
  )
)

ui <-
  navbarPage(
    "Invitae panel comparison",
    theme = shinytheme("flatly"),
    collapsible = T,
    header =
      tags$head(# includeHTML("google-analytics.js"),
        tags$style(
          HTML(
            "
                        #test {
                          padding: 100px;
                        }
                        .navbar {
                          margin: 0px;
                        }
                        .footer {
                            position: relative;
                            left: 0;
                            bottom: 0;
                            width: 100%;
                            background-color: #d7dfea;
                            # color: white;
                            text-align: center;
                        }
                        "
          )
        )),
    
    tabPanel("Convert", id = "test",
             sidebarLayout(
               sidebarPanel(width = 3,
                            bsCollapse(
                              open = "panel",
                              bsCollapsePanel(
                                p(icon("bars"), HTML('&nbsp;'), "Gene panels"), value = "panel",
                                
                                textAreaInput("panel_1", label = p("Panel 1"), width = "100%", value = invitae_ibd),
                                p(em("Invitae panel code"), 
                                  style="text-align: right; font-size:12px;"),
                                fluidRow(br()),
                                
                                radioButtons("panel_2_toggle", label = p("Panel 2"), width = "100%", 
                                             choices = c("Invitae panel"="invitae_panel", "Custom gene list"="gene_list"), 
                                             selected = "invitae_panel"), 
                                parameter_tabs,
                                fluidRow(br()),
                                
                                actionButton("go", label = "Submit")
                                )
                            )),
               mainPanel(
                 fluidRow(column(4, withSpinner(DT::dataTableOutput("table1"))),
                          column(4, withSpinner(DT::dataTableOutput("table_shared"))),
                          column(4, withSpinner(DT::dataTableOutput("table2"))))
             )
             ))
  )

server <- function(input, output) {
  invitae_base_url <- "https://www.invitae.com/en/providers/test-catalog/test-"
  
  # Check radio button specifies whether invitae panel or gene list is desired 
  # for panel 2 input and update text box as indicated
  observeEvent(input$panel_2_toggle, {
    updateTabsetPanel(inputId = "params", selected = input$panel_2_toggle)
  })
  
  # Obtain panel 1 information
  panel_1_html <- eventReactive(input$go, {
    shiny::validate(need(
      valid_url(sprintf("%s%s", invitae_base_url, input$panel_1)), 
      "Panel 1 is not valid"))
    get_panel_html(input$panel_1)
  })
  panel_1_name <- eventReactive(input$go, {get_panel_name(panel_1_html())})
  panel_1_genes <- eventReactive(input$go, {get_panel_genes(panel_1_html())})
  
  # Obtain panel 2 information
  ## Get invitae HTML if radio button == invitae
  panel_2_html <- eventReactive(input$go, {
    if(input$panel_2_toggle=="invitae_panel"){
      shiny::validate(need(
        valid_url(sprintf("%s%s", invitae_base_url, input$panel_2_invitae)), 
        "Panel 2 is not valid"))
      get_panel_html(input$panel_2_invitae)
    }
  })
  ## Set name based on radio button
  panel_2_name <- eventReactive(input$go, {
    if(input$panel_2_toggle=="invitae_panel"){get_panel_name(panel_2_html())}
    else if(input$panel_2_toggle=="gene_list"){"Custom gene list"}
  })
  ## Set genes based on radio button
  panel_2_genes <- eventReactive(input$go, {
    if(input$panel_2_toggle=="invitae_panel"){get_panel_genes(panel_2_html())}
    else if(input$panel_2_toggle=="gene_list"){trimws(unlist(strsplit(input$panel_2_custom, "\\,")))}
  })
  
  # Create tables
  title_shared <- eventReactive(input$go, {"Genes shared by both panels"})
  shared_genes <- reactive({intersect(panel_1_genes(), panel_2_genes())})
  panel_1_genes_unique <- reactive({setdiff(panel_1_genes(), shared_genes())})
  panel_2_genes_unique <- reactive({setdiff(panel_2_genes(), shared_genes())})
  output$table1 <- DT::renderDataTable({build_DT(tibble(panel_1_genes_unique()), title = panel_1_name())})
  output$table_shared <- DT::renderDataTable({build_DT(tibble(shared_genes()), title = title_shared())})
  output$table2 <- DT::renderDataTable({build_DT(tibble(panel_2_genes_unique()), title = panel_2_name())})
}

shinyApp(ui, server)


