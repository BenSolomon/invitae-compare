library(shinythemes)
library(shinyBS)
library(DT)
library(dplyr)
library(shinycssloaders)
library(RSelenium)
library(netstat)
library(tibble)

# TO DO
## CSS Spinners
## Support for custom gene lists
## URL error check
## Full gene list error check (compare scrape length to published number on page)
object_to_row <- function(x){
  names(x) <- deparse(substitute(x)) 
  enframe(x, "element", "value")
}

eCaps <- list(chromeOptions = list(
  args = c('--headless', '--disable-gpu', '--window-size=1280,800')
))

# Establishing RSelenium client
# I have set up Selenium differently on my windows vs. linux machines
# and this switch allows single script to work for both
if (Sys.info()['sysname'] == "Linux"){
  remDr <- remoteDriver(port = 4444L, extraCapabilities = eCaps, browser = "chrome")
  remDr$open()
}
if (Sys.info()['sysname'] == "Windows"){
  rsD <- rsDriver(browser = 'chrome',
                  verbose = F,
                  chromever = "103.0.5060.53",
                  port = free_port(),
                  extraCapabilities = eCaps)
  remDr <- rsD$client
}

# Customizes output for a failed selenium `findElement` query
try_seleniumQuery <- function(expr) {
  tryCatch(
    expr,
    error = function(c)
      "error",
    message = function(error) {
      error <- conditionMessage(error)
      if (!grepl("Selenium message", error)) {
        msg <- "Not selenium error"
        return(structure(msg, class = "try-error"))
      }
      error_lines <- unlist(strsplit(error, "\n"))
      query_error <-
        error_lines[grepl("Selenium message", error_lines)]
      msg <- unlist(strsplit(query_error, ":"))[2]
      return(structure(msg, class = "try-error"))
    }
  )
}

get_invitae_genes <- function(url){
  remDr$navigate(url)
  show_genes <- try_seleniumQuery(remDr$findElement(using = "xpath", "//div[contains(text(), 'Show all genes')]"))
  if (!class(show_genes) == "try-error") {show_genes$clickElement()}
  genes <- remDr$findElements(using = "xpath", "//div[contains(@class, 'GeneTileItem_geneTitle')]")
  unlist(lapply(genes, function(x) {x$getElementText()}))
}

get_invitae_name <- function(url){
  remDr$navigate(url)
  gsub("Invitae | \\|.*", "", unlist(remDr$getTitle()))
}

get_invitae_gene_count <- function(url){
  remDr$navigate(url)
  count <- remDr$findElement(using = "xpath", "//div[contains(@id, 'panelGenesCountExpanded')]")
  as.numeric(gsub("[a-zA-Z ]", "", unlist(count$getElementText())))
}

# Some panel defaults
invitae_ibd <- "https://www.invitae.com/en/providers/test-catalog/test-08122"
invitae_agam <- "https://www.invitae.com/en/providers/test-catalog/test-08111"
invitae_pid <- "https://www.invitae.com/en/providers/test-catalog/test-08100"
invitae_bc <- "https://www.invitae.com/en/providers/test-catalog/test-01202"

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
                                p(em("URL for an Invitae panel"), 
                                  style="text-align: right; font-size:12px;"),
                                fluidRow(br()),
                                textAreaInput("panel_2", label = p("Panel 2"), width = "100%", value = invitae_agam),
                                p(em("URL for an Invitae panel or custom list of genes separated by ',' or ' '"), 
                                  style="text-align: right; font-size:12px;"),
                                fluidRow(br()),
                                actionButton("go", label = "Submit")
                                )
                            )),
               mainPanel(
                 fluidRow(column(4, withSpinner(DT::dataTableOutput("table1"))),
                          column(4, withSpinner(DT::dataTableOutput("table_shared"))),
                          column(4, withSpinner(DT::dataTableOutput("table2")))), 
                 div(h3("Debug"), withSpinner(DT::dataTableOutput("debug_table")))
             )
             ))
  )


build_DT <- function(df, title){
  datatable(
    df,
    rownames = FALSE,
    filter = "bottom",
    caption = htmltools::tags$caption(
      style =
        'caption-side: top;
        text-align: left;
        color: black;
        font-size:150%',
      title
    ),
    class = "cell-border",
    callback=JS('$(\'div.has-feedback input[type="search"]\').attr( "placeholder", "Search genes" )'), # Placeholder text in search box
    options = list(
      dom = "tp", # Keep only table (t) and paginator (p)
      # Hide headers
      headerCallback = JS(
        "function(thead, data, start, end, display){",
        "  $(thead).remove();",
        "}"),
      # Center column values
      columnDefs = list(
        list(className = 'dt-center', targets = 0)
      )
    )
  )}

server <- function(input, output) {
  
  # Static panel names
  # title_panel_1 <- eventReactive(input$go, {"Genes unique to panel 1"})
  # title_panel_2 <- eventReactive(input$go, {"Genes unique to panel 2"})
  title_shared <- eventReactive(input$go, {"Genes shared by both panels"})
  
  # # Dynamic panel names
  title_panel_1 <- eventReactive(input$go, {
    panel <- get_invitae_name(input$panel_1)
    sprintf("Genes unique to %s", panel)})
  title_panel_2 <- eventReactive(input$go, {
    panel <- get_invitae_name(input$panel_2)
    sprintf("Genes unique to %s", panel)})
  
  # Demo genes for debugging
  # genes_panel_1 <- eventReactive(input$go, {as.character(1:50)})
  # genes_panel_2 <- eventReactive(input$go, {as.character(26:75)})
  
  genes_panel_1 <- eventReactive(input$go, {get_invitae_genes(input$panel_1)})
  genes_panel_2 <- eventReactive(input$go, {get_invitae_genes(input$panel_2)})
  
  shared_genes <- reactive({intersect(genes_panel_1(), genes_panel_2())})
  genes_panel_1_unique <- reactive({setdiff(genes_panel_1(), shared_genes())})
  genes_panel_2_unique <- reactive({setdiff(genes_panel_2(), shared_genes())})
  
  output$table1 <- DT::renderDataTable({build_DT(tibble(genes_panel_1_unique()), title = title_panel_1())})
  output$table_shared <- DT::renderDataTable({build_DT(tibble(shared_genes()), title = title_shared())})
  output$table2 <- DT::renderDataTable({build_DT(tibble(genes_panel_2_unique()), title = title_panel_2())})
  
  
  panel_1_length <- reactive({length(genes_panel_1())})
  panel_2_length <- reactive({length(genes_panel_2())})
  shared_length <- reactive({length(shared_genes())})
  
  expected_1_length <- eventReactive(input$go, {get_invitae_gene_count(input$panel_1)})
  expected_2_length <- eventReactive(input$go, {get_invitae_gene_count(input$panel_2)})
  
  debug <- reactive({as.data.frame(bind_rows(
   object_to_row(panel_1_length()), 
   object_to_row(panel_2_length()), 
   object_to_row(shared_length()), 
   object_to_row(expected_1_length()), 
   object_to_row(expected_2_length()) 
  ))})
  
  output$debug_table <- DT::renderDataTable({datatable(debug(), rownames = F)})
}

shinyApp(ui, server)


