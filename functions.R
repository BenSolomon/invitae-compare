library(shinythemes)
library(shinyBS)
library(DT)
library(dplyr)
library(shinycssloaders)
library(tibble)
library(rvest)
library(jsonlite)

get_panel_html <- function(test_code){
  url <- sprintf("https://www.invitae.com/en/providers/test-catalog/test-%s", test_code)
  panel_html <- read_html(url)
  return(panel_html)
}

get_panel_name <- function(panel_html){
  title <- panel_html %>% 
    html_nodes("head") %>% 
    html_nodes("title") %>% 
    html_text()
  title <- gsub(pattern = "\\|.*", replacement = "", title)
  title <- gsub(pattern = "Invitae", replacement = "", title)
  title <- trimws(title)
  return(title)
}

get_panel_genes <- function(panel_html){
  json_data <- panel_html %>% 
    html_element("body") %>% 
    html_nodes("script") %>% 
    html_nodes(xpath = "//*[@id='__NEXT_DATA__']") %>% 
    html_text() %>% 
    jsonlite::fromJSON()
  genes <- json_data$props$pageProps$panelData$panel$geneTests$name
  return(genes)
}

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
      ),
      language = list(
        zeroRecords = "No additional genes")
    )
  )}

valid_url <- function(url){
  url_status <- attributes(curlGetHeaders(url))$status
  ifelse(url_status >= 200 & url_status < 300, TRUE, FALSE)
}
