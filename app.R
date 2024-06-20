library(DT)
library(shinydashboard)
library(dplyr)
library(tibble)
library(janitor)
library(shiny)

# Using trick to add/remove rows from https://github.com/yihanwu/Nutrient_Calculator/blob/master/app.R
# Load reference tables
bar_weight_df <- read.csv("data/tabella peso barre ca.csv")
net_weight_df <- read.csv("data/tabella rete elettrosaldata da ca.csv")

ui <- dashboardPage(
  dashboardHeader(title = "Travapp"),
  
  ## Sidebar content
  ## Get all the values needed to compute the weight
  dashboardSidebar(
      textInput("unit", label = "1. Unità Strutturale", value = "test"),
      textInput("n_trav", label = "2. N. travata", "A1"),
      selectInput("comp", 
                  label = "3. Componente", 
                  choices = c("Barre", 
                              "Barre integrative", 
                              "Reggistaffe", 
                              "Staffe"),
                  selected = "Barre"),
      
      ## These elements show only based on the component selection
      conditionalPanel("input.comp != 'Staffe'",
                       selectInput("p_bar", label = "Posizione barre", 
                                   choices = c("Sup", "Inf", "-")),
                       numericInput("n_bar", label = "Numero barre", 3),
                       numericInput("d_bar", label = "Diametro barre", 16),
                       numericInput("l_bar", label = "Lunghezza barre", 710),
                       ),
      conditionalPanel("input.comp == 'Staffe'",
                       numericInput("l_trav", "Lunghezza trave", 52),
                       numericInput("pitch", "Passo", 10),
                       numericInput("d_bracket", label = "Diametro Staffa", 8),
                       numericInput("l_bracket", label = "Lunghezza staffa", 150),
                       numericInput("l_bracket2", label = "Lunghezza staffa 2", 0)),
      actionButton("add", "Aggiungi elemento"),
      actionButton("remove", "Rimuovi elemento")
  ),
  
  ## Body content
  dashboardBody(
    
    # Big numbers on top
    fluidRow(
      valueBoxOutput("bar_weight_box"),
      valueBoxOutput("bracket_weight_box"),
      valueBoxOutput("total_weight_box")
    ),
    
    tags$head(
      tags$style(HTML("
    .sep {
      width: 20px;
      height: 1px;
      float: left;
    }
    .dataTables_wrapper .dataTable tr.odd {
      background-color: #FFFFFF; /* White */
    }
    .dataTables_wrapper .dataTable tr.even {
      background-color: #E6E6E6; /* Grey90 */
    }
    .dataTables_wrapper .dataTable tr.last {
      background-color: #D6DEFF; /* Light cyan-lime green */
    }
  "))
    ),
    
    # Tables
    fluidRow(
      box(title = "Barre",
          width = 12,
          DTOutput("bar_df"),
          verbatimTextOutput("bar_weight"))),
      fluidRow(
        box(title = "Staffe",
            width = 12,
            DTOutput("bracket_df"),
            )
    ),
    tags$footer(
      style = "position: absolute; bottom: 0; width: 100%; text-align: center;",
      "Created by ", 
      tags$a(href = 'mailto:nbalboniwork@gmail.com', "Nicola Balboni")
    )
  )
)

server <- function(input, output) {
  unit_df <- reactiveValues()
  unit_df$bar_df <- tibble()
  unit_df$bracket_df <- tibble()
  
  # Add row
  observeEvent(input$add, {
    
    # Add to the new row the shared elements between bars and brackets
    new_row <- tibble(unit = input$unit,
                          n_trav = input$n_trav,
                          comp = input$comp)
    
    # If the element is a bar, add to the new row specific elements for bars
    # p = position, n = number, d = diameter, l = length
    if(input$comp != "Staffe"){
      new_row <- new_row |> 
        mutate(p_bar = input$p_bar,
               n_bar = input$n_bar,
               d_bar = input$d_bar,
               l_bar = input$l_bar)
      
      # Get bar weight and section area
      bar_spec <- bar_weight_df |> 
        filter(diametro_barra == new_row$d_bar) |> 
        select(peso, area_sezione)
      
      new_row <- new_row |> 
        mutate(section_area = bar_spec$area_sezione,
               w_bar = round(n_bar*bar_spec$peso*l_bar/100, 2))
      
      # Bind new row to the bar df
      isolate(unit_df$bar_df <- bind_rows(unit_df$bar_df, new_row))
      print(unit_df$bar_df)
      
    }else if(input$comp == "Staffe"){
      
      # If the element is a bracket, add specific bracket elements
      new_row <- new_row |> 
        mutate(l_trav = input$l_trav,
               pitch = input$pitch,
               d_bracket = input$d_bracket,
               l_bracket = input$l_bracket,
               l_bracket2 = input$l_bracket2,
               n_bracket = ifelse(nrow(unit_df$bracket_df) == 0, 
                                  round(l_trav/pitch) + 1, 
                                  round(l_trav/pitch)))
      
      # Get bracket weight and section area
      bracket_spec <- bar_weight_df |> 
        filter(diametro_barra == new_row$d_bracket) |> 
        select(peso, area_sezione)
      
      # Check how many bracket types are present and compute weight accordingly
      if(input$comp == "Staffe" & input$l_bracket2 == 0){
        new_row <- new_row |> 
          mutate(w_bracket = round(l_bracket*n_bracket*bracket_spec$peso/100, 2))
      }else if(input$comp == "Staffe" & input$l_bracket2 != 0){
        new_row <- new_row |> 
          mutate(w_bracket = round((l_bracket+l_bracket2)*n_bracket*bracket_spec$peso/100, 2))
      }
      
      # Add new bracket row to bracket df
      isolate(unit_df$bracket_df <- bind_rows(unit_df$bracket_df, new_row))
      print(unit_df$bracket_df)
      
    }
  })
  
  # Remove row
  observeEvent(input$remove, {
    isolate(unit_df$bar_df <- unit_df$bar_df[-as.numeric(input$bar_df_rows_selected),])
    isolate(unit_df$bracket_df <- unit_df$bracket_df[-as.numeric(input$bracket_df_rows_selected),])
  })
  
  # Compute weights
  ## Compute total weight of the bars
  total_bar_w <- reactive({
    if(!is.null(unit_df$bar_df) & nrow(unit_df$bar_df) > 0){
        unit_df$bar_df |> 
        group_by(comp) |> 
        summarise(comp_weight = sum(w_bar, na.rm = T) |> 
                    round(2)) |> 
        ungroup() |> 
        mutate(tot_weight = sum(comp_weight) |> 
                 round(2))
    }else{
        tibble(comp_weight = 0, tot_weight = 0)
      }
  })
  
  # Compute total weight of the brackets
  total_bracket_w <- reactive({
    if(!is.null(unit_df$bracket_df) & nrow(unit_df$bracket_df) > 0){
        sum(unit_df$bracket_df$w_bracket) |> round(2)
    }else{0}
  })
  
  # Add adorn totals
  bar_tot <- reactive({
    if(!is.null(unit_df$bar_df) & nrow(unit_df$bar_df) > 0){
      unit_df$bar_df |> 
        relocate(c(n_bar, w_bar), .after = last_col()) |> 
        adorn_totals(where = "row", fill = "-", na.rm = T, name = "Totale", 
                     c(n_bar, w_bar))
    }
  })
  
  bracket_tot <- reactive({
    if(!is.null(unit_df$bracket_df) & nrow(unit_df$bracket_df) > 0){
      unit_df$bracket_df |> 
        relocate(c(n_bracket, w_bracket), .after = last_col()) |> 
        adorn_totals(where = "row", fill = "-", na.rm = T, name = "Totale", 
                     c(n_bracket, w_bracket))
    }
  })
  
  # Output the two datatables
  ## Define output column names
  bar_df_names <- c("US", "N. Travata", "Componente", "Posizione barra",
                    "Diametro barra", "Lunghezza barra", "Area sezione", "Numero barre", "Peso barra")
  bracket_df_names <- c("US", "N. Travata", "Componente", "Lunghezza trave",
                        "Passo", "Diametro staffa", "Lunghezza staffe",
                        "Lunghezza staffe 2", "Numero staffe", "Peso staffe")
  output$bar_df <- renderDT(
    datatable(bar_tot(), 
              colnames = bar_df_names,
              rownames = F,
              extensions = "Buttons", 
              options = list(
                             info = TRUE,
                             paging = TRUE,
                             searching = TRUE,
                             fixedColumns = FALSE,
                             # autoWidth = TRUE,
                             ordering = TRUE,
                             dom = 'tB<"sep">lip',
                             pageLength = 15,
                             buttons = c('copy', 'csv', 'excel', 'pdf'),
                             rowCallback = JS(
                               "function(row, data, index) {
       if (index === this.api().data().length - 1) {
         $(row).addClass('last');
       }
     }")))
  )
  output$bracket_df <- renderDT(
    datatable(bracket_tot(), 
              colnames = bracket_df_names,
              rownames = F, 
              extensions = "Buttons", 
              options = list(
                             info = TRUE,
                             paging = TRUE,
                             searching = TRUE,
                             fixedColumns = FALSE,
                             # autoWidth = TRUE,
                             ordering = TRUE,
                             dom = 'tB<"sep">lip',
                             pageLength = 15,
                             buttons = c('copy', 'csv', 'excel', 'pdf'),
                             rowCallback = JS(
                               "function(row, data, index) {
             if (index === this.api().data().length - 1) {
               $(row).addClass('last');
             }
            }")))
  )
  
  # Print total weights of bars and brackets if they have at least 1 entry
  observe({
    if(!is.null(unit_df$bar_df) & nrow(unit_df$bar_df) > 0){
      output$bar_weight <- renderText({
        paste0("Peso ", total_bar_w()$comp, ": ", total_bar_w()$comp_weight, " kg\n")
      },
      sep = "")
    }
  })
  
  # Print them also in the big boxes on top
  output$bar_weight_box <- renderValueBox({
    valueBox(value = paste(unique(total_bar_w()$tot_weight), "kg"), 
             "Barre", icon = icon("align-right"), color = "blue")
  })
  output$bracket_weight_box <- renderValueBox({
    valueBox(value = paste(total_bracket_w(), "kg"), 
             "Staffe", icon = icon("infinity"), color = "yellow")
  })
  output$total_weight_box <- renderValueBox({
    valueBox(value = paste(unique(total_bar_w()$tot_weight) + total_bracket_w(), "kg"),
             "Peso US", icon = icon("cube"), color = "green")
  })
}

shinyApp(ui, server)

## TODO: round to ceiling second decimal places on weight
## TODO: striping (https://stackoverflow.com/questions/58925830/control-row-stripe-color-in-datatable-output/58931156#58931156)i

# conv_names <- tibble(df = c("unit", "n_trav", "comp", "p_bar", "d_bar", "l_bar",
#                             "l_trav", "pitch", "d_bracket", "l_bracket", "l_bracket2",
#                             "n_bracket","w_bracket", "section_area", "bar_weight",
#                             "tot_weight"),
#                      out = c("US", "N. Travata", "Componente", "Posizione barra", 
#                              "Diametro barra", "Lunghezza barra", "Lunghezza trave",
#                              "Passo", "Diametro staffa", "Lunghezza staffe", 
#                              "Lunghezza staffe 2", "Numero staffe", "Peso staffe", 
#                              "Area sezione", "Peso barra", "Peso Totale"))