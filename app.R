library(tidyverse)
library(ggthemes)
library(ggridges)
library(ggrepel)
library(shiny)
library(shinythemes)
library(shinycssloaders)
library(glue)
library(magrittr)
library(treemapify)
library(DT)
library(assertthat)
library(plotly)
##### load data ##### 

data <- read_rds('data/data.rds')
Series <- read_csv("data/HNP_StatsSeries.csv")

##### functions ####

filter_topics <- function(topic='All'){
  if (topic=='All') {
    Series$`Indicator Name`
  }else{
    Series %>% filter(Topic==topic) %$% `Indicator Name`
  }
}

name_to_cod <- function(name){
  Series %>% filter(`Indicator Name`==name) %$% `Series Code`
}

dist_plot <- function(filt_data, top_countries = F, bottom_countries = F, specific_countries = NA) {
  
  selected_countries <- specific_countries
  
  if (top_countries) {
    
    top <- filt_data %>%
      group_by(country) %>%
      summarise(value = max(value)) %>%
      top_n(3, value) %$% country
    
    ifelse(length(top)<=4,selected_countries <- c(selected_countries,top), 
           warning('too many countries in the right tale'))
  }
  if (bottom_countries) {
    
    bottom <- filt_data %>%
      group_by(country) %>%
      summarise(value = min(value)) %>%
      top_n(-3, value) %$% country
    
    ifelse(length(bottom)<=4,selected_countries <- c(selected_countries,bottom), 
           warning('too many countries in the left tale'))
  }
  
  filt_data %>% 
    ggplot(. , aes(value, factor(year)))+
    geom_density_ridges(alpha = 0.2)+
    geom_text_repel(data = . %>%
                      filter(country %in% selected_countries),
                    aes(label = cod, color = country),
                    nudge_y = 0.5, fontface = "bold")+
    theme_tufte()+
    theme(legend.position = "bottom")+
    scale_color_gdocs() +
    scale_fill_gdocs() +
    labs(y = "Año",
         title= glue('{unique(filt_data$indicator)}'))
  
}

grow_treemap <- function(filt_data, yr=2010, group='inc_group') {
  
  filt_data %>% 
    filter(year==yr) %>% 
    ggplot(., aes_string(area = 'value',label = 'cod',subgroup = group, fill= group)) + 
    geom_treemap(fixed = TRUE)+
    geom_treemap_subgroup_border(color='grey', fixed = T)+
    geom_treemap_text(colour = "black", place = "left",
                      grow = F, fixed =  TRUE)+
    geom_treemap_subgroup_text(aes_string(label=group),colour = "white",alpha=0.6, place = "top",
                               grow = F, fixed =  TRUE, reflow=T)+
    labs(title= glue("Treemap of {unique(filt_data$indicator)}, year {yr}"))+
    theme_tufte()+
    theme(legend.position = 'none',
          strip.text = element_text(family="Times", face="bold", size=20))
}

correlation_plot <- function(filt_data,input_x,input_y, yr=2016, group ='inc_group',linear_relation=T){
  
  check_x <- filt_data %>% filter(indicator_cod==name_to_cod(input_x),year==yr)
  check_y <- filt_data %>% filter(indicator_cod==name_to_cod(input_y),year==yr)
  
  assert_that(not_empty(check_x), msg = glue('There is no available data for {input_x} for the year {yr}'))
  assert_that(not_empty(check_y), msg = glue('There is no available data for {input_y} for the year {yr}'))
  
  plot <- filt_data %>% 
    filter(year==yr) %>% 
    select(-indicator) %>% 
    spread(.,key = indicator_cod,value) %>%
    select(country,inc_group,region, x=name_to_cod(input_x),y=name_to_cod(input_y)) %>% 
    ggplot(., aes_string(x='x',
                         y='y',
                         label = 'country',
                         color=group))+
    geom_point()+
    theme_tufte()+
    labs(x=input_x,y=input_y,title = 'Scatterplot')
  
  if (linear_relation) {
    plot <- plot+
      geom_smooth(method = 'lm',se = F)
  }
  
  ggplotly(plot)
}

series_code="SE.PRM.NENR"

series_info <- function(series_code){
  Series %>% filter(`Series Code`==series_code) %>%
  gather() %>% na.omit()
}


  
##### UI ##### 

ui <- fluidPage(theme = shinytheme("paper"),
                
                tags$head(
                  tags$style(type='text/css', 
                             ".nav-tabs {font-size: 30px} ")),
                tabsetPanel(type = "tabs",
                            ## Density ##
                            tabPanel("density",
                                     titlePanel("Countries distribution over features"),
                                     sidebarLayout(
                                       ## distribution ##
                                       sidebarPanel(selectizeInput('topic_dist',
                                                                   label='Choose the topic',
                                                                   choices=c('All',unique(Series$Topic)),
                                                                   selected='All'),
                                                    htmlOutput('feature_dist'),
                                                    checkboxInput('top_3',label = 'highlight top countries',value = TRUE),
                                                    checkboxInput('last_3',label = 'highlight last countries',value = FALSE),
                                                    htmlOutput("countryUI"),
                                                    # submitButton("Update", icon("refresh")),
                                                    downloadButton('download_dist','Download data'),
                                                    br(),
                                                    "Autor: ",a("Diego Kozlowski", href="https://sites.google.com/view/diego-kozlowski")
                                       ),
                                       mainPanel(
                                         plotOutput("dist_plot", width = "100%", height = "700px")%>% 
                                           withSpinner(color="#0dc5c1"),
                                         h3('Raw data'),
                                         dataTableOutput('filt_data_dist'),
                                         h3('Metadata of the serie'),
                                         dataTableOutput('metadata_dist')
                                       ))),
                            ## treemap ##
                            tabPanel("treemap",
                                     titlePanel("Treemaps of features over time"),
                                     sidebarLayout(
                                       sidebarPanel(
                                         selectizeInput('topic_treemap',
                                                        label='Choose the topic',
                                                        choices=c('All',unique(Series$Topic)),
                                                        selected='All'),
                                         htmlOutput('feature_treemap'),
                                         selectizeInput("group",
                                                        label =  "Choose indicator",
                                                        choices = c('income groups'='inc_group','region'),
                                                        selected = "inc_group"),
                                         htmlOutput("yearUI"),
                                         downloadButton('download_treemap','Download data'),
                                         br(),
                                         
                                         "Autor: ",a("Diego Kozlowski", href="https://sites.google.com/view/diego-kozlowski")
                                       ),
                                       mainPanel(
                                         
                                         plotOutput("treemap", width = "100%", height = "700px")%>% 
                                           withSpinner(color="#0dc5c1"),
                                         h3('Raw data'),
                                         dataTableOutput('filt_data_treemaps'),
                                         h3('Metadata of the serie'),
                                         dataTableOutput('metadata_treemap')
                                         
                                       )
                                     )
                            ),
                            ## scatterplot ##
                            tabPanel("scatterplot",
                                     titlePanel("Relation between two features"),
                                     sidebarLayout(
                                       sidebarPanel(
                                         selectizeInput('topic_scatter_x',
                                                        label='Choose the topic x',
                                                        choices=c('All',unique(Series$Topic)),
                                                        selected='Social Protection & Labor: Economic activity'),
                                         htmlOutput('input_x'),
                                         selectizeInput('topic_scatter_y',
                                                        label='Choose the topic y',
                                                        choices=c('All',unique(Series$Topic)),
                                                        selected='Social Protection & Labor: Labor force structure'),
                                         htmlOutput('input_y'),
                                                    selectizeInput("group_scatter",
                                                                   label =  "Choose indicator",
                                                                   choices = c('income groups'='inc_group','region'),
                                                                   selected = "region"),
                                                    checkboxInput('scatter_smooth',label = 'Make lineal model',value = TRUE),
                                                    htmlOutput("yearUI_scatter"),
                                                    downloadButton('download_scatter','Download data'),
                                                    br(),
                                                    
                                                    "Autor: ",a("Diego Kozlowski", href="https://sites.google.com/view/diego-kozlowski")
                                       ),
                                       mainPanel(
                                         
                                         plotlyOutput("scatter", width = "100%", height = "700px")%>% 
                                           withSpinner(color="#0dc5c1"),
                                         h3('Raw data'),
                                         dataTableOutput('filt_data_scatter'),
                                         h3('Metadata of the series'),
                                         htmlOutput('xname'),
                                         dataTableOutput('x_metadata_scatter'),
                                         hr(),
                                         htmlOutput('yname'),
                                         dataTableOutput('y_metadata_scatter')
                                       )
                                     )
                            )
                )
)
##### server ##### 

server <- function (input, output, session) {
  
  
  #### distribution tab ####
  
  # side bar
  
  output$feature_dist <-  renderUI({
    selectizeInput("feature_dist",
                 label =  "Choose indicator",
                 choices = filter_topics(input$topic_dist),
                 selected = "Prevalence of HIV, total (% of population ages 15-49)")})

  
  filt_data_dist <- reactive({data[data$indicator_cod==name_to_cod(input$feature_dist),] %>% unnest()})

  output$countryUI = renderUI({
    selectInput(inputId = "specific_countries", 
                label = "Choose countries to highlight", 
                choices = filt_data_dist() %$% country, 
                multiple = TRUE)})
  #download
  
  output$download_dist <- downloadHandler(
    filename = function() {glue('{unique(filt_data_dist() %$% indicator_cod)}.csv')}, 
    content =  function(file){write.csv(filt_data_dist(),file, row.names = FALSE)})
  
  #main panel
  output$dist_plot =  renderPlot({
      
    dist_plot(filt_data_dist(),
              top_countries = input$top_3, 
              bottom_countries = input$last_3, 
              specific_countries = input$specific_countries)
  })
  
  output$filt_data_dist <- renderDataTable(
    datatable(  
      filt_data_dist() %>%
        select(-indicator, -indicator_cod),
      options = list(scrollX = TRUE)
    )
  )
  
  output$metadata_dist <- renderDataTable({
    datatable(series_info(series_code = name_to_cod(input$feature_dist) ),rownames = F,colnames = '',
              options = list(ordering=F)) %>% 
      formatStyle(columns = 'key', fontSize = '125%',fontWeight = 'bold')
  })
  
  #### Treemap tab ####

  # sidebar
  
  output$feature_treemap <-  renderUI({
    selectizeInput("feature_treemap",
                   label =  "Choose indicator",
                   choices = filter_topics(input$topic_treemap),
                   selected = "Prevalence of HIV, total (% of population ages 15-49)")})
  
  
  filt_data_treemaps <- reactive({data[data$indicator_cod==name_to_cod(input$feature_treemap),] %>% unnest()})
  
  output$yearUI= renderUI({
    sliderInput(inputId = "year", 
                label = "Choose year", 
                min = min(filt_data_treemaps() %$% year),
                max = max(filt_data_treemaps() %$% year),
                value=min(filt_data_treemaps() %$% year))
  })
  
  #download
  
  output$download_treemap <- downloadHandler(
    filename = function() {glue('{unique(filt_data_treemaps() %$% indicator_cod)}.csv')} , 
    content =  function(file){write.csv(filt_data_treemaps(),file, row.names = FALSE)})
  
  # mainpanel
  output$treemap =  renderPlot({
    grow_treemap(filt_data_treemaps(),group = input$group,yr = input$year)
  })
  
  output$filt_data_treemaps <- renderDataTable(
    datatable(  
      filt_data_treemaps() %>%
        select(-indicator, -indicator_cod),
      options = list(scrollX = TRUE)
    )
  )
  
  output$metadata_treemap <- renderDataTable({
    datatable(series_info(series_code = name_to_cod(input$feature_treemap) ),rownames = F,colnames = '',
              options = list(ordering=F)) %>% 
      formatStyle(columns = 'key', fontSize = '125%',fontWeight = 'bold')
  })

  #### scatterplot ####
  
  # sidebar
  
  output$input_x <-  renderUI({
    selectizeInput("xvar_scatter",
                   label =  "Choose indicator for x axis",
                   choices = filter_topics(input$topic_scatter_x),
                   selected ='Share of women in wage employment in the nonagricultural sector (% of total nonagricultural employment)')})
  
  output$input_y <-  renderUI({
    selectizeInput("yvar_scatter",
                   label =  "Choose indicator for y axis",
                   choices = filter_topics(input$topic_scatter_y),
                   selected = 'Labor force, female (% of total labor force)')})
  
  filt_data_scatter <- reactive({data[data$indicator_cod%in%c(name_to_cod(input$xvar_scatter),
                                                              name_to_cod(input$yvar_scatter)),] %>% 
      unnest()})
  
  
  min_year <- reactive({
    min(filt_data_scatter() %>%
          select(indicator_cod,country,year,value) %>% 
          spread(.,indicator_cod,value) %>% 
          na.omit() %$% year)
    })
    
  max_year <- reactive({
    max(filt_data_scatter() %>%
          select(indicator_cod,country,year,value) %>% 
          spread(.,indicator_cod,value) %>% 
          na.omit() %$% year)
  })
  
  
  output$yearUI_scatter= renderUI({
    sliderInput(inputId = "year_scatter", 
                label = "Choose year", 
                min = min_year(),
                max = max_year(),
                value= 2010,
                animate= animationOptions(interval = 2000))
  })

  
  #download
  output$download_scatter <- downloadHandler(
    filename = function() {glue('{unique(filt_data_scatter() %$% indicator_cod)}.csv')} , 
    content =  function(file){write.csv(filt_data_scatter(),file, row.names = FALSE)})
  
  # mainpanel

  output$scatter =  renderPlotly({
    correlation_plot(filt_data = filt_data_scatter(),
                     input_x = input$xvar_scatter,
                     input_y = input$yvar_scatter,
                     group = input$group_scatter,
                     yr = input$year_scatter,
                     linear_relation = input$scatter_smooth)
  })
  
  output$filt_data_scatter <- renderDataTable(
    datatable(  
      filt_data_scatter() %>%
        select(-indicator_cod),
      options = list(scrollX = TRUE)
    )
  )
  
  output$xname= renderUI({
    h4(input$xvar_scatter)
  })
  
  output$x_metadata_scatter <- renderDataTable({
    datatable(series_info(series_code = name_to_cod(input$xvar_scatter) ),rownames = F,colnames = '',
              options = list(ordering=F)) %>% 
      formatStyle(columns = 'key', fontSize = '125%',fontWeight = 'bold')
  })
  
  output$yname= renderUI({
    h4(input$yvar_scatter)
  })
  
  output$y_metadata_scatter <- renderDataTable({
    datatable(series_info(series_code = name_to_cod(input$yvar_scatter) ),rownames = F,colnames = '',
              options = list(ordering=F)) %>% 
      formatStyle(columns = 'key', fontSize = '125%',fontWeight = 'bold')
  })
  
  
}
##### RUN ##### 

shinyApp(ui, server)





