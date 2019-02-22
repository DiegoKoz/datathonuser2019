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

##### load data ##### 

data <- read_rds('data/data.rds')

##### functions ####

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
      top_n(3, rev(value)) %$% country
    
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


grow_treemap <- function(filt_data, yr=2016, group='inc_group') {
  
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




##### UI ##### 

ui <- fluidPage(theme = shinytheme("paper"),
                
                tabsetPanel(type = "tabs",
                            tabPanel("density",
                                     
                titlePanel("Countries distribution over features"),
                
                sidebarLayout(
                  sidebarPanel(selectizeInput("feature",
                                              label =  "Choose indicator",
                                              choices = unique(data$indicator),
                                              selected = "Prevalence of HIV, total (% of population ages 15-49)"),
                               checkboxInput('top_3',label = 'highlight top countries',value = TRUE),
                               checkboxInput('last_3',label = 'highlight last countries',value = FALSE),
                               htmlOutput("countryUI"),
                               # submitButton("Update", icon("refresh")),
                               
                               "Autor: ",a("Diego Kozlowski", href="https://sites.google.com/view/diego-kozlowski")
                  ),
                  mainPanel(
                                         plotOutput("plot", width = "800px", height = "600px")%>% 
                                           withSpinner(color="#0dc5c1")
                                ))),
                tabPanel("treemap",
                         titlePanel("Countries distribution over features"),
                         
                         sidebarLayout(
                           sidebarPanel(selectizeInput("feature_treemap",
                                                       label =  "Choose indicator",
                                                       choices = unique(data$indicator),
                                                       selected = "Prevalence of HIV, total (% of population ages 15-49)"),
                           selectizeInput("group",
                                          label =  "Choose indicator",
                                          choices = c('income groups'='inc_group','region'),
                                          selected = "inc_group"),
                           htmlOutput("yearUI"),
                                        "Autor: ",a("Diego Kozlowski", href="https://sites.google.com/view/diego-kozlowski")
                           ),
                           mainPanel(
                         
                                         plotOutput("treemap", width = "800px", height = "600px")%>% 
                                                       withSpinner(color="#0dc5c1")
                                            )
                                )
                  )
                )
)

##### server ##### 


server <- function (input, output) {
  
filt_data <- reactive({data[data$indicator==input$feature,] %>% unnest()})

filt_data_treemaps <- reactive({data[data$indicator==input$feature_treemap,] %>% unnest()})
  
  output$countryUI = renderUI({
    selectInput(inputId = "specific_countries", 
                label = "Choose countries to highlight", 
                choices = filt_data() %$% country, 
                multiple = TRUE)})
  
  
  output$yearUI= renderUI({
    sliderInput(inputId = "year", 
                label = "Choose year", 
                min = min(filt_data_treemaps() %$% year),
                max = max(filt_data_treemaps() %$% year),
                value=min(filt_data_treemaps() %$% year),
                animate= animationOptions(interval = 2000))
  })
  
  output$plot =  renderPlot({
      
    dist_plot(filt_data(),top_countries = input$top_3, bottom_countries = input$last_3, specific_countries = input$specific_countries)

  })
  
  output$treemap =  renderPlot({
    
    grow_treemap(filt_data_treemaps(),group = input$group,yr = input$year)
    
  })
  
}
##### RUN ##### 

shinyApp(ui, server)





