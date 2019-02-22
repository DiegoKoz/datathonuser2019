library(tidyverse)
library(ggthemes)
library(ggridges)
library(ggrepel)
library(shiny)
library(shinythemes)
library(shinycssloaders)
library(glue)

##### datos ##### 

distribuciones_impo <- read_rds("distribuciones_impo.RDS") 
distribuciones_expo <- read_rds("distribuciones_expo.RDS") 

##### UI ##### 

ui <- fluidPage(theme = shinytheme("paper"),
                
        titlePanel("Distribución de centralidad de los nodos"),
                
        sidebarLayout(
                  sidebarPanel(selectInput("data",
                                           label =  "Elegir tipo de datos",
                                          choices = c("Importaciones","Exportaciones"),
                                          selected = "Importaciones"),
                               
                               selectInput("var",
                                           label =  "Elegir métrica de centralidad",
                                          choices = c("Grado","Intermediacion","Autovalor","Autovalor ponderado por el comercio total"= "Autovalor_ponderado" ),
                                          selected = "Grado"),
                               selectInput("paises",
                                           label ="Marcar países en el gráfico",
                                           choices = distribuciones_expo$pais,
                                           selected = c("United States", "China"),
                                          multiple = TRUE),
                               
                               "Autor: ",a("Diego Kozlowski", href="https://sites.google.com/view/diego-kozlowski")
                               ),
                  mainPanel(plotOutput("plot", width = "800px", height = "600px")%>% 
                                           withSpinner(color="#0dc5c1"),
                            helpText("El código se encuentra disponible en el",
                                     a("Repositorio de GitHub", href="https://github.com/DiegoKoz/grafo_comercio_agregado")),
                            helpText("Las medidas de centralidad y el análisis de la información se encuentra en el siguiente",
                                     a("artículo", href="https://drive.google.com/file/d/1puJFu9cWRMrFg_MUUtnhKExeFcU6RJHg/view"))
                            
                           )
                  
                      )
)
##### server ##### 

server <- function (input, output) {
  

 # base <- reactive ({bases[[input$data]] })
  
  output$plot =  renderPlot({
  base <- switch(input$data,
           "Exportaciones" = distribuciones_expo,
           "Importaciones" = distribuciones_impo)
  
  #eval(input$var)
  #base <- distribuciones_impo
    ggplot(base , aes_string(input$var, "yr"))+
      geom_density_ridges(alpha = 0.2)+
      geom_text_repel(data = base %>%
                        filter(pais %in% input$paises),
                      aes(label = cod, color = pais),
                      nudge_y = 0.5, fontface = "bold")+
      theme_tufte()+
      theme(legend.position = "none")+
      scale_color_gdocs() +
      scale_fill_gdocs() +
      labs(y = "Año",
           title= glue('Distribución de {input$var} de los nodos'),
           subtitle = glue("{input$data}, threshold 1%, según año."))
    })
 
}

##### RUN ##### 

shinyApp(ui, server)





