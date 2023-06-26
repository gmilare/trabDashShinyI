library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(ggspatial)
library(readr)
library(lubridate)
library(sf)
library(here)

dados <- read_csv(here("dados","focos_ano_matopiba.csv"))
dados$ano_mes <- lubridate::ym(dados$ano_mes)
dados$ano <- lubridate::year(dados$ano_mes)

focosano <- dados |>
  group_by(ano, nm_micr) |>
  summarise(totalano = sum(n)) |>
  rename(nm_micro = nm_micr)

micror <-st_read(here("dados","microrregioes_matopiba.shp"))
#juntando a tabela com o shapefile através do atributo "nm_micro"
shp_micro <- left_join(micror, focosano, by = "nm_micro")
#cria atributo "area" e cálcula de área das microrregiões
shp_micro$area <-st_area(shp_micro$geometry) / 1000000
#criar atributo "D" e calcula a densidade de focos por km²
shp_micro <- shp_micro |>
 mutate(D = as.numeric(totalano / area))



ui <- dashboardPage(
  dashboardHeader(title = "Queimadas"),
  dashboardSidebar(sidebarMenu(
    menuItem(
      text = "Sobre",
      tabName = "sobre",
      icon = icon("info")
    ),
    menuItem(
      text = "  Visão geral",
      tabName = "visao_geral",
      icon = icon("eye")
    ),
    menuItem(
      text = "Microrregiões",
      tabName = "micro",
      icon = icon("map")
    )
  )),
  dashboardBody(tabItems(
    tabItem(tabName = "sobre",
            titlePanel(""),
            includeMarkdown("texto_sobre.md")),
    tabItem(
      tabName = "visao_geral",
      titlePanel("Visão geral"),
      p(""),
      fluidRow(
        valueBoxOutput(outputId = "n_micro", width = 6),
        valueBoxOutput(outputId = "m_focos", width = 6)
      ),
      fluidRow(tabBox(
        width = 12,
        tabPanel(
          selectInput(
            inputId = "ano",
            label = "Para visualizar o mapa de ocorrência de focos de queimadas por microrregião, selecione o ano:",
            choices = unique(dados$ano)
          )
          ,
          plotOutput("map", height = "600px")
        )
      ))
    ),
    tabItem(
      tabName = "micro",
      titlePanel("Dados para microrregiões"),
      fluidRow(shinydashboard::box(
        title = "",
        width = 12,
        fluidRow(column(
          width = 3,
          selectInput(
            inputId = "micro_sel",
            label = "Selecione um microrregião:",
            choices = unique(dados$nm_micr)
          )
        )))),
        fluidRow(shinydashboard::box(column(
          title = "",
          width = 12,
          h3("Gráfico com a ocorrência número de focos de queimadas mensais por km²"),
          plotOutput("micro_plot"),
          h3("Tabela com número de focos anuais"),
          DT::dataTableOutput("tabela")
        )

          )


      ))
)))


server <- function(input, output, session) {
  output$n_micro <- renderValueBox({
    mm <- n_distinct(dados$nm_micr)
    valueBox(
      value = mm,
      subtitle = "Número de microrregiões",
      icon = icon("map"),
      color = "red"
    )
  })

  output$m_focos <- renderValueBox({
    ff <- sum(dados$n)
    valueBox(
      value = ff,
      subtitle = "Total de focos (2001-2021)",
      icon = icon("fire"),
      color = "red"
    )
  })
  output$map <- renderPlot({
    shp_micro |>
      dplyr::filter(ano == input$ano) |>
      ggplot() +
      geom_sf(aes(fill = D)) +
      theme(text = element_text(size = 10)) +
      scale_fill_gradient(
        low = "#fff7ec",
        high = "#7f0000",
        limits = c(0, 0.2),
        name = "Focos/km²"
      ) +
      annotation_north_arrow(
        location = 'tl',
        height = unit(0.5, "cm"),
        width = unit(0.5, "cm"),
        style = north_arrow_orienteering(text_size = 5)
      )
  })

  output$micro_plot <- renderPlot({
    dados |>
      dplyr::filter(nm_micr == input$micro_sel) |>
      ggplot() +
      geom_line(aes(x = ano_mes, y = n),
                colour = "red") +
      scale_x_date(date_labels = "%b/%Y", date_breaks = "6 month") +
      xlab("Mês/Ano") + ylab("Focos/km²") +
      theme(text = element_text(size = 14),
            axis.text.x = element_text(angle = 75, vjust = .5))

  })

  output$tabela <- DT::renderDataTable({
    focosano |>
      dplyr::filter(nm_micro == input$micro_sel) |>
      select(ano, totalano) |>
      DT::datatable(rownames= FALSE,
                    colnames = c('Ano' = 'ano',
                                 'Número de focos' = 'totalano'))
  })

}



shinyApp(ui, server)
