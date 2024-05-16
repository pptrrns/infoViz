#---------------------- Integración a Shiny

# José Ángel Torrenc Hernández
# 175021
# Ciencia Política, ITAM

#---------------------- Cargar librerías

if(!require(pacman)) install.packages("pacman")
pacman::p_load(dplyr, stringr, tidyr, forcats, ggplot2, maps, sf, leaflet, leafpop, 
               lattice, plotly, shiny, shinythemes, DT, RColorBrewer, ggcorrplot)

#---------------------- Cargar bases de datos

grd <- read.csv("dataset/grd.csv")

hdi <-  read.csv("dataset/hdi.csv")

wiidglobal <- read.csv("dataset/wiidglobal.csv")

#---------------------- Helpers

source("color_palette.R")

#---------------------- Unimos bases de datos

new_Data <- left_join(wiidglobal, grd, by = c("identifier", "iso3", 
                                              "country", "year"))

new_Data <- left_join(new_Data, hdi, by = c("identifier", "iso3", 
                                            "country", "year"))

new_Data <- new_Data  %>% 
  filter(year >= 1950) %>% 
  filter(identifier != "Area")

#---------------------- Dataset para mapas

map_df <- new_Data %>% 
  select(country, iso3, region_wb, incomegroup, year, gini)

map_df <- pivot_wider(map_df, 
                      names_from = year, 
                      values_from = gini)

world <- map("world", fill = TRUE, plot = FALSE)

world <- st_as_sf(world)

world <- world %>% 
  mutate(ID = gsub("USA","United States", ID),
         ID = gsub("Republic of Congo","Congo, Republic of the", ID),
         ID = gsub("Democratic Republic of the Congo","Congo, Democratic Republic of the", ID),
         ID = gsub("UK","United Kingdom", ID),
         ID = gsub("South Korea", "Korea, Republic of", ID),
         ID = gsub("North Korea","Korea, DPR", ID),
         ID = gsub("Turkey"," Turkiye", ID),)

world <- left_join(world, map_df, by = c("ID" = "country"))

world$region <- "Region"

steps <- c("#1984c5", "#22a7f0", "#63bff0", "#de6e56", "#e14b31", "#c23728")

steps <- c("#1984c5", "#22a7f0", "#63bff0", "#df979e", "#d7658b", "#c80064")

# colors <- rev(colorRampPalette(brewer.pal(11, "PRGn"))(100))

color_palette <- colorNumeric(steps, domain = c(0, 100))

# color_palette <- color.palette(steps, space = "rgb")

# rm(list = "steps")

# rm(list = "map_df")

leaflet() %>%
  # addProviderTiles(providers$CartoDB.Voyager) %>%
  addProviderTiles(providers$CartoDB.Positron) %>% 
  setView(lng = 0, 
          lat = 30,
          zoom = 3) %>% 
  addPolygons(data = world,
              fillColor = ~color_palette(world$y2020),
              color = "transparent",
              fillOpacity = 0.7,
              weight = 1)

#---------------------- Datasets para gráficas

new_Data <- new_Data %>%
  select(c(identifier, country, iso3, region_wb, incomegroup, year, population, gdp, # Datos país
           gini, ginia, coef_ineq, ineq_inc, ihdi, bottom5, bottom20, bottom40, middle50, top20, top10, top5, top10, # Datos desigualdad
           gdi, gii, hdi_f, hdi_m, pr_f, pr_m, # Datos genero
           hdi, abr, le, le_f, le_m, mmr, mys, mys_f, mys_m)) # Datos desarrollo humano

develop_data <- new_Data
  
new_Data <- pivot_longer(data = new_Data,
                         cols = population:mys_m,
                         names_to = "variable",
                         values_to = "data")

new_Data$region <- "Region"

new_Data <- new_Data %>% 
  filter(!is.na(data))

new_Data <- mutate_if(new_Data, is.numeric, ~round(., 2))

#---------------------- Datasets para serie de tiempo Gini

time_Serie <- wiidglobal %>%
  filter(subarea %in% c("South Asia",
                        "East Asia and the Pacific",
                        "Latin America and the Caribbean",
                        "Middle East and North Africa",
                        "Sub-Saharan Africa",
                        "Europe and Central Asia",
                        "North America"))

#---------------------- Tabla Gender

genero <- pivot_wider(new_Data, 
                      names_from = variable, 
                      values_from = data)

genero <- genero %>%
  filter(year >= 1950) %>%
  select(c(identifier, country, iso3, region_wb, incomegroup, year, population, gdp,
           gdi, gii, hdi_f, hdi_m, pr_f, pr_m)) %>% 
  filter(!(is.na(gdi) & is.na(gii) & is.na(hdi_f) &is.na(hdi_m) & is.na(pr_f) & is.na(pr_m))) %>% 
  
  rename("alpha code" = iso3) %>% 
  rename("region world bank" = region_wb) %>%
  rename("income group" = incomegroup) %>%
  rename("gdp per capita" = gdp) %>% 
  
  rename("gender development index" = gdi) %>% 
  rename("gender inequality index" = gii) %>% 
  rename("human development index (Female)" = hdi_f) %>% 
  rename("human development index (Male)" = hdi_m) %>% 
  rename("women in congress" = pr_f) %>%
  rename("male in congress" = pr_m)

colnames(genero) <- str_to_title(colnames(genero))

#---------------------- Tabla Inequality

ineq <- pivot_wider(new_Data, 
                    names_from = variable, 
                    values_from = data)

ineq <- ineq %>%
  filter(year >= 1950) %>% 
  select(c(identifier, country, iso3, region_wb, incomegroup, year, population, gdp,
           gini, ginia, bottom5, bottom20, bottom40, middle50, top20, top10, top5, coef_ineq, ineq_inc, ihdi)) %>%
  rename("alpha code" = iso3) %>% 
  rename("region world bank" = region_wb) %>% 
  rename("income group" = incomegroup) %>%
  rename("gdp per capita" = gdp) %>% 
  
  rename("Gini Index" = gini) %>% 
  rename("Absolute Gini" = ginia) %>% 
  rename("Coefficient of Human Inequality" = coef_ineq) %>% 
  rename("Inequality in Income" = ineq_inc) %>% 
  rename("Inequality-adjusted Human Development Index" = ihdi) %>% 
  
  rename("Bottom 5% Income Share" = bottom5) %>% 
  rename("Bottom 20% Income Share" = bottom20) %>% 
  rename("Bottom 40% Income Share" = bottom40) %>% 
  rename("Middle 50% Income Share" = middle50) %>% 
  rename("Top 20% Income Share" = top20) %>% 
  rename("Top 10% Income Share" = top10) %>% 
  rename("Top 5% Income Share" = top5)
  
colnames(ineq) <- str_to_title(colnames(ineq))

#---------------------- Tabla Development

develop <- pivot_wider(new_Data, 
                    names_from = variable, 
                    values_from = data)

develop <- develop %>%
  filter(year >= 1950) %>% 
  select(c(identifier, country, iso3, region_wb, incomegroup, year, population, gdp,
           hdi, abr, le, le_f, le_m, mmr, mys, mys_f, mys_m)) %>%
  filter(!(is.na(hdi) & is.na(abr) & is.na(le) & is.na(le_f) & is.na(le_m) & is.na(mmr) & is.na(mys) & is.na(mys_f) & is.na(mys_m))) %>%
    
  rename("alpha code" = iso3) %>% 
  rename("region world bank" = region_wb) %>% 
  rename("income group" = incomegroup) %>%
  rename("gdp per capita" = gdp) %>% 
    
  rename("Human Development Index" = hdi) %>% 
  rename("Adolescent Birth Rate" = abr) %>% 
  rename("Life Expectancy at Birth" = le) %>% 
  rename("Life Expectancy at Birth (Female)" = le_f) %>% 
  rename("Life Expectancy at Birth (Male)" = le_m) %>% 
  rename("Maternal Mortality Ratio" = mmr) %>% 
  rename("Mean Years of Schooling" = mys) %>% 
  rename("Mean Years of Schooling (Female)" = mys_f) %>% 
  rename("Mean Years of Schooling (Male)" = mys_m)
    
colnames(develop) <- str_to_title(colnames(develop))
  
#---------------------- Data correlación 

df <- develop_data %>%
  select_if(is.numeric) %>% 
  select(-c(year, population, gdp, gini, ginia, bottom5, bottom20, bottom40, 
            middle50, top20, top10, top5, coef_ineq, ineq_inc, ihdi))

matriz <- cor(df, 
            method = "pearson", 
            use = "pairwise.complete.obs")

my_palette <- brewer.pal(n = 3,
                         name = "PRGn")

#---------------------- Tema 

tema <- theme(plot.title = element_text(size = 15, color = "#1B2128", face = 'bold'),
              plot.subtitle = element_text(size = 14, color = "#747577", vjust = 1.5),
              axis.line = element_blank(),
              panel.border = element_blank(),
              panel.background = element_blank(),
              plot.background = element_rect(color = "white", fill = "white"),
              axis.title = element_blank(),
              axis.text.x = element_text(size = 12),
              axis.text.y = element_text(size = 12),
              axis.ticks = element_blank(),
              legend.text = element_text(size = 12))

#----------------------

# Shiny App

#---------------------- About

about_page <- tabPanel(
  title = "About",
  fluidRow(
    column(8, offset = 2,
           div(style = "padding: 20px; text-align: center;",
               br(),
               img(src = "logo_ITAM.png", height = "45px", 
                   style = "margin-bottom: 25px;"),
               br(),
               h1("Desarrollo humano, desigualdad y pobreza", 
                  style = "color: #2c3e50;"),
               tags$hr(),
               h3("José Ángel Torrens Hernández"),
               h4("Ciencia Política") # ,
           #     br(),
           #     h3("Referencias"),
           #     p("UNDP. Human development index. Technical report, United Nations Development Programme, 2024.", style = "font-size: 14px;"),
           #     p("UNU-WIDER. Government revenue dataset. Technical report, United Nations University World Institute for Development Economics Research (UNU-WIDER), 2023a.", style = "font-size: 14px;"),
           #     p("UNU-WIDER. World income inequality database. Technical report, United Nations University World Institute for Development Economics Research (UNU-WIDER), 2023b.", style = "font-size: 14px;")
             )
           )
    )
  )

#---------------------- Página Desigualdad

main_page <- tabPanel(title = "Desigualdad",
                      sidebarLayout(
                        sidebarPanel(title = "Desigualdad",
                                     
                                     h2("Desigualdad"),
                                     p("El 25 de septiembre de 2015, los Estados Miembros de las Naciones Unidas (NN.UU.) adoptaron 17 Objetivos de Desarrollo Sostenible (ODS) para poner fin a la pobreza, proteger el planeta, el medio ambiente y asegurar la prosperidad para todos. Lo resultados de dichos Objetivos deberían alcanzare en 15 años, es decir, para 2030 deberían materializarse y concretarse como una realidad para todas las personas."),
                                     p("En este tablero de Shiny podrás explorar cómo se comportan algunos indicadores relacionados con dos Objetivos: (1 ODS) erradicar la pobreza y (10 ODS) reducir las desigualdades. Como podrás notar, a pesar de los esfuerzos por erradicar la pobreza y reducir la desigualdad, millones de personas aún viven en condiciones precarias, enfrentando dificultades para asegurar su subsistencia."),
                                     
                                     tags$hr(),
                                     
                                     h3("Indicadores de Desigualdad"),
                                     
                                     selectInput("col_year", 
                                                 h4("Selecciona un año:"),
                                                 choices = names(world)[sapply(world, is.numeric)],
                                                 selected = "2022"),
                                     
                                      sliderInput("range", 
                                                  h4("Selecciona un rango:"),
                                                  min = 0,
                                                  max = 100,
                                                  value = c(0, 100), 
                                                  step = 0.1),

                                     tags$hr(),
                                     
                                     radioButtons("choice", 
                                                  label = h4("Selecciona una región:"),
                                                  choices = c("World" = "Region",
                                                              "East Asia and the Pacific (EAP)" = "East Asia and the Pacific",
                                                              "Europe and Central Asia (ECA)" = "Europe and Central Asia",
                                                              "Latin America and the Caribbean (LAC)" = "Latin America and the Caribbean",
                                                              "Middle East and North Africa (MENA)" = "Middle East and North Africa",
                                                              "North America (NA)" = "North America",
                                                              "South Asia (SA)" = "South Asia",
                                                              "Sub-Saharan Africa (SSA)" = "Sub-Saharan Africa"),
                                                  selected = "Region")

                        ),
                        
                        mainPanel(
                          tabsetPanel(
                            tabPanel(
                              
                              title = "Desigualdad por ingreso",
                              
                              h3("Coeficiente de Gini (Serie 1950 - 20202)"),
                              p("El Coeficiente de Gini es una medida para estimar la desigualdad en los ingresos dentro de un país y se computa como una variable entre 0 y 1, donde 0 se corresponde con la perfecta igualdad y 1 se corresponde con la perfecta desigualdad."),
                              leafletOutput("map", height = 400),
                              tags$hr(),
                              
                              fluidRow(column(4, h4("Coeficiente de Gini por región"),
                                              p("Serie 1950 - 2020"),
                                              plotOutput("timeserie")),
                                       
                                       column(4, h4("Distribución del Coeficiente de Gini"),
                                              p("Datos reportados para el año seleccionado"),
                                              plotOutput("density")),
                                       
                                       column(4, h4("Participación en el ingreso nacional"),
                                              p("Porcentaje de participación entre diferentes percentiles"),
                                              plotOutput("boxplot"))
                                       
                              )
                            ),
                            
                            tabPanel(
                              
                              title = "Tabla extracto",
                              
                              h3("Tabla extracto: indicadores de desigualdad por ingreso"),
                              dataTableOutput("tabla1"),
                              p("UNDP. Human development index. Technical report, United Nations Development Programme, 2024.", style = "font-size: 14px;"),
                              p("UNU-WIDER. Government revenue dataset. Technical report, United Nations University World Institute for Development Economics Research (UNU-WIDER), 2023a.", style = "font-size: 14px;"),
                              p("UNU-WIDER. World income inequality database. Technical report, United Nations University World Institute for Development Economics Research (UNU-WIDER), 2023b.", style = "font-size: 14px;")
                              
                              ),
                            
                            tabPanel(
                              
                              title = "Desigualdad por género",
                              
                              fluidRow(column(6, h3("Participación Política (Serie 1990 - 2022)"),
                                              p("% de asientos ocupados en el Parlamento por hombres y mujeres. La línea muestra el promedio global, mientras que el marcador de los puntos las observaciones a nivel país."),
                                              plotOutput("des_pol")),
                                       
                                       column(6, h3("Desigualdad por género (2020)"),
                                              p("Distribución de la evaluación de más de 190 países en indicadores como el Índice de Desarrollo de Género (GDI) y el Índice de Desigualdad de Género (GII)."),
                                              plotOutput("des_genero"))
                              ),
                              
                              tags$hr(),
                              h3("Tabla extracto: indicadores de desigualdad por género"),
                              dataTableOutput("tabla2"),
                              p("UNDP. Human development index. Technical report, United Nations Development Programme, 2024.", style = "font-size: 14px;"),
                              p("UNU-WIDER. Government revenue dataset. Technical report, United Nations University World Institute for Development Economics Research (UNU-WIDER), 2023a.", style = "font-size: 14px;"),
                              p("UNU-WIDER. World income inequality database. Technical report, United Nations University World Institute for Development Economics Research (UNU-WIDER), 2023b.", style = "font-size: 14px;")
                              
                            )
                            )
                          )
                        )
                      )

#---------------------- Página Desarrollo Humano

develop_page <- tabPanel(title = "Desarrollo Humano",
                         sidebarLayout(
                           sidebarPanel(title = "Desarrollo Humano",
                                        h2("Desarrollo Humano"),
                                        p("El 25 de septiembre de 2015, los Estados Miembros de las Naciones Unidas (NN.UU.) adoptaron 17 Objetivos de Desarrollo Sostenible (ODS) para poner fin a la pobreza, proteger el planeta, el medio ambiente y asegurar la prosperidad para todos. Lo resultados de dichos Objetivos deberían alcanzare en 15 años, es decir, para 2030 deberían materializarse y concretarse como una realidad para todas las personas."),
                                        p("En este tablero de Shiny podrás explorar cómo se comportan algunos indicadores relacionados con dos Objetivos: (1 ODS) erradicar la pobreza y (10 ODS) reducir las desigualdades. Como podrás notar, a pesar de los esfuerzos por erradicar la pobreza y reducir la desigualdad, millones de personas aún viven en condiciones precarias, enfrentando dificultades para asegurar su subsistencia."),
                                        tags$hr(),
                                        
                                        h3("Relación entre indicadores de Desarrollo Humano"),
                                        
                                        selectInput("year", 
                                                    h4("Selecciona un año:"),
                                                    choices = 1990:2020,
                                                    selected = "2020"),
                                        
                                        selectInput("dep_", 
                                                    h4("Selecciona una variable dependiente (eje y):"),
                                                    choices = c("GDP" = "gdp",
                                                                "Human Development Index" = "hdi", 
                                                                "Adolescent Birth Rate" = "abr",
                                                                "Life Expectancy at Birth" = "le",
                                                                "Life Expectancy at Birth (Female)" = "le_f",
                                                                "Life Expectancy at Birth (Male)" = "le_m",
                                                                "Maternal Mortality Ratio" = "mmr", 
                                                                "Mean Years of Schooling" = "mys",
                                                                "Mean Years of Schooling (Female)" = "mys_f",
                                                                "Mean Years of Schooling (Male)" = "mys_m",
                                                                "Gini Index" = "gini",
                                                                "Absolute Gini" = "ginia",
                                                                "Coefficient of Human Inequality" = "coef_ineq",
                                                                "Inequality in Income" = "ineq_inc",
                                                                "Inequality-adjusted Human Development Index (IHDI)" = "ihdi",
                                                                "Bottom 5% Income Share" = "bottom5",
                                                                "Bottom 20% Income Share" = "bottom20",
                                                                "Bottom 40% Income Share" = "bottom40",
                                                                "Middle 50% Income Share" = "middle50",
                                                                "Top 20% Income Share" = "top20",
                                                                "Top 10% Income Share" = "top10",
                                                                "Top 5% Income Share" = "top5",
                                                                "Gender Development Index (GDI)" = "gdi",
                                                                "Gender Inequality Index (GII)" = "gii", 
                                                                "Female Human Development Index (HDI-F)" = "hdi_f", 
                                                                "Male Human Development Index (HDI-M)" = "hdi_m",
                                                                "Share of seats in Parliament held by women" = "pr_f",
                                                                "Share of seats in Parliament held by men" = "pr_m"),
                                                    selected = "hdi"),
                                        
                                        selectInput("indep_", 
                                                    h4("Selecciona una variable independiente (eje x):"),
                                                    choices = c("Year" = "year",
                                                                "GDP" = "gdp",
                                                                "Human Development Index" = "hdi", 
                                                                "Adolescent Birth Rate" = "abr",
                                                                "Life Expectancy at Birth" = "le",
                                                                "Life Expectancy at Birth (Female)" = "le_f",
                                                                "Life Expectancy at Birth (Male)" = "le_m",
                                                                "Maternal Mortality Ratio" = "mmr", 
                                                                "Mean Years of Schooling" = "mys",
                                                                "Mean Years of Schooling (Female)" = "mys_f",
                                                                "Mean Years of Schooling (Male)" = "mys_m",
                                                                "Gini Index" = "gini",
                                                                "Absolute Gini" = "ginia",
                                                                "Coefficient of Human Inequality" = "coef_ineq",
                                                                "Inequality in Income" = "ineq_inc",
                                                                "Inequality-adjusted Human Development Index (IHDI)" = "ihdi",
                                                                "Bottom 5% Income Share" = "bottom5",
                                                                "Bottom 20% Income Share" = "bottom20",
                                                                "Bottom 40% Income Share" = "bottom40",
                                                                "Middle 50% Income Share" = "middle50",
                                                                "Top 20% Income Share" = "top20",
                                                                "Top 10% Income Share" = "top10",
                                                                "Top 5% Income Share" = "top5",
                                                                "Gender Development Index (GDI)" = "gdi",
                                                                "Gender Inequality Index (GII)" = "gii", 
                                                                "Female Human Development Index (HDI-F)" = "hdi_f", 
                                                                "Male Human Development Index (HDI-M)" = "hdi_m",
                                                                "Share of seats in Parliament held by women" = "pr_f",
                                                                "Share of seats in Parliament held by men" = "pr_m"),
                                                    selected = "le"),
                                        
                                        
                                        selectInput("color_", 
                                                    h4("Selecciona una variable de color:"),
                                                    choices = c("Region World Bank" = "region_wb",
                                                                "Income Group" = "incomegroup"),
                                                    selected = "region_wb")
                                        
                                        
                           ),
                           
                           mainPanel(
                             tabsetPanel(
                               tabPanel(
                                 title = "Indicadores de Desarrollo Humano",
                                 # fluidRow(column(6, h3("Indicadores de Desarrollo Humano (1990- 2020)"),
                                 #                 plotOutput("disp")),
                                 #          
                                 #          column(6, 
                                 #                 br(), 
                                 #                 br(),
                                 #                 br(),
                                 #                 plotOutput("correlation"))
                                 
                                 fluidRow(h3("Indicadores de Desarrollo Humano (1990- 2020)"),
                                                  plotOutput("disp")
                                 ),
                                 
                                 tags$hr(),
                                 h3("Tabla extracto: Desarrollo Humano"),
                                 dataTableOutput("tabla3"),
                                 p("UNDP. Human development index. Technical report, United Nations Development Programme, 2024.", style = "font-size: 14px;"),
                                 p("UNU-WIDER. Government revenue dataset. Technical report, United Nations University World Institute for Development Economics Research (UNU-WIDER), 2023a.", style = "font-size: 14px;"),
                                 p("UNU-WIDER. World income inequality database. Technical report, United Nations University World Institute for Development Economics Research (UNU-WIDER), 2023b.", style = "font-size: 14px;")
                                 
                               )
                             )
                           )
                         )
)

#---------------------- Interfaz de usuario

ui <- navbarPage(title = "Integración a Shiny",
                 theme = shinytheme('cosmo'),
                 main_page,
                 develop_page,
                 about_page
)

#---------------------- Servidor

server <- function(input, output){
  
  #---------------------- Mapa

  observe({
    
    selected_year <- input$col_year
    
    min <- input$range[1]
    max <- input$range[2]
    
    world <- filter(world, 
                    .data[[selected_year]] > min &
                      .data[[selected_year]] < max)
    
    output$map = renderLeaflet(
      
      leaflet() %>%
        addProviderTiles(providers$CartoDB.Voyager) %>%
        addProviderTiles(providers$CartoDB.Positron) %>% 
        setView(lng = 0, 
                lat = 30,
                zoom = 3) %>% 
        addPolygons(data = world,
                    fillColor = ~color_palette(world[[selected_year]]),
                    color = "transparent",
                    fillOpacity = 0.7,
                    weight = 1,
                    popup = paste0("<strong>País: </strong>", world$ID, "<br>",
                                   "<strong>Región: </strong>", world$region_wb, "<br>",
                                   "<strong>Income Group: </strong>", world$incomegroup, "<br>",
                                   "<strong>Año: </strong>", selected_year, "<br>",
                                   "<strong>Coeficiente de Gini: </strong>", world[[selected_year]], "<br>"
                    )
        )
    )
    
  })
  
  #---------------------- Gráfica de densidad
  
  observe({
    
    selected_year <- input$col_year
    selected_region <- input$choice
    
    output$density = renderPlot(
      
      world %>% 
        filter(case_when(selected_region == "Region" ~ region == selected_region,
                         selected_region %in% c("East Asia and the Pacific", 
                                                "Europe and Central Asia", 
                                                "Latin America and the Caribbean", 
                                                "Middle East and North Africa", 
                                                "North America", "South Asia", 
                                                "Sub-Saharan Africa") ~ region_wb == selected_region,
                         TRUE ~ FALSE
        )) %>% 
        ggplot(aes(x = .data[[selected_year]])) +
        geom_density(alpha = 0.3, 
                     color = "royalblue4",
                     fill = "#8DA0CB") +
        labs(x = "Coeficiente de Gini",
             y = "") +
        tema +
        scale_x_continuous(breaks = seq(0, 100, by = 5)) +
        xlim(0, 100) +
        labs(y = "",
             x = "")
    )
    
  })
  
  #---------------------- Gráfica de caja y brazos
  
  observe({
    
    selected_year <- input$col_year
    selected_region <- input$choice
    
    output$boxplot = renderPlot(
      
      new_Data %>%
        filter(variable %in% c("bottom5", "bottom20", "bottom40",
                               "top5", "top10", "top20", "middle50")) %>%
        mutate(variable = factor(variable, 
                                 levels = c("bottom5", "bottom20", "bottom40", "middle50",
                                            "top20", "top10", "top5")) %>% 
                 fct_recode("Bottom 5" = "bottom5", 
                            "Bottom 20" = "bottom20",
                            "Bottom 40" = "bottom40",
                            "Middle 50" = "middle50",
                            "Top 20" = "top20",
                            "Top 10" = "top10",
                            "Top 5" = "top5")) %>%
        filter(year == selected_year) %>% 
        filter(case_when(selected_region == "Region" ~ region == selected_region,
                         selected_region %in% c("East Asia and the Pacific", 
                                                "Europe and Central Asia", 
                                                "Latin America and the Caribbean", 
                                                "Middle East and North Africa", 
                                                "North America", "South Asia", 
                                                "Sub-Saharan Africa") ~ region_wb == selected_region,
                         TRUE ~ FALSE)) %>%         
        ggplot(aes(variable, data)) + 
        geom_jitter(fill = "lightgrey", color = "lightgrey", alpha = 0.3) +
        geom_boxplot(fill = "#8DA0CB", color = "royalblue4", alpha = 0.5) + 
        tema +
        ylim(0, 100) +
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
      
    )
    
  })
  
  #---------------------- Serie de tiempo
  
  observe({
    
    selected_region <- input$choice
    
    output$timeserie = renderPlot(
      
      time_Serie %>%
        filter(case_when(selected_region == "Region" ~ region == selected_region,
                         selected_region %in% c("East Asia and the Pacific", 
                                                "Europe and Central Asia", 
                                                "Latin America and the Caribbean", 
                                                "Middle East and North Africa", 
                                                "North America", "South Asia", 
                                                "Sub-Saharan Africa") ~ subarea == selected_region,
                         TRUE ~ FALSE)) %>% 
        mutate(subarea = case_when(subarea == "East Asia and the Pacific" ~ "EAAP",
                                   subarea == "Europe and Central Asia" ~ "ECA",
                                   subarea == "Latin America and the Caribbean" ~ "LAC",
                                   subarea == "Middle East and North Africa" ~ "MENA",
                                   subarea == "North America" ~ "NA",
                                   subarea == "South Asia" ~ "SA",
                                   subarea == "Sub-Saharan Africa" ~ "SSA",
                                   TRUE ~ as.character(subarea))) %>%
        ggplot(aes(year, 
                   gini,
                   group = subarea, 
                   colour = subarea)) + 
        geom_line(size = 1) +
        tema +
        scale_color_brewer(palette = "Dark2", 
                           name = "") +
        theme(legend.position = "top") +
        scale_x_continuous(breaks = seq(min(wiidglobal$year), 
                                        max(wiidglobal$year), by = 10))
      
    )
  })
  
  #---------------------- Desigualdad política
  
  observe({
    
    selected_region <- input$choice
    
    
    output$des_pol = renderPlot(
      
      new_Data %>%
        filter(case_when(selected_region == "Region" ~ region == selected_region,
                         selected_region %in% c("East Asia and the Pacific", 
                                                "Europe and Central Asia", 
                                                "Latin America and the Caribbean", 
                                                "Middle East and North Africa", 
                                                "North America", "South Asia", 
                                                "Sub-Saharan Africa") ~ region_wb == selected_region,
                         TRUE ~ FALSE)) %>%
        filter(year >= 1990) %>% 
        filter(variable %in% c("pr_f", "pr_m")) %>%
        group_by(variable, year) %>%
        summarise(data = mean(data, na.rm = T)) %>%
        ggplot(aes(x = year, y = data, group = variable, color = variable)) + 
        geom_line(size = 1.5) +
        geom_jitter(data = new_Data %>% 
                      filter(variable %in% c("pr_f", "pr_m")) %>% 
                      filter(year >= 1990),
                    aes(x = year, y = data), alpha = 0.25) +
        tema +
        labs(y = "Porcentaje de asientos ocupados",
             x = "") +
        scale_x_continuous(breaks = seq(1990, 2022, by = 2)) +
        theme(legend.position = "top") +
        scale_color_manual(values = c("pr_f" = "#8DA0CB", 
                                      "pr_m" = "#66C2A5"), 
                           name = "", labels = c("Porcentaje de Mujeres", 
                                                 "Porcentaje de Hombres"))
      
      
    )
  })
  
  #---------------------- Desigualdad de género
  
  observe({
    
    selected_region <- input$choice
    output$des_genero = renderPlot(
      
      new_Data %>%
        filter(year == 2020) %>% 
        filter(variable %in% c("gdi", "gii", "hdi_f")) %>% 
        ggplot(aes(data,
                   fill = variable)) + 
        geom_density(size = 0.4, alpha = 0.35, color = "white") + 
        theme(legend.position = "top") +
        scale_x_continuous(breaks = seq(0, 1, by = .1)) +
        scale_fill_manual(values = c("gdi" = "#FC8D62",
                                     "gii" = "#66C2A5",
                                     "hdi_f" = "#8DA0CB"), 
                          name = "", 
                          labels = c("Índice de Desarrollo de Género",
                                     "Índice de Desigualdad de Género",
                                     "Índice de Desarrollo Humano en Mujeres")) +
        tema +
        guides(fill = guide_legend(ncol = 1))
    )
    
  })
  
  #---------------------- Desarrollo Humano
  
  observe({
    
    selected_var1 <- input$dep_
    selected_var2 <- input$indep_
    selected_col <- input$color_
    selected_year <- input$year
    
    output$disp = renderPlot(
      
      develop_data %>%
        filter(year == selected_year) %>% 
        ggplot(aes(x = .data[[selected_var2]], 
                   y = .data[[selected_var1]],
                   color = .data[[selected_col]])) +
        geom_point(alpha = 0.6, size = 4) +
        tema +
        scale_color_brewer(palette = "Dark2", name = "") +
        geom_smooth(method = "lm", se = F, alpha = 0.8, linetype = "dotted") +
        xlim(min(develop_data[[selected_var2]]), max(develop_data[[selected_var2]])) +
        ylim(min(develop_data[[selected_var1]]), max(develop_data[[selected_var1]])) +
        theme(legend.position = "top") +
        guides(fill = guide_legend(ncol = 2))
        
  
    ) 
  })
  
  #---------------------- Correlación
  
  # observe({
  #   
  #   output$correlation = renderPlot(
  #     
  #     ggcorrplot(matriz,
  #                type = "lower",
  #                outline.col = "white",
  #                colors = my_palette,
  #                lab = TRUE,
  #                lab_col = "black",
  #                lab_size = 2.3,
  #                digits = 2) +
  #       labs(y = "",
  #            x = "") +
  #       tema +
  #       theme_void() +
  #       theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
  #             legend.position = "none")
  #     
  #   ) 
  # })
  
  #---------------------- Tablas
  
  output$tabla1 <- renderDataTable({
    
    # Tabla 1
    datatable(ineq,
              options = list(pageLength = 5,
                             scrollX = TRUE,
                             paging = "none",
                             searching = TRUE))
  })

  # Tabla 2
    output$tabla2 <- renderDataTable({
      datatable(genero,
                options = list(pageLength = 3,
                               scrollX = TRUE,
                               paging = "none",
                               searching = TRUE))
      
    })
    
    # Tabla 3
    output$tabla3 <- renderDataTable({
      datatable(develop,
                options = list(pageLength = 3,
                               scrollX = TRUE,
                               paging = "none",
                               searching = TRUE))
      
    })
    
} # Aquí termina el servidor 

shinyApp(ui, server)