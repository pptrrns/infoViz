#--------------------

# Cargar librerías

library(ggplot2)
library(dplyr)
library(sf)
library(rnaturalearth)
library(patchwork)
library(stargazer)
library(tidyr)
library(RColorBrewer)
library(forcats)
library(ggcorrplot)

dir("dataset")

# Cargar bases de datos

grd <- read.csv("dataset/grd.csv")
colnames(grd)

hdi <-  read.csv("dataset/hdi.csv")
colnames(hdi)

wiidglobal <- read.csv("dataset/wiidglobal.csv")
colnames(wiidglobal)

new_Data <- left_join(wiidglobal, grd, by = c("identifier", "iso3", 
                                              "country", "year"))

new_Data <- left_join(new_Data, hdi, by = c("identifier", "iso3", 
                                            "country", "year"))

new_Data <- new_Data  %>% 
  filter(year >= 1950) %>% 
  filter(identifier != "Area")

# colnames(new_Data)

new_Data <- new_Data %>%
  relocate(year, .before = iso3) %>% 
  relocate(region, .after = iso3) %>% 
  relocate(subarea, .after = region) %>% 
  relocate(region_wb, .after = subarea) %>% 
  relocate(incomegroup, .after = region_wb) %>% 
  relocate(population, .after = incomegroup) %>% 
  relocate(gdp, .after = population) %>% 
  relocate(identifier, .before = country)

tema <- theme(plot.title = element_text(size = 18, color = "#1B2128", face = 'bold'),
              plot.subtitle = element_text(size = 12, color = "#747577", vjust = 1.5),
              axis.line = element_blank(),
              panel.border = element_blank(),
              panel.background = element_blank(),
              plot.background = element_rect(color = "white", fill = "white"),
              legend.position = "none",
              plot.margin = margin(1, 1, 1, 1, "cm"),
              axis.title = element_blank(),
              axis.text.x = element_text(angle = 90, vjust=0.5, hjust=1, size = 8),
              axis.text.y = element_text(size = 8),
              axis.ticks = element_blank(),
              aspect.ratio = 1)

#--------------------

# Estadísticas descriptivas y matriz de correlación

# summary(new_Data)

# colnames(new_Data)

# Estadísticas descriptivas y matriz de correlación

stargazer(new_Data[c("population","gdp","gini", "bottom5", "bottom20", "bottom40", "top5", "top10", "top20", "middle50",
                 "abr", "gdi", "gii", "hdi", "le", "mmr","pr_f", "pr_m")],
          title="Estadísticas Descriptivas (Serie 1950 - 2022)",
          digits = 1,
          flip = F,
          omit.summary.stat = c("p25", "p75"))

#--------------------

# Visualizaciones

new_Data <- new_Data %>% 
  select(c(identifier, country, year,
           population, gdp, gini, bottom5, bottom20, bottom40, top5, top10, top20, middle50,
           abr, gdi, gii, hdi, le, mmr, pr_f, pr_m))
  
new_Data <- pivot_longer(data = new_Data,
               cols = population:pr_m,
               names_to = "variable",
               values_to = "data")

# Distribución del ingreso

new_Data %>%
  filter(variable %in% c("bottom5", "bottom20", "bottom40", "top5",
                         "top10", "top20", "middle50")) %>%
  mutate(variable = factor(variable, levels = c("bottom5", "bottom20", "bottom40", "middle50",
                                                "top20", "top10", "top5")) %>% 
           fct_recode("Bottom 5" = "bottom5", "Bottom 20" = "bottom20", "Bottom 40" = "bottom40",
                      "Middle 50" = "middle50", "Top 20" = "top20", "Top 10" = "top10",
                      "Top 5" = "top5")) %>% 
  ggplot(aes(variable, data)) + 
  geom_jitter(fill = "lightgrey", color = "lightgrey", alpha = 0.025) +
  geom_boxplot(fill = "royalblue", color = "royalblue4", alpha = 0.5) + 
  theme_minimal() +
  labs(y = "Porcentaje del ingreso nacional",
       x = "")

# Distribución de algunas variables de interés

new_Data %>%
  filter(variable %in% c("gdi", "gii", "hdi")) %>% 
  ggplot(aes(data, color = variable)) + 
  geom_density(size = .8) + 
  theme_minimal() +
  labs(title = "Distribución de Índices de desarrollo",
       y = "Densidad",
       x = "") +
  theme(legend.position = "top") +
  scale_x_continuous(breaks = seq(0, 1, by = .1)) +
  scale_color_manual(values = c("gdi" = "#FC8D62", "gii" = "#66C2A5", "hdi" = "#8DA0CB"), 
                     name = "", labels = c("Índice de Desarrollo de Género",
                                           "Índice de Desigualdad de Género",
                                           "Índice de Desarrollo Humano"
                      )
            )

# Porcentaje de hombres y mujeres en el Parlamento

new_Data %>%
  filter(year >= 1990) %>% 
  filter(variable %in% c("pr_f", "pr_m")) %>%
  group_by(variable, year) %>%
  summarise(data = mean(data, na.rm = T)) %>%
  ggplot(aes(x = year, y = data, group = variable, color = variable)) + 
  geom_line(size = 1, size = 2) +
  geom_jitter(data = new_Data %>% 
               filter(variable %in% c("pr_f", "pr_m")) %>% 
               filter(year >= 1990),
             aes(x = year, y = data), alpha = 0.15) +
  theme_minimal() +
  labs(title = "Porcentaje de asientos ocupados en el parlamento por hombres y mujeres",
       subtitle = "Serie 1990 - 2022 | Promedio a nivel global",
       y = "Porcentaje de asientos ocupados",
       x = "") +
  # scale_color_brewer(palette = "Set2", name = "") +
  scale_x_continuous(breaks = seq(1990, 2022, by = 2)) +
  theme(legend.position = "top") +
  scale_color_manual(values = c("pr_f" = "#FC8D62", "pr_m" = "#66C2A5"), 
                     name = "", labels = c("Porcentaje de Mujeres", "Porcentaje de Hombres"))

#-------------------

# Otras visualizaciones

# Coeficiente de Gini a nivel global

wiidglobal %>%
  filter(subarea %in% c("World (overall)",
                        "World (between-countries)",
                        "World (within-countries)")) %>% 
  ggplot(aes(year, gini,
             group = subarea, colour = subarea)) + 
  geom_line(size = .8) +
  theme_minimal() +
  labs(title = "Coeficiente de Gini a nivel global",
       subtitle = "Serie 1950 - 2022",
       y = "Coeficiente de Gini",
       X = "Año") +
  scale_color_brewer(palette = "Set2", name = "") +
  theme(legend.position = "top") +
  scale_x_continuous(breaks = seq(min(wiidglobal$year), max(wiidglobal$year), by = 5))

# Coeficiente de Gini por región

wiidglobal %>%
  filter(subarea %in% c("South Asia",
                        "East Asia and the Pacific",
                        "Latin America and the Caribbean",
                        "Middle East and North Africa",
                        "Sub-Saharan Africa",
                        "Europe and Central Asia",
                        "North America")) %>% 
  ggplot(aes(year, gini,
             group = subarea, colour = subarea)) + 
  geom_line(size = .8) +
  theme_minimal() +
  labs(title = "Coeficiente de Gini por región",
       subtitle = "Serie 1950 - 2022",
       y = "Coeficiente de Gini",
       x = "Año") +
  scale_color_brewer(palette = "Set2", name = "") +
  theme(legend.position = "top") +
  scale_x_continuous(breaks = seq(min(wiidglobal$year), max(wiidglobal$year), by = 5))

# Coeficiente de Gini en México

wiidglobal %>%
  filter(!is.na(iso3)) %>% 
  filter(country %in% c("Mexico")) %>% 
  ggplot(aes(year, gini,
             group = iso3, colour = iso3)) + 
  geom_line(size = .8) +
  theme_minimal() +
  theme(legend.position = "top") +
  labs(title = "Coeficiente de Gini a nivel país: México",
       subtitle = "Serie 1950 - 2022",
       y = "Coeficiente de Gini",
       X = "Año") +
  scale_color_brewer(palette = "Set2", name = "")

#-------------------

# Matriz de correlación

new_Data <- left_join(wiidglobal, grd, by = c("identifier", "iso3", 
                                              "country", "year"))

new_Data <- left_join(new_Data, hdi, by = c("identifier", "iso3", 
                                            "country", "year"))

new_Data <- new_Data  %>% 
  filter(year >= 1950) %>% 
  filter(identifier != "Area")

new_Data <- new_Data %>% 
  select(c(population, gdp, gini, bottom5, bottom20, bottom40, top5, top10, top20, middle50,
           abr, gdi, gii, hdi, le, mmr, pr_f, pr_m))

corr <- cor(new_Data, method = "pearson", use = "pairwise.complete.obs") # Run a correlation

my_palette <- brewer.pal(n = 3, name = "PRGn")

ggcorrplot(corr,
           type = "lower",
           outline.col = "white",
           colors = my_palette,
           lab = TRUE,
           lab_col = "black",
           lab_size = 2.5,
           digits = 2) +
  theme_minimal() +
  labs(title = "Matriz de correlación",
       y = "",
       x = "") +
  tema