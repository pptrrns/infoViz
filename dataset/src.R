#--------------------

# Cargar librerías
if(!require(pacman)) install.packages("pacman")
pacman::p_load(janitor, haven, labelled, readxl, ggplot2, dplyr, stringr, tidyr, stargazer) 

#--------------------

# Country data

# un_rg <- read.csv("Country Data/un-regional-groups.csv")

iso3 <- read.csv("Country Data/iso.csv")

iso3 <- iso3 %>% select(c(name, alpha.3)) %>% 
  rename(country_name = name,
         iso3 = alpha.3)

#--------------------

# Human Development Data
# https://hdr.undp.org/data-center/documentation-and-downloads
hdr <- read.csv("HDR/HDR21-22_Composite_indices_complete_time_series.csv") %>%
  pivot_longer(
               cols = hdi_1990:mf_2021,
               names_to = c("index"),
               values_to = "index_data") %>% 
  mutate(year = as.numeric(substr(index, nchar(index) - 3, nchar(index))),
         index = substr(index, 1, nchar(index) - 4),
         identifier = paste(iso3, year, sep = "")) %>% 
  select(-c(hdi_rank_2021, region))
hdr$index <- gsub("_$", "", hdr$index)

# Human Development Data -- Codebook
hdr_codebook <- read_xlsx("HDR/HDR21-22_Composite_indices_metadata.xlsx", 
                          col_names = TRUE) %>% 
  clean_names() %>% 
  drop_na() %>% 
  select(-time_series)

# Human Development Data
hdr <- hdr %>% 
  filter(!index %in% c("diff_hdi_phdi", "eys", "eys_f", "eys_m", "gni_pc_f", "gni_pc_m", "gnipc", 
                       "ineq_le", "lfpr_f", "lfpr_m", "se_f", "se_m",
                       "co2_prod", "country", "gdi_group", "hdi_rank", 
                       "hdi_rank", "iso3", "loss", "mf", "phdi", 
                       "rankdiff_hdi_phdi", "region", "gii_Rank"))

hdr_codebook <- hdr_codebook %>% 
  filter(!short_name %in% c("diff_hdi_phdi", "eys", "eys_f", "eys_m", "gni_pc_f", "gni_pc_m", "gnipc", 
                            "ineq_le", "lfpr_f", "lfpr_m", "se_f", "se_m",
                            "co2_prod", "country", "gdi_group", "hdi_rank", 
                            "hdi_rank", "iso3", "loss", "mf", "phdi", 
                            "rankdiff_hdi_phdi", "region", "gii_Rank")) %>% 
  arrange(short_name)

hdr <- hdr %>% 
  pivot_wider(names_from = index, values_from = index_data)

hdr_world <- hdr %>% 
  filter(str_detect(iso3, "^ZZ")) %>% 
  select(-hdicode)

hdr_world <- hdr_world %>% 
  select(order(colnames(hdr_world))) %>% 
  relocate(identifier, .before = abr) %>% 
  relocate(country, .after = identifier) %>%
  relocate(iso3, .after = country) %>%
  relocate(year, .after = iso3)

hdr <- full_join(iso3, hdr, by = "iso3") %>% 
  select(-country) %>% 
  rename(country = country_name) %>% 
  filter(!country == "") %>% 
  filter(!year == "")

hdr <- hdr %>% 
  select(order(colnames(hdr))) %>% 
  relocate(identifier, .before = abr) %>% 
  relocate(country, .after = identifier) %>%
  relocate(iso3, .after = country) %>%
  relocate(year, .after = iso3)

hdr[hdr == ""] <- NA

hdr <- as.data.frame(hdr)

# \usepackage{dcolumn}
# iqr = FALSE
# pct = FALSE
stargazer(hdr,
          type = "text", title = "Human Development Data (UNDP - HDI) | Descriptive statistics", 
          digits = 1, min.max = TRUE, mean.sd = FALSE,
          nobs = TRUE, median = FALSE, align = T)

#--------------------

# Government Revenue Dataset
# https://www.wider.unu.edu/database/data-and-resources-grd
grd <- read_dta("GRD/UNUWIDERGRD_2023.dta", encoding = "UTF-8") %>% 
  clean_names() %>% 
  select(c(identifier, country, iso, year, 
            tax_income, tax_indiv, tax_corp, tax_payr_workf, tax_property,
            tax_g_s, vat))  %>%
  rename(iso3 = iso)

colnames(grd)
grd <- remove_val_labels(grd)
grd <- remove_attributes(grd, "format.stata")

for (i in 1:ncol(grd)) {
  attr(grd[[i]], "label") <- NULL
}

grd <- full_join(iso3, grd, by = "iso3")

grd <- grd %>% 
  relocate(identifier, .before = country_name) %>%
  relocate(country_name, .after = iso3) %>% 
  mutate(year = as.numeric(substr(identifier, nchar(identifier) - 3, nchar(identifier)))) %>% 
  relocate(year, .after = country_name) %>% 
  select(-country) %>%
  rename(country = country_name) %>% 
  filter(!identifier == "",
         !country == "")

grd <- as.data.frame(grd)

# \usepackage{dcolumn}
# iqr = FALSE
# pct = FALSE
stargazer(grd,
          type = "text", title = "Government Revenue Dataset (UN UWIDER) | Descriptive statistics", 
          digits = 1, min.max = TRUE, mean.sd = FALSE,
          nobs = TRUE, median = FALSE, align = T)

#--------------------

# World Income Inequality Database -- Companion Country
# https://www.wider.unu.edu/database/world-income-inequality-database-wiid
wiidcountry <- read_dta("WIID/wiidcountry.dta") %>%
  clean_names()

wiidcountry <- remove_val_labels(wiidcountry)
wiidcountry <- remove_attributes(wiidcountry, "format.stata")

for (i in 1:ncol(wiidcountry)) {
  attr(wiidcountry[[i]], "label") <- NULL
}

wiidcountry <- wiidcountry %>% 
  mutate(id = paste(c3, year, sep = "")) %>% 
  rename(identifier = id,
         iso3 = c3) %>% 
  select(-c(c2, starts_with("ge"), starts_with("a"),
            starts_with("dy"), starts_with("y"), sharetype, conversion, palma, s80s20, cv,
            former, histent, sd)) %>% 
  mutate(region_wb = case_when(
    region_wb == 1 ~ "North America",
    region_wb == 2 ~ "Latin America and the Caribbean",
    region_wb == 3 ~ "Europe and Central Asia",
    region_wb == 4 ~ "Middle East and North Africa",
    region_wb == 5 ~ "Sub-Saharan Africa",
    region_wb == 6 ~ "South Asia",
    region_wb == 7 ~ "East Asia and the Pacific",
    region_wb == 30 ~ "Non EU Soviet",
    TRUE ~ NA
  ),
  region_un_sub = case_when(
    region_un_sub == 101 ~ "Northern America",
    region_un_sub == 102 ~ "Central America",
    region_un_sub == 103 ~ "Caribbean",
    region_un_sub == 104 ~ "South America",
    region_un_sub == 201 ~ "Northern Europe",
    region_un_sub == 202 ~ "Western Europe",
    region_un_sub == 203 ~ "Eastern Europe",
    region_un_sub == 204 ~ "Southern Europe",
    region_un_sub == 301 ~ "Northern Africa",
    region_un_sub == 302 ~ "Western Africa",
    region_un_sub == 303 ~ "Middle Africa",
    region_un_sub == 304 ~ "Eastern Africa",
    region_un_sub == 305 ~ "Southern Africa",
    region_un_sub == 401 ~ "Western Asia",
    region_un_sub == 402 ~ "Central Asia",
    region_un_sub == 403 ~ "Southern Asia",
    region_un_sub == 404 ~ "Eastern Asia",
    region_un_sub == 405 ~ "South-eastern Asia",
    region_un_sub == 501 ~ "Australia and New Zealand",
    region_un_sub == 502 ~ "Micronesia",
    region_un_sub == 503 ~ "Melanesia",
    region_un_sub == 504 ~ "Polynesia",
    TRUE ~ NA
  ),
  incomegroup = case_when(
    incomegroup == 0 ~ "All income groups (world)",
    incomegroup == 1 ~ "High income",
    incomegroup == 2 ~ "Upper middle income",
    incomegroup == 3 ~ "Lower middle income",
    incomegroup == 4 ~ "Low income",
    TRUE ~ NA
  ),
  region_un = case_when(
    region_un == 1 ~ "Americas",
    region_un == 2 ~ "Europe",
    region_un == 3 ~ "Africa",
    region_un == 4 ~ "Asia",
    region_un == 5 ~ "Oceania",
    TRUE ~ NA
  )
) %>% 
  mutate(year = as.numeric(substr(identifier, nchar(identifier) - 3, nchar(identifier))))

g7 <- read.csv("Country Data/g7.csv")

g20 <- read.csv("Country Data/g20.csv")

g77 <- read.csv("Country Data/g77.csv")

oecd <- read.csv("Country Data/oecd.csv")

brics <- read.csv("Country Data/brics.csv")

wiidcountry <- full_join(iso3, wiidcountry, by = "iso3") %>% 
  select(-c(country)) %>%
  rename(country = country_name) %>% 
  filter(!identifier == "") %>% 
  relocate(identifier, .before = country) %>%
  relocate(iso3, .after = identifier) %>% 
  relocate(country, .after = iso3) %>%
  relocate(year, .after = country) %>% 
  filter(!is.na(country))

wiidcountry <- wiidcountry %>%  
  mutate(g7 = ifelse(iso3 %in% g7$Code, 1, 0)) %>% 
  mutate(g20 = ifelse(iso3 %in% g20$Code, 1, 0)) %>% 
  mutate(oecd = ifelse(iso3 %in% oecd$Code, 1, 0)) %>% 
  mutate(brics = ifelse(iso3 %in% brics$Code, 1, 0)) %>% 
  relocate(g7, .after = eu) %>% 
  relocate(g20, .after = g7) %>% 
  relocate(oecd, .after = g20) %>% 
  relocate(brics, .after = oecd) 

# ineq <- full_join(wiidcountry, hdr, grd, by = c("identifier", "iso3", "country", "year")) %>% 
#  filter(!is.na(identifier)) %>% 
#  filter(!is.na(country)) %>% 
#  filter(!is.na(iso3)) %>% 
#  filter(!is.na(year))

rm(list= ls()[!(ls() %in% c("hdr_codebook", "hdr", "hdr_world", "grd", "wiidcountry"))])

write.csv(grd, "grd.csv", row.names = FALSE)
write.csv(hdr, "hdi.csv", row.names = FALSE)
write.csv(wiidcountry, "wiidcountry.csv", row.names = FALSE)

#--------------------

# World Income Inequality Database -- Companion Global
# https://www.wider.unu.edu/database/world-income-inequality-database-wiid
wiidglobal <- read_dta("WIID/wiidglobal.dta")

wiidglobal <- remove_val_labels(wiidglobal)
wiidglobal <- remove_attributes(wiidglobal, "format.stata")

for (i in 1:ncol(wiidglobal)) {
  attr(wiidglobal[[i]], "label") <- NULL
}

wiidglobal <- wiidglobal %>% mutate(c3 = na_if(c3, ""))

wiidglobal <- wiidglobal %>% 
  mutate(id = case_when(is.na(c3) ~ "Area",
    TRUE ~ paste(c3, year, sep = ""))) %>% 
  rename(identifier = id,
         iso3 = c3,
         fecha = year,
         region = area) %>% 
  select(-c(interpolated, 
            starts_with("ge"), starts_with("a"),
            starts_with("dy"), starts_with("y"),
            palma, s80s20, sd)) %>%
  rename(year = fecha) %>% 
  mutate(
  region_wb = case_when(
    region_wb == 1 ~ "North America",
    region_wb == 2 ~ "Latin America and the Caribbean",
    region_wb == 3 ~ "Europe and Central Asia",
    region_wb == 4 ~ "Middle East and North Africa",
    region_wb == 5 ~ "Sub-Saharan Africa",
    region_wb == 6 ~ "South Asia",
    region_wb == 7 ~ "East Asia and the Pacific",
    region_wb == 30 ~ "Non EU Soviet",
    TRUE ~ NA),
  incomegroup = case_when(
    incomegroup == 0 ~ "All income groups (world)",
    incomegroup == 1 ~ "High income",
    incomegroup == 2 ~ "Upper middle income",
    incomegroup == 3 ~ "Lower middle income",
    incomegroup == 4 ~ "Low income",
    TRUE ~ NA),
  region = case_when(
    region == 0 ~ "World",
    region == 1 ~ "Region",
    region == 2 ~ "Income group",
    region == 3 ~ "Country",
    TRUE ~ NA),
  subarea = case_when(
    subarea == 0 ~ "World (overall)",
    subarea == 1 ~ "World (between-countries)",
    subarea == 2 ~ "World (within-countries)",
    subarea == 3 ~ "World (population weighted sum)",
    subarea == 4 ~ "World (Shapley between-country share)",
    subarea == 11 ~ "North America",
    subarea == 12 ~ "Latin America and the Caribbean",
    subarea == 13 ~ "Europe and Central Asia",
    subarea == 14 ~ "Middle East and North Africa",
    subarea == 15 ~ "Sub-Saharan Africa",
    subarea == 16 ~ "South Asia",
    subarea == 17 ~ "East Asia and the Pacific",
    subarea == 21 ~ "High income",
    subarea == 22 ~ "Upper middle income",
    subarea == 23 ~ "Lower middle income",
    subarea == 24 ~ "Low income",
    subarea == 300 ~ "Country",
    TRUE ~ NA))

write.csv(wiidglobal, "wiidglobal.csv", row.names = FALSE)