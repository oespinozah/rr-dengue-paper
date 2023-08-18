rm(list = ls())

# Cargamos los paquetes ---------------------------------------------------

library(sf)
library(tidyverse)
library(rio)
library(spdep)
library(RColorBrewer)
library(lattice)
library(INLA)
library(tigris)
library(ggplot2)
library(dplyr)
library(SpatialEpi)

# Cargamos las funciones

source("src/prep_nac.R")
source("src/m_clima.R")

# Leemos y pre-procesamos los datos ---------------------------------------

fecha_estudio <- Sys.Date()
# fecha_estudio <- as.Date("2023-02-10")

data_dengue <- readRDS("data/raw/dengue_total.rds")
data_dengue <- data_dengue[!(data_dengue$ano == lubridate::epiyear(fecha_estudio) &
                               data_dengue$semana > lubridate::epiweek(fecha_estudio) - # TENER EN CUENTA >= -> >
                               1 ), ]
data_dengue <- data_dengue %>% filter(ano >= 2023)
shape_distritos <- st_read("data/geo/CDC_Distritos.shp")
poblacion <- import("data/raw/poblacion_distrito_2011_2023.xlsx")
clima <- readRDS("data/raw/clima_dis_sem_final.rds")
clima <- clima %>% select(ano, semana, ubigeo, tmean, hrel, prcp) %>% filter(ano >= 2022)
dist_list <- data_dengue %>% select(ubigeo) %>% unique() %>% pull()
shape_distritos <- shape_distritos %>%
  filter(ubigeo %in% c(dist_list) )
poblacion <- poblacion %>% filter(ubigeo %in% dist_list)
shape_distritos <- shape_distritos %>%
  mutate(
    departamen = ifelse(provincia == "CALLAO" | provincia == "LIMA", 
                        "LIMA METROPOLITANA Y CALLAO", departamen),
    departamen = ifelse(departamen == "LIMA" & provincia != "LIMA", 
                        "LIMA PROVINCIA", departamen))
data_dengue <- data_dengue %>%
  mutate(
    departamento = ifelse(provincia == "CALLAO" | provincia == "LIMA", 
                          "LIMA METROPOLITANA Y CALLAO", departamento),
    departamento = ifelse(departamento == "LIMA" & provincia != "LIMA", 
                          "LIMA PROVINCIA", departamento))
data_dengue <- data_dengue %>% count(ano, semana, departamento, ubigeo, name = "cases")
i <- 1
matriz_control <- matrix(NA, ncol = 2, nrow = length(unique(shape_distritos$departamen)))
for (depart_modelo in sort(unique(shape_distritos$departamen)) ) { #[10]
  matriz_control[i,1] <- depart_modelo
  matriz_control[i,2] <- (data_dengue %>%
                            filter(departamento %in% depart_modelo) %>%
                            select(ano, semana))[1,1]
  i <- i + 1
}
matriz_control <- data.frame(departamen = matriz_control[,1], ano = matriz_control[,2])
data_dengue <- data_dengue %>% select(-departamento) %>%
  pivot_wider(values_from = cases, names_from = ubigeo, values_fill = 0) %>%
  pivot_longer(names_to = "ubigeo", values_to = "cases", cols = -c(ano, semana)) %>%
  left_join( (data_dengue %>% select(ubigeo, departamento) %>% unique()) )
# data_dengue <- data_dengue %>% filter(departamento %in% (data_dengue %>% group_by(departamento, ano) %>%
#                                               summarise(n = sum(cases)) %>% ungroup() %>%
#                                               filter(ano==lubridate::epiyear(fecha_estudio)) %>%
#                                               filter(n>=10) %>% select(departamento) %>% pull()))
# shape_distritos <- shape_distritos %>% filter(departamen %in% (data_dengue %>% group_by(departamento, ano) %>%
#                                                              summarise(n = sum(cases)) %>% ungroup() %>%
#                                                              filter(ano==lubridate::epiyear(fecha_estudio)) %>%
#                                                              filter(n>=10) %>% select(departamento) %>% pull()))

# Definimos la función para ajustar el modelo -----------------------------

# ano_inicio <- matriz_control %>% select(ano) %>% pull()
data_depart <- prep_nac(data_dengue, poblacion, shape_distritos)
ubigeo_lista <- data_depart$ubigeo %>% unique %>% sort
pred <- rbind(cbind(lubridate::epiyear(fecha_estudio),
                    max(clima$semana[clima$ano == lubridate::epiyear(fecha_estudio)]) + 1,
                    unique(clima$ubigeo), NA, NA, NA),
              cbind(lubridate::epiyear(fecha_estudio),
                    max(clima$semana[clima$ano == lubridate::epiyear(fecha_estudio)]) + 2,
                    unique(clima$ubigeo), NA, NA, NA) )
pred <- as.data.frame(pred)
names(pred) <- c("ano","semana","ubigeo","tmean", "hrel", "prcp")
pred$ano <- pred$ano %>% as.numeric()
pred$semana <- pred$semana %>% as.numeric()
pred$tmean <- pred$tmean %>% as.numeric()
pred$hrel <- pred$hrel %>% as.numeric()
pred$prcp <- pred$prcp %>% as.numeric()
clima <- rbind(clima,pred)
data_depart <- data_depart %>% right_join(clima,by = c("ano", "semana", "ubigeo")) %>%
  filter(ubigeo %in% ubigeo_lista) %>% arrange(ubigeo, ano, semana)
temporal <- data_depart %>% filter(ubigeo %in% ubigeo_lista[1])
temporal_retraso <- m_clima(base = temporal,temperatura = "tmean",precipitacion = "prcp",humedad = "hrel",n = 10)
for (u in ubigeo_lista[-1]) {
  temporal <- data_depart %>% filter(ubigeo %in% u)
  temporal_retraso_1 <- m_clima(base = temporal,temperatura = "tmean",precipitacion = "prcp",humedad = "hrel",n = 10)
  temporal_retraso <- rbind(temporal_retraso, temporal_retraso_1)
}
data_depart <- cbind(data_depart, temporal_retraso)
data_depart <- data_depart %>% filter(ano >= 2023)
data_depart <- data_depart[!is.na(data_depart$pobtot),]
data_depart$cases <- data_depart$cases %>% as.numeric()
shape_distritos$struct <- 1:length(shape_distritos$ubigeo)
data_depart <- data_depart %>% left_join(shape_distritos, by = "ubigeo")
data_depart$anosemana <- as.numeric(
  ifelse(data_depart$semana < 10,paste(data_depart$ano, data_depart$semana, sep = "0"),
         paste(data_depart$ano, data_depart$semana, sep = "") ) )
data_depart$ESP <- NA
data_depart$ESP[!is.na(data_depart$cases)] <-
  expected(population = data_depart$pobtot[!is.na(data_depart$cases)],
           cases = data_depart$cases[!is.na(data_depart$cases)],n.strata = 1)
data_depart$ESP <- ceiling(data_depart$ESP)
nb <- poly2nb(shape_distritos)
nb2INLA("data/processed/map.adj", nb)
g <- inla.read.graph(filename = "data/processed/map.adj")
f3 <- cases ~
  scale(temp_3) + scale(temp_5) + scale(temp_7) + scale(temp_9) + scale(temp_10) +
  scale(prcp_6) + scale(prcp_7) + scale(prcp_10) +
  scale(hrel_3) + scale(hrel_6) + scale(hrel_7) + 
  f(struct, model = "bym", constr = T, scale.model = T, graph = g) +
  f(anosemana, model="iid")
res <- inla(formula = f3, data = data_depart,family = "poisson", E = ESP,
            control.compute = list(waic = T),control.predictor = list(link = 1))
# summary(res)
data_depart$RR <- round(res$summary.fitted.values$mean,2)
data_depart$LI <- round(res$summary.fitted.values$`0.025quant`,2)
data_depart$LS <- round(res$summary.fitted.values$`0.975quant`,2)
data_depart <- data_depart %>% rename(casos = cases)
data_depart$SMR <- round(data_depart$casos/data_depart$ESP,2)
data_depart$incidencia <- data_depart$casos/data_depart$pobtot
data_depart %>% select(ano, semana, ubigeo, casos, incidencia, SMR, RR, LI, LS, ) %>% saveRDS("data/processed/dengue.rds")



map <- st_read("data/geo/CDC_Distritos.shp") %>% 
  left_join(
    readRDS("data/processed/dengue.rds") %>% mutate(RR = if_else(RR >= 5, 5, RR))
  ) 

map %>% filter(!is.na(semana)) %>% 
  filter(semana >= 11) %>% 
  ggplot(aes(geometry = geometry)) + 
  geom_sf(aes(fill = RR), size = 0.1) +
  facet_wrap( ~ semana, nrow = 2) +
  theme_void() +
  scale_fill_gradient2(low = '#0571b0', high = '#ca0020', mid = '#ffffbf',
                       midpoint = 1, na.value = "grey", guide = "colourbar") +
  geom_sf(
    data = st_read("data/geo/CDC_Departamentos.shp"),
    fill = "transparent",
    size = 0.5
  )



