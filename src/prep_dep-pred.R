library(tidyverse)

# base <- data_dengue
# depart <- "LORETO"
# base_pob <- poblacion
# shape <- shape_distritos

prep_dep_pred <- function(base, depart, base_pob, shape, ano_inicio){
    data_depart <- base %>% filter(departamento %in% depart, ano >= ano_inicio) %>%
        select(-departamento)
    dist <- shape$ubigeo
    temporal <- data_depart %>% select(ano, semana) %>% unique() %>% cbind(dist[1])
    names(temporal)[3] <- c("ubigeo")
    temporal <- temporal %>%
        left_join(data_depart,by = c("ano","semana","ubigeo")) %>%
        mutate(cases = ifelse(is.na(cases), 0, cases))
    for (i in 2:length(dist)) {
        temporal_1 <- data_depart %>% select(ano, semana) %>% unique() %>% cbind(dist[i])
        names(temporal_1)[3] <- c("ubigeo")
        temporal_1 <- temporal_1 %>%
            left_join(data_depart,by = c("ano","semana","ubigeo")) %>%
            mutate(cases = ifelse(is.na(cases), 0, cases))
        temporal <- rbind(temporal, temporal_1)
    }
    data_depart <- temporal
    data_dist <- data_depart %>%
        filter(ubigeo == dist[1])
    anos <- data_dist %>%
        filter(semana==53) %>%
        select(ano) %>%  t() %>% t()

    for (i in anos) {
        if ( data_dist %>% filter(ano==i, semana==52) %>% select(cases) %>% nrow() != 0 ) {
            a <- data_dist %>% filter(ano==i, semana==52) %>% select(cases)
        } else {
            a <- 0
        }
        b <- data_dist %>% filter(ano==i, semana==53) %>% select(cases)
        if ( data_dist %>% filter(ano==i, semana==52) %>% select(cases) %>% nrow() != 0 ) {
            c <- data_dist %>% filter(ano==i+1, semana==1) %>% select(cases)
        } else {
            c <- 0
        }
        a <- a + b/2
        c <- c + b/2
        data_dist %>% filter(ano==i, semana==52) %>% mutate(cases=a)
        data_dist %>% filter(ano==i+1, semana==1) %>% mutate(cases=c)
    }
    temporal <- data_dist %>% filter(semana!=53)
    for (j in 2:length(dist)) {
        data_dist <- data_depart %>%
            filter(ubigeo == dist[j])
        anos <- data_dist %>%
            filter(semana==53) %>%
            select(ano) %>%  t() %>% t()
        for (i in anos) {
            if ( data_dist %>% filter(ano==i, semana==52) %>% select(cases) %>% nrow() != 0 ) {
                a <- data_dist %>% filter(ano==i, semana==52) %>% select(cases)
            } else {
                a <- 0
            }
            b <- data_dist %>% filter(ano==i, semana==53) %>% select(cases)
            if ( data_dist %>% filter(ano==i, semana==52) %>% select(cases) %>% nrow() != 0 ) {
                c <- data_dist %>% filter(ano==i+1, semana==1) %>% select(cases)
            } else {
                c <- 0
            }
            a <- a + b/2
            c <- c + b/2
            data_dist %>% filter(ano==i, semana==52) %>% mutate(cases=a)
            data_dist %>% filter(ano==i+1, semana==1) %>% mutate(cases=c)
        }
        data_dist <- data_dist %>% filter(semana!=53)
        temporal <- rbind(temporal, data_dist)
    }
    base_pob <- base_pob %>% select(ano,ubigeo,pobtot)
    base_pob$ano <- base_pob$ano %>% as.numeric()

    pred <- rbind(cbind(lubridate::epiyear(Sys.Date()),
                        max(temporal$semana[temporal$ano == lubridate::epiyear(Sys.Date())]) + 1,
                        unique(temporal$ubigeo), NA),
                  cbind(lubridate::epiyear(Sys.Date()),
                        max(temporal$semana[temporal$ano == lubridate::epiyear(Sys.Date())]) + 2,
                        unique(temporal$ubigeo), NA) )
    pred <- as.data.frame(pred)
    names(pred) <- c("ano","semana","ubigeo","cases")

    temporal <- rbind(temporal,pred)

    temporal$ano <- temporal$ano %>% as.numeric()
    temporal$semana <- temporal$semana %>% as.numeric()
    temporal <- temporal %>% left_join(base_pob, by = c("ano","ubigeo"))
    return(temporal)
}
