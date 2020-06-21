#Encuesta Casen 2017.
#Encuesta Nacional de la Juventud 2018.

# Paquetes ----------------------------------------------------------------

library(haven)
library(janitor)
library(tidyverse)
library(desuctools)
library(dplyr)
library(sjmisc)
library(personograph)

# Bases de datos ----------------------------------------------------------
casen_2017 <- readRDS('inputs/casen_2017.rds')
injuv_2018 <- readRDS('inputs/injuv_2018.rds')


# Gráfico 1 ---------------------------------------------------------------
casen_2017 <- casen_2017 %>% 
  filter(edad > 17) %>% 
  filter(!(is.na(r23))) %>%
  filter(!(is.na(expr_div))) %>% 
  mutate(orientacion = case_when(r23 == 1 ~ 1,
                                 r23 %in% c(2:4) ~ 2,
                                 r23 %in% c(8:9) ~ 9),
         orientacion = haven::labelled(orientacion,
                                       labels = c('Heterosexual' = 1,
                                                  'Homo/bi sexual u otro' = 2,
                                                  'Sin dato' = 9),
                                       label = 'Orientación sexual'),
         ano = 2017)

gg <- desuctools::tabla_vars_segmentos(.data = casen_2017,
                                       .vars = vars(orientacion),
                                       .segmentos = vars(ano),
                                       .wt = expr_div)

casen2017 <- list(first=0.0198, second=0.9802)
personograph(casen2017, colors=list(first="deepskyblue", second="azure3"),
             fig.title = "Mayores de 18 años homo o bisexuales en Chile",
             fig.cap = 'DESUC, a partir de Casen 2017',
             draw.legend = FALSE,  dimensions=c(5,20))
png("outputs/orgullo_gay_casen2017.png")


# Gráfico 2 ---------------------------------------------------------------
injuv_2018 <- injuv_2018 %>% 
  mutate(orientacion = case_when(P118 == 1 ~ 1,
                                 P118 %in% c(2:5) ~ 2,
                                 P118 == 99 ~ 9),
         orientacion = haven::labelled(orientacion,
                                       labels = c('Heterosexual' = 1,
                                                  'Homo/bi sexual u otro' = 2,
                                                  'Sin dato' = 9),
                                       label = 'Orientación sexual'),
         ano = 2018)

gg <- desuctools::tabla_vars_segmentos(.data = injuv_2018,
                                       .vars = vars(orientacion),
                                       .segmentos = vars(ano),
                                       .wt = FACTOR)

injuv2018 <- list(first=0.072, second=0.928)
personograph(injuv2018, colors=list(first="deepskyblue", second="azure3"),
             fig.title = "Jóvenes entre 15 a 29 años homo o bisexuales en Chile",
             fig.cap = 'DESUC, a partir de ENJUV 2018',
             draw.legend = FALSE,  dimensions=c(5,20))
png("outputs/orgullo_gay_injuv2018.png")
