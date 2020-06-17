# Se utiliza Encuesta Nacional del Medio Ambiente 2017 - 2018 
# Disponible en: https://mma.gob.cl/encuestas-nacionales-del-medio-ambiente/

# Paquetes ----------------------------------------------------------------

library(readxl)
library(janitor)
library(sjlabelled)
library(haven)
library(tidyverse)
library(desuctools)
library(ggplot2)
library(scales)

# BBDD ----------------------------------------------------------------

base <- readxl::read_xlsx('base.xlsx') %>%
  clean_names() %>%
  select(id, p3c, region, pond)

base$total=1

# Recodificaciones  ----------------------------------------------------------------

base <- base %>% 
  mutate(p3c_r = case_when(p3c <= 3 ~ 1,
                           p3c == 4 ~ 2,
                           p3c == 5 ~ 2,
                           TRUE ~ NA_real_),
         p3c_r = haven::labelled(p3c_r,
                                labels = c('Mala' = 1,
                                           'Buena' = 2),
                                label = '¿Cómo evalúa usted el estado de los ríos y lagos en su región?'))

base <- base %>%
  mutate_at(vars(region),
            ~ labelled(., labels = c('I De Tarapacá' = 1,
'II De Antofagasta' = 2,
'III De Atacama' = 3,
'IV De Coquimbo' = 4,
'V De Valparaíso' = 5, 
'VI De Ohiggins' = 6,
'VII Del Maule' = 7,
'VIII Del Biobío' = 8,
'IX De La Araucanía' = 9,
'X De Los Lagos' = 10,
'XI De Aysén' = 11, 
'XII De Magallanes' = 12,
'RM Metropolitana' = 13,
'XIV De Los Ríos' = 14,
'XV De Arica y Par.' = 15,
'XVI Del Ñuble' = 16)) %>% 
              as_factor(.))

# Datos a graficar -------------------------------------------------------

tab <- base %>% 
  tabla_vars_segmentos(.vars = vars(p3c_r),
                       .segmentos = vars(region),
                       miss = NA,
                       .wt = pond) %>%
  filter(!is.na(pregunta_cat))

# Pivotear base
tab_pivot <- tab %>%
  pivot_wider(id_cols = c(segmento_cat:pregunta_lab), names_from = pregunta_cat, values_from = prop)

# Gráfico -------------------------------------------------------

gg_sequia <-   ggplot(data = tab_pivot) +
  geom_segment(aes(x=Buena, xend=Mala, y=pregunta_lab, yend=pregunta_lab), color = 'grey50', size = 1.2) +
  geom_point(aes(x=Mala, y=pregunta_lab, color = 'Regular/\nMala/\nPésima'), size = 4) +
  geom_point(aes(x=Buena, y=pregunta_lab, color = 'Buena/\nExcelente\n'), size = 4) + 
  scale_x_continuous('', limits = c(0,1), labels = function(x) scales::percent(x, accuracy = 1)) +
  scale_y_discrete(labels = wrap_format(30)) +
  scale_shape_manual(values=c(19,20)) +
  scale_colour_manual(values=c("darkgreen", "grey20")) +
  theme_minimal() +
  facet_grid(rows = 'segmento_cat',
             switch =  'both') +
  theme(axis.text.y = element_blank(),
        legend.title = element_blank(),
        plot.title = element_text(size = rel(1), face='bold'),
        plot.subtitle = element_text(size = rel(0.9)),
        strip.text.y.left = element_text(angle = 0)) +
  labs(title = "Día mundial contra la desertificación y la sequía ",
       subtitle = "¿Cómo evalúa usted el estado de los ríos y lagos en su región?",
       caption = "Datos ponderados. n = 7.601. Se omite la categoría “Ns-Nr”. \nEncuesta Nacional de Medio Ambiente 2018",
       y ='')

ggsave('outputs/200617_dia_mundial_sequia.png',
       width = 5,
       height = 5,
       scale = 3,
       units = 'cm')
