#Encuesta Nacional de Auditoría a la Democracia 2018.

# Paquetes ----------------------------------------------------------------

library(haven)
library(janitor)
library(tidyverse)
library(desuctools)
library(dplyr)
library(sjmisc)
library(scales)

# Bases de datos ----------------------------------------------------------

base <- readRDS('inputs/enad_2018_2.rds')

base$total=1

# Recodificaciones ----------------------------------------------------------

base <- base %>% 
  mutate(p41a_r = case_when(p41a <= 2 ~ 1,
                            p41a == 3 ~ 2,
                            p41a == 4 ~ 2,
                            p41a >= 8 ~ 9,
                            TRUE ~ NA_real_),
         p41a_r = labelled(p41a_r,
                           labels = c('Muy bien + Bien' = 1,
                                      'Muy mal + Mal' = 2,
                                      'NS/NR' = 9),
                           label = 'Fiscalizar los actos de gobierno'))

base <- base %>% 
  mutate(p41b_r = case_when(p41b <= 2 ~ 1,
                            p41b == 3 ~ 2,
                            p41b == 4 ~ 2,
                            p41b >= 8 ~ 9,
                            TRUE ~ NA_real_),
         p41b_r = labelled(p41b_r,
                           labels = c('Muy bien + Bien' = 1,
                                      'Muy mal + Mal' = 2,
                                      'NS/NR' = 9),
                           label = 'Representar los intereses de las personas'))

base <- base %>% 
  mutate(p41c_r = case_when(p41c <= 2 ~ 1,
                            p41c == 3 ~ 2,
                            p41c == 4 ~ 2,
                            p41c >= 8 ~ 9,
                            TRUE ~ NA_real_),
         p41c_r = labelled(p41c_r,
                           labels = c('Muy bien + Bien' = 1,
                                      'Muy mal + Mal' = 2,
                                      'NS/NR' = 9),
                           label = 'Elaborar y aprobar leyes'))

# Insumos gráfico ----------------------------------------------------------

colores_graficos <- RColorBrewer::brewer.pal(3, 'Blues')

gg_var_y <- function(.data, 
                       var_y,
                       fill) {
  .data %>% 
    ggplot(aes(y = {{ var_y }}, x = .data$prop, fill = {{ fill }})) +
    geom_col(width = .5) +
    geom_text(aes(label = round(.data$prop * 100, 0)),
              nudge_x = 0.03,
              nudge_y = -0.08,
              hjust = 0,
              size = rel(4)) + 
    scale_x_continuous(limits = c(0, 1), 
                       labels = scales::percent) +
    scale_y_discrete(labels = wrap_format(14)) +
    scale_fill_manual(values = colores_graficos, guide = 'none') +
    coord_flip() +
    theme_minimal(base_family = 'Calibri') +
    theme(plot.title.position = 'plot',
          plot.title = element_text(size = rel(1), face = 'bold', hjust = 0),
          plot.subtitle = element_text(size = rel(0.9), hjust = 0),
          plot.caption = element_text(size = rel(0.7)),
          axis.title.x = element_blank(), 
          axis.text.x  = element_text(size = rel(1.1)))
}

# Gráficos ----------------------------------------------------------

tab1 <- desuctools::tabla_vars_segmentos(base,
                                       .vars = vars(p40),
                                       .segmentos = vars(total),
                                       .wt = pond)

gg_congreso1 <- tab1 %>%
  filter(pregunta_cat != 'No sabe',
         pregunta_cat != 'No responde') %>%
  gg_var_y(var_y = pregunta_cat, fill = pregunta_cat) +
  labs(title = '1. ¿Cuál de las siguientes cree Ud. que es 
la tarea más importante que tiene el Congreso?',
       subtitle = ' ',
       x = 'Porcentaje de respuesta',
       caption = ' ')

gg_congreso1

tab2 <- desuctools::tabla_vars_segmentos(base,
                                        .vars = vars(p41a_r, p41b_r, p41c_r),
                                        .segmentos = vars(total),
                                        .wt = pond)

gg_congreso2 <- tab2 %>%
  filter(pregunta_cat == 'Muy bien + Bien') %>%
  gg_var_y(var_y = pregunta_lab, fill = pregunta_lab) +
  labs(title = '2. ¿Y qué tan bien o qué tan mal cree Ud. que el 
Congreso chileno cumple con cada una de ellas? ',
       subtitle = '% Muy bien + Bien',
       x = ' ',
       caption = 'Encuesta Nacional de Auditoría a la Democracia 2018, PNUD - DESUC')

gg_congreso2

gg_congreso <- ggpubr::ggarrange(gg_congreso1, gg_congreso2)

gg_congreso

ggsave('outputs/200704_dia_congreso.png',
       width = 5,
       height = 5,
       scale = 3,
       units = 'cm')
