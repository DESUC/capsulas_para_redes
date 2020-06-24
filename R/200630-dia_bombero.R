#Encuesta Nacional de Auditoría a la Democracia 2018.

# Paquetes ----------------------------------------------------------------

library(haven)
library(janitor)
library(tidyverse)
library(desuctools)
library(dplyr)
library(sjmisc)

# Bases de datos ----------------------------------------------------------

enad_2018 <- readRDS('inputs/enad_2018.rds')

# Insumos gráfico ----------------------------------------------------------

gg <- desuctools::tabla_vars_segmentos(.data = enad_2018,
                                       .vars = vars(p60A_rec:p60S_rec),
                                       .segmentos = vars(TOTAL),
                                       .wt = pond) %>% 
  filter(pregunta_cat == 'Mucha + bastante') %>% 
  mutate(cat_interes = case_when(pregunta_lab == 'Bomberos' ~ "Sí",
                                  TRUE ~ "No"),
         prop = round(prop*100,0)) %>% 
  mutate(pregunta_lab = fct_reorder(pregunta_lab, .x = prop, .fun = mean, .desc = TRUE))

# Gráfico ----------------------------------------------------------

ggplot(gg, aes(x=pregunta_lab, y=prop)) +
  geom_segment(aes(x=pregunta_lab, xend=pregunta_lab, y=0, yend=prop, color = cat_interes), size = 3) +
  geom_point(aes(fill = cat_interes, color = (cat_interes)), size=7, shape=21, stroke=2) +
  geom_text(aes(label = prop),
            hjust = 0.5, nudge_y = 7, size = 4.5) +
  scale_fill_manual(values = c("darkblue","red3")) +
  scale_color_manual(values = c("darkblue","red3")) +
  ylim(0, 100) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.position = "none",
        panel.background = element_rect(fill = "white"),
        panel.grid.major.x = element_line(color = "grey"),
        panel.grid.major.y = element_blank()) +
  coord_flip() + 
  labs(title = "% Bastante + mucha confianza en instituciones",
       caption = "Encuesta Nacional de Auditoría a la Democracia 2018, PNUD - DESUC")

ggsave('outputs/dia_bomberos.png',
       width = 5,
       height = 5,
       scale = 3,
       units = 'cm')
