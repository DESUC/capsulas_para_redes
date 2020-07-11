# World Population Day
# 
# Evolución poblacional según proyecciones poblacionales en base 2017.

library(janitor)
library(tidyverse)

f_num <- function(x){
  scales::number(x, scale = .001, 
                 big.mark = '.', decimal.mark = ',')
}

# Lectura de base de datos
df_ano_sexo_edad_summary <- readRDS('inputs/ine_proyeccion_sexo_edad.rds')

# Selección de años a graficar
df_ano_filter <- df_ano_sexo_edad_summary %>% 
  filter(ano %in% seq(2002, 2035, by = 6))
  
# Cálculo de población total
df_ano_filter_tot <- df_ano_filter %>% 
  count(ano, wt = poblacion, name = 'pob_total')

# Cálculo de mediana por sexo y año.
df_ano_filter_median_sexo <- df_ano_filter %>% 
  group_by(ano, sexo) %>% 
  summarise(median = matrixStats::weightedMedian(edad, w = poblacion))

# Gráfico
df_ano_filter %>% 
  ggplot(aes(x = edad, 
             y = poblacion,
             fill = sexo)) +
  geom_area(alpha = .5, 
            position = 'identity') +
  geom_text(data = df_ano_filter_tot,
            aes(label = str_glue("{f_num(pob_total)} miles"), 
                fill = NULL),
            x = 2, y = 50000,
            size = rel(1.5),
            hjust = 0, vjust = 0.5,
            colour = 'white') +
  geom_vline(data = df_ano_filter_median_sexo, 
             aes(xintercept = median,
                 colour = sexo),
             show.legend = FALSE) + 
  geom_text(data = df_ano_filter_median_sexo, 
            aes(x = median,
                label = scales::number(median, accuracy = .1),
                hjust = if_else(sexo == 'hombre', 1.25, -0.25),
                colour = sexo),
            y = 50000,
            size = rel(1), vjust = 0.5,
            show.legend = FALSE) + 
scale_fill_brewer('Sexo', palette = 'Set1', aesthetics = c('fill', 'colour')) +
  scale_y_continuous('Personas (miles)', labels = f_num) +
  scale_x_continuous('Edad (años)', expand = c(0, 1),
                     breaks = c(0, 10, 18, 25, 40, 60, 65, 80)) + 
  facet_grid(rows = vars(fct_rev(ano))) +
  labs(title = 'Evolución poblacional en Chile según sexo y edad',
       subtitle = 'Para cada año se marca mediana de edad por sexo y población total',
       caption = 'Fuente: Proyecciones poblacionales INE, base 2017') +
  theme_minimal(base_size = 6) +
  theme(legend.position = 'top',
        plot.title.position = 'plot',
        legend.key.size = unit(.03, 'snpc'),
        axis.text.y = element_text(size = rel(.75)))

# Grabar gráfico
ggsave('outputs/200711-dia_poblacion_mundial.png',
       width = 8,
       height = 8,
       dpi = 'retina',
       units = 'cm')
 