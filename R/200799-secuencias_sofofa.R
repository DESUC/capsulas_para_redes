
library(factoextra)
library(car)
library(TraMineR)
library(ggpubr)
library(WeightedCluster)
library(cluster)
library(tinytex)

# Base de datos ----------------------------------------------------

sofofa.seq <- readRDS ('inputs/sofofa_seq_2019.rds')

# Gráfico secuencias individuales  ----------------------------------------------------

seqIplot(sofofa.seq, sortv= "from.start", with.missing = T, main = "Estudio de Trayectorias de Egresados EMTP 2013 - 2016: 
         Análisis de secuencias sociales", yaxis=FALSE,
         with.legend = TRUE, border = NA, ylab= 'Casos (n = 838)', xlab = 'Año transcurridos desde egreso
                                                                                                                         DESUC - CEPPE, 2019')
# Gráfico secuencias clusterizadas ----------------------------------------------------

om_sofofa <- seqdist(sofofa.seq, method = "OM", indel = 1, sm = "CONSTANT")

clusterward <- agnes(om_sofofa, diss=TRUE, method="ward")

# Clusters
cluster_at <- cutree(clusterward, k=4)

clusters_sofofa <- data.frame(cluster = c(factor(cluster_at, labels = c("Estudiantes trabajadores", "Trayectoria mixta","Estudiantes", "Trabajadores estables"))))

clusters_sofofa$cluster <- factor(clusters_sofofa$cluster,
                                  labels = c("Estudiantes trabajadores","Trayectoria mixta","Estudiantes", "Trabajadores estables"))
# Gráficos clusters
seqdplot(sofofa.seq[clusters_sofofa=="Estudiantes trabajadores",],with.legend=F, ylab= 'Frecuencia (n = 305)')
seqdplot(sofofa.seq[clusters_sofofa=="Trayectoria mixta",],with.legend=F, ylab= 'Frecuencia (n = 161)')
seqdplot(sofofa.seq[clusters_sofofa=="Estudiantes",],with.legend=F, ylab= 'Frecuencia (n = 264)')
seqdplot(sofofa.seq[clusters_sofofa=="Trabajadores estables",],with.legend=F, ylab= 'Frecuencia (n = 108)')

# Unir gráficos clusters
seqdplot(sofofa.seq, group = clusters_sofofa, border = NA, main = 'Cluster')


