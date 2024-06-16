### PCA ###

# Cargar librerías necesarias
library(tidyverse)
library(ggcorrplot)
library(ggrepel)

### cargar datos de ejemplo sobre caracteristicas fisico-quimicas de vinos
wine <- read.csv("../data/Wine.csv")

# Estandarizar los datos
# PCA es sensible a las escalas de las variables. 
# Si las variables tienen diferentes unidades o rangos, 
# las variables con mayores rangos dominarán el análisis. 
# Por lo tanto, estandarizamos los datos para que cada 
# variable tenga media 0 y varianza 1.
data_scaled <- scale(wine[,1:13])


# Calcular la matriz de covarianza
# La matriz de covarianza muestra cómo varían las variables conjuntamente.
# Si las variables están correlacionadas, la covarianza no será cero.
# PCA utiliza la matriz de covarianza para identificar las direcciones
# (componentes principales) en las que los datos varían más.
# es una matriz de correlaciones
cov_matrix <- cov(data_scaled)


# Calcular eigenvalores y eigenvectores
# Los eigenvalores y eigenvectores de la matriz de covarianza
# son fundamentales para PCA. Los eigenvalores indican 
# la cantidad de varianza explicada por cada componente principal, 
# mientras que los eigenvectores indican la dirección de esos componentes.
eigen_result <- eigen(cov_matrix)
eigen_values <- eigen_result$values
eigen_vectors <- eigen_result$vectors


# Calcular los componentes principales
# Los componentes principales son nuevas variables que se calculan
# como combinaciones lineales de las variables originales. 
# Cada componente principal explica una parte de la varianza total en los datos.
principal_components <- data_scaled %*% eigen_vectors


# Calcular la proporción de varianza explicada
# Es importante saber cuánta varianza explica cada componente principal
# para determinar cuántos componentes se deben considerar en el análisis. 
# Generalmente, los primeros componentes explican la mayor parte de la varianza.
explained_variance <- eigen_values / sum(eigen_values)
# La sumas de la varianza explicada debe ser igual a 1
sum(explained_variance)
# covertir vector a dataframe
pca.porcen <- data.frame(explained_variance)
# crear dos columnas con el numero de fila
pca.porcen$index <- as.factor(1:nrow(pca.porcen))
pca.porcen$index.cont <- 1:nrow(pca.porcen)

str(pca.porcen)

## grafico de el procentaje explocado por cada 
PCA_percentage_var <- pca.porcen %>%
  ggplot(aes(x = index, y = explained_variance*100, label = round(explained_variance*100, 2))) +
  geom_bar(stat = "identity") +
  geom_path(aes(x = index.cont), size = 1, colour = "Gray50") + # linea que conecta las columnas
  geom_point(size = 2) +
  geom_label_repel(size = 4, nudge_y = 0.5, direction = "y") +
  labs(x = "Principal components", y = "Percentage of explained variance") +
  theme(axis.text = element_text(size = 10),
        axis.title = element_text(size = 12),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"))

PCA_percentage_var

## calculamos la correlacion de variables
CorrelaVariables <- cor(cbind(data_scaled, principal_components))[1 : ncol(data_scaled), 
                                                     (ncol(data_scaled)+1):(ncol(data_scaled)+ncol(principal_components))]

## calculamos la comunalidad (participacion de cada vartiable dentro del compnete)
Comunali <- CorrelaVariables^2

# PCA1
cp_1 <- data.frame(Comunali) %>% 
  rownames_to_column() %>%
  as.tibble() %>%
  dplyr::select(rowname, V1) %>%
  rename(Variable_C1 = rowname, Component_1 = V1) %>%
  arrange(desc(Component_1)) %>%
  mutate(Component_1 = round(Component_1, 3))
# PCA2
cp_2 <- data.frame(Comunali) %>% 
  rownames_to_column() %>%
  as.tibble() %>%
  dplyr::select(rowname, V2) %>%
  rename(Variable_C2 = rowname, Component_2 = V2) %>%
  arrange(desc(Component_2)) %>%
  mutate(Component_2 = round(Component_2, 3))
# PCA3
cp_3 <- data.frame(Comunali) %>% 
  rownames_to_column() %>%
  as.tibble() %>%
  dplyr::select(rowname, V3) %>%
  rename(Variable_C3 = rowname, Component_3 = V3) %>%
  arrange(desc(Component_3)) %>%
  mutate(Component_3 = round(Component_3, 3))

# dataframe de los tes primeros componentes y 
# participacion de cada vartiable
df_stat_comun <- cbind(cp_1, cp_2, cp_3)

## unimos los datos originales con los componentes
PCA_Wine <- cbind(wine, data.frame(principal_components))


df_eigen <- data.frame(eigen_vectors, var = colnames(data_scaled))
 

PCA_Wine %>%
  ggplot() +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey60") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey60") +
  geom_point(aes(x = X1, y = X2, color = as.factor(Customer_Segment)), size = 4) +
  scale_color_manual(name = "Customer Segment", values = c("#9e6ebd", "#7aa457", "#cb6751")) +
  labs(x = paste0("PC1 (", round((explained_variance [1]*100), 2), "%)"),
       y = paste0("PC2 (", round((explained_variance [2]*100), 2), "%)")) +
  geom_segment(data = df_eigen, 
               aes(x = 0, xend = X1*10, y = 0, yend = X2*10),
               arrow = arrow(length = unit(0.2, "cm")), linewidth = 1.1,
               colour = "dodgerblue3", inherit.aes = FALSE, alpha = 0.8) +
  geom_text_repel(data = df_eigen, aes(x = (X1*10), y = (X2*10), label = var), 
            size = 3.5, colour = "#104E8B", fontface = "bold") +
  theme(axis.text.x = element_text(face="bold", size=12, colour = "gray16"), 
        axis.text.y = element_text(face="bold", size=12, colour = "gray16"),
        axis.title.y = element_text(face="bold", size = 14, angle = 90, colour = "grey32"),
        axis.title.x = element_text(face="bold", size = 14, angle = 00, colour = "grey32"),
        legend.text = element_text(size = 14),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.text.align = 0)




PCA_Wine %>%
  ggplot() +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey60") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey60") +
  geom_point(aes(x = X1, y = X3, color = as.factor(Customer_Segment)), size = 4) +
  scale_color_manual(name = "Customer Segment", values = c("#9e6ebd", "#7aa457", "#cb6751")) +
  labs(x = paste0("PC1 (", round((explained_variance [1]*100), 2), "%)"),
       y = paste0("PC3 (", round((explained_variance [3]*100), 2), "%)")) +
  geom_segment(data = df_eigen, 
               aes(x = 0, xend = X1*10, y = 0, yend = X3*10),
               arrow = arrow(length = unit(0.2, "cm")), linewidth = 1.1,
               colour = "dodgerblue3", inherit.aes = FALSE, alpha = 0.8) +
  geom_text_repel(data = df_eigen, aes(x = (X1*10), y = (X3*10), label = var), 
                  size = 3.5, colour = "#104E8B", fontface = "bold") +
  theme(axis.text.x = element_text(face="bold", size=12, colour = "gray16"), 
        axis.text.y = element_text(face="bold", size=12, colour = "gray16"),
        axis.title.y = element_text(face="bold", size = 14, angle = 90, colour = "grey32"),
        axis.title.x = element_text(face="bold", size = 14, angle = 00, colour = "grey32"),
        legend.text = element_text(size = 14),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.text.align = 0)


