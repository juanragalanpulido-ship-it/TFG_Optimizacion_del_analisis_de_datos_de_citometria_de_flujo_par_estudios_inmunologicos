library(tidyverse)
library(ggrepel)

# -----------------------------
# 1. Lectura de archivos
# -----------------------------

ruta <- "C:/Users/nquev/OneDrive/Desktop/Gráficos"   # <--- cambia esta ruta
archivos <- list.files(ruta, pattern = "\\.csv$", full.names = TRUE)

procesar_archivo <- function(archivo) {
  df <- read.csv(archivo)
  nombre <- tools::file_path_sans_ext(basename(archivo))
  
  algoritmo <- sub("[0-9]+.*", "", nombre)   # letras al inicio
  celulas   <- sub("^[A-Za-z]+", "", nombre) # números después
  
  df %>%
    filter(Capa %in% c("Capa1", "Capa2")) %>%
    mutate(Algoritmo = algoritmo,
           Celulas = paste0("Concat", celulas))
}

resultados <- map_df(archivos, procesar_archivo)

# -----------------------------
# 2. Heatmaps de F-measure
# -----------------------------
# Preparar Capa1
df_capa1 <- resultados %>%
  filter(Capa == "Capa1") %>%
  select(Celulas, Algoritmo, Weighted_F1) %>%
  mutate(Celulas_num = as.numeric(gsub("Concat|k", "", Celulas)))  # quitar "Concat" y "k"

df_capa1_long <- df_capa1 %>%
  pivot_longer(cols = Weighted_F1, names_to = "Metrica", values_to = "F_measure") %>%
  mutate(Celulas = factor(Celulas, levels = unique(Celulas[order(Celulas_num)])))

# Agregar solo el mejor valor por Celulas
df_capa1_long <- df_capa1_long %>%
  group_by(Celulas) %>%
  mutate(F_best = ifelse(F_measure == max(F_measure, na.rm = TRUE), round(F_measure, 2), NA)) %>%
  ungroup()

# Heatmap Capa1
p1 <- ggplot(df_capa1_long, aes(x = Celulas, y = Algoritmo, fill = F_measure)) +
  geom_tile(color = "white") +
  geom_text(aes(label = F_best), color = "black", size = 3) +
  scale_fill_gradient(limits = c(0, 1), low = "white", high = "red", breaks = seq(0, 1, by = 0.2)) +
  theme_minimal(base_size = 12) +
  theme(panel.background = element_rect(fill = "white", colour = "white"),
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Heatmap F-measure - Capa1", x = "Archivo", y = "Algoritmo", fill = "F-measure")

ggsave("heatmap_F_measure_Capa1_best.jpg", p1, width = 8, height = 6)

# Preparar Capa2 (igual que Capa1)
df_capa2 <- resultados %>%
  filter(Capa == "Capa2") %>%
  select(Celulas, Algoritmo, Weighted_F1) %>%
  mutate(Celulas_num = as.numeric(gsub("Concat|k", "", Celulas)))

df_capa2_long <- df_capa2 %>%
  pivot_longer(cols = Weighted_F1, names_to = "Metrica", values_to = "F_measure") %>%
  mutate(Celulas = factor(Celulas, levels = unique(Celulas[order(Celulas_num)])))

df_capa2_long <- df_capa2_long %>%
  group_by(Celulas) %>%
  mutate(F_best = ifelse(F_measure == max(F_measure, na.rm = TRUE), round(F_measure, 2), NA)) %>%
  ungroup()

# Heatmap Capa2
p2 <- ggplot(df_capa2_long, aes(x = Celulas, y = Algoritmo, fill = F_measure)) +
  geom_tile(color = "white") +
  geom_text(aes(label = F_best), color = "black", size = 3) +
  scale_fill_gradient(limits = c(0, 1), low = "white", high = "red", breaks = seq(0, 1, by = 0.2)) +
  theme_minimal(base_size = 12) +
  theme(panel.background = element_rect(fill = "white", colour = "white"),
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Heatmap F-measure - Capa2", x = "Archivo", y = "Algoritmo", fill = "F-measure")

ggsave("heatmap_F_measure_Capa2_best.jpg", p2, width = 8, height = 6)


# -----------------------------
# 3. Gráficos de barras por archivo y capa
# -----------------------------

# Reestructurar métricas
df_metricas <- resultados %>%
  pivot_longer(cols = starts_with("Weighted_"),
               names_to = "Metrica",
               values_to = "Valor") %>%
  mutate(Metrica = recode(Metrica,
                          "Weighted_F1" = "F-measure",
                          "Weighted_Precision" = "Precision",
                          "Weighted_Recall" = "Recall"))

# Fijar orden de métricas y algoritmos
orden_metricas <- c("F-measure", "Recall", "Precision")
orden_algoritmos <- c("FlowSOM", "Phenograph", "Xshift", "flowmeans")  # <- añade más si los tienes

df_metricas <- df_metricas %>%
  mutate(Metrica = factor(Metrica, levels = orden_metricas),
         Algoritmo = factor(Algoritmo, levels = orden_algoritmos))

# Crear carpeta de salida
dir.create("graficos_barras", showWarnings = FALSE)

# Generar un gráfico por archivo y capa
split(df_metricas, list(df_metricas$Celulas, df_metricas$Capa)) %>%
  walk(function(subdf) {
    if (nrow(subdf) > 0) {
      cel <- unique(subdf$Celulas)
      capa <- unique(subdf$Capa)
      
      p <- ggplot(subdf, aes(x = Algoritmo, y = Valor, fill = Metrica)) +
        geom_bar(stat = "identity", position = "dodge") +
        theme_minimal(base_size = 12) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        labs(title = paste("Archivo:", cel, "-", capa),
             x = "Algoritmo", y = "Valor", fill = "Métrica")
      
      ggsave(filename = paste0("graficos_barras/", cel, "_", capa, ".jpg"),
             plot = p, width = 8, height = 5)
    }
  })


# Leer los archivos CSV
df1 <- read.csv("C:/Users/nquev/OneDrive/Desktop/Gráficos/Jaccard/JaccardSimilarityMeasureCapa1.csv", sep=";", header=TRUE, stringsAsFactors=FALSE)
df2 <- read.csv("C:/Users/nquev/OneDrive/Desktop/Gráficos/Jaccard/JaccardSimilarityMeasureCapa2.csv", sep=";", header=TRUE, stringsAsFactors=FALSE)


library(tidyverse)

# Definir el orden deseado
niveles_archivo <- c("Concat24k", "Concat48k", "Concat120k", "Concat192k", "Concat240k")

# ---- Capa 1 ----
df_long <- df1 %>%
  pivot_longer(cols = starts_with("Concat"),
               names_to = "Archivo",
               values_to = "Jaccard") %>%
  mutate(Archivo = factor(Archivo, levels = niveles_archivo))

p1 <- ggplot(df_long, aes(x = Archivo, y = Algoritmo, fill = Jaccard)) +
  geom_tile(color = "white") +
  scale_fill_gradient(limits = c(0, 1),
                      low = "white", high = "red",
                      breaks = seq(0, 1, by = 0.2)) +
  theme_minimal(base_size = 12) +
  theme(panel.background = element_rect(fill = "white", colour = "white"),
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Jaccard Similarity Measure (Capa 1)",
       x = "Archivo", y = "Algoritmo", fill = "Jaccard")

ggsave("heatmap_Capa1JSM.jpg", p1, width = 8, height = 6, dpi = 300)


# ---- Capa 2 ----
df_long2 <- df2 %>%
  pivot_longer(cols = starts_with("Concat"),
               names_to = "Archivo",
               values_to = "Jaccard") %>%
  mutate(Archivo = factor(Archivo, levels = niveles_archivo))

p2 <- ggplot(df_long2, aes(x = Archivo, y = Algoritmo, fill = Jaccard)) +
  geom_tile(color = "white") +
  scale_fill_gradient(limits = c(0, 1),
                      low = "white", high = "red",
                      breaks = seq(0, 1, by = 0.2)) +
  theme_minimal(base_size = 12) +
  theme(panel.background = element_rect(fill = "white", colour = "white"),
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Jaccard Similarity Measure (Capa 2)",
       x = "Archivo", y = "Algoritmo", fill = "Jaccard")

ggsave("heatmap_Capa2JSM.jpg", p2, width = 8, height = 6, dpi = 300)

#------------------------------
# Cálculo de Rank-sum
#------------------------------

# -----------------------------
# 1. Preparar F-measure por archivo
# -----------------------------
df_f <- resultados %>%
  select(Celulas, Algoritmo, Capa, Weighted_F1) %>%
  pivot_wider(names_from = Capa, values_from = Weighted_F1, names_prefix = "F_") %>%
  mutate(Celulas_num = as.numeric(gsub("Concat|k", "", Celulas)))

# -----------------------------
# 2. Preparar Jaccard por archivo
# -----------------------------
df_j1 <- df1 %>%
  pivot_longer(cols = starts_with("Concat"),
               names_to = "Celulas", values_to = "Jaccard") %>%
  mutate(Celulas_num = as.numeric(gsub("Concat|k", "", Celulas)),
         Capa = "Capa1")

df_j2 <- df2 %>%
  pivot_longer(cols = starts_with("Concat"),
               names_to = "Celulas", values_to = "Jaccard") %>%
  mutate(Celulas_num = as.numeric(gsub("Concat|k", "", Celulas)),
         Capa = "Capa2")

df_j <- bind_rows(df_j1, df_j2) %>%
  pivot_wider(names_from = Capa, values_from = Jaccard, names_prefix = "J_")

# -----------------------------
# 3. Combinar F-measure y Jaccard
# -----------------------------
df_comb <- df_f %>%
  left_join(df_j, by = c("Celulas", "Celulas_num", "Algoritmo")) %>%
  select(Algoritmo, Celulas, F_Capa1, F_Capa2, J_Capa1, J_Capa2)

# -----------------------------
# 4. Calcular ranking por archivo
# -----------------------------
df_rank <- df_comb %>%
  group_by(Celulas) %>%
  mutate(
    rank_F_Capa1 = rank(-F_Capa1, ties.method = "min"),
    rank_F_Capa2 = rank(-F_Capa2, ties.method = "min"),
    rank_J_Capa1 = rank(-J_Capa1, ties.method = "min"),
    rank_J_Capa2 = rank(-J_Capa2, ties.method = "min")
  ) %>%
  rowwise() %>%
  mutate(media_rank = mean(c(rank_F_Capa1, rank_F_Capa2, rank_J_Capa1, rank_J_Capa2), na.rm = TRUE)) %>%
  ungroup()

# -----------------------------
# 5. Guardar tabla por archivo
# -----------------------------
dir.create("rankings_archivos", showWarnings = FALSE)

unique(df_rank$Celulas) %>%
  walk(function(cel) {
    df_temp <- df_rank %>% filter(Celulas == cel)
    write.csv(df_temp, file = paste0("rankings_archivos/ranking_", cel, ".csv"),
              row.names = FALSE)
  })

# -----------------------------
# 6. Ranking global por algoritmo
# -----------------------------
df_global_rank <- df_rank %>%
  group_by(Algoritmo) %>%
  summarise(
    sum_rank = sum(rank_F_Capa1, rank_F_Capa2, rank_J_Capa1, rank_J_Capa2, na.rm = TRUE),
    mean_rank = mean(c(rank_F_Capa1, rank_F_Capa2, rank_J_Capa1, rank_J_Capa2), na.rm = TRUE)
  ) %>%
  arrange(mean_rank)

# Guardar ranking global
write.csv(df_global_rank, file = "rankings_archivos/ranking_global_algoritmo.csv", row.names = FALSE)

#------------------------------
# TIEMPO VS RANK SUM - REESCRITO
#------------------------------

library(tidyverse)

# -----------------------------
# 1. Leer CSV de tiempos
# -----------------------------
tiempos <- read.csv(
  "C:/Users/nquev/OneDrive/Desktop/Gráficos/Tiempoejecucion/Tiempoejecucion.csv",
  stringsAsFactors = FALSE
) %>%
  # Homogeneizar nombres de algoritmos
  mutate(
    Algoritmo = case_when(
      tolower(Algoritmo) == "flowmeans" ~ "FlowMeans",
      tolower(Algoritmo) == "xshift"   ~ "Xshift",
      TRUE ~ Algoritmo
    )
  )

# -----------------------------
# 2. Preparar mean rank por archivo
# -----------------------------
df_mean_rank_archivo <- df_rank %>%
  # Homogeneizar nombres de algoritmos
  mutate(
    Algoritmo = case_when(
      tolower(Algoritmo) == "flowmeans" ~ "FlowMeans",
      tolower(Algoritmo) == "xshift"   ~ "Xshift",
      TRUE ~ Algoritmo
    ),
    # Limpiar 'k' final en Celulas
    Celulas_clean = gsub("k$", "", Celulas)
  ) %>%
  group_by(Celulas, Celulas_clean, Algoritmo) %>%
  summarise(mean_rank_archivo = mean(media_rank, na.rm = TRUE)) %>%
  ungroup() %>%
  # Unir con tiempos y eliminar posibles NA
  left_join(tiempos, by = c("Celulas_clean" = "Archivo", "Algoritmo")) %>%
  drop_na(Tiempo.s., mean_rank_archivo)

# -----------------------------
# 3. Gráfico rank vs tiempo por archivo
# -----------------------------
dir.create("graficos_tiempo", showWarnings = FALSE)

unique(df_mean_rank_archivo$Celulas) %>%
  walk(function(cel) {
    df_plot <- df_mean_rank_archivo %>% filter(Celulas == cel)
    
    p <- ggplot(df_plot, aes(x = Tiempo.s., y = mean_rank_archivo, color = Algoritmo)) +
      geom_point(size = 3) +
      geom_line(aes(group = Algoritmo), linetype = "dashed") +
      geom_text_repel(aes(label = Algoritmo), nudge_y = 0.2, segment.size = 0.3) +
      geom_hline(yintercept = 0, color = "black") +
      geom_vline(xintercept = 0, color = "black") +
      theme_minimal(base_size = 12) +
      theme(
        legend.position = "none",
        panel.grid = element_blank()  # quitar cuadrícula
      ) +
      labs(
        title = paste("Mean Rank vs Tiempo -", cel),
        x = "Tiempo de ejecución (s)",
        y = "Mean Rank por Archivo"
      )
    
    ggsave(
      filename = paste0("graficos_tiempo/rank_vs_tiempo_", cel, ".jpg"),
      plot = p, width = 7, height = 5
    )
  })
# -----------------------------
# 4. Gráfico global: mean rank overall vs tiempo medio
# -----------------------------
df_mean_rank_global <- df_rank %>%
  mutate(
    Algoritmo = case_when(
      tolower(Algoritmo) == "flowmeans" ~ "FlowMeans",
      tolower(Algoritmo) == "xshift"   ~ "Xshift",
      TRUE ~ Algoritmo
    )
  ) %>%
  group_by(Algoritmo) %>%
  summarise(mean_rank_overall = mean(media_rank, na.rm = TRUE)) %>%
  left_join(
    tiempos %>%
      group_by(Algoritmo) %>%
      summarise(Tiempo_medio = mean(Tiempo.s., na.rm = TRUE)),
    by = "Algoritmo"
  ) %>%
  drop_na(Tiempo_medio, mean_rank_overall)


p_global <- ggplot(df_mean_rank_global, aes(x = Tiempo_medio, y = mean_rank_overall, color = Algoritmo)) +
  geom_point(size = 4) +
  geom_text_repel(aes(label = Algoritmo), nudge_y = 0.2, segment.size = 0.3) +
  # Añadir líneas de los ejes X y Y en 0
  geom_hline(yintercept = 0, color = "black") +
  geom_vline(xintercept = 0, color = "black") +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "none",
    panel.grid = element_blank()  # eliminar grid para que solo se vean los ejes
  ) +
  labs(
    title = "Mean Rank Overall vs Tiempo Medio",
    x = "Tiempo medio de ejecución (s)",
    y = "Mean Rank Overall"
  )

ggsave("graficos_tiempo/rank_vs_tiempo_global.jpg", plot = p_global, width = 7, height = 5)
