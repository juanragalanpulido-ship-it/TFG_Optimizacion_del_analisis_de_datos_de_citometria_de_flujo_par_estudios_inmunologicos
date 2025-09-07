# ==========================================
# Cargar librerías
# ==========================================
library(dplyr)
library(clue)

# ==========================================
# 1️⃣ Listar archivos disponibles
# ==========================================
list.files()

# ==========================================
# 2️⃣ Leer datos
# ==========================================
tam_clust      <- read.csv2("ClusterSizesXshift.csv", stringsAsFactors = FALSE)
manualcluster  <- read.csv2("ManualCells.csv")

# Archivos mutual como caracteres para evitar pérdida de datos
mutual_capa1   <- read.csv2("XshiftCapa1.csv", stringsAsFactors = FALSE, check.names = FALSE)
mutual_capa2   <- read.csv2("XshiftCapa2.csv", stringsAsFactors = FALSE, check.names = FALSE)

# Limpiar nombres de columna
colnames(tam_clust) <- trimws(as.character(colnames(tam_clust)))

# ==========================================
# 3️⃣ Limpiar nombres de cluster en mutuals y eliminar cluster 0
# ==========================================
clean_mutual <- function(mutual_df){
  mutual_df$Cluster <- sub(".*->\\s*", "", mutual_df$Cluster)
  mutual_df <- mutual_df[mutual_df$Cluster != "0", ]
  return(mutual_df)
}

mutual_capa1 <- clean_mutual(mutual_capa1)
mutual_capa2 <- clean_mutual(mutual_capa2)

# ==========================================
# 4️⃣ Transponer y convertir NA a 0
# ==========================================
prepare_transpose <- function(mutual_df){
  df_t <- as.data.frame(t(mutual_df), stringsAsFactors = FALSE)
  
  # Primera fila como cabecera
  colnames(df_t) <- df_t[1, ]
  df_t <- df_t[-1, ]
  
  # Convertir a numérico y reemplazar NA por 0
  df_t[] <- lapply(df_t, function(x){
    x_num <- as.numeric(as.character(x))
    x_num[is.na(x_num)] <- 0
    return(x_num)
  })
  
  rownames(df_t) <- trimws(as.character(rownames(df_t)))
  return(df_t)
}

mutual_capa1_t <- prepare_transpose(mutual_capa1)
mutual_capa2_t <- prepare_transpose(mutual_capa2)

# ==========================================
# 5️⃣ Generar combinaciones Cluster × Population
# ==========================================
generate_combinations <- function(mutual_t){
  expand.grid(
    Cluster = rownames(mutual_t),
    Population = colnames(mutual_t),
    stringsAsFactors = FALSE
  )
}

combinations   <- generate_combinations(mutual_capa1_t)
combinationscap2 <- generate_combinations(mutual_capa2_t)

# ==========================================
# 6️⃣ Calcular TP, FP, FN y métricas
# ==========================================
calculate_metrics <- function(combinations_df, mutual_t){
  
  combinations_df$TP <- mapply(function(cluster, pop){
    TP <- mutual_t[cluster, pop]
    if(length(TP) == 0 || is.na(TP)) TP <- 0
    return(as.numeric(TP))
  }, combinations_df$Cluster, combinations_df$Population)
  
  combinations_df$FP <- mapply(function(cluster, TP){
    cluster_size <- as.numeric(tam_clust$Size[tam_clust$Cluster == as.numeric(cluster)])
    max(cluster_size - TP, 0)
  }, combinations_df$Cluster, combinations_df$TP)
  
  combinations_df$FN <- mapply(function(pop, TP){
    if(pop %in% manualcluster$Cluster){
      manual_size <- as.numeric(manualcluster$Cells[manualcluster$Cluster == pop])
      max(manual_size - TP, 0)
    } else {
      NA
    }
  }, combinations_df$Population, combinations_df$TP)
  
  combinations_df <- combinations_df %>%
    mutate(
      Precision = ifelse((TP + FP) > 0, TP / (TP + FP), NA),
      Recall    = ifelse((TP + FN) > 0, TP / (TP + FN), NA),
      F1        = ifelse((Precision + Recall) > 0,
                         2 * (Precision * Recall) / (Precision + Recall), 0)
    )
  
  return(combinations_df)
}

combinations    <- calculate_metrics(combinations, mutual_capa1_t)
combinationscap2 <- calculate_metrics(combinationscap2, mutual_capa2_t)

# ==========================================
# 7️⃣ Greedy Matching: cluster → población con mayor F1
# ==========================================
greedy_best_matches <- function(combinations_df){
  combinations_df %>%
    group_by(Cluster) %>%
    slice_max(order_by = F1, n = 1, with_ties = FALSE) %>%
    ungroup()
}

best_matches    <- greedy_best_matches(combinations)
best_matches2   <- greedy_best_matches(combinationscap2)

## ==========================================
# 8️⃣ Hungarian Matching: población → un único cluster
# ==========================================
hungarian_best_matches <- function(combinations_df){
  f1_matrix <- xtabs(F1 ~ Population + Cluster, data = combinations_df)
  
  # ✅ Convertir a matriz numérica y reemplazar NAs por 0
  f1_matrix <- as.matrix(f1_matrix)
  f1_matrix[is.na(f1_matrix)] <- 0
  
  assignment <- solve_LSAP(f1_matrix, maximum = TRUE)
  
  df <- data.frame(
    Population = rownames(f1_matrix),
    Cluster    = colnames(f1_matrix)[assignment],
    stringsAsFactors = FALSE
  )
  
  df$F1 <- mapply(function(pop, clust){
    f1_matrix[pop, clust]
  }, df$Population, df$Cluster)
  
  return(df)
}

best_matches_hungarian    <- hungarian_best_matches(combinations)
best_matches2_hungarian   <- hungarian_best_matches(combinationscap2)

# ==========================================
# 9️⃣ Añadir Precision y Recall a los mejores matches
# ==========================================
best_matches_hungarian <- merge(
  best_matches_hungarian,
  combinations[, c("Cluster", "Population", "Precision", "Recall")],
  by = c("Cluster", "Population"),
  all.x = TRUE
)

best_matches2_hungarian <- merge(
  best_matches2_hungarian,
  combinationscap2[, c("Cluster", "Population", "Precision", "Recall")],
  by = c("Cluster", "Population"),
  all.x = TRUE
)

# ==========================================
# 10️⃣ Calcular media ponderada de métricas
# ==========================================
calculate_weighted_metrics <- function(best_matches_df){
  df <- merge(best_matches_df, manualcluster[, c("Cluster", "Cells")],
              by.x = "Population", by.y = "Cluster")
  
  weighted_F1        <- sum(df$F1 * df$Cells) / sum(df$Cells)
  weighted_Precision <- sum(df$Precision * df$Cells, na.rm = TRUE) / sum(df$Cells)
  weighted_Recall    <- sum(df$Recall * df$Cells, na.rm = TRUE) / sum(df$Cells)
  
  list(
    df = df,
    weighted_F1 = weighted_F1,
    weighted_Precision = weighted_Precision,
    weighted_Recall = weighted_Recall
  )
}

res_capa1 <- calculate_weighted_metrics(best_matches_hungarian)
res_capa2 <- calculate_weighted_metrics(best_matches2_hungarian)

best_matches_hungarian    <- res_capa1$df
weighted_F1_capa1        <- res_capa1$weighted_F1
weighted_Precision_capa1 <- res_capa1$weighted_Precision
weighted_Recall_capa1    <- res_capa1$weighted_Recall

best_matches2_hungarian   <- res_capa2$df
weighted_F1_capa2        <- res_capa2$weighted_F1
weighted_Precision_capa2 <- res_capa2$weighted_Precision
weighted_Recall_capa2    <- res_capa2$weighted_Recall

# ==========================================
# 11️⃣ Guardar resultados finales a CSV
# ==========================================
write.csv(best_matches_hungarian, "BestMatchesCapa1.csv", row.names = FALSE)
write.csv(best_matches2_hungarian, "BestMatchesCapa2.csv", row.names = FALSE)

# Guardar resumen de medias ponderadas
summary_metrics <- data.frame(
  Capa = c("Capa1", "Capa2"),
  Weighted_F1 = c(weighted_F1_capa1, weighted_F1_capa2),
  Weighted_Precision = c(weighted_Precision_capa1, weighted_Precision_capa2),
  Weighted_Recall = c(weighted_Recall_capa1, weighted_Recall_capa2)
)

write.csv(summary_metrics, "WeightedMetricsSummary.csv", row.names = FALSE)

# ==========================================
# ✅ Fin del script
# ==========================================

