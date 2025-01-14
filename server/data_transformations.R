get_ranking_provinces <- function(df_prov, province_list, min_year = 2019, max_year = 2023) {
  df_prov <- as.data.table(df_prov)
  all_ranks <- lapply(min_year:max_year, function(year) {
    df_prov[AÑO == year & PROVINCIA_DESTINO %in% province_list & PROVINCIA_DESTINO != "Total",
            .(TURISTAS = sum(TURISTAS)),
            by = PROVINCIA_DESTINO
    ][order(-TURISTAS), PROVINCIA_DESTINO]
  })
  
  all_ranks_df <- as.data.table(do.call(cbind, all_ranks))
  setnames(all_ranks_df, as.character(min_year:max_year))
  return(all_ranks_df)
}


create_inverted_ranking <- function(data) {
  # Convertir el dataset a formato largo
  long_data <- data |> 
    tidyr::pivot_longer(
      cols = everything(),
      names_to = "Year", # Año como columna
      values_to = "Province" # Provincias como valores
    ) |> 
    mutate(Year = as.integer(Year))
  
  # Agrupar por año y calcular el ranking invertido
  ranked_data <- long_data |> 
    group_by(Year) |> 
    mutate(Position = dense_rank(desc(row_number()))) |> # Invertir el ranking
    arrange(Year, desc(Position)) |> # Ordenar por año y ranking invertido
    ungroup()
  
  # Devolver el dataset con las columnas requeridas
  ranked_data |> select(Year, Position, Province)
}
