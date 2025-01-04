library(data.table)

# Función para obtener el ranking de provincias
get_ranking_provinces <- function(df_prov, province_list, min_year = 2019, max_year = 2023) {
    # Validación de entradas
    if (!all(c("AÑO", "PROVINCIA_DESTINO", "TURISTAS") %in% names(df_prov))) {
        stop("El dataframe debe contener las columnas: 'AÑO', 'PROVINCIA_DESTINO' y 'TURISTAS'.")
    }

    df_prov <- as.data.table(df_prov)

    all_ranks <- lapply(min_year:max_year, function(year) {
        result <- df_prov[AÑO == year & PROVINCIA_DESTINO %in% province_list & PROVINCIA_DESTINO != "Total",
            .(TURISTAS = sum(TURISTAS)),
            by = PROVINCIA_DESTINO
        ][order(-TURISTAS), PROVINCIA_DESTINO]
        if (nrow(result) == 0) {
            return(character(0))
        }
        return(result)
    })

    if (all(sapply(all_ranks, length) == 0)) {
        warning("No se encontraron datos para las provincias especificadas en el rango de años.")
        return(NULL)
    }

    all_ranks_df <- as.data.table(do.call(cbind, all_ranks))
    setnames(all_ranks_df, as.character(min_year:max_year))
    return(all_ranks_df)
}


# Función para obtener el ranking en formato para Tableau
get_tableau_ranking <- function(df_prov, df_ranking_provinces, min_year = 2019, max_year = 2023) {
    # Validación de entradas
    if (!all(c("PROVINCIA_DESTINO") %in% names(df_prov))) {
        stop("El dataframe 'df_prov' debe contener la columna 'PROVINCIA_DESTINO'.")
    }

    df_prov <- as.data.table(df_prov)
    all_rank_year <- unique(df_prov[, .(PROVINCIA_DESTINO)])

    distintct_provinces <- df_ranking_provinces[[1]]
    all_rank_year <- all_rank_year[PROVINCIA_DESTINO %in% distintct_provinces]

    for (year in min_year:max_year) {
        rank_df_year <- data.table(
            PROVINCIA_DESTINO = df_ranking_provinces[[as.character(year)]],
            RANK = seq_len(nrow(df_ranking_provinces))
        )
        setnames(rank_df_year, "RANK", paste0("RANK_", year))
        all_rank_year <- merge(all_rank_year, rank_df_year, by = "PROVINCIA_DESTINO", all.x = TRUE)
    }

    sort_col <- paste0("RANK_", min_year)
    all_rank_year <- all_rank_year[order(get(sort_col), na.last = TRUE)]
    return(all_rank_year)
}


# Función para obtener datos para un bump chart
get_bumpchart_ranking <- function(df_ranking_tableau) {
    # Validación de entradas
    if (!any(grepl("^RANK_", names(df_ranking_tableau)))) {
        stop("El dataframe debe contener columnas con nombres en el formato 'RANK_YYYY'.")
    }

    df_ranking_tableau <- as.data.table(df_ranking_tableau)
    n_elements <- nrow(df_ranking_tableau)

    bump_format <- melt(df_ranking_tableau,
        id.vars = "PROVINCIA_DESTINO",
        measure.vars = patterns("^RANK_"),
        variable.name = "Year",
        value.name = "Position"
    )

    bump_format[, Year := as.numeric(sub("RANK_", "", Year))]
    bump_format[, Position := ifelse(is.na(Position), NA, n_elements - Position)]
    setnames(bump_format, "PROVINCIA_DESTINO", "Province")

    return(bump_format)
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
