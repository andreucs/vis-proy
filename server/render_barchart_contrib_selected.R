render_barchart_contrib_selected <- function(output, input, turismo_receptor) {
  output$province_barplot <- renderPlotly({
    
    
    base_provinces <- c("Madrid", "Barcelona", "Málaga", "Islas Baleares", "Las Palmas", "Illes Balears")
    
    selected_provinces <- input$prov_bump
    
    all_selected_provinces <- c(base_provinces, selected_provinces)
    
    data <- turismo_receptor |>
            filter(PROVINCIA_DESTINO != "Total Nacional") |>
            filter(AÑO == input$page3_year) |>
            group_by(PROVINCIA_DESTINO) |>
            summarise(TURISTAS = sum(TURISTAS, na.rm = TRUE)) |>
            mutate(Percentage = 100 * TURISTAS / sum(TURISTAS)) |>
            filter(PROVINCIA_DESTINO %in% all_selected_provinces)

    

    plot_ly(
      data = data,
      x = ~Percentage,
      y = ~reorder(PROVINCIA_DESTINO, Percentage),
      type = "bar",
      orientation = "h",
      marker = list(color = c("steelblue", "grey"))
    ) |> 
      layout(
        title = paste("Distribución de Turistas por Categoría - Año:", input$page3_year),
        xaxis = list(title = "Porcentaje (%)"),
        yaxis = list(title = ""),
        showlegend = FALSE
      )
  })
}