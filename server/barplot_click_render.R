render_barplot <- function(output, input, turismo_receptor, hex_prov) {
  output$barplot <- renderPlotly({
    clickData <- event_data("plotly_click", source = "hexmap_source")
    
    
    if (is.null(clickData)) {
     
      total_data <- turismo_receptor |> 
        filter(AÑO == input$year)
      
      top_countries <- total_data |> 
        group_by(PAIS_ORIGEN) |> 
        summarise(TURISTAS = sum(TURISTAS, na.rm = TRUE)) |> 
        arrange(desc(TURISTAS)) |> 
        slice_head(n = 5)
      
      max_turistas <- max(top_countries$TURISTAS)*1.3
      
      return(
        plot_ly(
          data = top_countries,
          y = ~TURISTAS,
          x = ~reorder(PAIS_ORIGEN, -TURISTAS),
          type = "bar",
          orientation = "v"
        ) |>
          add_text(
            text=~scales::comma(TURISTAS),
            y = ~TURISTAS,
            textposition="top middle",
            showlegend = FALSE
          ) |>
          layout(
            title = paste("Top 5 Países de Origen - Total Año:", input$year),
            xaxis = list(title = "Número de Turistas"),
            yaxis = list(title = "",
                         range=c(0, max_turistas))
          ) |>
          config(
            modeBarButtonsToRemove = c(
              "zoom2d", "pan2d", "select2d", "lasso2d", 
              "zoomIn2d", "zoomOut2d", "autoScale2d", 
              "hoverClosestCartesian", "hoverCompareCartesian"
            )
          )
      )
    }
    
    clicked_x <- clickData$x
    clicked_y <- clickData$y
    selected_province <- hex_prov |> 
      filter(round(X, 4) == round(clicked_x, 4) & round(Y, 4) == round(clicked_y, 4)) |> 
      pull(PROVINCIA_DESTINO)
    
    if (length(selected_province) == 0) {
      return(plot_ly() |> layout(title = "No se encontró ninguna provincia"))
    }
    
    province_data <- turismo_receptor |> 
      filter(AÑO == input$year, PROVINCIA_DESTINO == selected_province)
    
    if (nrow(province_data) == 0) {
      return(plot_ly() |> layout(title = paste("No hay datos disponibles para la provincia:", selected_province)))
    }
    
    top_countries <- province_data |> 
      group_by(PAIS_ORIGEN) |> 
      summarise(TURISTAS = sum(TURISTAS, na.rm = TRUE)) |> 
      arrange(desc(TURISTAS)) |> 
      slice_head(n = 5)
    
    max_turistas <- max(top_countries$TURISTAS)*1.3
    
    
    plot_ly(
      data = top_countries,
      y = ~TURISTAS,
      x = ~reorder(PAIS_ORIGEN, -TURISTAS),
      type = "bar",
      orientation = "v"
    ) |>
    add_text(
      text=~scales::comma(TURISTAS),
      y = ~TURISTAS,
      textposition="top middle",
      showlegend = FALSE
    ) |>
      layout(
        title = paste("Top 5 Países de Origen - Provincia:", selected_province),
        xaxis = list(title = "Número de Turistas"),
        yaxis = list(title = "",
                     range=c(0, max_turistas))
      ) |>
      config(
        modeBarButtonsToRemove = c(
          "zoom2d", "pan2d", "select2d", "lasso2d", 
          "zoomIn2d", "zoomOut2d", "autoScale2d", 
          "hoverClosestCartesian", "hoverCompareCartesian"
        )
      )
  })
}

