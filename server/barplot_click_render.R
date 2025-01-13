render_barplot <- function(output, input, turismo_receptor, hex_prov, color = "#449f9f") {
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
      
      max_turistas <- max(top_countries$TURISTAS) * 1.3
      
      return(
        plot_ly(
          data = top_countries,
          y = ~TURISTAS,
          x = ~ reorder(PAIS_ORIGEN, -TURISTAS),
          type = "bar",
          orientation = "v",
          marker = list(color = color, size=I(0)),
          hoverinfo = "x+y"
        ) |>
          add_text(
            text = ~ scales::comma(TURISTAS),
            y = ~TURISTAS,
            textposition = "top middle",
            showlegend = FALSE,
            hoverinfo = "none",
            textfont = list(size = 10)
          ) |>
          layout(
            title = list(
              text = paste("Top 5 países que más viajan a España en", input$year),
              font = list(size = 14)
            ),
            xaxis = list(title = "", showgrid = FALSE),
            yaxis = list(
              title = "Turistas",
              range = c(0, max_turistas),
              showgrid = FALSE,
              zeroline = FALSE
            )
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
      return(plot_ly() |>
               layout(
                 title = "Haz click sobre el texto correspondiente de cada provincia",
                 xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                 yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)
               ))
    }
    
    province_data <- turismo_receptor |>
      filter(AÑO == input$year, PROVINCIA_DESTINO == selected_province)
    
    if (nrow(province_data) == 0) {
      return(plot_ly() |>
               layout(
                 title = paste("No hay datos disponibles para", selected_province),
                 xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                 yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)
               ))
    }
    
    top_countries <- province_data |>
      group_by(PAIS_ORIGEN) |>
      summarise(TURISTAS = sum(TURISTAS, na.rm = TRUE)) |>
      arrange(desc(TURISTAS)) |>
      slice_head(n = 5)
    
    max_turistas <- max(top_countries$TURISTAS) * 1.3
    
    return(
      plot_ly(
        data = top_countries,
        y = ~TURISTAS,
        x = ~ reorder(PAIS_ORIGEN, -TURISTAS),
        type = "bar",
        orientation = "v",
        hoverinfo = "x+y",
        marker = list(color = color, size=I(0))
      ) |>
        add_text(
          text = ~ scales::comma(TURISTAS),
          y = ~TURISTAS,
          textposition = "top middle",
          showlegend = FALSE,
          hoverinfo = "none",
          textfont = list(size = 10)
        ) |>
        layout(
          title = list(
            text = paste("Top 5 países que más viajan a", selected_province, "en", input$year),
            font = list(size = 14)
          ),
          xaxis = list(title = "", showgrid = FALSE),
          yaxis = list(
            title = "Turistas",
            range = c(0, max_turistas),
            showgrid = FALSE,
            zeroline = FALSE
          )
        ) |>
        config(
          modeBarButtonsToRemove = c(
            "zoom2d", "pan2d", "select2d", "lasso2d",
            "zoomIn2d", "zoomOut2d", "autoScale2d",
            "hoverClosestCartesian", "hoverCompareCartesian"
          )
        )
    )
  })
}
