render_bump_chart <- function(output, input, get_ranking_provinces, create_inverted_ranking, turismo_receptor) {
  output$bump_chart <- renderPlotly({
    exclude_list <- c("Madrid", "Barcelona", "Málaga", "Islas Baleares", "Las Palmas", "Illes Balears")
    combined_selection <- c(input$prov_bump, exclude_list)
    
    data <- turismo_receptor |>
      get_ranking_provinces(combined_selection) |>
      create_inverted_ranking()
    
    
    data_highlight <- highlight_key(data, key = ~Province)
    
    p <- plot_ly(data_highlight)
    
    p <- p %>%
      group_by(Province) %>%
      add_lines(
        x = ~Year,
        y = ~Position,
        name = ~Province,
        mode = "lines+markers",
        marker = list(size = 10),
        hoverinfo = "text",
        text = ~paste("Provincia: ", Province, "<br>",
                      "Posición: ", rev(Position), "<br>",
                      "Año: ", Year),
        hoveron = "lines+points"
      )
    
    
    annotations <- list()
    
    for (provincia in unique(data$Province)) {
      data_provincia <- data %>% filter(Province == provincia)
      
      annotations <- append(annotations, list(
        list(
          x = min(data_provincia$Year) - 0.5,
          y = data_provincia$Position[data_provincia$Year == min(data_provincia$Year)],
          text = provincia,
          showarrow = FALSE,
          xanchor = "right",
          font = list(size = 12)
        )
      ))
      
      annotations <- append(annotations, list(
        list(
          x = max(data_provincia$Year) + 0.5, # Margen extra
          y = data_provincia$Position[data_provincia$Year == max(data_provincia$Year)],
          text = provincia,
          showarrow = FALSE,
          xanchor = "left",
          font = list(size = 12)
        )
      ))
    }
    
    p <- layout(
      p,
      title = list(
        text="Ranking de Provincias por número de turístas",
        x=0),
      xaxis = list(
        title = "",
        showgrid = FALSE,
        tickvals = unique(data$Year)
      ),
      yaxis = list(
        showgrid = FALSE,
        showticklabels = FALSE,
        title = ""
      ),
      legend = list(
        title = list(text = "Provincias")
      ),
      annotations = annotations
    ) |> hide_legend() |>
      config(
        modeBarButtonsToRemove = c(
          "zoom2d", "pan2d", "select2d", "lasso2d", 
          "zoomIn2d", "zoomOut2d", "autoScale2d", 
          "hoverClosestCartesian", "hoverCompareCartesian"
        )
      )
    
    
    p <- highlight(
      p,
      on = "plotly_click",
      off = "plotly_doubleclick",
      #selectize = T, SHIFT+click emula la accion
      dynamic = F
    )
    
    
    p
  })
}
