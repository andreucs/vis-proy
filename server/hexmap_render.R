render_hexmap <- function(output, input, filtered_data, hex_prov) {
  output$hexmap <- renderPlotly({
    data <- filtered_data()
    plot_ly(source = "hexmap_source", data = data) |>
      add_sf(
        split = ~PROVINCIA_DESTINO,
        color = ~m,
        hoveron = "fills",
        hovertext = ~paste("Provincia:", PROVINCIA_DESTINO, "<br>Turistas:", m),
        showlegend = FALSE
      ) |>
      add_text(
        data = hex_prov,
        x = ~X,
        y = ~Y,
        text = ~PROVINCIA_DESTINO,
        textfont = list(size = 10, color = "black"),
        showlegend = FALSE
      ) |>
      layout(
        title = paste("Mapa Hexagonal de España - Año", input$year),
        hovermode = "closest"
      )
  })
}