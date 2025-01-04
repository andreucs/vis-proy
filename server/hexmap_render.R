render_hexmap <- function(output, input,completo, hex_prov) {
  output$hexmap <- renderPlotly({
    plot_ly(source = "hexmap_source", data = completo) |>
      filter(AÑO == input$year) |>  
      add_sf(
        split = ~NULL,
        color = ~cut(m, 
                     breaks = c(0, 5e5, 1e6, 5e6, 10e6, Inf),
                     labels = c("0k-5k", "5k-1M", "1M-5M", "5M-10M", "10M+")),
        colors = c("#FFEDA0", "#FEB24C", "#FD8D3C", "#FC4E2A", "#E31A1C"), # Colores específicos
        hoveron = "fills",
        hovertext = ~paste("Provincia:", PROVINCIA_DESTINO, "<br>Turistas:", format(m, big.mark = ",")),
        showlegend = TRUE
      ) |>
      add_text(
        data = hex_prov,
        x = ~X,
        y = ~Y,
        text = ~label,
        textfont = list(size = 10, color = "black"),
        showlegend = FALSE
      ) |>
      layout(
        title = list(
          text = paste("Mapa Hexagonal de España - Año", input$year),
          xanchor = "center",
          font = list(size = 20)
        ),
        
        hovermode = "closest",
        legend = list(
          orientation = "h",        # Leyenda en orientación horizontal
          xanchor = "center",       # Centrar la leyenda
          x = 0.5,                  # Posición horizontal
          y = 1.1,
          font = list(size = 8)
        ),
        margin = list(t = 80) # Ajustar el margen superior para la leyenda
      ) |>
      event_register("plotly_click")
  })
}