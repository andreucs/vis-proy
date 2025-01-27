render_hexmap <- function(output, input, turismo_receptor, hex_prov, colors = c("#c6ffcf", "#6ac2b0", "#449f9c", "#1a5c73", "#103c5c")) {
  
  tur_total_year <- turismo_receptor |>
    group_by(AÑO, PROVINCIA_DESTINO) |>
    summarise(m=sum(TURISTAS))
  
  completo <- merge(tur_total_year, hex_prov, by = "PROVINCIA_DESTINO") |>
    st_as_sf()
  
  output$hexmap <- renderPlotly({
    plot_ly(source = "hexmap_source", data = completo) |>
      filter(AÑO == input$year) |>
      add_sf(
        split = ~NULL,
        color = ~cut(m, 
                     breaks = c(0, 5e5, 1e6, 5e6, 10e6, Inf),
                     labels = c("0k-500k", "500k-1M", "1M-5M", "5M-10M", "10M+")),
        colors = colors,
        hoveron = "fills+points",
        text=~paste("Provincia:", PROVINCIA_DESTINO, "<br>Turistas:", format(m, big.mark = ",")),
        hoverinfo = "text",
        showlegend = TRUE
      ) |>
      add_text(
        data = hex_prov,
        x = ~X,
        y = ~Y,
        text = ~label,
        hoverinfo="skip",
        textfont = list(size = 10, color = "black"),
        showlegend = FALSE
      ) |>
      layout(
        title = list(
          text = paste("Turistas por provincia en", input$year),
          xanchor = "center",
          font = list(size = 15)
        ),
        
        hovermode = "closest",
        legend = list(
          orientation = "v",
          xanchor = "left",
          x = 1.1,
          y = 0.5,
          font = list(size = 10)
        ),
        margin = list(t = 30, r = 100)
      ) |>
      event_register("plotly_click") |>
      config(
        modeBarButtonsToRemove = c(
          "zoom2d", "pan2d", "select2d", "lasso2d", 
          "zoomIn2d", "zoomOut2d", "autoScale2d", 
          "hoverClosestCartesian", "hoverCompareCartesian"
        )
      )
  })
}