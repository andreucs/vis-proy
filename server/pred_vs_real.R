render_pred_vs_real <- function(output, real, pred, color_real = "#2d7d8c", color_pred = "#88009d") {
  output$arimaPlot <- renderPlotly({

    data_filtrado <- real %>%
      dplyr::filter(Fecha %in% pred$fecha)
    
    datos_combinados <- merge(pred, data_filtrado, by.x = "fecha", by.y = "Fecha", all.x = TRUE)
    
    datos_combinados$ymin <- pmin(datos_combinados$pernoc.x, datos_combinados$pernoc.y, na.rm = TRUE)
    datos_combinados$ymax <- pmax(datos_combinados$pernoc.x, datos_combinados$pernoc.y, na.rm = TRUE)
    
    plot_ly(source = "pred_vs_real") %>%
      add_lines(x = ~real$Fecha, y = ~real$pernoc, name = "Real", line = list(color = color_real, width = 5)) %>%
      add_lines(x = ~pred$fecha, y = ~pred$pernoc, name = "Predicho", line = list(color = color_pred, width = 5)) %>%
      add_ribbons(
        x = ~datos_combinados$fecha,
        ymin = ~datos_combinados$ymin,
        ymax = ~datos_combinados$ymax,
        fillcolor = "#edc9ed", line = list(width = 0), name = "Pérdidas COVID",
        hoverinfo="none"
      ) %>%
      layout(
        title = "Estimación pernoctaciones modelo ARIMA vs Real",
        xaxis = list(title = "",
                     showgrid=F),
        yaxis = list(title = "Pernoctaciones"),
        legend = list(orientation = "h", y = -0.2)
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
