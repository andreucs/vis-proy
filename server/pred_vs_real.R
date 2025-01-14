render_pred_vs_real <- function(output, data1, pred, datos_combinados, color_real = "#2d7d8c", color_pred = "#88009d") {
  output$arimaPlot <- renderPlotly({
    plot_ly(source = "pred_vs_real") %>%
      add_lines(x = ~data1$Fecha, y = ~data1$pernoc, name = "Real", line = list(color = color_real, width = 5)) %>%
      add_lines(x = ~pred$fecha, y = ~pred$pernoc, name = "Predicho", line = list(color = color_pred, width = 5)) %>%
      add_ribbons(
        x = ~datos_combinados$fecha,
        ymin = ~datos_combinados$ymin,
        ymax = ~datos_combinados$ymax,
        fillcolor = "#edc9ed", line = list(width = 0), name = "Pérdidas COVID"
      ) %>%
      layout(
        title = "Estimación pernoctaciones modelo ARIMA vs Real",
        xaxis = list(title = ""),
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
