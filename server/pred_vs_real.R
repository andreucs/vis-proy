render_pred_vs_real <- function(output, data1, pred, datos_combinados) {
  output$arimaPlot <- renderPlotly({
    plot_ly(source = "pred_vs_real") %>%
      add_lines(x = ~data1$Fecha, y = ~data1$pernoc, name = "Real", line = list(color = "blue")) %>%
      add_lines(x = ~pred$fecha, y = ~pred$pernoc, name = "Predicho", line = list(color = "red")) %>%
      add_ribbons(
        x = ~datos_combinados$fecha,
        ymin = ~datos_combinados$ymin,
        ymax = ~datos_combinados$ymax,
        fillcolor = "rgba(255, 0, 0, 0.2)", line = list(width = 0), name = "Pérdidas COVID"
      ) %>%
      layout(
        title = "Predicción ARIMA vs Valores Reales",
        xaxis = list(title = "Fecha"),
        yaxis = list(title = "Pernoctaciones"),
        legend = list(orientation = "h", y = -0.2)
      )
  })
}
