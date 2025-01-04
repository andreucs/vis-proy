covid_events_render <- function(output, data) {
  output$linePlot <- renderPlotly({
    plot_ly(source = "covid_event_source", data, x = ~Fecha, y = ~pernoc, type = 'scatter', mode = 'lines', name = "Pernoctaciones", showlegend=F) %>%
      add_trace(
        x = c(as.Date("2020-03-14"), as.Date("2020-03-14")),
        y = c(0, max(data$pernoc)),
        type = "scatter",
        mode = "lines",
        line = list(color = "red", dash = "solid"),
        name = "Mar 2020: Estado de alarma",
        showlegend = T
      ) %>%
      add_trace(
        x = c(as.Date("2020-06-21"), as.Date("2020-06-21")),
        y = c(0, max(data$pernoc)),
        type = "scatter",
        mode = "lines",
        line = list(color = "red", dash = "dash"),
        name = "Jun 2020: Fin del estado de alarma",
        showlegend = T
      ) %>%
      add_trace(
        x = c(as.Date("2020-10-25"), as.Date("2020-10-25")),
        y = c(0, max(data$pernoc)),
        type = "scatter",
        mode = "lines",
        line = list(color = "red", dash = "dot"),
        name = "Oct 2020: Nuevo estado de alarma",
        showlegend = T
      ) %>%
      add_trace(
        x = c(as.Date("2023-07-05"), as.Date("2023-07-05")),
        y = c(0, max(data$pernoc)),
        type = "scatter",
        mode = "lines",
        line = list(color = "red", dash = "dashdot"),
        name = "Jul 2023: Fin de la crisis sanitaria",
        showlegend = T
      ) %>%
      layout(
        title = "Evolución de la gestión de la crisis en España",
        xaxis = list(title = ""),
        yaxis = list(title = "Pernoctaciones",
                     range=c(0, 120e6)),
        legend = list(
          x = 0.01,
          y = 0.99,
          bgcolor = "rgba(255,255,255,0.7)",
          bordercolor = "black",
          borderwidth = 1,
          title = list(text = NULL)
        )
      ) |>
      rangeslider()
  })
}