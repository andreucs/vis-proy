covid_events_render <- function(output, ocup, color_linea = "#2d7d8c", color_event = "#88009d") {
  data <- ocup |>
      filter(LUGAR_RESIDENCIA == "Total") |>
      select(AÑO, MES, PROVINCIA, PERNOCTACIONES) |>
      group_by(AÑO, MES) |>
      summarise(pernoc=sum(PERNOCTACIONES, na.rm = TRUE), .groups = "drop") 
    
  data$Fecha <- as.Date(paste(data$AÑO, data$MES, "01", sep = "-"))
  
  output$linePlot <- renderPlotly({
    plot_ly(source = "covid_event_source", data, x = ~Fecha, y = ~pernoc, type = 'scatter', mode = 'lines', name = "Pernoctaciones", showlegend=F, line = list(color=color_linea)) %>%
      add_trace(
        x = c(as.Date("2020-03-14"), as.Date("2020-03-14")),
        y = c(0, max(data$pernoc)),
        type = "scatter",
        mode = "lines",
        line = list(color = color_event, dash = "solid"),
        name = "Mar 2020: Estado de alarma",
        showlegend = T
      ) %>%
      add_trace(
        x = c(as.Date("2020-06-21"), as.Date("2020-06-21")),
        y = c(0, max(data$pernoc)),
        type = "scatter",
        mode = "lines",
        line = list(color = color_event, dash = "dash"),
        name = "Jun 2020: Fin del estado de alarma",
        showlegend = T
      ) %>%
      add_trace(
        x = c(as.Date("2020-10-25"), as.Date("2020-10-25")),
        y = c(0, max(data$pernoc)),
        type = "scatter",
        mode = "lines",
        line = list(color = color_event, dash = "dot"),
        name = "Oct 2020: Nuevo estado de alarma",
        showlegend = T
      ) %>%
      add_trace(
        x = c(as.Date("2023-07-05"), as.Date("2023-07-05")),
        y = c(0, max(data$pernoc)),
        type = "scatter",
        mode = "lines",
        line = list(color = color_event, dash = "dashdot"),
        name = "Jul 2023: Fin de la crisis sanitaria",
        showlegend = T
      ) %>%
      layout(
        title = list(text = "Evolución de la gestión de la crisis en España",
                      y=0.95),
        xaxis = list(title = "",
                     showgrid=F),
        yaxis = list(title = "Pernoctaciones",
                     range=c(0, 90e6),
                     showgrid=T),
        legend = list(
          x = 0.01,
          y = 0.89,
          bgcolor = "rgba(255,255,255,0.7)",
          bordercolor = "black",
          borderwidth = 1,
          title = list(text = NULL)
        )
      ) |>
      rangeslider() |>
      config(
        modeBarButtonsToRemove = c(
          "zoom2d", "pan2d", "select2d", "lasso2d", 
          "zoomIn2d", "zoomOut2d", "autoScale2d", 
          "hoverClosestCartesian", "hoverCompareCartesian"
        )
      )
  })
}