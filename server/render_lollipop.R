render_lollipop <- function(output, input, ocup1, ccaa) {
  
  output$lollipop <- renderPlotly({
    ccaa$Comunidad_autonoma <- gsub("^Región de", "", ccaa$Comunidad_autonoma)
    ccaa$Comunidad_autonoma <- gsub("^Comunidad de", "", ccaa$Comunidad_autonoma)
    ccaa$Comunidad_autonoma <- gsub("^Comunidad", "C.", ccaa$Comunidad_autonoma)
    
    ocup1 <- merge(ocup1, ccaa, by.x="PROVINCIA", by.y="Provincia") |> filter(LUGAR_RESIDENCIA != "Total")
    ocup1$anyomes <- paste(ocup1$AÑO, ocup1$MES, sep = "-")
    
    ocup1 <- ocup1 |>
      filter(AÑO > 2010) |>
      pivot_wider(
        names_from = LUGAR_RESIDENCIA,
        values_from = VIAJEROS,
        names_prefix = "viajeros_"
      )
    
    d <- ocup1 |> 
      group_by(Comunidad_autonoma) |>
      summarise(mean_residentes = 
                  mean(`viajeros_Residentes en España`, 
                       na.rm = TRUE), 
                mean_extranjeros = mean(`viajeros_Residentes en el Extranjero`, 
                                        na.rm = TRUE), .groups = "drop")
    
    d <- d[rev(order(d$mean_extranjeros)), ]
    d$Comunidad_autonoma <- factor(d$Comunidad_autonoma, levels = d$Comunidad_autonoma)
    
    fig <- d |>
      plot_ly() |>
      add_segments(x = ~mean_residentes, xend = ~mean_extranjeros, y = ~Comunidad_autonoma, yend = ~Comunidad_autonoma, showlegend = FALSE) |>
      add_markers(x = ~mean_residentes, y = ~Comunidad_autonoma, name = "Residentes", color = I("pink")) |>
      add_markers(x = ~mean_extranjeros, y = ~Comunidad_autonoma, name = "Extranjeros", color = I("blue")) |>
      layout(
        title = "Gender earnings disparity",
        xaxis = list(title = "Annual Salary (in thousands)"),
        margin = list(l = 65)
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