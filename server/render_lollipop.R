render_lollipop <- function(output, input, ocup1, ccaa) {
  
  output$lollipop <- renderPlotly({
    
    ocup1 <- merge(ocup1, ccaa, by.x="PROVINCIA", by.y="Provincia") |> filter(LUGAR_RESIDENCIA != "Total")
    ocup1$anyomes <- paste(ocup1$AÑO, ocup1$MES, sep = "-")
    
    ocup1 <- ocup1 |>
      filter(AÑO > 2010) |>
      group_by(AÑO, MES, LUGAR_RESIDENCIA, Comunidad_autonoma) |>
      summarise(total_pernoctaciones = sum(PERNOCTACIONES, na.rm = TRUE), .groups = "drop") |>
      pivot_wider(
        names_from = LUGAR_RESIDENCIA,
        values_from = total_pernoctaciones,
        names_prefix = "pernoctaciones_"
      )
    
    d <- ocup1 |> 
      group_by(Comunidad_autonoma) |>
      summarise(mean_residentes = 
                  mean(`pernoctaciones_Residentes en España`, 
                       na.rm = TRUE), 
                mean_extranjeros = mean(`pernoctaciones_Residentes en el Extranjero`, 
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
      )
    
  })
}