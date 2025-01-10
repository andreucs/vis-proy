render_lollipop <- function(output, input, ocup1, ccaa, color_residentes = "#2d7d8c", color_extranjeros = "#88009d") {
  
  output$lollipop <- renderPlotly({
    ccaa$Comunidad_autonoma <- gsub("^Región de", "", ccaa$Comunidad_autonoma)
    ccaa$Comunidad_autonoma <- gsub("^Comunidad de", "", ccaa$Comunidad_autonoma)
    ccaa$Comunidad_autonoma <- gsub("^Comunidad", "C.", ccaa$Comunidad_autonoma)
    
    ocup <- merge(ocup1, ccaa, by.x="PROVINCIA", by.y="Provincia", all.x=T) |> filter(LUGAR_RESIDENCIA != "Total")

    ocup$anyomes <- paste(ocup$AÑO, ocup$MES, sep = "-")
    
    ocup <- ocup |>
      filter(AÑO >= 2010) |>
      pivot_wider(
        names_from = LUGAR_RESIDENCIA,
        values_from = VIAJEROS,
        names_prefix = "viajeros_"
      ) |> filter(AÑO == input$year_loli)
    
    d <- ocup |>
      group_by(Comunidad_autonoma) |>
      summarise(mean_residentes = 
                  sum(`viajeros_Residentes en España`, 
                      na.rm = TRUE), 
                mean_extranjeros = sum(`viajeros_Residentes en el Extranjero`, 
                                       na.rm = TRUE), .groups = "drop")

    d <- d[order(d$mean_extranjeros), ]
    d$Comunidad_autonoma <- factor(d$Comunidad_autonoma, levels = d$Comunidad_autonoma)
    
    fig <- d |>
      plot_ly() |>
      add_segments(
        x = ~Comunidad_autonoma,       # Comunidad Autónoma en el eje x
        xend = ~Comunidad_autonoma,    # Línea recta vertical
        y = ~mean_residentes,          # Inicio en el eje y (Residentes)
        yend = ~mean_extranjeros,      # Fin en el eje y (Extranjeros)
        showlegend = FALSE, 
        color = I("#232323")
      ) |>
      add_markers(
        x = ~Comunidad_autonoma,       # Comunidad Autónoma en el eje x
        y = ~mean_residentes,          # Valor para Residentes en el eje y
        name = "Residentes", 
        color = I(color_residentes)
      ) |>
      add_markers(
        x = ~Comunidad_autonoma,       # Comunidad Autónoma en el eje x
        y = ~mean_extranjeros,         # Valor para Extranjeros en el eje y
        name = "Extranjeros", 
        color = I(color_extranjeros)
      ) |>
      layout(
        title = list(
              text="Promedio de viajeros por comunidad autónoma",
              x=0),
        xaxis = list(title = "",
                     zeroline=F,
                     showgrid=F,
                     categoryorder = "array",               # Orden definido manualmente
                     categoryarray = rev(d$Comunidad_autonoma),  # Invierte el orden de las etiquetas
                     tickangle = -45),
        yaxis = list(title = "",
                     zeroline=F),
        margin = list(l = 65),
        legend = list(
          x = 1,                
          y = 0.95,                 
          xanchor = "right",     
          yanchor = "top",
          bgcolor = "rgba(255,255,255,0.7)",
          bordercolor = "black",
          borderwidth = 1
        )
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