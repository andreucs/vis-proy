render_animated_bubble <- function(output, input, ocup, color_residentes = "#2d7d8c", color_extranjeros = "#88009d") {
  
  output$animated <- renderPlotly({
    
    o <- ocup |> mutate(date = AÑO + (MES - 1) / 12) |>
      filter(LUGAR_RESIDENCIA != "Total") |>
      filter(AÑO > 2010)
    
    # Gráfico para "Residentes en España"
    p1 <- o |>
      filter(LUGAR_RESIDENCIA == "Residentes en España") |>
      plot_ly(
        x = ~PERSONAL_EMPLEADO,
        y = ~GRADO_OCUPA_PLAZAS,
        size = ~VIAJEROS,
        marker = list(color = color_residentes),  # Color personalizado para burbujas
        name = "Turistas Nacionales"
      ) |>
      add_markers(frame = ~date, ids = ~PROVINCIA,
                  text = ~PROVINCIA, hoverinfo = "text", span = I(0)
      ) |>
      layout(
        xaxis = list(
          range = c(0, 1e5),
          title = "Personal Empleado (uds)",
          showgrid=F
        ),
        yaxis=list(
          title="Grado de Ocupación (%)",
          showgrid=T
        )
      )
    
    # Gráfico para "Residentes en el Extranjero"
    p2 <- o |>
      filter(LUGAR_RESIDENCIA == "Residentes en el Extranjero") |>
      plot_ly(
        x = ~PERSONAL_EMPLEADO,
        y = ~GRADO_OCUPA_PLAZAS,
        size = ~VIAJEROS,
        marker = list(color = color_extranjeros),  # Color personalizado para burbujas
        name = "Turistas Extranjeros"
      ) |>
      add_markers(frame = ~date, ids = ~PROVINCIA, text = ~PROVINCIA, hoverinfo = "text", span = I(0)) |>
      layout(
        xaxis = list(
          range = c(0, 1e5),
          title="Personal Empleado (uds)",
          showgrid=F
        ),
        yaxis=list(
          title="Grado de Ocupación (%)",
          showgrid=T
        )
      )
    
    # Combinar ambos gráficos en un subplot
    subplot(p1, p2, shareY = TRUE, titleX = TRUE) |>
      layout(
        title=list(text="Evolución Grado de Ocupación vs Personal Empleado en cada Provincia",
                   x=0),
        legend = list(
          x = 0.8,
          y = 0.9,
          bgcolor = "rgba(255,255,255,0.5)",
          bordercolor = "black",
          borderwidth = 1
        )) |>
      animation_opts(500, easing = "linear", redraw = TRUE) %>%
      animation_slider(currentvalue = list(  # Configurar el texto actual
        prefix = "",        # Elimina "YEAR" del slider
        font = list(color = "black")  # Cambiar el color a negro (opcional)
      ),
      steps = list(),       # Vaciar los steps elimina las etiquetas
      tickcolor = "rgba(0,0,0,0)") |>
      animation_button(
        label = "Iniciar"
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
