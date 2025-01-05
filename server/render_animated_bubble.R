render_animated_bubble <- function(output, input, ocup) {
  
  output$animated <- renderPlotly({
    
    o <- ocup |> mutate(date = AÑO + (MES - 1) / 12) |>
      filter(LUGAR_RESIDENCIA != "Total")|>
      filter(AÑO > 2010)
    # filter(PROVINCIA %in% c("Albacete", "Alicante", "Almería")) |>
    
    p1 <- o |>
      filter(LUGAR_RESIDENCIA == "Residentes en España") |>
      plot_ly(x = ~PERSONAL_EMPLEADO, y = ~GRADO_OCUPA_PLAZAS, size = ~VIAJEROS) |>
      add_markers(frame = ~date, ids = ~PROVINCIA) |>
      layout(
        xaxis=list(
          range=c(0, 1e5),
          title="Hola"
        )
      )
    
    p2 <- o |>
      filter(LUGAR_RESIDENCIA == "Residentes en el Extranjero") |>
      plot_ly(x = ~PERSONAL_EMPLEADO, y = ~GRADO_OCUPA_PLAZAS, size = ~VIAJEROS)|>
      add_markers(frame = ~date, ids = ~PROVINCIA, text = ~PROVINCIA, hoverinfo="text") |>
      layout(
        xaxis=list(
          range=c(0, 1e5)
        )
      )
    
    subplot(p1,p2, shareY = T, titleX = T) |>
      animation_opts(500, easing = "linear", redraw =T) %>%
      animation_button( x = 1, xanchor = "right", y = 0, yanchor = "bottom") %>%
      animation_slider(currentvalue = list(prefix = "YEAR ", font = list(color="red")))
    
  })
}