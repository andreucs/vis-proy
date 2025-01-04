render_violin_plot <- function(output, input, turismo_receptor, hex_prov) {
  output$violin_plot <- renderPlotly({

    clickData <- event_data("plotly_click", source = "hexmap_source")

    if (is.null(clickData)) {
     
      total_data <- turismo_receptor |> 
        filter(AÑO == input$year)
      
      fig <- total_data |> 
        plot_ly(
          x = ~ESTANCIA_MEDIA,
          y = ~MES_COD,
          split = ~MES_COD,
          type = 'violin',
          orientation = "h",
          text = ~paste("Estancia Media: ", ESTANCIA_MEDIA, "<br>País de Origen: ", PAIS_ORIGEN),
          hoverinfo = "text",
          box = list(
            visible = TRUE
          ),
          meanline = list(
            visible = TRUE
          )
        ) |> 
        layout(
          xaxis = list(
            title = "Estancia Media"
          ),
          yaxis = list(
            title = "Mes",
            categoryorder = "array",
            categoryarray = rev(c("Ene", "Feb", "Mar", "Abr", "May", "Jun", 
                                  "Jul", "Ago", "Sep", "Oct", "Nov", "Dic")),
            zeroline = FALSE
          )
        ) |> hide_legend()
      
      return(fig)
    }
    

    clicked_x <- clickData$x
    clicked_y <- clickData$y
    

    selected_province <- hex_prov |> 
      filter(round(X, 4) == round(clicked_x, 4) & round(Y, 4) == round(clicked_y, 4)) |> 
      pull(PROVINCIA_DESTINO)
    

    if (length(selected_province) == 0) {
      return(plot_ly() |> layout(title = "No se encontró ninguna provincia"))
    }
    #selected_var <- rlang::sym(input$var)
    
    

    province_data <- turismo_receptor |> 
      filter(AÑO == input$year, PROVINCIA_DESTINO == selected_province) |>
      select(MES_COD, ESTANCIA_MEDIA, PAIS_ORIGEN)
    
    print(province_data)
    if (nrow(province_data) == 0) {
      return(plot_ly() |> layout(title = paste("No hay datos disponibles para la provincia:", selected_province)))
    }
    
    mean_data <- province_data |> 
      group_by(MES_COD) |> 
      summarise(Media = mean(ESTANCIA_MEDIA, na.rm = TRUE))
    
    print(mean_data)

    fig <- province_data |> 
      plot_ly(
        x = ~ESTANCIA_MEDIA,
        y = ~MES_COD,
        split = ~MES_COD,
        type = 'violin',
        orientation = "h",
        text = ~paste(input$var, ": ", ESTANCIA_MEDIA, "<br>País de Origen: "),
        hoverinfo = "text",
        box = list(
          visible = TRUE
        ),
        meanline = list(
          visible = TRUE
        )
      )
    

    fig <- fig |> 
      layout(
        xaxis = list(
          title = "Estancia Media"
        ),
        yaxis = list(
          title = "Mes",
          categoryorder = "array",
          categoryarray = rev(c("Ene", "Feb", "Mar", "Abr", "May", "Jun", 
                                "Jul", "Ago", "Sep", "Oct", "Nov", "Dic")),
          zeroline = FALSE
        )
      ) |> hide_legend()
    

    fig
  })
}