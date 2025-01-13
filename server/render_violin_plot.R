render_violin_plot <- function(output, input, turismo_receptor, hex_prov, color = "#2d7d8c") {
  output$violin_plot <- renderPlotly({

    clickData <- event_data("plotly_click", source = "hexmap_source")
    selected_var <- rlang::sym(input$var)

    if (is.null(clickData)) {
      if (input$var == "ESTANCIA_MEDIA") {
        total_data <- turismo_receptor |> 
          filter(AÑO == input$year)
        
        fig <- total_data |> 
          plot_ly(
            x = ~MES_COD,
            y = ~ESTANCIA_MEDIA,
            split = ~MES_COD,
            type = 'violin',
            orientation = "v",
            color = I("#88009d"),
            fillcolor = I(color),
            line = list(color = color),
            text = ~paste("Estancia Media: ", ESTANCIA_MEDIA, "<br>País de Origen: ", PAIS_ORIGEN),
            hoverinfo = "text",
            points = F,
            box = list(
              visible = TRUE
            ),
            meanline = list(
              visible = TRUE
            )
          ) |> 
          layout(
            title = list(
              text = paste("Distribución de los turistas por país de residencia en España durante", input$year),
              x=0,
              font=list(
                size=14
              )
              ),
            xaxis = list(
              title = ""
            ),
            yaxis = list(
              title = "",
              categoryorder = "array",
              categoryarray = rev(c("Ene", "Feb", "Mar", "Abr", "May", "Jun", 
                                    "Jul", "Ago", "Sep", "Oct", "Nov", "Dic")),
              zeroline = FALSE
            )
          ) |> hide_legend() |>
          config(
            modeBarButtonsToRemove = c(
              "zoom2d", "pan2d", "select2d", "lasso2d", 
              "zoomIn2d", "zoomOut2d", "autoScale2d", 
              "hoverClosestCartesian", "hoverCompareCartesian"
            )
          )
        
        return(fig)
      } else if (input$var == "TURISTAS") {

        d <- turismo_receptor |> 
          filter(AÑO == input$year) |>
          group_by(MES_COD) |>
          summarise(vis = sum(TURISTAS), na.rm=TRUE)
        
        max_sum_turistas <- d |>
          arrange(desc(vis)) |>
          slice(1) |>
          pull(vis)
        
        fig <- d |>
          plot_ly() |>
          add_lines(
            x=~MES_COD,
            y=~vis,
            color = I(color)
          ) |>
          add_markers(
            x=~MES_COD,
            y=~vis,
            text=~vis,
            hoverinfo = "text",
            color = I(color)
          ) |> hide_legend() |>
          layout(
            title = list(
              text = paste("Número de turistas por mes en España en", input$year),
            x=0,
            font=list(
              size=14
            )),
            xaxis = list(
              title = "",
              showgrid=FALSE,
              zeroline=FALSE
            ),
            yaxis = list(
              title = "",
              range = c(0, max_sum_turistas*1.25),
              zeroline=TRUE,
              zerolinecolor="rgba(128, 128, 128, 0.20)",
              zerolinewidth=2,
              tickpadding = 35)
          ) |>
          config(
            modeBarButtonsToRemove = c(
              "zoom2d", "pan2d", "select2d", "lasso2d", 
              "zoomIn2d", "zoomOut2d", "autoScale2d", 
              "hoverClosestCartesian", "hoverCompareCartesian"
            )
          )
        
        return(fig)
      }
    }
    

    clicked_x <- clickData$x
    clicked_y <- clickData$y
    

    selected_province <- hex_prov |> 
      filter(round(X, 4) == round(clicked_x, 4) & round(Y, 4) == round(clicked_y, 4)) |> 
      pull(PROVINCIA_DESTINO)
    

    if (length(selected_province) == 0) {
      return(plot_ly() |> layout(title = "Haz click sobre el texto correspondiente de cada provincia",
                                                                                    xaxis= list(showgrid=FALSE,
                                                                                                zeroline=FALSE,
                                                                                                showticklabels=FALSE),
                                                                                    yaxis=list(showgrid=FALSE,
                                                                                               zeroline=FALSE,
                                                                                               showticklabels=FALSE)))
    }
    

    province_data <- turismo_receptor |> 
      filter(AÑO == input$year, PROVINCIA_DESTINO == selected_province) |>
      select(MES_COD, !!selected_var, PAIS_ORIGEN)
    
    if (nrow(province_data) == 0) {
      return(plot_ly() |> layout(title = paste("No hay datos disponibles para la provincia:", selected_province),
                                 xaxis= list(showgrid=FALSE,
                                             zeroline=FALSE,
                                             showticklabels=FALSE),
                                 yaxis=list(showgrid=FALSE,
                                            zeroline=FALSE,
                                            showticklabels=FALSE)))
    }
    
    if ((input$var == "ESTANCIA_MEDIA") && (!is.null(clickData))) {
      
      fig <- province_data |>
        plot_ly() |>
        add_trace(
          x = ~MES_COD,
          y = ~.data[["ESTANCIA_MEDIA"]],
          split = ~MES_COD,
          type = 'violin',
          orientation = "v",
          color = I("#88009d"),
          fillcolor = I(color),
          line = list(color = color),
          text = ~paste(input$var, ": ", .data[[input$var]], "<br>País de Origen: ", PAIS_ORIGEN),
          hoverinfo = "text",
          box = list(
            visible = TRUE
          ),
          meanline = list(
            visible = TRUE
          )
        ) |> 
        layout(
          title = list(
            text = paste("Distribución de los turistas por país de residencia en", selected_province, "en durante", input$year),
            x=0,
            font=list(
              size=14
            )),
          
            
          xaxis = list(
            title = "",
            categoryorder = "array",
            categoryarray = (c("Ene", "Feb", "Mar", "Abr", "May", "Jun", 
                                "Jul", "Ago", "Sep", "Oct", "Nov", "Dic")),
            showgrid=FALSE
          ),
          yaxis = list(
            title = "",
            zeroline = FALSE,
            showgrid=TRUE
          )
        ) |> hide_legend() |>
        config(
          modeBarButtonsToRemove = c(
            "zoom2d", "pan2d", "select2d", "lasso2d", 
            "zoomIn2d", "zoomOut2d", "autoScale2d", 
            "hoverClosestCartesian", "hoverCompareCartesian"
          )
        )
      
      
      return(fig)
    }
    
    if (input$var == "TURISTAS") {
      
        d <- province_data |>
          group_by(MES_COD) |>
          summarise(vis = sum(TURISTAS), na.rm=TRUE)
        
        max_sum_turistas <- d |>
          arrange(desc(vis)) |>
          slice(1) |>
          pull(vis)
        
        fig <- d |>
        plot_ly() |>
        add_lines(
          x=~MES_COD,
          y=~vis,
          color = I(color)
        ) |>
        add_markers(
          x=~MES_COD,
          y=~vis,
          text=~vis,
          hoverinfo = "text",
          color = I(color)
        ) |> hide_legend() |>
        layout(
          title = list(
            text = paste("Número de turistas por mes en", selected_province, "en", input$year),
            x=0,
            font=list(
              size=14
            )),
          xaxis = list(
            title = "",
            showgrid=FALSE,
            zeroline=FALSE
          ),
          yaxis = list(
            title = "",
            range = c(0, max_sum_turistas*1.25),
            zeroline=TRUE,
            zerolinecolor="rgba(128, 128, 128, 0.20)",
            zerolinewidth=2,
            tickpadding = 35)
          ) |>
          config(
            modeBarButtonsToRemove = c(
              "zoom2d", "pan2d", "select2d", "lasso2d", 
              "zoomIn2d", "zoomOut2d", "autoScale2d", 
              "hoverClosestCartesian", "hoverCompareCartesian"
            )
          )
        return(fig)
    }
    
  })
}