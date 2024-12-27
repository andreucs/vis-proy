get_filtered_data <- function(input, completo) {
  reactive({
    req(input$year)
    completo |> filter(AÑO == input$year)
  })
}