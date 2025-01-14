library(readr)
library(plotly)
library(dplyr)
library(sf)
library(mapSpain)
library(bslib)
library(bsicons)
library(forecast)
library(tidyr)
ocup <- read.csv("data/ocupacion.csv")

ocup_nacional <- ocup |>
                 filter(LUGAR_RESIDENCIA == "Total") |>
                 select(AÑO, MES, PROVINCIA, PERNOCTACIONES) |>
                 group_by(AÑO, MES) |>
                 summarise(pernoc=sum(PERNOCTACIONES, na.rm = TRUE), .groups = "drop")

data <- ocup_nacional
data$Fecha <- as.Date(paste(data$AÑO, data$MES, "01", sep = "-"))

data_filtered <- ocup_nacional[ocup_nacional$AÑO < 2020, ]

ts_data <- ts(data_filtered$pernoc, start = c(1999, 1), frequency = 12)

modelo <- Arima(ts_data, order = c(1, 0, 2), seasonal = c(0, 1, 1), include.drift = TRUE)


predicciones <- forecast(modelo, h = 42)

data1 <- data |> 
        filter(Fecha >= "2019-1-1", Fecha <= "2023-6-1")

pred <- data.frame(
  fecha = seq(from = as.Date("2020-01-01"), by = "month", length.out = 42),
  pernoc = predicciones$mean
)


data1_filtrado <- data1 %>%
  filter(Fecha %in% pred$fecha)

datos_combinados <- merge(pred, data1_filtrado, by.x = "fecha", by.y = "Fecha", all.x = TRUE)


datos_combinados$ymin <- pmin(datos_combinados$pernoc.x, datos_combinados$pernoc.y, na.rm = TRUE)
datos_combinados$ymax <- pmax(datos_combinados$pernoc.x, datos_combinados$pernoc.y, na.rm = TRUE)


arima_data <- merge(data1, pred, by.x = "Fecha", by.y = "fecha", all.x = TRUE) |>
             merge(datos_combinados, by.x = "Fecha", by.y = "fecha", all.x = TRUE)

arima_data <- arima_data |> select(Fecha, AÑO.x, MES.x, pernoc.x.x, pernoc.x.y, ymin, ymax) |>
    rename("AÑO" = "AÑO.x", "MES" = "MES.x", "Real" = "pernoc.x.x", "Predicho" = "pernoc.x.y")


arima_data |> write.csv2("data/arima_data.csv", row.names = FALSE)











