ocup <- readxl::read_excel("data/eoh_ccaa.xlsx")

ocup_nacional <- ocup |>
  filter(LUGAR_RESIDENCIA == "Total") |>
  select(AÑO, MES, CCAA, PERNOCTACIONES) |>
  group_by(AÑO, MES) |>
  summarise(pernoc=sum(PERNOCTACIONES, na.rm = TRUE), .groups = "drop")

data <- ocup_nacional
data$Fecha <- as.Date(paste(data$AÑO, data$MES, "01", sep = "-"))


print(data)