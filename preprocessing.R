# Load Australian wine sales data
wine_raw <- read_csv("AustralianWines.csv", show_col_types = FALSE)

# Convert to a monthly tsibble with one series per varietal
wine_ts <- wine_raw |>
  mutate(
    Month = yearmonth(Month, format = "%b-%y") - 1200L,
    Rose = as.numeric(na_if(Rose, "*"))
  ) |>
  pivot_longer(
    cols = -Month,
    names_to = "Varietal",
    values_to = "Sales"
  ) |>
  mutate(Varietal = recode(
    Varietal,
    "Sweet white" = "Sweet White",
    "Dry white" = "Dry White",
    "sparkling" = "Sparkling"
  )) |>
  as_tsibble(
    key = Varietal,
    index = Month
  )

# Calculate historical June-to-July and June-to-August sales ratios
seasonal_ratios <- wine_ts |>
  filter(
    Varietal == "Rose",
    Month >= yearmonth("1989 Jun"),
    Month <= yearmonth("1993 Aug"),
    month(as.Date(Month)) %in% 6:8
  ) |>
  mutate(
    Year = year(as.Date(Month)),
    MonthName = month(as.Date(Month), label = TRUE, abbr = TRUE)
  ) |>
  as_tibble() |>
  select(Year, MonthName, Sales) |>
  pivot_wider(
    names_from = MonthName,
    values_from = Sales
  ) |>
  mutate(
    July_ratio = Jul / Jun,
    August_ratio = Aug / Jun
  )

# Retrieve June 1994 Rose sales
june_1994 <- wine_ts |>
  filter(
    Varietal == "Rose",
    Month == yearmonth("1994 Jun")
  ) |>
  pull(Sales)

# Estimate missing sales using the median ratios from 1989–1993
imputed_values <- seasonal_ratios |>
  summarise(
    July = june_1994 * median(July_ratio),
    August = june_1994 * median(August_ratio)
  )

# Replace the two missing Rose observations
wine_ts <- wine_ts |>
  mutate(
    Sales = case_when(
      Varietal == "Rose" & Month == yearmonth("1994 Jul") ~ imputed_values$July,
      Varietal == "Rose" & Month == yearmonth("1994 Aug") ~ imputed_values$August,
      TRUE ~ Sales
    )
  )
