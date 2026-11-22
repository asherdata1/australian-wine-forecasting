library(shiny)
library(fpp3)
library(readr)

options(shiny.autoreload = TRUE)

source("preprocessing.R")

wine_colors <- c(
  "Red" = "#B22222",
  "Rose" = "#E78AA6",
  "Sweet White" = "#8064B0",
  "Dry White" = "#D4A72C",
  "Sparkling" = "#3D9C9A",
  "Fortified" = "#8B5A2B"
)

# User interface
ui <- fluidPage(
  titlePanel("Australian Wine Sales"),

  sidebarLayout(
    sidebarPanel(
      selectInput(
        "varietals",
        "Select Wine Varietals:",
        choices = unique(wine_ts$Varietal),
        selected = "Red",
        multiple = TRUE
      ),
      sliderInput(
        "dates",
        "Select Analysis Period:",
        min = 1980,
        max = 1994,
        value = c(1980, 1994),
        step = 1,
        sep = "",
        width = "100%"
      ),
      sliderInput(
        "train_pct",
        "Training data (%)",
        min = 60,
        max = 90,
        value = 80,
        step = 5
      ),
      numericInput(
        "horizon",
        "Forecast horizon (months):",
        value = 12,
        min = 1,
        max = 36,
        step = 1
      ),
      hr(),
      h3("Training Model Specifications"),
      uiOutput("model_summary_ui")
    ),
    mainPanel(
      fluidRow(
        column(
          6,
          checkboxGroupInput(
            "models",
            "Display Forecast Models:",
            choices = c("TSLM", "ETS", "ARIMA"),
            selected = c("TSLM", "ETS", "ARIMA"),
            inline = TRUE
          )
        ),
        column(
          6,
          checkboxGroupInput(
            "interval_levels",
            "Prediction Intervals:",
            choices = c("80%" = 80, "95%" = 95),
            selected = c(80, 95),
            inline = TRUE
          )
        )
      ),
      sliderInput(
        "plot_zoom",
        "Forecast Zoom Level:",
        min = 0,
        max = 100,
        value = 0,
        step = 10,
        width = "100%"
      ),
      tabsetPanel(
        tabPanel(
          "Historical Sales",
          plotOutput("sales_plot")
        ),

        tabPanel(
          "Validation Forecasts",
          uiOutput("validation_sections_ui")
        ),

        tabPanel(
          "Future Forecasts",
          uiOutput("future_sections_ui")
        ),

        tabPanel(
          "About",
          h3("About This Application"),
          p(
            "This application explores monthly Australian wine sales ",
            "from January 1980 through December 1994 across six wine varietals."
          ),

          h4("Missing Data"),
          p(
            "Rose sales for July and August 1994 are estimated from June 1994 ",
            "using median July/June and August/June ratios from 1989–1993. ",
            "Rose validation metrics may therefore include estimated observations."
          ),

          h4("Forecasting Methods"),
          p(
            "Three forecasting approaches are compared: time series linear ",
            "regression (TSLM) with trend and seasonality, exponential ",
            "smoothing (ETS), and autoregressive integrated moving average ",
            "(ARIMA)."
          ),

          h4("Model Evaluation"),
          p(
            "Models are trained using a chronological split of the selected ",
            "analysis period. Performance is evaluated using RMSE, MAE, ",
            "and MAPE on both training and validation observations. ",
            "The dashed line marks the start of validation. Sidebar specifications ",
            "describe the training models. ",
            "Future forecasts are generated using models refitted on ",
            "the full selected period."
          ),

          h4("Reproducing Results"),
          p(
            "Select the desired wine varietals and analysis period, ",
            "adjust the training percentage and forecast horizon, ",
            "then choose which models and prediction intervals to display. ",
            "The forecast zoom control changes only the visible plot range, ",
            "not the underlying model estimates or accuracy metrics."
          )
        )
      )
    )
  )
)



# Server logic
server <- function(input, output, session) {
  check_history <- function(data) {
    req(nrow(data) > 0)
    validate(need(
      n_distinct(data$Month) >= 24,
      "At least 24 months are needed for modeling. Extend the analysis period or increase the training percentage."
    ))
  }

  filtered_data <- reactive({
    wine_ts |>
      filter(
        Varietal %in% input$varietals,
        year(as.Date(Month)) >= input$dates[1],
        year(as.Date(Month)) <= input$dates[2]
      )
  })

  # Split each series chronologically.
  split_data <- reactive({
    filtered_data() |>
    group_by_key() |>
    mutate(
      n_train = floor(n() * input$train_pct / 100),
      Set = if_else(row_number() <= n_train, "Training", "Validation")
    ) |>
    ungroup()
  })

  training_data <- reactive({
    split_data() |>
      filter(Set == "Training") |>
      select(Month, Varietal, Sales)
  })

  validation_data <- reactive({
    split_data() |>
      filter(Set == "Validation") |>
      select(Month, Varietal, Sales)
  })

  # Fit models on the training window.
  fitted_models <- reactive({
    check_history(training_data())
    training_data() |>
      model(
        TSLM = TSLM(Sales ~ trend() + season()),
        ETS = ETS(Sales),
        ARIMA = ARIMA(Sales)
      )
  })

  # Refit models on the full selected period.
  future_models <- reactive({
    check_history(filtered_data())
    filtered_data() |>
      model(
        TSLM = TSLM(Sales ~ trend() + season()),
        ETS = ETS(Sales),
        ARIMA = ARIMA(Sales)
      )
  })

  # Forecast the held-out validation months.
  validation_forecasts <- reactive({
    fitted_models() |>
      forecast(
        new_data = validation_data() |>
          select(Month, Varietal)
      )
  })

  training_accuracy <- reactive({
    fitted_models() |>
      accuracy() |>
      select(Varietal, .model, RMSE, MAE, MAPE) |>
      arrange(Varietal, RMSE)
  })

  # Score forecasts against validation observations.
  validation_accuracy <- reactive({
    validation_forecasts() |>
      accuracy(validation_data()) |>
      select(Varietal, .model, RMSE, MAE, MAPE) |>
      arrange(Varietal, RMSE)
  })

  model_accuracy <- reactive({
    bind_rows(
      training_accuracy() |> mutate(Period = "Training"),
      validation_accuracy() |> mutate(Period = "Validation")
    ) |>
    select(Varietal, .model, Period, RMSE, MAE, MAPE) |>
    arrange(Varietal, .model, Period)
  })

  accuracy_comparison <- reactive({
    training_accuracy() |>
      rename(
        Train_RMSE = RMSE,
        Train_MAE = MAE,
        Train_MAPE = MAPE
      ) |>
      inner_join(
        validation_accuracy() |>
          rename(
            Valid_RMSE = RMSE,
            Valid_MAE = MAE,
            Valid_MAPE = MAPE
          ),
        by = c("Varietal", ".model")
      ) |>
      arrange(Varietal, Valid_RMSE)
  })

  future_forecasts <- reactive({
    future_models() |>
      forecast(h = input$horizon)
  })

  # Zoom the plots without changing model inputs.
  zoom_start <- reactive({
    months <- sort(unique(filtered_data()$Month))

    total_months <- length(months)

    months_displayed <- round(
      total_months - (total_months - min(12, total_months)) *
        input$plot_zoom / 100
    )

    months[total_months - months_displayed + 1]
  })

  output$sales_plot <- renderPlot({
    filtered_data() |>
      ggplot(aes(x = Month, y = Sales, color = Varietal)) +
      geom_line() +
      scale_color_manual(values = wine_colors) +
      labs(
        title = "Monthly Australian Wine Sales",
        x = "Month",
        y = "Sales",
        color = "Varietal"
      ) +
      theme_minimal()
  })

  output$model_summary_ui <- renderUI({
    tagList(
      lapply(input$varietals, function(v) {
        tagList(
          h4(v),
          tableOutput(paste0("model_", make.names(v)))
        )
      })
    )
  })

  output$validation_sections_ui <- renderUI({
    lapply(input$varietals, function(v) {
      tagList(
        h3(v),

        plotOutput(
          paste0("validation_plot_", make.names(v)),
          height = "300px"
        ),

        h4("Model Accuracy Comparison"),

        tableOutput(
          paste0("accuracy_", make.names(v))
        ),

        hr()
      )
    })
  })

  output$future_sections_ui <- renderUI({
    tagList(
      p(
        "Accuracy metrics reflect historical training and validation performance. ",
        "Future forecasts are generated from models refitted on the full selected period."
      ),

      lapply(input$varietals, function(v) {
        tagList(
          h3(v),

          plotOutput(
            paste0("future_plot_", make.names(v)),
            height = "300px"
          ),

          h4("Historical Model Accuracy"),

          tableOutput(
            paste0("future_accuracy_", make.names(v))
          ),

          hr()
        )
      })
    )
  })

  # Create validation plots and tables for each varietal.
  observe({
    lapply(input$varietals, function(v) {
      local({
        varietal <- v

        output[[paste0("validation_plot_", make.names(varietal))]] <- renderPlot({

          actual <- filtered_data() |>
            filter(Varietal == varietal)

          predictions <- validation_forecasts() |>
            filter(
              Varietal == varietal,
              .model %in% input$models
            )

          ggplot() +
            geom_line(
              data = actual,
              aes(x = Month, y = Sales, color = "Actual")
            ) +
            geom_vline(
              data = tibble(Month = min(validation_data()$Month)),
              aes(xintercept = Month),
              linetype = "dashed", color = "gray60"
            ) +
            geom_line(
              data = predictions,
              aes(x = Month, y = .mean, color = .model),
              linewidth = 0.8
            ) +
            scale_color_manual(
              values = c(
                "Actual" = "gray40",
                "TSLM" = "#2563EB",
                "ETS" = "#0D9488",
                "ARIMA" = "#C026D3"
              )
            ) +
            labs(
              title = paste(varietal, "— Validation Forecast"),
              x = "Month",
              y = "Sales",
              color = "Series"
            ) +
            {
              coord_cartesian(
                xlim = c(
                  zoom_start(),
                  max(validation_data()$Month)
                )
              )
            } +
            theme_minimal()
        })

        output[[paste0("accuracy_", make.names(varietal))]] <- renderTable({
          accuracy_comparison() |>
            filter(Varietal == varietal) |>
            select(
              Model = .model,
              "Train RMSE" = Train_RMSE,
              "Train MAE" = Train_MAE,
              "Train MAPE" = Train_MAPE,
              "Valid RMSE" = Valid_RMSE,
              "Valid MAE" = Valid_MAE,
              "Valid MAPE" = Valid_MAPE
            )
        }, digits = 2)
      })
    })
  })

  # Display training-model specifications for each varietal.
  observe({
    lapply(input$varietals, function(v) {
      local({
        varietal <- v

        output[[paste0("model_", make.names(varietal))]] <- renderTable({
          fitted_models() |>
            filter(Varietal == varietal) |>
            as_tibble() |>
            pivot_longer(
              cols = c(TSLM, ETS, ARIMA),
              names_to = "Model",
              values_to = "Specification"
            ) |>
            mutate(Specification = as.character(Specification)) |>
            select(Model, Specification)
        })
      })
    })
  })

  # Create future plots and tables for each varietal.
  observe({
    lapply(input$varietals, function(v) {
      local({
        varietal <- v

        output[[paste0("future_plot_", make.names(varietal))]] <- renderPlot({

          actual <- filtered_data() |>
            filter(Varietal == varietal)

          predictions <- future_forecasts() |>
            filter(
              Varietal == varietal,
              .model %in% input$models
            )

          intervals <- if (nrow(predictions) > 0) {
            predictions |>
              hilo(level = c(80, 95)) |>
              unpack_hilo(c("80%", "95%"))
          } else {
            NULL
          }

          ggplot() +
            geom_line(
              data = actual,
              aes(x = Month, y = Sales, color = "Actual")
            ) +
            {
              if (!is.null(intervals) && "95" %in% input$interval_levels) {
                geom_ribbon(
                  data = intervals,
                  aes(
                    x = Month,
                    ymin = `95%_lower`,
                    ymax = `95%_upper`,
                    fill = .model,
                    group = .model
                  ),
                  alpha = 0.10
                )
              }
            } +
            {
              if (!is.null(intervals) && "80" %in% input$interval_levels) {
                geom_ribbon(
                  data = intervals,
                  aes(
                    x = Month,
                    ymin = `80%_lower`,
                    ymax = `80%_upper`,
                    fill = .model,
                    group = .model
                  ),
                  alpha = 0.20
                )
              }
            } +
            geom_line(
              data = predictions,
              aes(x = Month, y = .mean, color = .model),
              linewidth = 0.8
            ) +
            scale_color_manual(
              values = c(
                "Actual" = "gray40",
                "TSLM" = "#2563EB",
                "ETS" = "#0D9488",
                "ARIMA" = "#C026D3"
              )
            ) +
            scale_fill_manual(
              values = c(
                "TSLM" = "#2563EB",
                "ETS" = "#0D9488",
                "ARIMA" = "#C026D3"
              ),
              guide = "none"
            ) +
            labs(
              title = paste(varietal, "— Future Sales Forecast"),
              x = "Month",
              y = "Sales",
              color = "Series"
            ) +
            {
              if (!is.null(zoom_start())) {
                coord_cartesian(
                  xlim = c(
                    zoom_start(),
                    max(future_forecasts()$Month)
                  )
                )
              }
            } +
            theme_minimal()
        })

        output[[paste0("future_accuracy_", make.names(varietal))]] <- renderTable({
          accuracy_comparison() |>
            filter(Varietal == varietal) |>
            select(
              Model = .model,
              "Train RMSE" = Train_RMSE,
              "Train MAE" = Train_MAE,
              "Train MAPE" = Train_MAPE,
              "Valid RMSE" = Valid_RMSE,
              "Valid MAE" = Valid_MAE,
              "Valid MAPE" = Valid_MAPE
            )
        }, digits = 2)
      })
    })
  })
}



# Launch application
shinyApp(ui, server)
