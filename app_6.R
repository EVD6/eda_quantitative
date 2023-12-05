library(shiny)
library(shinythemes)

stunting <- c(31.2, 21.1, 25.2, 17, 18, 18.6, 19.8, 15.2, 18.5, 15.4, 14.8, 20.2, 20.8, 16.4, 19.2, 20, 8, 32.7,
              35.3, 27.8, 26.9, 24.6, 23.9, 22.1, 20.5, 28.2, 27.2, 27.7, 23.8, 35, 26.1, 26.1, 30, 34.6)
IPM <- c(72.80, 72.71, 73.26 ,73.52, 72.14, 70.90, 72.16, 70.45, 72.24, 76.46, 81.65, 73.12, 72.79, 80.64, 72.75,
         73.32, 76.44, 69.46, 65.90, 68.63, 71.63, 71.84, 77.44, 71.83, 73.81, 70.28, 72.82, 72.23, 69.81, 66.92,
         70.22, 69.47, 65.89, 61.39)
not_sel <- "-"

about_page <- tabPanel(
  title = "Tentang",
  titlePanel("Tentang"),
  "Eksplorasi Data Kuantitatif",
  br(),
  "Fradha Intan Arassah G1501221018",
  br(),
  "Diaztri Hazam G1501221032",
  br(),
  "Firda Aulia Maghfiroh G1501222049"
)

satu_page <- tabPanel(
  title = "Satu Variabel",
  sidebarLayout(
    sidebarPanel(
      radioButtons("data_source", "Sumber Data:",
                   choices = c("Unggah Dokumen", "Data Tersedia")),
      conditionalPanel(
        condition = "input.data_source == 'Unggah Dokumen'",
        fileInput("file", "Masukkan Dokumen CSV", multiple = T, accept = ".csv"),
        selectInput("variable", "Pilih Variabel", choices = c(not_sel)),
        radioButtons("pl1", "Pilih Tipe Plot",
                     choices = c(Histogram = "Histogram", Boxplot = "Boxplot", Dotplot = "Dotplot"))
      ),
      conditionalPanel(
        condition = "input.data_source == 'Data Tersedia'",
        selectInput("dataset", "Pilih Data", choices = c("Prevalensi Stunting (%)", "IPM")),
        radioButtons("pl2", "Pilih Tipe Plot",
                     choices = c(Histogram = "Histogram", Boxplot = "Boxplot", Dotplot = "Dotplot"))
      )
    ),
    mainPanel(
      tableOutput("num1_summary_table"),
      plotOutput("plot")
    )
  )
)

dua_page <- tabPanel(
  title = "Dua Variabel"
)

ui <- navbarPage(
  title = strong("Eksplorasi Data Kuantitatif"),
  theme = shinytheme('simplex'),
  satu_page,
  dua_page,
  about_page
)

server <- function(input, output, session) {
  
  # Reactive Sumber Data
  dtSource <- reactive({
    input$data_source
  })
    
  # Unggah Dokumen
  # Fungsi reactive untuk pembacaan dokumen
  fileInput <- reactive({
    req(input$file)
    lapply(input$file$datapath, read.csv)
  })
  
  # Update Variabel
  observe({
    updateSelectInput(session, "variable", choices = colnames(fileInput()[[1]]))
  })
  
  # Reactive variabel
  varInput <- reactive({
    input$variable
  })
  
  # Fungsi reactive untuk memilih tipe plot
  plot1Input <- reactive({
    input$pl1
  })
  
  # Dataset Tersedia
  # Fungsi reactive untuk pemilihan dataset tersedia
  datasetInput <- reactive({
    switch(input$dataset,
           "Prevalensi Stunting (%)" = stunting,
           "IPM" = IPM)
  })
  
  # Fungsi reactive untuk memilih tipe plot
  plot2Input <- reactive({
    input$pl2
  })
  
  # Output untuk tabel statistik deskriptif
  output$num1_summary_table <- renderTable({
    if(dtSource() == "Unggah Dokumen"){
      df <- data.frame(fileInput())
      var <- df[, varInput()]
      ukuran <- length(var)
      rata2 <- mean(var)
      std <- sd(var)
      min <- min(var)
      q1 <- quantile(var, probs = 0.25)
      med <- quantile(var, probs = 0.5)
      q3 <- quantile(var, probs = 0.75)
      max <- max(var)
      data.frame("Ukuran" = ukuran, "Rata2" = rata2, "Dev Std" = std, "Terkecil" = min,
                 "Q1" = q1, "Nilai.Tengah" = med, "Q3" = q3, "Terbesar" = max)
    } else if(dtSource() == "Data Tersedia"){
      dataset <- datasetInput()
      ukuran <- length(dataset)
      rata2 <- mean(dataset)
      std <- sd(dataset)
      min <- min(dataset)
      q1 <- quantile(dataset, probs = 0.25)
      med <- quantile(dataset, probs = 0.5)
      q3 <- quantile(dataset, probs = 0.75)
      max <- max(dataset)
      data.frame("Ukuran" = ukuran, "Rata2" = rata2, "Dev Std" = std, "Terkecil" = min,
                   "Q1" = q1, "Nilai.Tengah" = med, "Q3" = q3, "Terbesar" = max)
    }
  })
  
  # Output plot
  output$plot <- renderPlot({
    if(dtSource() == "Unggah Dokumen"){
      df <- data.frame(fileInput())
      var <- df[, varInput()]
      if(plot1Input() == "Histogram")
         hist(var, col = 'gray', main = paste("Histogram", input$variable), xlab = paste(input$variable))
      else if(plot1Input() == "Boxplot")
         boxplot(var, main = paste("Boxplot", input$variable), horizontal = TRUE)
      else if(plot1Input() == "Dotplot")
         stripchart(var, method = "stack", pch = 19, ylim = c(0,10),
                 main = paste("Dotplot", input$variable), xlab = paste(input$variable))
    } else if(dtSource() == "Data Tersedia"){
      dataset <- datasetInput()
      if(plot2Input() == "Histogram")
        hist(dataset, col = 'gray', main = paste("Histogram", input$dataset), xlab = paste(input$dataset))
      else if(plot2Input() == "Boxplot")
        boxplot(dataset, main = paste("Boxplot", input$dataset), horizontal = TRUE)
      else if(plot2Input() == "Dotplot")
        stripchart(dataset, method = "stack", pch = 19, ylim = c(0,10),
                   main = paste("Dotplot", input$dataset), xlab = paste(input$dataset))
    }
  })
}

shinyApp(ui = ui, server = server)