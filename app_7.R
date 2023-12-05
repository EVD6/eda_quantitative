library(shiny)
library(shinythemes)

stunting <- c(31.2, 21.1, 25.2, 17, 18, 18.6, 19.8, 15.2, 18.5, 15.4, 14.8, 20.2, 20.8, 16.4, 19.2, 20, 8, 32.7,
              35.3, 27.8, 26.9, 24.6, 23.9, 22.1, 20.5, 28.2, 27.2, 27.7, 23.8, 35, 26.1, 26.1, 30, 34.6)
IPM <- c(72.80, 72.71, 73.26 ,73.52, 72.14, 70.90, 72.16, 70.45, 72.24, 76.46, 81.65, 73.12, 72.79, 80.64, 72.75,
         73.32, 76.44, 69.46, 65.90, 68.63, 71.63, 71.84, 77.44, 71.83, 73.81, 70.28, 72.82, 72.23, 69.81, 66.92,
         70.22, 69.47, 65.89, 61.39)
lk <- c(159, 160, 161, 175, 180, 160, 163, 175, 179, 170)
pr <- c(149, 160, 165, 165, 155, 155, 155, 159, 164, 162)
TB <- data.frame("Laki2" = lk , "Perempuan" = pr)

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
  title = "Dua Variabel",
  sidebarLayout(
    sidebarPanel(
      radioButtons("data2_source", "Sumber Data:",
                   choices = c("Unggah Dokumen", "Data Tersedia")),
      conditionalPanel(
        condition = "input.data2_source == 'Unggah Dokumen'",
        fileInput("file2", "Masukkan Dokumen CSV", multiple = T, accept = ".csv"),
        selectInput("var1_fl", "Pilih Variabel 1", choices = c(not_sel)),
        selectInput("var2_fl", "Pilih Variabel 2", choices = c(not_sel)),
        radioButtons("pl3", "Pilih Tipe Plot",
                     choices = c(Histogram = "Histogram", Boxplot = "Boxplot", Dotplot = "Dotplot"))
      ),
      conditionalPanel(
        condition = "input.data2_source == 'Data Tersedia'",
        selectInput("dataset2", "Pilih Data", choices = c("Tinggi Badan (cm)")),
        selectInput("var1_dt", "Pilih Variabel 1", choices = c(not_sel)),
        selectInput("var2_dt", "Pilih Variabel 2", choices = c(not_sel)),
        radioButtons("pl4", "Pilih Tipe Plot",
                     choices = c(Histogram = "Histogram", Boxplot = "Boxplot", Dotplot = "Dotplot"))
      )
    ),
    mainPanel(
      tableOutput("num2_summary_table"),
      plotOutput("plot2")
    )
  )
)

ui <- navbarPage(
  title = strong("Eksplorasi Data Kuantitatif"),
  theme = shinytheme('simplex'),
  satu_page,
  dua_page,
  about_page
)

server <- function(input, output, session) {
  
  # fungsi reactive untuk sumber data
  dtSource <- reactive({input$data_source})
  dtSource2 <- reactive({input$data2_source})
    
  # unggah dokumen
  ## fungsi reactive untuk pembacaan dokumen
  fileInput <- reactive({
    req(input$file)
    lapply(input$file$datapath, read.csv)
  })
  file2Input <- reactive({
    req(input$file2)
    lapply(input$file2$datapath, read.csv)
  })
  
  ## fungsi observe untuk nama variabel
  observe({updateSelectInput(session, "variable", choices = colnames(fileInput()[[1]]))})
  observe({updateSelectInput(session, "var1_fl", choices = colnames(file2Input()[[1]]))})
  observe({updateSelectInput(session, "var2_fl", choices = colnames(file2Input()[[1]]))})
  
  ## fungsi reactive variabel
  varInput <- reactive({input$variable})
  var1flInput <- reactive({input$var1_fl})
  var2flInput <- reactive({input$var2_fl})
  
  
  # dataset tersedia
  ## fungsi reactive untuk pemilihan dataset tersedia
  datasetInput <- reactive({
    switch(input$dataset,
           "Prevalensi Stunting (%)" = stunting,
           "IPM" = IPM)
  })
  dataset2Input <- reactive({
    switch(input$dataset2,
           "Tinggi Badan (cm)" = TB)
  })
  
  ## fungsi observe nama variabel
  observe({updateSelectInput(session, "var1_dt", choices = colnames(dataset2Input()))})
  observe({updateSelectInput(session, "var2_dt", choices = colnames(dataset2Input()))})
  
  ## fungsi reactive variabel
  var1dtInput <- reactive({input$var1_dt})
  var2dtInput <- reactive({input$var2_dt})
  
  
  # fungsi reactive untuk memilih tipe plot
  plot1Input <- reactive({input$pl1})
  plot2Input <- reactive({input$pl2})
  plot3Input <- reactive({input$pl3})
  plot4Input <- reactive({input$pl4})
  
  # output untuk tabel statistik deskriptif satu variabel
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
      data.frame("Ukuran" = ukuran, "Rata2" = rata2, "Dev.Std" = std, "Terkecil" = min,
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
      data.frame("Ukuran" = ukuran, "Rata2" = rata2, "Dev.Std" = std, "Terkecil" = min,
                   "Q1" = q1, "Nilai.Tengah" = med, "Q3" = q3, "Terbesar" = max)
    }
  })
  
  # output plot satu variabel
  output$plot <- renderPlot({
    if(dtSource() == "Unggah Dokumen"){
      df <- data.frame(fileInput())
      var <- df[, varInput()]
      if(plot1Input() == "Histogram")
         hist(var, col = 'gray', main = paste("Histogram", input$variable), xlab = paste(input$variable))
      else if(plot1Input() == "Boxplot")
         boxplot(var, main = paste("Boxplot", input$variable), horizontal = TRUE)
      else if(plot1Input() == "Dotplot")
         stripchart(var, method = "stack", pch = 21, bg = "gray", col = "black", cex = 5, ylim = c(0,10),
                 main = paste("Dotplot", input$variable), xlab = paste(input$variable))
    } else if(dtSource() == "Data Tersedia"){
      dataset <- datasetInput()
      if(plot2Input() == "Histogram")
        hist(dataset, col = 'gray', main = paste("Histogram", input$dataset), xlab = paste(input$dataset))
      else if(plot2Input() == "Boxplot")
        boxplot(dataset, main = paste("Boxplot", input$dataset), horizontal = TRUE)
      else if(plot2Input() == "Dotplot")
        stripchart(dataset, method = "stack", pch = 21, bg = "gray", col = "black", cex = 5, ylim = c(0,5),
                   main = paste("Dotplot", input$dataset), xlab = paste(input$dataset))
    }
  })
  
  # output untuk tabel statistik deskriptif dua variabel
  output$num2_summary_table <- renderTable({
    if(dtSource2() == "Unggah Dokumen"){
      df <- data.frame(file2Input())
      var_1 <- df[, var1flInput()]
      ukuran_1 <- length(var_1)
      rata2_1 <- mean(var_1)
      std_1 <- sd(var_1)
      min_1 <- min(var_1)
      q1_1 <- quantile(var_1, probs = 0.25)
      med_1 <- quantile(var_1, probs = 0.5)
      q3_1 <- quantile(var_1, probs = 0.75)
      max_1 <- max(var_1)
      var_2 <- df[, var2flInput()]
      ukuran_2 <- length(var_2)
      rata2_2 <- mean(var_2)
      std_2 <- sd(var_2)
      min_2 <- min(var_2)
      q1_2 <- quantile(var_2, probs = 0.25)
      med_2 <- quantile(var_2, probs = 0.5)
      q3_2 <- quantile(var_2, probs = 0.75)
      max_2 <- max(var_2)
      data.frame("Ukuran" = c(ukuran_1, ukuran_2), "Rata2" = c(rata2_1, rata2_2), "Dev.Std" = c(std_1, std_2),
                 "Terkecil" = c(min_1, min_2), "Q1" = c(q1_1, q1_2), "Nilai.Tengah" = c(med_1, med_2),
                 "Q3" = c(q3_1, q3_2), "Terbesar" = c(max_1, max_2))
    } else if(dtSource2() == "Data Tersedia"){
      df2 <- data.frame(dataset2Input())
      var_1 <- df2[, var1dtInput()]
      ukuran_1 <- length(var_1)
      rata2_1 <- mean(var_1)
      std_1 <- sd(var_1)
      min_1 <- min(var_1)
      q1_1 <- quantile(var_1, probs = 0.25)
      med_1 <- quantile(var_1, probs = 0.5)
      q3_1 <- quantile(var_1, probs = 0.75)
      max_1 <- max(var_1)
      var_2 <- df[, paste("var2dtInput()")]
      ukuran_2 <- length(var_2)
      rata2_2 <- mean(var_2)
      std_2 <- sd(var_2)
      min_2 <- min(var_2)
      q1_2 <- quantile(var_2, probs = 0.25)
      med_2 <- quantile(var_2, probs = 0.5)
      q3_2 <- quantile(var_2, probs = 0.75)
      max_2 <- max(var_2)
      data.frame("Ukuran" = c(ukuran_1, ukuran_2), "Rata2" = c(rata2_1, rata2_2), "Dev.Std" = c(std_1, std_2),
                 "Terkecil" = c(min_1, min_2), "Q1" = c(q1_1, q1_2), "Nilai.Tengah" = c(med_1, med_2),
                 "Q3" = c(q3_1, q3_2), "Terbesar" = c(max_1, max_2))
    }
  })
  
  # output plot dua variabel
  output$plot2 <- renderPlot({
    if(dtSource2() == "Unggah Dokumen"){
      df <- data.frame(file2Input())
      var1 <- df[, var1flInput()]
      var2 <- df[, var2flInput()]
      if(plot3Input() == "Histogram"){
        par(mfrow = c(2, 1))
        hist(var1, col = 'gray', main = paste("Histogram", input$var1_fl), xlab = paste(input$var1_fl))
        hist(var2, col = 'gray', main = paste("Histogram", input$var2_fl), xlab = paste(input$var1_fl))
      } else if(plot3Input() == "Boxplot"){
        par(mfrow = c(2, 1))
        boxplot(var1, main = paste("Boxplot", input$var1_fl), horizontal = TRUE)
        boxplot(var2, main = paste("Boxplot", input$var2_fl), horizontal = TRUE)
      } else if(plot3Input() == "Dotplot"){
        par(mfrow = c(2, 1))
        stripchart(var1, method = "stack", pch = 21, bg = "gray", col = "black", cex = 3, ylim = c(0,10),
                   main = paste("Dotplot", input$var1_fl), xlab = paste(input$var1_fl))
        stripchart(var2, method = "stack", pch = 21, bg = "gray", col = "black", cex = 3, ylim = c(0,10),
                   main = paste("Dotplot", input$var2_fl), xlab = paste(input$var2_fl))
      }
    } else if(dtSource2() == "Data Tersedia"){
      dataset <- dataset2Input()
      if(plot4Input() == "Histogram")
        hist(dataset, col = 'gray', main = paste("Histogram", input$dataset), xlab = paste(input$dataset))
      else if(plot4Input() == "Boxplot")
        boxplot(dataset, main = paste("Boxplot", input$dataset), horizontal = TRUE)
      else if(plot4Input() == "Dotplot")
        stripchart(dataset, method = "stack", pch = 21, bg = "gray", col = "black", cex = 5, ylim = c(0,5),
                   main = paste("Dotplot", input$dataset), xlab = paste(input$dataset))
    }
  })
}

shinyApp(ui = ui, server = server)