<div align="center">

<img width="250" alt="Visualisasi Data Kuantitatif" src="https://www.singlegrain.com/wp-content/uploads/2019/03/SG-5-Powerful-Ways-to-Increase-Sales-by-Leveraging-Data-Effectively.jpg">

# Visualisasi Data Kuantitatif

• [Tentang Dashboard](#memo-tentang-dashboard)
• [Deskripsi Syntax](#computer-deskripsi-syntax)
• [Dokumentasi](#camera_flash-dokumentasi)

</div>

## :bookmark_tabs: Main Menu

- [Tentang Dashboard](#memo-tentang-dashboard)
- [Deskripsi Syntax](#computer-deskripsi-syntax)
- [Dokumentasi](#camera_flash-dokumentasi)
- [Demo](#video_camera-demo)
- [Anggota Tim Pengembang](#fireworks-anggota-tim-pengembang)

## :memo: Tentang Dashboard 
Deskripsi dashboard untuk visualisasi data kuantitatif

<div align="justify">
Dalam project ini menggunakan topik eksplorasi dan visualisasi dari data kuantitatif, dengan data yang digunakan pada project ini adalah data kuantitatif. Data kuantitatif adalah jenis data yang menyatakan atau mengukur jumlah atau kuantitas suatu variabel. Data kuantitatif yang sudah tersedia dalam dashboard ini adalah data prevelensi stunting dan data IPM. Data ini dapat diukur dan dihitung dalam dashboard ini berdasarkan nilai ukuran, mean, standard deviation, nilai terkecil, Q1, median, Q3 dan nilai terbesar.

Eksplorasi data merupakan awal proses untuk mengidentifikasi pola, anomali, dan tren pada data yang mungkin tersembunyi. Hal ini juga melibatkan penggunaan metode statistik dan visualisasi untuk mendapatkan pemahaman yang lebih baik tentang struktur dan karakteristik data. Sementara Visualisasis Data adalah proses representasi grafis dari data, pada dashboard ini digunakan plot histogram, boxplot dan dotplot untuk mempermudah memahami pola, tren, dan relasi di dalam data. Visualisasi juga membantu membuat informasi yang kompleks lebih dapat dipahami dan dapat menjadi alat eksplorasi yang tepat.

Hubungan antara data kuantitatif, visualisasi, dan eksplorasi terletak pada pemanfaatan visualisasi untuk menjelajahi dan memahami data kuantitatif. Melalui visualisasi, maka bisa lebih efektif untuk mengeksplorasi distribusi, pola, dan relasi di antara variabel kuantitatif, juga membantu membuat inferensi dan pengambilan keputusan yang lebih baik berdasarkan data tersebut.

</div>

## :computer: Deskripsi Syntax

- Paket R yang digunakan:

```{r}
library(shiny)
library(shinythemes)
library(shinyjs)
library(colourpicker)
library(plotly)
```

- Mempersipakan data yang tersedia

Akan disediakan 3 data, yaitu data presentase stunting, IPM, dan tinggi badan laki-laki & perempuan
```{r}
stunting <- c(31.2, 21.1, 25.2, 17, 18, 18.6, 19.8, 15.2, 18.5, 15.4, 14.8, 20.2, 20.8, 16.4, 19.2, 20, 8, 32.7,
              35.3, 27.8, 26.9, 24.6, 23.9, 22.1, 20.5, 28.2, 27.2, 27.7, 23.8, 35, 26.1, 26.1, 30, 34.6)
IPM <- c(72.80, 72.71, 73.26 ,73.52, 72.14, 70.90, 72.16, 70.45, 72.24, 76.46, 81.65, 73.12, 72.79, 80.64, 72.75,
         73.32, 76.44, 69.46, 65.90, 68.63, 71.63, 71.84, 77.44, 71.83, 73.81, 70.28, 72.82, 72.23, 69.81, 66.92,
         70.22, 69.47, 65.89, 61.39)
lk <- c(159, 160, 161, 175, 180, 160, 163, 175, 179, 170)
pr <- c(149, 160, 165, 165, 155, 155, 155, 159, 164, 162)
TB <- data.frame("Laki2" = lk , "Perempuan" = pr)
```
- Submenu menggunakan tabPanel Shiny

Terdapat 3 submenu, yaitu submenu Satu Variabel, Dua Variabel, dan Tentang.

Pada Submenu Satu Variabel, akan terdiri dari 2 kondisi yaitu, analisis statistik deskriptif menggunakan data yang dapat di unggah sendiri atau menggunakan data yang tersedia. Terdapat tiga pilihan visualisasi data kuantitatif, yaitu histogram, boxplot, dan dotplot.

```{r}
not_sel <- "-"
satu_page <- tabPanel(
  title = "Satu Variabel",
  sidebarLayout(
    sidebarPanel(
      radioButtons("data_source", strong("Sumber Data:"),
                   choices = c("Unggah Dokumen", "Data Tersedia")),
      conditionalPanel(
        condition = "input.data_source == 'Unggah Dokumen'",
        fileInput("file", strong("Masukkan Dokumen CSV"), multiple = T, accept = ".csv"),
        selectInput("variable", strong("Pilih Variabel"), choices = c(not_sel)),
        tags$hr(),
        radioButtons("pl1", strong("Pilih Tipe Plot"), inline = TRUE,
                     choices = c(Histogram = "Histogram", Boxplot = "Boxplot", Dotplot = "Dotplot"))
      ),
      conditionalPanel(
        condition = "input.data_source == 'Unggah Dokumen' & input.pl1 == 'Histogram'",
        sliderInput("bin1", strong("Pilih Ukuran Binwidth"), min = 5, max = 150, value = 50),
        textInput("jdl_his_1", strong("Masukkan Judul"), "Histogram")
      ),
      conditionalPanel(
        condition = "input.data_source == 'Unggah Dokumen' & input.pl1 == 'Boxplot'",
        checkboxInput("vtc1", "Boxplot Vertikal", value = FALSE),
        textInput("jdl_box_1", strong("Masukkan Judul"), "Boxplot")
      ),
      conditionalPanel(
        condition = "input.data_source == 'Unggah Dokumen' & input.pl1 == 'Dotplot'",
        sliderInput("size1", strong("Pilih Ukuran Titik"), min = 0.1, max = 5, value = 2),
        textInput("jdl_dot_1", strong("Masukkan Judul"), "Dotplot")
      ),
      conditionalPanel(
        condition = "input.data_source == 'Data Tersedia'",
        selectInput("dataset", strong("Pilih Data"), choices = c("Prevalensi Stunting (%)", "Lama Waktu Bekerja (jam)")),
        tags$hr(),
        radioButtons("pl2", strong("Pilih Tipe Plot"), inline = TRUE,
                     choices = c(Histogram = "Histogram", Boxplot = "Boxplot", Dotplot = "Dotplot"))
      ),
      conditionalPanel(
        condition = "input.data_source == 'Data Tersedia' & input.pl2 == 'Histogram'",
        sliderInput("bin2", strong("Pilih Ukuran Binwidth"), min = 5, max = 150, value = 50),
        textInput("jdl_his_2", strong("Masukkan Judul"), "Histogram")
      ),
      conditionalPanel(
        condition = "input.data_source == 'Data Tersedia' & input.pl2 == 'Boxplot'",
        checkboxInput("vtc2", "Boxplot Vertikal", value = FALSE),
        textInput("jdl_box_2", strong("Masukkan Judul"), "Boxplot")
      ),
      conditionalPanel(
        condition = "input.data_source == 'Data Tersedia' & input.pl2 == 'Dotplot'",
        sliderInput("size2", strong("Pilih Ukuran Titik"), min = 0.1, max = 5, value = 2),
        textInput("jdl_dot_2", strong("Masukkan Judul"), "Dotplot")
      ),
      tags$b("Pilih Warna:"),
      fluidRow(
        column(width = 4, colourInput("col_his", "Histogram", "grey", showColour = "background")),
        column(width = 4, colourInput("col_box", "Boxplot", "grey", showColour = "background")),
        column(width = 4, colourInput("col_dot", "Dotplot", "grey", showColour = "background"))
      )
    )
    ,
    mainPanel(
      tabsetPanel(
        tabPanel(
          title = "Statistik Deskriptif",
          br(),
          tableOutput("num1_summary_table")
        ),
        tabPanel(
          title = "Plot",
          br(),
          plotlyOutput("plot"),
          plotOutput("dotplot")
        )
      )
    )
  )
)
```

Kemudian submenu Dua Variabel akan terdiri dari 2 kondisi yaitu, analisis statistik deskriptif menggunakan data yang dapat di unggah sendiri atau menggunakan data yang tersedia. Terdapat tiga pilihan visualisasi data kuantitatif, yaitu histogram, boxplot, dan dotplot. Submenu ini dapat membandingkan analisis deskriptif dari dua variabel sekaligus.

```{r}
dua_page <- tabPanel(
  title = "Dua Variabel",
  sidebarLayout(
    sidebarPanel(
      radioButtons("data2_source", strong("Sumber Data:"),
                   choices = c("Unggah Dokumen", "Data Tersedia")
      ),
      conditionalPanel(
        condition = "input.data2_source == 'Unggah Dokumen'",
        fileInput("file2", strong("Masukkan Dokumen CSV"), multiple = T, accept = ".csv"),
        selectInput("var1_fl", strong("Pilih Variabel 1"), choices = c(not_sel)),
        selectInput("var2_fl", strong("Pilih Variabel 2"), choices = c(not_sel)),
        tags$hr(),
        radioButtons("pl3", strong("Pilih Tipe Plot"), inline = TRUE,
                     choices = c(Histogram = "Histogram", Boxplot = "Boxplot", Dotplot = "Dotplot"))
      ),
      conditionalPanel(
        condition = "input.data2_source == 'Unggah Dokumen' & input.pl3 == 'Histogram'",
        fluidRow(
          column(width = 6, textInput("nama_val", strong("Masukkan Values"), "Values")),
          column(width = 6, textInput("jdl_his_3", strong("Masukkan Judul"), "Histogram")))
      ),
      conditionalPanel(
        condition = "input.data2_source == 'Unggah Dokumen' & input.pl3 == 'Boxplot'",
        checkboxInput("vtc3", "Boxplot Vertikal", value = FALSE),
        fluidRow(
          column(width = 6, textInput("nama_val2", strong("Masukkan Values"), "Values")),
          column(width = 6, textInput("jdl_box_3", strong("Masukkan Judul"), "Boxplot")))
      ),
      conditionalPanel(
        condition = "input.data2_source == 'Unggah Dokumen' & input.pl3 == 'Dotplot'",
        sliderInput("size3", strong("Pilih Ukuran Titik"), min = 0.1, max = 5, value = 2),
        fluidRow(
          column(width = 6, textInput("nama_val3", strong("Masukkan Values"), "Values")),
          column(width = 6, textInput("jdl_dot_3", strong("Masukkan Judul"), "Dotplot")))
      ),
      conditionalPanel(
        condition = "input.data2_source == 'Data Tersedia'",
        selectInput("dataset2", strong("Pilih Data"), choices = c("Tinggi Badan (cm)")),
        selectInput("var1_dt", strong("Pilih Variabel 1"), choices = c(not_sel)),
        selectInput("var2_dt", strong("Pilih Variabel 2"), choices = c(not_sel)),
        tags$hr(),
        radioButtons("pl4", strong("Pilih Tipe Plot"), inline = TRUE,
                     choices = c(Histogram = "Histogram", Boxplot = "Boxplot", Dotplot = "Dotplot"))
      ),
      conditionalPanel(
        condition = "input.data2_source == 'Data Tersedia' & input.pl4 == 'Histogram'",
        textInput("jdl_his_4", strong("Masukkan Judul"), "Histogram")
      ),
      conditionalPanel(
        condition = "input.data2_source == 'Data Tersedia' & input.pl4 == 'Boxplot'",
        checkboxInput("vtc4", "Boxplot Vertikal", value = FALSE),
        textInput("jdl_box_4", strong("Masukkan Judul"), "Boxplot")
      ),
      conditionalPanel(
        condition = "input.data2_source == 'Data Tersedia' & input.pl4 == 'Dotplot'",
        sliderInput("size4", strong("Pilih Ukuran Titik"), min = 0.1, max = 5, value = 2),
        textInput("jdl_dot_4", strong("Masukkan Judul"), "Dotplot")
      )
    ),
    mainPanel(
      tabsetPanel(
        tabPanel(
          title = "Statistik Deskriptif",
          br(),
          tableOutput("num2_summary_table")
        ),
        tabPanel(
          title = "Plot",
          br(),
          plotlyOutput("plot2"),
          plotOutput("dotplot2")
        )
      )
    )
  )
)
```

Terakhir, Submenu Tentang yang mendeskripsikan tentang visualisasi data kuantitatif

```{r}
about_page <- tabPanel(
  title = "Tentang",
  titlePanel("Tentang"),
  "Eksplorasi Data Kuantitatif",
  br(),
  "Data kuantitatif adalah jenis data yang menyatakan atau mengukur jumlah atau kuantitas suatu variabel. Data kuantitatif yang sudah tersedia dalam dashboard ini adalah data prevelensi stunting dan data IPM. Data ini dapat diukur dan dihitung dalam dashboard ini berdasarkan nilai ukuran, mean, standard deviation, nilai terkecil, Q1, median, Q3 dan nilai terbesar.",
  br(),
  "Eksplorasi data merupakan awal proses untuk mengidentifikasi pola, anomali, dan tren pada data yang mungkin tersembunyi. Hal ini juga melibatkan penggunaan metode statistik dan visualisasi untuk mendapatkan pemahaman yang lebih baik tentang struktur dan karakteristik data. Sementara Visualisasis Data adalah proses representasi grafis dari data, pada dashboard ini digunakan plot histogram, boxplot dan dotplot untuk mempermudah memahami pola, tren, dan relasi di dalam data. Visualisasi juga membantu membuat informasi yang kompleks lebih dapat dipahami dan dapat menjadi alat eksplorasi yang tepat.",
  br(),
  "Hubungan antara data kuantitatif, visualisasi, dan eksplorasi terletak pada pemanfaatan visualisasi untuk menjelajahi dan memahami data kuantitatif. Melalui visualisasi, maka bisa lebih efektif untuk mengeksplorasi distribusi, pola, dan relasi di antara variabel kuantitatif, juga membantu membuat inferensi dan pengambilan keputusan yang lebih baik berdasarkan data tersebut."
)
```

- Front End menggunakan RShiny UI

Hasil dari tabPanel akan ditampilkan menggunakan Rshiny Ui dengan syntax sebagai berikut:
```{r}
ui <- navbarPage(
  title = strong("Eksplorasi Data Kuantitatif"),
  theme = shinytheme('simplex'),
  satu_page,
  dua_page,
  about_page
)
```

- Back End menggunakan RShiny Server

Untuk melakukan analisis deskriptif data kuantitatif pada dashboard, diperlukan server dengan syntax sebagai berikut:
```{r}
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
           "Lama Waktu Bekerja (jam)" = kerja)
  })
  dataset2Input <- reactive({
    switch(input$dataset2,
           "Tinggi Badan (cm)" = TB)
  })
  
  ## fungsi observe nama variabel
  observe({updateSelectInput(session, "var1_dt", choices = colnames(dataset2Input()))})
  observe({updateSelectInput(session, "var2_dt", choices = colnames(dataset2Input()))})
  
  ## fungsi reactive variabel
  x1 <- reactive({input$var1_dt})
  x2 <- reactive({input$var2_dt})
  
  
  # fungsi reactive untuk memilih tipe plot
  plot1Input <- reactive({input$pl1})
  plot2Input <- reactive({input$pl2})
  plot3Input <- reactive({input$pl3})
  plot4Input <- reactive({input$pl4})
  
  #
  bin1Input <- reactive({input$bin1})
  bin2Input <- reactive({input$bin2})
  
  # fungsi observe untuk tampilan plot
  observeEvent(input$pl1,{
    if(input$pl1 == "Histogram"){
      show("plot")
      hide("dotplot")
    } else if (input$pl1 == "Boxplot"){
      show("plot")
      hide("dotplot")
    } else if(input$pl1 == "Dotplot"){
      show("dotplot")
      hide("plot")
    }
  })
  
  observeEvent(input$pl2,{
    if(input$pl2 == "Histogram"){
      show("plot")
      hide("dotplot")
    } else if (input$pl2 == "Boxplot"){
      show("plot")
      hide("dotplot")
    } else if(input$pl2 == "Dotplot"){
      show("dotplot")
      hide("plot")
    }
  })
  
  observeEvent(input$pl3,{
    if(input$pl3 == "Histogram"){
      show("plot2")
      hide("dotplot2")
    } else if (input$pl3 == "Boxplot"){
      show("plot2")
      hide("dotplot2")
    } else if(input$pl3 == "Dotplot"){
      show("dotplot2")
      hide("plot2")
    }
  })
  
  observeEvent(input$pl4,{
    if(input$pl4 == "Histogram"){
      show("plot2")
      hide("dotplot2")
    } else if (input$pl4 == "Boxplot"){
      show("plot2")
      hide("dotplot2")
    } else if(input$pl4 == "Dotplot"){
      show("dotplot2")
      hide("plot2")
    }
  })
  
  # output untuk tabel statistik deskriptif satu variabel
  output$num1_summary_table <- renderTable({
    
    ## untuk pilihan unggah dokumen
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
      stat <- c("Banyak Data", "Rata-rata", "Deviasi Standar", "Nilai Terkecil", "Q1", "Nilai Tengah", "Q3", "Nilai Terbesar")
      val <- c(ukuran, rata2, std, min, q1, med, q3, max)
      tbl <- data.frame(stat, val)
      colnames(tbl) <- c("", paste(input$variable))
      tbl
    }
    
    ## untuk pilihan data tersedia
    else if(dtSource() == "Data Tersedia"){
      dataset <- datasetInput()
      ukuran <- length(dataset)
      rata2 <- mean(dataset)
      std <- sd(dataset)
      min <- min(dataset)
      q1 <- quantile(dataset, probs = 0.25)
      med <- quantile(dataset, probs = 0.5)
      q3 <- quantile(dataset, probs = 0.75)
      max <- max(dataset)
      stat <- c("Banyak Data", "Rata-rata", "Deviasi Standar", "Nilai Terkecil", "Q1", "Nilai Tengah", "Q3", "Nilai Terbesar")
      val <- c(ukuran, rata2, std, min, q1, med, q3, max)
      tbl <- data.frame(stat, val)
      colnames(tbl) <- c("", paste(input$dataset))
      tbl
    }
  })
  
  # output plot satu variabel
  output$plot <- renderPlotly({
    
    ## untuk pilihan unggah dokumen
    if(dtSource() == "Unggah Dokumen"){
      df <- data.frame(fileInput())
      var <- df[, varInput()]
      
      ### histogram
      if(plot1Input() == "Histogram")
        plot_ly(data = df, x = var, type = "histogram", nbinsx = bin1Input(),
                marker = list(color = input$col_his,
                              line = list(color = "black", width = 0.5))) %>%
        layout(title = input$jdl_his_1,
               xaxis = list(title = paste(input$variable), zeroline = FALSE),
               yaxis = list(title = "Frekuensi", zeroline = FALSE))
      
      ### boxplot
      else if(plot1Input() == "Boxplot" & input$vtc1 == TRUE)
        plot_ly(data = df, y = var, type = "box", fillcolor = input$col_box, name = " ",
                line = list(color = "black")) %>%
        layout(title = input$jdl_box_1,
               yaxis = list(title = paste(input$variable), zeroline = FALSE))
      else if(plot1Input() == "Boxplot" & input$vtc1 == FALSE)
        plot_ly(data = df, x = var, type = "box", fillcolor = input$col_box, name = " ",
                line = list(color = "black")) %>%
        layout(title = input$jdl_box_1,
               xaxis = list(title = paste(input$variable), zeroline = FALSE))
    }
    
    ## untuk pilihan data tersedia
    else if(dtSource() == "Data Tersedia"){
      dataset <- datasetInput()
      
      ### histogram
      if(plot2Input() == "Histogram")
        plot_ly(x = ~dataset, type = "histogram",  nbinsx = bin2Input(),
                marker = list(color = input$col_his,
                              line = list(color = "black", width = 0.5))) %>%
        layout(title = input$jdl_his_2,
               xaxis = list(title = paste(input$dataset), zeroline = FALSE),
               yaxis = list(title = "Frekuensi", zeroline = FALSE))
      ### boxplot
      else if(plot2Input() == "Boxplot"& input$vtc2 == TRUE)
        plot_ly(y = ~dataset, type = "box", fillcolor = input$col_box, name = " ",
                line = list(color = "black")) %>%
        layout(title = input$jdl_box_2,
               yaxis = list(title = paste(input$dataset), zeroline = FALSE))
      else if(plot2Input() == "Boxplot"& input$vtc2 == FALSE)
        plot_ly(x = ~dataset, type = "box", fillcolor = input$col_box, name = " ",
                line = list(color = "black")) %>%
        layout(title = input$jdl_box_2,
               xaxis = list(title = paste(input$dataset), zeroline = FALSE))
    }
  })
  
  # output khusus dotplot satu variabel
  output$dotplot <- renderPlot({
    
    ## untuk pilihan unggah dokumen 
    if(dtSource() == "Unggah Dokumen"){
      df <- data.frame(fileInput())
      var <- df[, varInput()]
      if(plot1Input() == "Dotplot")
        stripchart(var, method = "stack", pch = 21, bg = input$col_dot, col = "black", cex = input$size1, ylim = c(0,10),
                   main = paste(input$jdl_dot_1), xlab = paste(input$variable))
    }
    
    ## untuk pilihan data tersedia
    else if(dtSource() == "Data Tersedia"){
      dataset <- datasetInput()
      if(plot2Input() == "Dotplot")
        stripchart(dataset, method = "stack", pch = 21, bg = input$col_dot, col = "black", cex = input$size2, ylim = c(0,5),
                   main = paste(input$jdl_dot_2), xlab = paste(input$dataset))
    }
  })
  
  # output untuk tabel statistik deskriptif dua variabel
  output$num2_summary_table <- renderTable({
    
    ## untuk pilihan unggah dokumen
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
      stat <- c("Banyak Data", "Rata-rata", "Deviasi Standar", "Nilai Terkecil", "Q1", "Nilai Tengah", "Q3", "Nilai Terbesar")
      val1 <- c(ukuran_1, rata2_1, std_1, min_1, q1_1, med_1, q3_1, max_1)
      val2 <- c(ukuran_2, rata2_2, std_2, min_2, q1_2, med_2, q3_2, max_2)
      tbl <- data.frame(stat, val1, val2)
      colnames(tbl) <- c("", paste(input$var1_fl), paste(input$var2_fl))
      tbl
    }
    
    ## untuk pilihan data tersedia
    else if(dtSource2() == "Data Tersedia"){
      df2 <- req(dataset2Input())
      xx1 <- req(x1())
      var_1 <- df2[,xx1]
      ukuran_1 <- length(var_1)
      rata2_1 <- mean(var_1)
      std_1 <- sd(var_1)
      min_1 <- min(var_1)
      q1_1 <- quantile(var_1, probs = 0.25)
      med_1 <- quantile(var_1, probs = 0.5)
      q3_1 <- quantile(var_1, probs = 0.75)
      max_1 <- max(var_1)
      xx2 <- req(x2())
      var_2 <- df2[,xx2]
      ukuran_2 <- length(var_2)
      rata2_2 <- mean(var_2)
      std_2 <- sd(var_2)
      min_2 <- min(var_2)
      q1_2 <- quantile(var_2, probs = 0.25)
      med_2 <- quantile(var_2, probs = 0.5)
      q3_2 <- quantile(var_2, probs = 0.75)
      max_2 <- max(var_2)
      stat <- c("Banyak Data", "Rata-rata", "Deviasi Standar", "Nilai Terkecil", "Q1", "Nilai Tengah", "Q3", "Nilai Terbesar")
      val1 <- c(ukuran_1, rata2_1, std_1, min_1, q1_1, med_1, q3_1, max_1)
      val2 <- c(ukuran_2, rata2_2, std_2, min_2, q1_2, med_2, q3_2, max_2)
      tbl <- data.frame(stat, val1, val2)
      colnames(tbl) <- c("", paste(input$var1_dt), paste(input$var2_dt))
      tbl
    }
  })
  
  # output plot dua variabel
  output$plot2 <- renderPlotly({
    
    ## untuk pilihan unggah dokumen
    if(dtSource2() == "Unggah Dokumen"){
      df <- data.frame(file2Input())
      var1 <- df[, var1flInput()]
      var2 <- df[, var2flInput()]
      df <- data.frame("input$var1_fl" = var1, "input$var2_fl" = var2)
      colnames(df) <- c(paste(input$var1_fl), paste(input$var2_fl))
      df <- stack(df)
      
      ### histogram
      if(plot3Input() == "Histogram"){
        df %>% group_by(ind) %>%
          do(p = plot_ly(., x = ~values, name = ~ind, type = "histogram", color = ~ind)) %>%
          subplot(nrows = 2, shareX = TRUE, shareY = TRUE) %>%
          layout(title = input$jdl_his_3,
                 xaxis = list(title = paste(input$nama_val)))
      }
      
      ### boxplot
      else if(plot3Input() == "Boxplot" & input$vtc3 == TRUE){
        plot_ly(df, y = ~values, color = ~ind, type = "box") %>%
          layout(title = input$jdl_box_3,
                 yaxis = list(title = paste(input$nama_val2), zeroline = FALSE))
      }
      else if(plot3Input() == "Boxplot" & input$vtc3 == FALSE){
        plot_ly(df, x = ~values, color = ~ind, type = "box") %>%
          layout(title = input$jdl_box_3,
                 xaxis = list(title = paste(input$nama_val2), zeroline = FALSE))
      }
    }
    
    ## untuk pilihan data tersedia
    else if(dtSource2() == "Data Tersedia"){
      df2 <- req(dataset2Input())
      df2 <- data.frame(df2)
      df2 <- stack(df2)
      
      ### histogram
      if(plot4Input() == "Histogram"){
        df2 %>% group_by(ind) %>%
          do(p = plot_ly(., x = ~values, name = ~ind, type = "histogram", color = ~ind)) %>%
          subplot(nrows = 2, shareX = TRUE, shareY = TRUE) %>%
          layout(title = input$jdl_his_4,
                 xaxis = list(title = paste(input$dataset2)),
                 yaxis = list(title = "Frekuensi", zeroline = FALSE))
      }
      
      ### boxplot
      else if(plot4Input() == "Boxplot"& input$vtc4 == TRUE){
        plot_ly(df2, y = ~values, color = ~ind, type = "box") %>%
          layout(title = input$jdl_box_4,
                 yaxis = list(title = paste(input$dataset2), zeroline = FALSE))
      }
      else if(plot4Input() == "Boxplot"& input$vtc4 == FALSE){
        plot_ly(df2, x = ~values, color = ~ind, type = "box") %>%
          layout(title = input$jdl_box_4,
                 xaxis = list(title = paste(input$dataset2), zeroline = FALSE))
      }
    }
  })
  
  # output khusus dotplot dua variabel
  output$dotplot2 <- renderPlot({
    
    ## untuk pilihan unggah dokumen
    if(dtSource2() == "Unggah Dokumen"){
      df <- data.frame(file2Input())
      var1 <- df[, var1flInput()]
      var2 <- df[, var2flInput()]
      df <- data.frame("input$var1_fl" = var1, "input$var2_fl" = var2)
      colnames(df) <- c(paste(input$var1_fl), paste(input$var2_fl))
      df <- stack(df)
      if(plot3Input() == "Dotplot"){
        stripchart(values ~ ind, data = df, method = "stack", pch = 19, col = c("lightgreen", "lightblue"),
                   cex = input$size3, main = paste(input$jdl_dot_3), xlab = paste(input$nama_val3))
      }
      ## untuk pilihan data tersedia
    } else if(dtSource2() == "Data Tersedia") {
      df2 <- req(dataset2Input())
      df2 <- data.frame(df2)
      df2 <- stack(df2)
      if(plot4Input() == "Dotplot")
        stripchart(values ~ ind, data = df2, method = "stack", pch = 19, col = c("lightgreen", "lightblue"),
                   cex = input$size4, main = paste(input$jdl_dot_4), xlab = paste(input$dataset2))
    }
  })
}
```

Terakhir, UI dan Server dijalankan secara bersamaan
```{r}
shinyApp(ui = ui, server = server)
```

## :camera_flash: Dokumentasi

1️⃣ Screenshot Submenu Satu Variabel
![1](https://github.com/EVD6/eda_quantitative/assets/111562146/2e5dbdfa-5cf0-451f-ac81-099dd36dace4)
![2](https://github.com/EVD6/eda_quantitative/assets/111562146/72c9fbe6-0ab9-4b72-8048-5eb2766f93b0)
![3](https://github.com/EVD6/eda_quantitative/assets/111562146/ab93234d-d937-43b5-a86c-0138dcc28e42)
![4](https://github.com/EVD6/eda_quantitative/assets/111562146/cb50bf3c-47e2-467d-8257-0c44c19ffdc2)
![5](https://github.com/EVD6/eda_quantitative/assets/111562146/baa54ade-a1f0-43f6-b4fb-beb87a7cbd2d)

2️⃣ Screenshot Submenu Dua Variabel
![6](https://github.com/EVD6/eda_quantitative/assets/111562146/181863de-8746-45f8-82ad-2d786b5b2f5a)
![7](https://github.com/EVD6/eda_quantitative/assets/111562146/7a9fc149-5886-4fa5-8421-b94ee2ebd110)
![8](https://github.com/EVD6/eda_quantitative/assets/111562146/9c45391a-6862-4d1d-8f3a-a34847f21266)

3️⃣ Screenshot Submenu Tentang
![9](https://github.com/EVD6/eda_quantitative/assets/111562146/d34c5e90-0405-4546-84a7-ed389d00e41b)


## :video_camera: Demo

| url                      |
| ------------------------ | 
|[https://icyarassah.shinyapps.io/visualisasidatakuantitatif/](https://icyarassah.shinyapps.io/visualisasidatakuantitatif/)| 

## :fireworks: Anggota Tim Pengembang

1. Fradha Intan Arassah   (G1501221018) : UI dan Server
2. Diaztri Hazam          (G1501221032) : UI dan Server
3. Firda Aulia Maghfiroh  (G1501222049) : Server dan Technical Writer
