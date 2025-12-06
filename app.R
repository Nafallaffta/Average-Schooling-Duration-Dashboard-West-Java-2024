# ============================================================
# app.R — Dashboard Analisis Spasial Rata-Rata Lama Sekolah Jabar
# Version: 2.0 (LENGKAP)
# ============================================================

# ----------------------------
# Load libraries
# ----------------------------
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(dplyr)
library(plotly)
library(leaflet)
library(DT)
library(spdep)
library(spatialreg)
library(sf)
library(sp)
library(openxlsx)
library(tidyr)
library(lmtest)
library(car)
library(ggplot2)
library(GGally)
library(DiagrammeR)

# ============================================================
# 1. GLOBAL SECTION (Data & Model)
# ============================================================

# --- Load data shapefile dan Excel ---

Indo <- st_read("shapefile/RBI_50K_2023_Jawa Barat.shp") %>% st_zm(drop = TRUE, what = "ZM") # hilangkan dimensi Z dan M
data <- read.xlsx("data/DATA SPASIAL.xlsx")

# --- Merge shapefile dan data ---
jabar_merged <- Indo %>%
  left_join(data, by = c("WADMKK" = "Kabupaten/Kota"))

# --- Konversi ke Spatial untuk analisis spasial ---
jabar_sp <- as_Spatial(jabar_merged)
row.names(jabar_sp) <- jabar_sp$WADMKK

# --- Spatial weight matrix ---
W <- poly2nb(jabar_sp, row.names = row.names(jabar_sp), queen = TRUE)
WL <- nb2listw(W, style = "W", zero.policy = TRUE)

# --- Model (OLS + Spasial) ---
ols_model <- lm(`Rata.Rata.Lama.Sekolah` ~ 
                  PAD + DAU.Pendidikan + PDRB + Tingkat.Kemiskinan +
                  Rasio.Guru.dan.Murid.SD + Rasio.Guru.dan.Murid.SMP + Rasio.Guru.dan.Murid.SMA +
                  APM.SD + APM.SMP + APM.SMA + APM.PT,
                data = data)

sar_model <- lagsarlm(formula(ols_model), data = data, listw = WL, method = "eigen", zero.policy = TRUE)
sem_model <- errorsarlm(formula(ols_model), data = data, listw = WL, method = "eigen", zero.policy = TRUE)
sdm_model <- lagsarlm(formula(ols_model), data = data, listw = WL, Durbin = TRUE, method = "eigen", zero.policy = TRUE)
sdem_model <- errorsarlm(formula(ols_model), data = data, listw = WL, Durbin = TRUE, method = "eigen", zero.policy = TRUE)
sac_model <- sacsarlm(formula(ols_model), data = data, listw = WL, method = "eigen", zero.policy = TRUE)
gns_model <- sacsarlm(formula(ols_model), data = data, listw = WL, Durbin = TRUE, method = "eigen", zero.policy = TRUE)

cat("✓ Semua data dan model spasial berhasil dimuat!\n")

# ============================================================
# 2. UI SECTION
# ============================================================

ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(title = "Analisis Spasial RLS Jawa Barat", titleWidth = 350),
  
  dashboardSidebar(
    width = 250,
    sidebarMenu(
      menuItem("About Dashboard", tabName = "about", icon = icon("info-circle")),
      menuItem("Data Overview", tabName = "data_overview", icon = icon("table")),
      menuItem("Analisis Deskriptif", tabName = "descriptive", icon = icon("chart-bar")),
      menuItem("Neighbor Visualization", tabName = "neighbor", icon = icon("map-marked-alt")),
      menuItem("Autokorelasi Spasial", tabName = "autocorr", icon = icon("project-diagram")),
      menuItem("Model Comparison", tabName = "model_comp", icon = icon("balance-scale")),
      menuItem("Prediction", tabName = "prediction", icon = icon("chart-line"))
    )
  ),
  
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    ),
    tabItems(
      # =====================================================
      # 1. TENTANG DASHBOARD
      # =====================================================
      tabItem(tabName = "about",
              fluidRow(
                box(width = 12, title = "Deskripsi Dashboard", status = "primary", solidHeader = TRUE,
                    h4("Tujuan Dashboard"),
                    p("Dashboard ini menyajikan analisis spasial komprehensif untuk variabel Rata-Rata Lama Sekolah (RLS) di Provinsi Jawa Barat. Analisis mencakup eksplorasi data, autokorelasi spasial, dan pemodelan spasial lanjutan."),
                    hr(),
                    h4("Sumber Data"),
                    tags$ul(
                      tags$li("BPS (Badan Pusat Statistik) - Data sosial ekonomi"),
                      tags$li("Shapefile RBI 50K - Peta administrasi Jawa Barat")
                    ),
                    p(strong("Tahun Data:"), "2024"),
                    p(strong("Jumlah Kabupaten/Kota:"), nrow(data)),
                    hr(),
                    h4("Metodologi"),
                    p("Alur analisis yang digunakan dalam dashboard ini:"),
                    grVizOutput("flowchart", height = "800px",width = "100%" ),
                    hr(),
                    h4("👤 Pembuat"),
                    tags$ul(
                      tags$li("Fatih Zahrani (140610230014)"),
                      tags$li("Rahma Aulia Putri (140610230037)"),
                      tags$li("Nafalla Afftanur Rismawanti (140610230044)")
                    ),
                    p("Dashboard Version 2.0 - 2025")
                )
              )),
      
      # =====================================================
      # 2. DATA OVERVIEW
      # =====================================================
      tabItem(tabName = "data_overview",
              fluidRow(
                valueBoxOutput("n_obs", width = 4),
                valueBoxOutput("n_vars", width = 4),
                valueBoxOutput("data_year", width = 4)
              ),
              fluidRow(
                box(width = 12, title = "Tabel Data", status = "info", solidHeader = TRUE,
                    sliderInput("n_rows", "Jumlah Baris:", min = 5, max = 50, value = 10),
                    downloadButton("download_data", "Download Data (Excel)", class = "btn-primary"),
                    hr(),
                    DTOutput("data_table"))
              ),
              fluidRow(
                box(width = 6, title = "Peta Distribusi RLS", status = "primary", solidHeader = TRUE,
                    leafletOutput("overview_map", height = "400px")),
                box(width = 6, title = "Correlation Heatmap", status = "warning", solidHeader = TRUE,
                    plotlyOutput("corr_heatmap", height = "400px"))
              )),
      
      # =====================================================
      # 3. ANALISIS DESKRIPTIF
      # =====================================================
      tabItem(tabName = "descriptive",
              fluidRow(
                box(width = 12, title = "Pilih Variabel", status = "primary", solidHeader = TRUE,
                    pickerInput("desc_vars", "Pilih Variabel:", choices = NULL, multiple = TRUE,
                                options = list(actionsBox = TRUE, liveSearch = TRUE, maxOptions = 5)),
                    checkboxInput("show_summary_table", "Tampilkan Tabel Statistik Deskriptif", value = FALSE),
                    downloadButton("download_plots", "Download Visualisasi", class = "btn-success"))
              ),
              fluidRow(
                box(width = 6, title = "Histogram", status = "info", solidHeader = TRUE,
                    plotlyOutput("histogram", height = "400px")),
                box(width = 6, title = "Boxplot", status = "warning", solidHeader = TRUE,
                    plotlyOutput("boxplot", height = "400px"))
              ),
              fluidRow(
                conditionalPanel(
                  condition = "input.show_summary_table == true",
                  box(width = 12, title = "Statistik Deskriptif", status = "success", solidHeader = TRUE,
                      DTOutput("summary_table"))
                )
              ),
              fluidRow(
                box(width = 12, title = "Scatter Plot Matrix", status = "primary", solidHeader = TRUE,
                    plotOutput("scatter_matrix", height = "600px"))
              )),
      
      # =====================================================
      # 4. NEIGHBOR VISUALIZATION
      # =====================================================
      tabItem(tabName = "neighbor",
              fluidRow(
                box(width = 12, title = "Pilih Kabupaten/Kota", status = "primary", solidHeader = TRUE,
                    pickerInput("selected_cities", "Pilih Kabupaten/Kota:", 
                                choices = NULL, multiple = TRUE,
                                options = list(actionsBox = TRUE, liveSearch = TRUE, maxOptions = 5)),
                    checkboxInput("show_weight_matrix", "Tampilkan Matriks Bobot Spasial (W)", value = FALSE))
              ),
              fluidRow(
                box(width = 12, title = "Peta Tetangga Spasial", status = "info", solidHeader = TRUE,
                    p("Wilayah terpilih berwarna MERAH, dan wilayah tetangganya berwarna KUNING"),
                    leafletOutput("neighbor_map", height = "600px"))
              ),
              fluidRow(
                conditionalPanel(
                  condition = "input.show_weight_matrix == true",
                  box(width = 12, title = "Matriks Bobot Spasial (W)", status = "warning", solidHeader = TRUE,
                      plotlyOutput("weight_matrix_heatmap", height = "600px"))
                )
              )),
      
      # =====================================================
      # 5. AUTOKORELASI SPASIAL
      # =====================================================
      tabItem(tabName = "autocorr",
              fluidRow(
                box(width = 12, title = "Pilih Variabel untuk Analisis", status = "primary", solidHeader = TRUE,
                    selectInput("autocorr_var", "Pilih Variabel:", 
                                choices = c("Rata.Rata.Lama.Sekolah", names(data)[!names(data) %in% c("Kabupaten/Kota")]),
                                selected = "Rata.Rata.Lama.Sekolah"))
              ),
              fluidRow(
                valueBoxOutput("moranI_value", width = 6),
                valueBoxOutput("gearyC_value", width = 6)
              ),
              fluidRow(
                box(width = 6, title = "Moran's I Scatterplot", status = "warning", solidHeader = TRUE,
                    plotlyOutput("moran_plot", height = "500px")),
                box(width = 6, title = "Tabel Interpretasi", status = "info", solidHeader = TRUE,
                    tableOutput("autocorr_interpretation"))
              ),
              # =======================
              # Tambahkan tabBox peta
              # =======================
              fluidRow(
                box(width = 12, status = "success", solidHeader = TRUE,
                    title = "Peta Autokorelasi Spasial (LISA & Getis-Ord Gi*)",
                    tabBox(width = 12,
                           tabPanel("LISA Cluster Map (Local Moran’s I)",
                                    leafletOutput("lisa_map", height = "500px")),
                           tabPanel("Raw Getis–Ord G* (Proporsi Massa Tetangga)",
                                    leafletOutput("getisord_raw_map", height = "500px")),
                           tabPanel("Hot/Cold Spots (z-score)",
                                    leafletOutput("getisord_hotcold_map", height = "500px"))
                    )
                )
              )
      ),
      # =====================================================
      # 6. MODEL COMPARISON
      # =====================================================
      tabItem(tabName = "model_comp",
              fluidRow(
                box(width = 12, title = "Hasil LM Tests (Lagrange Multiplier)", status = "primary", solidHeader = TRUE,
                    p("LM Test digunakan untuk menentukan model spasial mana yang paling sesuai."),
                    tableOutput("lm_tests_table"))
              ),
              fluidRow(
                box(width = 12, title = "Interpretasi LM Tests", status = "info", solidHeader = TRUE,
                    uiOutput("lm_interpretation"))
              ),
              fluidRow(
                box(width = 12, title = "Pilih Kriteria Evaluasi", status = "warning", solidHeader = TRUE,
                    checkboxGroupInput("eval_criteria", "Pilih Kriteria:",
                                       choices = c("AIC", "BIC"),
                                       selected = c("AIC", "BIC"), inline = TRUE))
              ),
              fluidRow(
                box(width = 7, title = "Tabel Perbandingan Model", status = "success", solidHeader = TRUE,
                    DTOutput("model_eval_table")),
                box(width = 5, title = "Grafik Perbandingan", status = "primary", solidHeader = TRUE,
                    plotlyOutput("model_eval_plot", height = "400px"))
              ),
              fluidRow(
                box(width = 12, title = "Detail Model Terpilih", status = "info", solidHeader = TRUE,
                    selectInput("model_type", "Pilih Model:",
                                choices = c("OLS","SAR","SEM","SDM","SDEM","SAC","GNS"),
                                selected = "SAR"),
                    verbatimTextOutput("model_summary"))
              ),
              # BAGIAN BARU: UJI ASUMSI
              fluidRow(
                box(width = 12, title = "Uji Asumsi Model Terpilih", status = "warning", solidHeader = TRUE, collapsible = TRUE,
                    p("Hasil uji asumsi untuk model yang dipilih di atas."),
                    tabsetPanel(
                      tabPanel("Multikolinearitas (VIF)",
                               br(),
                               p("VIF > 10 mengindikasikan multikolinearitas yang serius."),
                               tableOutput("vif_table")),
                      tabPanel("Normalitas Residual",
                               br(),
                               uiOutput("normality_interpretation")),
                      tabPanel("Homoskedastisitas",
                               br(),
                               uiOutput("bp_interpretation")),
                      tabPanel("Linearitas",
                               br(),
                               uiOutput("reset_interpretation")),
                      tabPanel("Autokorelasi Spasial",
                               br(),
                               uiOutput("moran_interpretation"))
                    ))
              )
      ),
      
      # =====================================================
      # 7. PREDICTION
      # =====================================================
      tabItem(tabName = "prediction",
              fluidRow(
                box(width = 12, title = "Pilih Model untuk Prediksi", status = "primary", solidHeader = TRUE,
                    pickerInput("pred_models", "Pilih Model:", 
                                choices = c("OLS","SAR","SEM","SDM","SDEM","SAC","GNS"),
                                selected = "SAR", multiple = TRUE,
                                options = list(actionsBox = TRUE, maxOptions = 3)),
                    downloadButton("download_pred", "Download Hasil Prediksi", class = "btn-success"))
              ),
              fluidRow(
                box(width = 6, title = "Actual vs Predicted", status = "info", solidHeader = TRUE,
                    plotlyOutput("actual_vs_pred", height = "500px")),
                box(width = 6, title = "Residual Plot", status = "warning", solidHeader = TRUE,
                    plotlyOutput("residual_plot", height = "500px"))
              ),
              fluidRow(
                box(width = 12, title = "Summary Model", status = "success", solidHeader = TRUE,
                    verbatimTextOutput("pred_model_summary"))
              ),
              fluidRow(
                box(width = 12, title = "Peta Prediksi", status = "primary", solidHeader = TRUE,
                    leafletOutput("predicted_map", height = "600px"))
              ))
    )
  )
)
# ============================================================
# 3. SERVER SECTION
# ============================================================

server <- function(input, output, session) {
  
  # ============================================================
  # REACTIVE VALUES & UPDATES
  # ============================================================
  
  # Update choices untuk variabel picker
  observe({
    var_choices <- names(data)[!names(data) %in% c("Kabupaten/Kota")]
    
    updatePickerInput(session, "desc_vars", 
                      choices = var_choices,
                      selected = "Rata.Rata.Lama.Sekolah")
    
    updatePickerInput(session, "selected_cities",
                      choices = data$`Kabupaten/Kota`,
                      selected = data$`Kabupaten/Kota`[1])
  })
  
  # ============================================================
  # 1. TENTANG DASHBOARD
  # ============================================================
  output$flowchart <- renderGrViz({
    grViz("
    digraph flowchart {
      graph [rankdir = TB, bgcolor = transparent, splines = ortho]
      node [shape = box, style = filled, fillcolor = '#CFE2FF', fontname = 'Poppins', fontsize = 13, width = 3, height = 1]
      edge [color = '#555555', penwidth = 1.3, fontname = 'Poppins', fontsize = 12]

      # Nodes
      Start [shape = ellipse, fillcolor = '#A2D2FF', label = 'Start']
      Input [label = 'Input Data']
      Deskriptif [label = 'Analisis Statistik\\nDeskriptif']
      OLS [label = 'Estimasi Parameter\\n(dengan OLS)']
      UjiKlasik [label = 'Uji Asumsi Klasik']
      Dependensi [shape = diamond, fillcolor = '#FFE69A', label = 'Ada Dependensi Spasial\\npada Residual?']
      
      LM [label = 'Uji Lagrange Multiplier']
      Spasial [label = 'Estimasi Model Spasial\\nEkonometrik']
      PilihModel [label = 'Pilih Model Terbaik']
      UjiSpasial [label = 'Uji Asumsi Model Spasial']
      Sensitivitas [label = 'Uji Sensitivitas\\nBobot Spasial']
      Interpretasi [label = 'Interpretasi Hasil']
      Finish [shape = ellipse, fillcolor = '#B9FBC0', label = 'Finish']
      OLSSelesai [shape = ellipse, fillcolor = '#B9FBC0', label = 'Finish (OLS Cukup)']

      # Flow
      Start -> Input -> Deskriptif -> OLS -> UjiKlasik -> Dependensi
      Dependensi -> LM [label = 'Ya']
      Dependensi -> OLSSelesai [label = 'Tidak']
      LM -> Spasial -> PilihModel -> UjiSpasial -> Sensitivitas -> Interpretasi -> Finish
    }
  ")
  })
  
  
  # ============================================================
  # 2. DATA OVERVIEW
  # ============================================================
  
  output$n_obs <- renderValueBox({
    valueBox(nrow(data), "Jumlah Observasi", icon = icon("map-marker-alt"), color = "blue")
  })
  
  output$n_vars <- renderValueBox({
    valueBox(ncol(data)-1, "Jumlah Variabel", icon = icon("list"), color = "green")
  })
  
  output$data_year <- renderValueBox({
    valueBox("2024", "Tahun Data", icon = icon("calendar"), color = "yellow")
  })
  
  output$data_table <- renderDT({
    datatable(head(data, input$n_rows), 
              options = list(scrollX = TRUE, pageLength = 10),
              rownames = FALSE)
  })
  
  output$download_data <- downloadHandler(
    filename = function() {
      paste0("Data_RLS_Jabar_", Sys.Date(), ".xlsx")
    },
    content = function(file) {
      write.xlsx(data, file)
    }
  )
  
  output$overview_map <- renderLeaflet({
    pal <- colorNumeric("YlOrRd", domain = jabar_merged$Rata.Rata.Lama.Sekolah)
    
    leaflet(jabar_merged) %>%
      addTiles() %>%
      addPolygons(
        fillColor = ~pal(Rata.Rata.Lama.Sekolah),
        weight = 1,
        opacity = 1,
        color = "white",
        fillOpacity = 0.7,
        label = ~paste0(WADMKK, ": ", round(Rata.Rata.Lama.Sekolah, 2), " tahun"),
        highlightOptions = highlightOptions(weight = 3, color = "#666", fillOpacity = 0.9)
      ) %>%
      addLegend(pal = pal, values = ~Rata.Rata.Lama.Sekolah,
                title = "RLS (tahun)", position = "bottomright")
  })
  
  output$corr_heatmap <- renderPlotly({
    num_vars <- data %>% select(where(is.numeric))
    cor_matrix <- cor(num_vars, use = "complete.obs")
    
    plot_ly(z = cor_matrix, x = colnames(cor_matrix), y = colnames(cor_matrix),
            type = "heatmap", colorscale = "RdBu", zmid = 0,
            text = round(cor_matrix, 2), hovertemplate = "%{y} vs %{x}<br>Correlation: %{z}<extra></extra>") %>%
      layout(title = "Correlation Matrix", xaxis = list(tickangle = -45))
  })
  
  # ============================================================
  # 3. ANALISIS DESKRIPTIF
  # ============================================================
  
  output$histogram <- renderPlotly({
    req(input$desc_vars)
    
    plots <- lapply(input$desc_vars, function(var) {
      plot_ly(data, x = ~get(var), type = "histogram", name = var,
              marker = list(line = list(color = "white", width = 1))) %>%
        layout(xaxis = list(title = var), yaxis = list(title = "Frequency"))
    })
    
    subplot(plots, nrows = ceiling(length(input$desc_vars)/2), shareX = FALSE, shareY = FALSE, titleX = TRUE, titleY = TRUE)
  })
  
  output$boxplot <- renderPlotly({
    req(input$desc_vars)
    
    plots <- lapply(input$desc_vars, function(var) {
      plot_ly(data, y = ~get(var), type = "box", name = var, boxmean = TRUE) %>%
        layout(yaxis = list(title = var))
    })
    
    subplot(plots, nrows = ceiling(length(input$desc_vars)/2), shareX = FALSE, shareY = FALSE)
  })
  
  output$summary_table <- renderDT({
    req(input$desc_vars)
    
    summary_df <- data %>%
      select(all_of(input$desc_vars)) %>%
      summarise(across(everything(), list(
        Min = ~min(., na.rm = TRUE),
        Q1 = ~quantile(., 0.25, na.rm = TRUE),
        Median = ~median(., na.rm = TRUE),
        Mean = ~mean(., na.rm = TRUE),
        Q3 = ~quantile(., 0.75, na.rm = TRUE),
        Max = ~max(., na.rm = TRUE),
        SD = ~sd(., na.rm = TRUE)
      ))) %>%
      pivot_longer(everything(), names_to = "Variable_Stat", values_to = "Value") %>%
      separate(Variable_Stat, into = c("Variable", "Statistic"), sep = "_") %>%
      pivot_wider(names_from = Statistic, values_from = Value)
    
    datatable(
      summary_df,
      options = list(
        dom = 't',         # hanya tabel tanpa kontrol lain
        paging = FALSE,    # hilangkan pagination
        searching = FALSE, # hilangkan search box
        ordering = FALSE,  # hilangkan sorting
        info = FALSE,      # hilangkan "Showing 1 of..."
        scrollX = TRUE     # bisa scroll kalau tabel lebar
      ),
      rownames = FALSE
    ) %>%
      formatRound(columns = 2:8, digits = 2)
  })
  
  output$scatter_matrix <- renderPlot({
    req(input$desc_vars)
    
    if(length(input$desc_vars) > 1) {
      ggpairs(data[, input$desc_vars], 
              upper = list(continuous = wrap("cor", size = 3)),
              lower = list(continuous = wrap("points", alpha = 0.5, size = 0.8)),
              diag = list(continuous = wrap("densityDiag", alpha = 0.5))) +
        theme_minimal()
    } else {
      ggplot() + 
        annotate("text", x = 0.5, y = 0.5, label = "Pilih minimal 2 variabel untuk scatter matrix", size = 6) +
        theme_void()
    }
  })
  
  output$download_plots <- downloadHandler(
    filename = function() {
      paste0("Descriptive_Plots_", Sys.Date(), ".pdf")
    },
    content = function(file) {
      pdf(file, width = 11, height = 8)
      
      # Histogram
      for(var in input$desc_vars) {
        p <- ggplot(data, aes(x = get(var))) +
          geom_histogram(bins = 20, fill = "steelblue", color = "white") +
          labs(title = paste("Histogram of", var), x = var, y = "Frequency") +
          theme_minimal()
        print(p)
      }
      
      # Boxplot
      for(var in input$desc_vars) {
        p <- ggplot(data, aes(y = get(var))) +
          geom_boxplot(fill = "lightblue") +
          labs(title = paste("Boxplot of", var), y = var) +
          theme_minimal()
        print(p)
      }
      
      dev.off()
    }
  )
  
  # ============================================================
  # 4. NEIGHBOR VISUALIZATION
  # ============================================================
  output$neighbor_map <- renderLeaflet({
    req(input$selected_cities)
    
    # Pastikan urutan wilayah antara jabar_merged dan daftar tetangga (W) sama
    jabar_merged <- jabar_merged[order(jabar_merged$WADMKK), ]
    
    # Buat ulang daftar ketetanggaan dengan urutan yang sama
    jabar_sp <- as_Spatial(jabar_merged)
    row.names(jabar_sp) <- jabar_sp$WADMKK
    W <- poly2nb(jabar_sp, row.names = row.names(jabar_sp), queen = TRUE)
    
    # Identifikasi wilayah terpilih
    selected_idx <- which(jabar_merged$WADMKK %in% input$selected_cities)
    
    # Cari tetangga berdasarkan indeks W
    neighbors_idx <- unique(unlist(W[selected_idx]))
    
    # Buat kolom warna
    jabar_merged$color <- "gray"
    jabar_merged$color[selected_idx] <- "red"
    if (length(neighbors_idx) > 0) {
      jabar_merged$color[neighbors_idx] <- "yellow"
    }
    
    # Visualisasi
    leaflet(jabar_merged) %>%
      addTiles() %>%
      addPolygons(
        fillColor = ~color,
        weight = 2,
        opacity = 1,
        color = "black",
        fillOpacity = 0.6,
        label = ~paste0(WADMKK, " (RLS: ", round(Rata.Rata.Lama.Sekolah, 2), " tahun)"),
        highlightOptions = highlightOptions(weight = 4, color = "#333", fillOpacity = 0.9)
      ) %>%
      addLegend(
        colors = c("red", "yellow", "gray"),
        labels = c("Terpilih", "Tetangga", "Lainnya"),
        title = "Status Wilayah",
        position = "bottomright"
      )
  })
  
  output$weight_matrix_heatmap <- renderPlotly({
    # Pastikan urutan sama
    jabar_merged <- jabar_merged[order(jabar_merged$WADMKK), ]
    
    # Buat daftar ketetanggaan (Queen contiguity)
    jabar_sp <- as_Spatial(jabar_merged)
    row.names(jabar_sp) <- jabar_sp$WADMKK
    W <- poly2nb(jabar_sp, row.names = row.names(jabar_sp), queen = TRUE)
    
    # Konversi ke matriks bobot biner
    W_matrix <- nb2mat(W, style = "B", zero.policy = TRUE)
    city_order <- row.names(W_matrix)
    
    # Plot heatmap interaktif
    plot_ly(
      z = W_matrix,
      x = city_order,
      y = city_order,
      type = "heatmap",
      colorscale = list(c(0, "white"), c(1, "darkblue")),
      hovertemplate = "%{y} → %{x}<br>Neighbor: %{z}<extra></extra>"
    ) %>%
      layout(
        title = "Spatial Weight Matrix (W)",
        xaxis = list(tickangle = -45, title = ""),
        yaxis = list(title = "")
      )
  })
  # ============================================================
  # 5. AUTOKORELASI SPASIAL
  # ============================================================
  
  selected_var <- reactive({
    req(input$autocorr_var)
    var <- jabar_merged[[input$autocorr_var]]
    names(var) <- rownames(jabar_merged)  # penting supaya sesuai WL
    var
  })
  
  # Moran's I
  moran_result <- reactive({
    x <- selected_var()
    moran.test(x, WL, zero.policy = TRUE)
  })
  
  # Geary's C
  geary_result <- reactive({
    x <- selected_var()
    geary.test(x, WL, zero.policy = TRUE)
  })
  
  # ValueBox Moran
  output$moranI_value <- renderValueBox({
    m <- moran_result()
    valueBox(
      value = round(m$estimate[1], 4),
      subtitle = paste0("Moran's I (p-value: ", round(m$p.value, 4), ")"),
      icon = icon("project-diagram"),
      color = if(m$p.value < 0.05) "green" else "red"
    )
  })
  
  # ValueBox Geary
  output$gearyC_value <- renderValueBox({
    g <- geary_result()
    valueBox(
      value = round(g$estimate[1], 4),
      subtitle = paste0("Geary's C (p-value: ", round(g$p.value, 4), ")"),
      icon = icon("chart-area"),
      color = if(g$p.value < 0.05) "green" else "red"
    )
  })
  
  # Moran Scatterplot
  output$moran_plot <- renderPlotly({
    x <- selected_var()
    wx <- lag.listw(WL, x, zero.policy = TRUE)
    
    # Standardisasi
    x_std <- scale(x)[,1]
    wx_std <- scale(wx)[,1]
    
    # Nama daerah
    region_names <- data$`Kabupaten/Kota`
    
    # Tentukan kuadran
    quadrant <- case_when(
      x_std > 0 & wx_std > 0 ~ "Q1: High-High",
      x_std < 0 & wx_std > 0 ~ "Q2: Low-High",
      x_std < 0 & wx_std < 0 ~ "Q3: Low-Low",
      x_std > 0 & wx_std < 0 ~ "Q4: High-Low"
    )
    
    df_moran <- data.frame(
      x = x_std, 
      wx = wx_std,
      region = region_names,
      quadrant = quadrant,
      x_original = x,
      wx_original = wx
    )
    
    # Warna per kuadran
    colors <- c("Q1: High-High" = "#d62728",   # Merah
                "Q2: Low-High" = "#ff7f0e",    # Oranye
                "Q3: Low-Low" = "#1f77b4",     # Biru
                "Q4: High-Low" = "#2ca02c")    # Hijau
    
    # Plot dengan plotly
    p <- plot_ly(df_moran, 
                 x = ~x, 
                 y = ~wx,
                 color = ~quadrant,
                 colors = colors,
                 type = 'scatter',
                 mode = 'markers',
                 marker = list(size = 10, 
                               line = list(color = 'white', width = 1)),
                 text = ~paste0("<b>", region, "</b><br>",
                                "Nilai: ", round(x_original, 2), "<br>",
                                "Spatial Lag: ", round(wx_original, 2), "<br>",
                                "Kuadran: ", quadrant),
                 hoverinfo = 'text') %>%
      add_lines(x = ~x, y = fitted(lm(wx ~ x, data = df_moran)),
                name = "Trend Line",
                line = list(color = 'black', width = 2, dash = 'dash'),
                hoverinfo = 'skip',
                showlegend = TRUE) %>%
      layout(
        title = list(text = paste("<b>Moran's I Scatterplot -", input$autocorr_var, "</b>"),
                     font = list(size = 16)),
        xaxis = list(title = paste("Standardized", input$autocorr_var),
                     zeroline = TRUE,
                     zerolinewidth = 2,
                     zerolinecolor = 'gray'),
        yaxis = list(title = paste("Spatial Lag of", input$autocorr_var),
                     zeroline = TRUE,
                     zerolinewidth = 2,
                     zerolinecolor = 'gray'),
        hovermode = 'closest',
        legend = list(x = 0.02, y = 0.98,
                      bgcolor = 'rgba(255,255,255,0.8)',
                      bordercolor = 'black',
                      borderwidth = 1),
        shapes = list(
          # Garis vertikal
          list(type = "line", x0 = 0, x1 = 0, 
               y0 = min(df_moran$wx) * 1.1, y1 = max(df_moran$wx) * 1.1,
               line = list(color = "gray", width = 1, dash = "dot")),
          # Garis horizontal
          list(type = "line", x0 = min(df_moran$x) * 1.1, x1 = max(df_moran$x) * 1.1,
               y0 = 0, y1 = 0,
               line = list(color = "gray", width = 1, dash = "dot"))
        )
      )
    
    p
  })
  
  # Tabel Interpretasi
  output$autocorr_interpretation <- renderTable({
    m <- moran_result()
    g <- geary_result()
    
    data.frame(
      Test = c("Moran's I", "Geary's C"),
      Value = c(round(m$estimate[1], 4), round(g$estimate[1], 4)),
      `P-Value` = c(round(m$p.value, 4), round(g$p.value, 4)),
      Interpretation = c(
        if(m$p.value < 0.05) {
          if(m$estimate[1] > 0) "Positive spatial autocorrelation (clustering)" else "Negative spatial autocorrelation (dispersion)"
        } else "No significant spatial autocorrelation",
        if(g$p.value < 0.05) {
          if(g$estimate[1] < 1) "Positive spatial autocorrelation" else "Negative spatial autocorrelation"
        } else "No significant spatial autocorrelation"
      ),
      check.names = FALSE
    )
  })
  
  # LISA Map
  output$lisa_map <- renderLeaflet({
    x <- selected_var()
    lisa <- localmoran(x, WL, zero.policy = TRUE)
    
    # Standardisasi
    x_std <- scale(x)
    x_lag_std <- scale(lag.listw(WL, x, zero.policy = TRUE))
    
    lisa_class <- rep("Not Significant", length(x))
    sig <- lisa[, 5] < 0.05
    
    lisa_class[sig & x_std > 0 & x_lag_std > 0] <- "High-High"
    lisa_class[sig & x_std < 0 & x_lag_std < 0] <- "Low-Low"
    lisa_class[sig & x_std > 0 & x_lag_std < 0] <- "High-Low"
    lisa_class[sig & x_std < 0 & x_lag_std > 0] <- "Low-High"
    
    jabar_merged$lisa_class <- lisa_class
    
    pal <- colorFactor(c("red", "blue", "pink", "lightblue", "white"),
                       domain = c("High-High", "Low-Low", "High-Low", "Low-High", "Not Significant"))
    
    leaflet(jabar_merged) %>%
      addTiles() %>%
      addPolygons(
        fillColor = ~pal(lisa_class),
        weight = 1,
        opacity = 1,
        color = "black",
        fillOpacity = 0.7,
        label = ~paste0(WADMKK, ": ", lisa_class),
        highlightOptions = highlightOptions(weight = 3, fillOpacity = 0.9)
      ) %>%
      addLegend(pal = pal, values = ~lisa_class,
                title = "LISA Cluster", position = "bottomright")
  })
  
  # Getis-Ord Gi*(Raw & Hot/Cold)
  output$getisord_raw_map <- renderLeaflet({
    x <- selected_var()
    
    # --- Hitung Getis–Ord G* (Raw) ---
    Wb <- nb2listw(W, style = "B", zero.policy = TRUE)
    Wb_mat <- listw2mat(Wb)
    
    x_raw <- x
    x_raw[is.na(x_raw)] <- 0
    sum_x <- sum(x_raw)
    
    # Perhitungan G* raw (proporsi massa tetangga)
    num_Gs <- as.numeric((Wb_mat + diag(1, nrow(Wb_mat))) %*% x_raw)
    den_Gs <- sum_x
    G_star_raw <- num_Gs / den_Gs
    
    jabar_merged$G_star_raw <- G_star_raw
    
    pal <- colorNumeric("viridis", domain = jabar_merged$G_star_raw)
    
    leaflet(jabar_merged) %>%
      addTiles() %>%
      addPolygons(
        fillColor = ~pal(G_star_raw),
        weight = 1,
        opacity = 1,
        color = "black",
        fillOpacity = 0.7,
        label = ~paste0(WADMKK, ": G* raw = ", round(G_star_raw, 3)),
        highlightOptions = highlightOptions(weight = 3, fillOpacity = 0.9)
      ) %>%
      addLegend(pal = pal, values = ~G_star_raw,
                title = "Raw Getis–Ord G*", position = "bottomright")
  })
  # Getis-Ord Gi*(Raw & Hot/Cold)
  output$getisord_hotcold_map <- renderLeaflet({
    x <- selected_var()
    
    # Hitung z(Gi*)
    gi_z <- as.numeric(localG(x, WL, zero.policy = TRUE))
    jabar_merged$z_Gistar <- gi_z
    
    # Klasifikasi hotspot/coldspot signifikan (p<=0.05)
    jabar_merged$hotcold <- case_when(
      gi_z >=  1.96 ~ "Hot spot (p<=0.05)",
      gi_z <= -1.96 ~ "Cold spot (p<=0.05)",
      TRUE          ~ "Not significant"
    )
    
    pal <- colorFactor(
      palette = c("Hot spot (p<=0.05)" = "#b2182b",
                  "Cold spot (p<=0.05)" = "#2166ac",
                  "Not significant"      = "grey85"),
      domain = jabar_merged$hotcold
    )
    
    leaflet(jabar_merged) %>%
      addTiles() %>%
      addPolygons(
        fillColor = ~pal(hotcold),
        weight = 1,
        opacity = 1,
        color = "black",
        fillOpacity = 0.7,
        label = ~paste0(WADMKK, ": ", hotcold, 
                        " (z = ", round(z_Gistar, 2), ")"),
        highlightOptions = highlightOptions(weight = 3, fillOpacity = 0.9)
      ) %>%
      addLegend(pal = pal, values = ~hotcold,
                title = "Getis–Ord Gi* Hot/Cold Spots", position = "bottomright")
  })
  # ============================================================
  # 6. MODEL COMPARISON
  # ============================================================
  
  output$lm_tests_table <- renderTable({
    lm_sar <- lm.RStests(ols_model, WL, test = c("LMlag", "LMerr", "RLMlag", "RLMerr", "SARMA"))
    
    lm_df <- data.frame(
      Test = names(lm_sar),
      Statistic = sapply(lm_sar, function(x) round(x$statistic, 4)),
      `P-Value` = sapply(lm_sar, function(x) round(x$p.value, 4)),
      Significant = sapply(lm_sar, function(x) ifelse(x$p.value < 0.05, "Yes", "No")),
      check.names = FALSE
    )
    
    lm_df
  }, striped = TRUE, hover = TRUE, bordered = TRUE)
  
  output$lm_interpretation <- renderUI({
    lm_sar <- lm.RStests(ols_model, WL, test = c("LMlag", "LMerr", "RLMlag", "RLMerr", "SARMA"))
    
    lm_df <- data.frame(
      Test = names(lm_sar),
      Statistic = sapply(lm_sar, function(x) round(x$statistic, 4)),
      P_Value = sapply(lm_sar, function(x) round(x$p.value, 4)),
      Significant = sapply(lm_sar, function(x) ifelse(x$p.value < 0.05, TRUE, FALSE)),
      check.names = FALSE
    )
    
    interpretation <- "<h4>Interpretasi:</h4><ul>"
    
    adjRSlag_sig <- lm_df$Significant[lm_df$Test == "adjRSlag"]
    adjRSerr_sig <- lm_df$Significant[lm_df$Test == "adjRSerr"]
    
    if(!is.na(adjRSlag_sig) && !is.na(adjRSerr_sig)) {
      if(adjRSlag_sig && !adjRSerr_sig) {
        interpretation <- paste0(interpretation, "<li><b>Model SAR</b> lebih sesuai (adjRSlag signifikan)</li>")
      } else if(adjRSerr_sig && !adjRSlag_sig) {
        interpretation <- paste0(interpretation, "<li><b>Model SEM</b> lebih sesuai (adjRSerr signifikan)</li>")
      } else if(adjRSlag_sig && adjRSerr_sig) {
        interpretation <- paste0(interpretation, "<li><b>Model SDM atau GNS</b> perlu dipertimbangkan (kedua efek signifikan)</li>")
      } else {
        interpretation <- paste0(interpretation, "<li>Tidak ada efek spasial signifikan, <b>OLS</b> cukup</li>")
      }
    } else {
      interpretation <- paste0(interpretation, "<li>Data LM test tidak lengkap, tidak bisa diinterpretasi</li>")
    }
    
    interpretation <- paste0(interpretation, "</ul>")
    HTML(interpretation)
  })
  
  
  model_eval_df <- reactive({
    models_list <- list(
      OLS = ols_model,
      SAR = sar_model,
      SEM = sem_model,
      SDM = sdm_model,
      SDEM = sdem_model,
      SAC = sac_model,
      GNS = gns_model
    )
    
    eval_df <- data.frame(
      Model = names(models_list),
      AIC = sapply(models_list, AIC),
      BIC = sapply(models_list, BIC),
      LogLik = sapply(models_list, logLik),
      R2 = c(
        summary(ols_model)$r.squared,
        summary(sar_model)$NK,
        summary(sem_model)$NK,
        summary(sdm_model)$NK,
        summary(sdem_model)$NK,
        summary(sac_model)$NK,
        summary(gns_model)$NK
      )
    )
    
    eval_df
  })
  
  output$model_eval_table <- renderDT({
    df <- model_eval_df()
    
    selected_cols <- c("Model", input$eval_criteria)
    df_filtered <- df[, selected_cols, drop = FALSE]
    
    datatable(df_filtered, options = list(pageLength = 10), rownames = FALSE) %>%
      formatRound(columns = 2:ncol(df_filtered), digits = 2)
  })
  
  output$model_eval_plot <- renderPlotly({
    req(input$eval_criteria)
    
    df <- model_eval_df()
    
    plots <- lapply(input$eval_criteria, function(crit) {
      plot_ly(df, x = ~Model, y = ~get(crit), type = "bar", name = crit,
              marker = list(color = "steelblue")) %>%
        layout(yaxis = list(title = crit), showlegend = FALSE)
    })
    
    subplot(plots, nrows = length(input$eval_criteria), shareX = TRUE, titleY = TRUE)
  })
  
  output$model_summary <- renderPrint({
    model <- switch(input$model_type,
                    "OLS" = ols_model,
                    "SAR" = sar_model,
                    "SEM" = sem_model,
                    "SDM" = sdm_model,
                    "SDEM" = sdem_model,
                    "SAC" = sac_model,
                    "GNS" = gns_model)
    summary(model)
  })
  
  # ========================================================
  # UJI ASUMSI - OUTPUTS
  # ========================================================
  
  # 1. VIF (Multikolinearitas)
  output$vif_table <- renderTable({
    tryCatch({
      # Untuk model OLS, langsung ambil VIF
      if(input$model_type == "OLS") {
        vif_values <- vif(ols_model)
      } else {
        # Untuk model spasial, hitung VIF dari model OLS yang sama
        base_formula <- `Rata.Rata.Lama.Sekolah` ~ 
          PAD + DAU.Pendidikan + PDRB + Tingkat.Kemiskinan +
          Rasio.Guru.dan.Murid.SD + Rasio.Guru.dan.Murid.SMP + Rasio.Guru.dan.Murid.SMA +
          APM.SD + APM.SMP + APM.SMA + APM.PT
        
        temp_lm <- lm(base_formula, data = data)
        vif_values <- vif(temp_lm)
      }
      
      vif_df <- data.frame(
        Variable = names(vif_values),
        VIF = round(vif_values, 4),
        Status = ifelse(vif_values > 10, "Multikolinearitas Serius", 
                        ifelse(vif_values > 5, "OK", "OK"))
      )
      vif_df
    }, error = function(e) {
      data.frame(Error = "Tidak dapat menghitung VIF untuk model ini")
    })
  }, striped = TRUE, hover = TRUE, bordered = TRUE)
  
  # 2. Normalitas (Shapiro-Wilk Test)
  output$normality_interpretation <- renderUI({
    tryCatch({
      model <- switch(input$model_type,
                      "OLS" = ols_model,
                      "SAR" = sar_model,
                      "SEM" = sem_model,
                      "SDM" = sdm_model,
                      "SDEM" = sdem_model,
                      "SAC" = sac_model,
                      "GNS" = gns_model)
      
      residual_model <- residuals(model)
      test_result <- shapiro.test(residual_model)
      
      interpretation <- paste0(
        "<h4>Uji Shapiro-Wilk</h4>",
        "<p><b>Statistic:</b> ", round(test_result$statistic, 4), "</p>",
        "<p><b>P-value:</b> ", round(test_result$p.value, 4), "</p>",
        "<hr>",
        "<h5>Interpretasi:</h5>",
        if(test_result$p.value < 0.05) {
          "<p style='color: red;'><b>Residual TIDAK berdistribusi normal</b> (p-value < 0.05)</p>"
        } else {
          "<p style='color: green;'><b>Residual berdistribusi normal</b> (p-value ≥ 0.05)</p>"
        }
      )
      
      HTML(interpretation)
    }, error = function(e) {
      HTML(paste0("<p style='color: red;'>Error: ", e$message, "</p>"))
    })
  })
  
  # 3. Homoskedastisitas (Breusch-Pagan Test)
  output$bp_interpretation <- renderUI({
    tryCatch({
      if(input$model_type == "OLS") {
        test_result <- bptest(ols_model)
      } else {
        model <- switch(input$model_type,
                        "SAR" = sar_model,
                        "SEM" = sem_model,
                        "SDM" = sdm_model,
                        "SDEM" = sdem_model,
                        "SAC" = sac_model,
                        "GNS" = gns_model)
        test_result <- bptest.Sarlm(model)
      }
      
      interpretation <- paste0(
        "<h4>Uji Breusch-Pagan</h4>",
        "<p><b>Statistic:</b> ", round(test_result$statistic, 4), "</p>",
        "<p><b>P-value:</b> ", round(test_result$p.value, 4), "</p>",
        "<hr>",
        "<h5>Interpretasi:</h5>",
        if(test_result$p.value < 0.05) {
          "<p style='color: red;'><b>Terjadi heteroskedastisitas</b> (p-value < 0.05)</p>"
        } else {
          "<p style='color: green;'><b>Homoskedastisitas terpenuhi</b> (p-value ≥ 0.05)</p>"
        }
      )
      
      HTML(interpretation)
    }, error = function(e) {
      HTML(paste0("<p style='color: red;'>Error: ", e$message, "</p>"))
    })
  })
  
  # 4. Linearitas (RESET Test)
  output$reset_interpretation <- renderUI({
    tryCatch({
      base_formula <- `Rata.Rata.Lama.Sekolah` ~ 
        PAD + DAU.Pendidikan + PDRB + Tingkat.Kemiskinan +
        Rasio.Guru.dan.Murid.SD + Rasio.Guru.dan.Murid.SMP + Rasio.Guru.dan.Murid.SMA +
        APM.SD + APM.SMP + APM.SMA + APM.PT
      
      temp_lm <- lm(base_formula, data = data)
      test_result <- resettest(temp_lm)
      
      interpretation <- paste0(
        "<h4>RESET Test</h4>",
        "<p><b>Statistic:</b> ", round(test_result$statistic, 4), "</p>",
        "<p><b>P-value:</b> ", round(test_result$p.value, 4), "</p>",
        "<hr>",
        "<h5>Interpretasi:</h5>",
        if(test_result$p.value < 0.05) {
          "<p style='color: red;'><b>Model TIDAK linear</b> (p-value < 0.05)</p>"
        } else {
          "<p style='color: green;'><b>Asumsi linearitas terpenuhi</b> (p-value ≥ 0.05)</p>"
        }
      )
      
      HTML(interpretation)
    }, error = function(e) {
      HTML(paste0("<p style='color: red;'>Error: ", e$message, "</p>"))
    })
  })
  
  # 5. Autokorelasi Spasial (Moran's I Test)
  output$moran_interpretation <- renderUI({
    tryCatch({
      model <- switch(input$model_type,
                      "OLS" = ols_model,
                      "SAR" = sar_model,
                      "SEM" = sem_model,
                      "SDM" = sdm_model,
                      "SDEM" = sdem_model,
                      "SAC" = sac_model,
                      "GNS" = gns_model)
      
      residual_model <- residuals(model)
      test_result <- moran.test(residual_model, WL, zero.policy = TRUE)
      
      interpretation <- paste0(
        "<h4>Moran's I Test</h4>",
        "<p><b>Moran I statistic:</b> ", round(test_result$statistic, 4), "</p>",
        "<p><b>P-value:</b> ", round(test_result$p.value, 4), "</p>",
        "<hr>",
        "<h5>Interpretasi:</h5>",
        if(test_result$p.value < 0.05) {
          "<p style='color: red;'><b>Terdapat autokorelasi spasial pada residual</b> (p-value < 0.05)</p>"
        } else {
          "<p style='color: green;'><b>Tidak ada autokorelasi spasial pada residual</b> (p-value ≥ 0.05)</p>"
        }
      )
      
      HTML(interpretation)
    }, error = function(e) {
      HTML(paste0("<p style='color: red;'>Error: ", e$message, "</p>"))
    })
  })
  # ========================================================
  # UJI ASUMSI - OUTPUTS
  # ========================================================
  
  # 1. VIF (Multikolinearitas)
  output$vif_table <- renderTable({
    tryCatch({
      # Untuk model OLS, langsung ambil VIF
      if(input$model_type == "OLS") {
        vif_values <- vif(ols_model)
      } else {
        # Untuk model spasial, hitung VIF dari model OLS yang sama
        base_formula <- `Rata.Rata.Lama.Sekolah` ~ 
          PAD + DAU.Pendidikan + PDRB + Tingkat.Kemiskinan +
          Rasio.Guru.dan.Murid.SD + Rasio.Guru.dan.Murid.SMP + Rasio.Guru.dan.Murid.SMA +
          APM.SD + APM.SMP + APM.SMA + APM.PT
        
        temp_lm <- lm(base_formula, data = data)
        vif_values <- vif(temp_lm)
      }
      
      vif_df <- data.frame(
        Variable = names(vif_values),
        VIF = round(vif_values, 4),
        Status = ifelse(vif_values > 10, "Multikolinearitas Serius", 
                        ifelse(vif_values > 5, "OK", "OK"))
      )
      vif_df
    }, error = function(e) {
      data.frame(Error = "Tidak dapat menghitung VIF untuk model ini")
    })
  }, striped = TRUE, hover = TRUE, bordered = TRUE)
  
  # 2. Normalitas (Shapiro-Wilk Test)
  output$normality_test <- renderPrint({
    tryCatch({
      model <- switch(input$model_type,
                      "OLS" = ols_model,
                      "SAR" = sar_model,
                      "SEM" = sem_model,
                      "SDM" = sdm_model,
                      "SDEM" = sdem_model,
                      "SAC" = sac_model,
                      "GNS" = gns_model)
      
      residual_model <- residuals(model)
      shapiro.test(residual_model)
    }, error = function(e) {
      cat("Error:", e$message)
    })
  })
  
  # 3. Homoskedastisitas (Breusch-Pagan Test)
  output$bp_test <- renderPrint({
    tryCatch({
      if(input$model_type == "OLS") {
        bptest(ols_model)
      } else {
        model <- switch(input$model_type,
                        "SAR" = sar_model,
                        "SEM" = sem_model,
                        "SDM" = sdm_model,
                        "SDEM" = sdem_model,
                        "SAC" = sac_model,
                        "GNS" = gns_model)
        bptest.Sarlm(model)
      }
    }, error = function(e) {
      cat("Error:", e$message)
    })
  })
  
  # 4. Linearitas (RESET Test)
  output$reset_test <- renderPrint({
    tryCatch({
      base_formula <- `Rata.Rata.Lama.Sekolah` ~ 
        PAD + DAU.Pendidikan + PDRB + Tingkat.Kemiskinan +
        Rasio.Guru.dan.Murid.SD + Rasio.Guru.dan.Murid.SMP + Rasio.Guru.dan.Murid.SMA +
        APM.SD + APM.SMP + APM.SMA + APM.PT
      
      temp_lm <- lm(base_formula, data = data)
      resettest(temp_lm)
    }, error = function(e) {
      cat("Error:", e$message)
    })
  })
  
  # 5. Autokorelasi Spasial (Moran's I Test)
  output$moran_test <- renderPrint({
    tryCatch({
      model <- switch(input$model_type,
                      "OLS" = ols_model,
                      "SAR" = sar_model,
                      "SEM" = sem_model,
                      "SDM" = sdm_model,
                      "SDEM" = sdem_model,
                      "SAC" = sac_model,
                      "GNS" = gns_model)
      
      residual_model <- residuals(model)
      moran.test(residual_model, WL, zero.policy = TRUE)
    }, error = function(e) {
      cat("Error:", e$message)
    })
  })
  # ============================================================
  # 7. PREDICTION
  # ============================================================
  
  pred_results <- reactive({
    req(input$pred_models)
    
    models_list <- list(
      OLS = ols_model,
      SAR = sar_model,
      SEM = sem_model,
      SDM = sdm_model,
      SDEM = sdem_model,
      SAC = sac_model,
      GNS = gns_model
    )
    
    selected_models <- models_list[input$pred_models]
    
    pred_data <- data.frame(
      Kabupaten_Kota = data$`Kabupaten/Kota`,
      Actual = data$Rata.Rata.Lama.Sekolah
    )
    
    for (model_name in names(selected_models)) {
      model <- selected_models[[model_name]]
      
      # --- solusi utama di sini ---
      if (model_name %in% c("SAC", "GNS")) {
        # karena SAC & GNS tidak punya fungsi predict()
        preds <- model$fitted.values
      } else {
        preds <- as.vector(predict(model))
      }
      # -----------------------------
      
      pred_data[[paste0("Pred_", model_name)]] <- preds
    }
    
    pred_data
  })
  
  output$actual_vs_pred <- renderPlotly({
    df <- pred_results()
    
    pred_cols <- grep("Pred_", names(df), value = TRUE)
    
    p <- plot_ly()
    
    for(col in pred_cols) {
      model_name <- gsub("Pred_", "", col)
      p <- p %>% add_trace(x = df$Actual, y = df[[col]], 
                           type = "scatter", mode = "markers", name = model_name,
                           marker = list(size = 8, opacity = 0.7))
    }
    
    p <- p %>% 
      add_trace(x = range(df$Actual), y = range(df$Actual), 
                type = "scatter", mode = "lines", name = "Perfect Fit",
                line = list(color = "red", dash = "dash")) %>%
      layout(title = "Actual vs Predicted",
             xaxis = list(title = "Actual RLS"),
             yaxis = list(title = "Predicted RLS"),
             hovermode = "closest")
    
    p
  })
  
  output$residual_plot <- renderPlotly({
    df <- pred_results()
    
    pred_cols <- grep("Pred_", names(df), value = TRUE)
    
    p <- plot_ly()
    
    for(col in pred_cols) {
      model_name <- gsub("Pred_", "", col)
      residuals <- df$Actual - df[[col]]
      p <- p %>% add_trace(x = df[[col]], y = residuals, 
                           type = "scatter", mode = "markers", name = model_name,
                           marker = list(size = 8, opacity = 0.7))
    }
    
    p <- p %>% 
      add_trace(x = range(df[, pred_cols]), y = c(0, 0), 
                type = "scatter", mode = "lines", name = "Zero Line",
                line = list(color = "red", dash = "dash")) %>%
      layout(title = "Residual Plot",
             xaxis = list(title = "Predicted RLS"),
             yaxis = list(title = "Residuals"),
             hovermode = "closest")
    
    p
  })
  
  output$pred_model_summary <- renderPrint({
    req(input$pred_models)
    
    df <- pred_results()
    pred_cols <- grep("Pred_", names(df), value = TRUE)
    
    cat("=== SUMMARY PREDIKSI ===\n\n")
    
    for(col in pred_cols) {
      model_name <- gsub("Pred_", "", col)
      residuals <- df$Actual - df[[col]]
      rmse <- sqrt(mean(residuals^2))
      mae <- mean(abs(residuals))
      mape <- mean(abs(residuals / df$Actual)) * 100
      
      cat(paste0("Model: ", model_name, "\n"))
      cat(paste0("  RMSE: ", round(rmse, 4), "\n"))
      cat(paste0("  MAE: ", round(mae, 4), "\n"))
      cat(paste0("  MAPE: ", round(mape, 2), "%\n\n"))
    }
  })
  
  output$predicted_map <- renderLeaflet({
    df <- pred_results()
    
    # Ambil prediksi dari model pertama yang dipilih
    pred_col <- grep("Pred_", names(df), value = TRUE)[1]
    
    jabar_merged$predicted <- df[[pred_col]]
    
    pal <- colorNumeric("Greens", domain = jabar_merged$predicted)
    
    leaflet(jabar_merged) %>%
      addTiles() %>%
      addPolygons(
        fillColor = ~pal(predicted),
        weight = 1,
        opacity = 1,
        color = "white",
        fillOpacity = 0.7,
        label = ~paste0(WADMKK, 
                        "<br>Actual: ", round(Rata.Rata.Lama.Sekolah, 2),
                        "<br>Predicted: ", round(predicted, 2)) %>% lapply(htmltools::HTML),
        highlightOptions = highlightOptions(weight = 3, fillOpacity = 0.9)
      ) %>%
      addLegend(pal = pal, values = ~predicted,
                title = "Predicted RLS", position = "bottomright")
  })
  
  output$download_pred <- downloadHandler(
    filename = function() {
      paste0("Prediction_Results_", Sys.Date(), ".xlsx")
    },
    content = function(file) {
      write.xlsx(pred_results(), file)
    }
  )
  
}

# ============================================================
# 4. RUN APP
# ============================================================

shinyApp(ui = ui, server = server)
