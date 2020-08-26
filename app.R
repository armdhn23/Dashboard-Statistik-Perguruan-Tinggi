#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(dashboardthemes)
library(shinydashboardPlus)
library(shinyWidgets)
library(shinycssloaders)
library(DT)
library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(tidyr)
library(readr)
library(stringr)
library(plotly)
library(rCharts)
library(echarts4r)

# Define UI for application that draws a histogram

ui <- dashboardPagePlus(
  dashboardHeader(title= "Dashboard PT",
                  tags$li(class="dropdown",tags$a(href="https://www.linkedin.com/in/ashari-ramadhan-7607a3141/", 
                                                  icon("linkedin"), "Profil", target="_blank"))),
  dashboardSidebar(
    sidebarMenu(
      menuItem(tabName = "pendidikan_tinggi", "Mahasiswa", icon = icon("book-reader")), #ada koma
      menuItem(tabName = "dosen", "Dosen", icon = icon("chalkboard-teacher")),
      menuItem(tabName = "kelembagaan", "Kelembagaan", icon = icon("graduation-cap")),
      menuItem(tabName = "tentang", "Tentang", icon = icon("address-card"))
    )
  ),
  dashboardBody( shinyDashboardThemes(
    theme = "poor_mans_flatly"
  ),
  fluidPage(
    tags$head(
      tags$meta(name="dicoding:email", content="asharir42@gmail.com")
    )
  ),
  # setBackgroundColor(
  #  color = "white",
  # shinydashboard = T
  #),
  tabItems(
    tabItem(tabName = "pendahuluan",
            "Pendahuluan"
    ),
    tabItem(tabName = "kelembagaan",
            tabsetPanel(
              tabPanel(id = "institusi","Institusi",
                       fluidRow(
                         box(width = 6,
                             uiOutput("radio_jumlah_perlembaga"),
                             uiOutput("label_jumlah_per_lembaga")
                         ),
                         box(width = 6,title =  "Akreditasi Perguruan Tinggi (2018)",
                             selectInput(inputId = "pilih_akreditasi_prodi" , "Pilih Akreditasi",
                                         choices = c("Semua Akreditasi","A", "B", "C"), width = "50%"),
                             plotlyOutput("bar_akreditasi_pt", height = "700px")
                         )
                       )
              ),
              tabPanel(id = "prodi","Program Studi",
                       fluidRow(
                         box(title = "Akreditasi Program Studi (2018)",
                           selectInput(inputId = "pilih_akreditasi_prodi" , "Pilih Akreditasi",
                                       choices = c("Semua Akreditasi","A", "B", "C"), width = "50%"),
                           withSpinner(
                             plotlyOutput("bar_akreditasi_prodi", height = "600"))
                         ),
                         box(title = "Akreditasi Program Studi berdasarkan Jenjang (2018)",
                           selectInput(inputId = "pilih_jenjang_prodi" , "Pilih Jenjang",width = "50%",
                                       choices = c("D1-D4", "S1", "S2","S3",	"Profesi",	"Spesialis","Semua Jenjang"),
                                       selected = "Semua Jenjang"),
                           withSpinner(
                             plotlyOutput("bar_jenjang_prodi", height = "600"))
                         ),
                       ),
                       fluidRow(
                         box(width = 12)
                       ),
              ),
              tabPanel(id = "tabel_lembaga", "Tabel",
                       selectInput("pilih_tabel_lembaga", "Pilih Data",
                                   choices = c("Bentuk Pendidikan",
                                               "Akreditasi Perguruan Tinggi", "Akreditasi Program Studi")),
                       dataTableOutput("data_table_lembaga"),
                       style = "overflow-y: scroll;overflow-x: scroll;"
              )
            )
    ),
    tabItem(tabName = "dosen",
            tabsetPanel(
              tabPanel("Grafik",
                       fluidRow(
                         box(width = 4, height = "400px",#title = "Persenate Professor berdasarkan Bidang Ilmu",
                               plotOutput("plot1", height = "1px"),
                             "Persentase Professor berdasarkan Bidang Ilmu (2018)",
                               showOutput("pie_prof", "nvd3")),
                         box(width = 4, height = "400px",
                             "Rasio Professor berdasarkan Wilayah (2018)",
                               plotlyOutput("bar_rasio_prof", height = "350px")),
                         box(width = 4,  height = "400px",
                            h5( "Persentase Dosen Berdasarkan Pendidikan Tertinggi (2018)"),
                             uiOutput("provinsi_dosen_tetap"),
                             withSpinner(
                               plotlyOutput("pie_dostep", height = "300px")))
                       ),
                       fluidRow(
                         box(width = 12,
                             uiOutput("radio_dosen"),
                             column(width = 6,
                                    withSpinner(
                                      plotlyOutput("dosen_jabatan"))),
                             column(width = 6,
                                    withSpinner(
                                      plotlyOutput("column_umur_dosen",
                                                      width = "100%", height = "400px")))
                         )),
                       fluidRow(
                         box(width = 12)
                       )
              ),
              tabPanel("Tabel",
                       selectInput("tabel_dosen", "Pilih Data Dosen Berdasarkan",
                                   choices = c("Bentuk Pendidikan", "Kelompok Umur")),
                       DT::dataTableOutput("data_tabel_dosen"),
                       style = "overflow-y: scroll;overflow-x: scroll;"
                       
              )
            )
            
    ),
    tabItem(tabName  = "pendidikan_tinggi",
            fluidRow(
              tabsetPanel(
                tabPanel(id = "gambaran_umum","Mahasiswa",
                         box(width = 12,
                             box(width = 4,
                                 withSpinner(plotlyOutput("do_bidang_ilmu"))),
                             box(width = 4,
                                 withSpinner(plotlyOutput("apk"))),
                             box(width = 4,
                                 withSpinner(echarts4rOutput("do_jk"))),
                         ),
                         box(width = 12,
                             box(width = 4, height = "760px",
                                 withSpinner(plotlyOutput("jumlah_mahasiswa", height = "750px"))),
                             box(width = 4, height = "760px",
                                 withSpinner(plotlyOutput("putus_kuliah", height = "750px"))),
                             box(width = 4, height = "760px",
                                 withSpinner(plotlyOutput("lulusan", height = "750px")))
                         )
                ),
                tabPanel(id = "sbmptn","SBMPTN",
                         box(width = 12,
                             # A static infoBox
                             valueBox("Sumber Data:","LTMPT", icon = icon("credit-card"), color = "olive"),
                             valueBox("714.652", "Jumlah Peserta SBMPTN 2019", icon = icon("credit-card"), color = "olive"),
                             valueBox("168.742","Jumlah peserta lulus SBMPTN 2019", icon = icon("credit-card"), color = "olive"),
                             prettyRadioButtons("menu_sbmptn","Pilih Menu", choices = c("Lihat Persaingan", "Lihat Program Studi"),
                                                inline = T, shape = "curve", fill = T,icon = icon("check"), animation = "pulse"),
                         ),
                         conditionalPanel("input.menu_sbmptn == 'Lihat Persaingan'",
                                          box(width = 4,
                                              selectizeInput("region", "Pilih region", choices = NULL),
                                              selectInput("rumpun", "Pilih Rumpun", 
                                                          c("Semua Rumpun","Saintek", "Soshum"), ),
                                              radioButtons("pilih_pt", "Pilih Perguruan Tinggi", 
                                                           c("Semua PT", "Pilih PT"), inline = T),
                                              conditionalPanel("input.pilih_pt == 'Pilih PT'",
                                                               uiOutput("ui_univ")),
                                              radioButtons("pilih_prodi", "Pilih Program Studi",
                                                           c("Semua Prodi", "Pilih Prodi"), inline = T),
                                              conditionalPanel("input.pilih_prodi == 'Pilih Prodi'",
                                                               uiOutput("ui_prodi")),
                                              actionBttn(inputId = "action_button_saing",label =  "Lihat Persaingan", 
                                                         style = "unite", color = "primary", size = "xs")),
                                          box(width = 8, title = "Grafik Lollipop",
                                              withSpinner(plotOutput("grafik_saing")))
                         ),
                         conditionalPanel("input.menu_sbmptn == 'Lihat Program Studi'",
                                          fluidRow(
                                            column(5,
                                                   box(width = 10,
                                                       selectizeInput(inputId= "profil_univ", label = "Perguruan Tinggi", choices = NULL),
                                                       uiOutput("ui_prodi_profil"),
                                                       actionBttn(inputId = "action_button_profil",label =  "Lihat Profil", 
                                                                  style = "unite", color = "primary", size = "xs")),
                                                   box(width = 10, title = "Informasi",
                                                       uiOutput("box_profil"),
                                                       uiOutput("box_profil_pt"))),
                                            column(6, idInput = "aaa",
                                                   box(title = "Peminat x Daya Tampung", width = 12,
                                                       withSpinner(plotlyOutput("grafik_profil")) 
                                                   )
                                            )
                                          )
                         )),
                tabPanel("Tabel Data",
                         DT::dataTableOutput("lihat_data_pt")
                )
              ))
    ), #disni ada koma
     tabItem( tabName = "tentang",
         box(width = 12,
            h3(("Tentang")),
            includeHTML("www/equ-biasa.html")
         )
    
    )
  )   
  ),
  dashboardFooter(
    left_text = "Made with ❤ in Palu for Dicoding's Challenge️"
  ),
  title = "Dashboard Pendidikan Tinggi"
)

server <- function(input, output, session) {
  showModal(modalDialog(
    title = "Halo!",
    "Selamat Datang di Dashboard Statistik Pendidikan Tinggi.",
    footer = modalButton("Tutup"),
    easyClose = TRUE
  ))
  ## Backend fitur input perbandingan
  data_reg<- read.csv("data/Full.csv")
  pilihan_reg<- data_reg$REGION
  pilihan_reg<- as.factor(pilihan_reg)
  pilihan_region <- levels(pilihan_reg)
  updateSelectizeInput(session, 'region', choices = pilihan_region, server = TRUE)
  
  data_site<-read.csv("data/Full.csv")
  SiteInfo<- data_site
  #filter UNIV
  observe({
    if(input$region != ".SEMUA REGION"){
      subRegionSelected <- input$region
      temp<- SiteInfo %>% filter(REGION == subRegionSelected)
      output$ui_univ <- renderUI({
        multiInput("kode_univ", "Pilih minimal 1", choices = unique(sort(temp$NAMA_UNIV)))
      }) 
    } else{
      output$ui_univ <- renderUI({
        temp<- SiteInfo %>% filter(NAMA_UNIV != ".SEMUA UNIV") %>% arrange(NAMA_UNIV)
        multiInput("kode_univ", "Pilih minimal 1", choices = unique(sort(temp$NAMA_UNIV)))
      })
    }
  })
  
  #filter PRODI
  observe({
    if(input$pilih_pt =="Semua PT" & input$rumpun == "Semua Rumpun"){
      if(input$region != ".SEMUA REGION"){
        SiteInfo<- SiteInfo %>% filter(REGION == input$region)
      } else{
        SiteInfo <- SiteInfo
      }
      temp<- SiteInfo %>% dplyr::filter(PROGRAM_STUDI != ".SEMUA PRODI")
      output$ui_prodi <- renderUI({
        multiInput("kode_prodi", "Pilih minimal 1", choices = unique(sort(temp$PROGRAM_STUDI)))
      })
    } else if(input$pilih_pt =="Semua PT" & input$rumpun != "Semua Rumpun"){
      temp<- SiteInfo %>% dplyr::filter(PROGRAM_STUDI != ".SEMUA PRODI", rumpun == input$rumpun)
      output$ui_prodi <- renderUI({
        multiInput("kode_prodi", "Pilih minimal 1", choices = unique(sort(temp$PROGRAM_STUDI)))
      })
    } else if(input$pilih_pt !="Semua PT" & input$rumpun == "Semua Rumpun"){
      if(is.null(input$kode_univ)){
        selected_univ<- unique(SiteInfo$NAMA_UNIV)
      } else{
        selected_univ <- input$kode_univ
      }
      temp<- SiteInfo %>% dplyr::filter(NAMA_UNIV %in% c(selected_univ)) %>% filter(PROGRAM_STUDI != ".SEMUA PRODI")
      output$ui_prodi <- renderUI({
        multiInput("kode_prodi", "Pilih minimal 1", choices = unique(sort(temp$PROGRAM_STUDI)))
      })
    } else if(input$pilih_pt !="Semua PT" & input$rumpun !="Semua Rumpun"){
      if(is.null(input$kode_univ)){
        selected_univ<- unique(SiteInfo$NAMA_UNIV)
      } else{
        selected_univ <- input$kode_univ
      }
      
      temp<- SiteInfo %>% dplyr::filter(NAMA_UNIV %in% c(selected_univ), rumpun == input$rumpun)  %>% filter(PROGRAM_STUDI != ".SEMUA PRODI")
      pilihan_prodi<- temp$PROGRAM_STUDI
      output$ui_prodi <- renderUI({
        multiInput("kode_prodi", "Pilih minimal 1", choices = unique(sort(pilihan_prodi)))
      })
    } else{
      temp<- SiteInfo %>% dplyr::filter(PROGRAM_STUDI != ".SEMUA PRODI")
      output$ui_prodi <- renderUI({
        multiInput("kode_prodi", "Pilih minimal 1", choices = unique(sort(temp$PROGRAM_STUDI)))
      })
    } 
  })
  
  # choice di fitur profil prodi
  data_profil<- read.csv("data/Full.csv")
  pilihan_univ<- data_profil$NAMA_UNIV
  updateSelectizeInput(session, 'profil_univ', choices = pilihan_univ[-1], 
                       server = TRUE, selected = "UNIVERSITAS TADULAKO")
  
  SiteInfo_profil<- data_profil
  observe({
    if(!is.null(input$profil_univ)){
      subUnivSelected_profil <- input$profil_univ
      temp<- SiteInfo_profil %>% dplyr::filter(NAMA_UNIV == subUnivSelected_profil)
      pilihan_prodi_profil<- temp$PROGRAM_STUDI
      output$ui_prodi_profil <- renderUI({
        selectizeInput(inputId ='ui_profil_prodi', choices = pilihan_prodi_profil[-1], 
                       label = "Program Studi", selected = "STATISTIKA")
      })
    }
  })
  
  # backend filter plot lihat_persaingan
  action_button <- eventReactive(input$action_button_saing, {
    
    region_filter<- (input$region)
    pt_filter<- (input$pilih_pt)
    prodi_filter<- (input$pilih_prodi)
    filter_univ<- (input$kode_univ)
    filter_prodi<- (input$kode_prodi)
    
    if(input$rumpun != "Semua Rumpun"){
      filter_rumpun <- input$rumpun
    } else {
      filter_rumpun<- c("Saintek", "Soshum")
    }
    data_filter<- read.csv("data/Full.csv")
    data_filter<- data_filter %>% filter(rumpun %in% filter_rumpun)
    
    if(region_filter == ".SEMUA REGION" & pt_filter == "Semua PT" & prodi_filter == "Semua Prodi"){
      testing_data <- data_filter %>% filter(REGION != ".SEMUA REGION")
    } else if(region_filter == ".SEMUA REGION" & pt_filter == "Semua PT" & prodi_filter != "Semua Prodi"){
      testing_data <- data_filter %>% filter(REGION != ".SEMUA REGION") %>% filter(PROGRAM_STUDI %in% c(filter_prodi))
    } else if(region_filter == ".SEMUA REGION" & pt_filter != "Semua PT" & prodi_filter == "Semua Prodi"){
      testing_data <- data_filter %>% filter(REGION != ".SEMUA REGION") %>% filter(NAMA_UNIV %in% c(filter_univ))
    } else if(region_filter == ".SEMUA REGION" & pt_filter != "Semua PT" & prodi_filter != "Semua Prodi"){
      testing_data <- data_filter %>% filter(REGION != ".SEMUA REGION") %>% filter(NAMA_UNIV %in% c(filter_univ),
                                                                                   PROGRAM_STUDI %in% c(filter_prodi))
    } else if(region_filter != ".SEMUA REGION" & pt_filter == "Semua PT" & prodi_filter == "Semua Prodi"){
      testing_data <- data_filter %>% filter(REGION == region_filter)
    } else if(region_filter != ".SEMUA REGION" & pt_filter == "Semua PT" & prodi_filter != "Semua Prodi"){
      testing_data <- data_filter %>% filter(REGION == region_filter) %>% filter(PROGRAM_STUDI %in% c(filter_prodi))
    } else if(region_filter != ".SEMUA REGION" & pt_filter != "Semua PT" & prodi_filter == "Semua Prodi"){
      testing_data <- data_filter %>% filter(REGION == region_filter) %>% filter(NAMA_UNIV %in% c(filter_univ))
    } else{
      testing_data <- data_filter %>% filter(REGION == region_filter) %>% filter(NAMA_UNIV %in% c(filter_univ),
                                                                                 PROGRAM_STUDI %in% c(filter_prodi))
    }
    
    grafik_saing_function <- function(data){
      data$PROGRAM_STUDI <- trimws(data$PROGRAM_STUDI)
      data <- data %>% mutate(TITLE = paste0(NAMA_UNIV,"\n", "(", PROGRAM_STUDI,"]"))
      data<- data %>%
        arrange((PEMINAT_2019)) %>%  
        mutate(TITLE=factor(TITLE, levels=TITLE))
      if(nrow(data) > 8){
        data <- data[(nrow(data)-9):nrow(data),]
      } else {
        data<-data
      }
      palette <- brewer.pal(5, "RdYlBu")[-(2:4)]
      kode_region <- region_filter
      ggplot(data, aes(x = PEMINAT_2019, y = TITLE, color = PEMINAT_2019)) +
        geom_point(size = 11) +
        geom_segment(aes(xend = 1, yend = TITLE), size = 2) +
        geom_text(aes(label = PEMINAT_2019), color = "white", size = 3) +
        scale_x_continuous("", expand = c(0,0), limits = c(1,max(data$PEMINAT_2019)+100), position = "top") +
        scale_color_gradientn(colors = palette) +
        labs(title = "Perbandingan Jumlah Peminat Program Studi SBMPTN 2019", 
             caption = "Source: LTMPT") +  theme(
               axis.title.y = element_text(size = 0),
               axis.text.y = element_text(size = 8))
    }
    
    grafik_saing_function(testing_data)
  })
  
  #handle error jika univ null
  observeEvent(input$action_button_saing, {
    if(input$pilih_pt == "Pilih PT" & is.null(input$kode_univ)){
      showModal(modalDialog(
        title = "Error",
        "Pilih minimal 1 Perguruan Tinggi"
      ))
    }
  })
  
  #handle error jika prodi null
  observeEvent(input$action_button_saing, {
    if(input$pilih_prodi == "Pilih Prodi" & is.null(input$kode_prodi)){
      showModal(modalDialog(
        title = "Error",
        "Pilih minimal 1 Program Studi"
      ))
    }
  })
  
  output$grafik_saing<- renderPlot({
    action_button()
    
  })
  
  ##box profil
  action_button_profil_info<- eventReactive(input$action_button_profil, {
    data_filter<- read.csv('data/data profil prodi.csv')
    data_filter<- data_filter %>% filter(NAMA_UNIV == input$profil_univ, PROGRAM_STUDI == input$ui_profil_prodi)
    info_data <- data_filter[c('NAMA_UNIV', 'AKREDITASI_PT', 'PROGRAM_STUDI',
                               'JENIS_PORTOFOLIO','DAYA_TAMPUNG__2020','DAYA_TAMPUNG__2019', 'PEMINAT__2019')]
    
    info_data <- info_data %>% mutate(PERSAINGAN_B = round(PEMINAT__2019/DAYA_TAMPUNG__2019))
    if(info_data$PERSAINGAN_B == 0){
      info_data$PERSAINGAN_B <-1
    }
    rangking_data<- read.csv("data/data_ranking.csv")
    
    p(paste0("Persaingan ", "1:", info_data$PERSAINGAN_B,
             " (1 dari ", info_data$PERSAINGAN_B, " orang diterima pada tahun 2019)"
    ))
  })
  
  output$box_profil<- renderUI({
    action_button_profil_info()
  })
  
  action_button_profil_info_pt<- eventReactive(input$action_button_profil, {
    data_filter<- read.csv('data/data profil prodi.csv')
    data_filter<- data_filter %>% filter(NAMA_UNIV == input$profil_univ, PROGRAM_STUDI == input$ui_profil_prodi)
    info_akreditasi <- data_filter['AKREDITASI_PT']
    
    rangking_data<- read.csv("data/data_ranking.csv")
    rangking_data<- rangking_data %>% filter(Nama.PT == input$profil_univ)
    
    p(paste0("Akreditasi Perguruan Tinggi ", info_akreditasi$AKREDITASI_PT,". "))
  })
  
  output$box_profil_pt<- renderUI({
    action_button_profil_info_pt()
  })
  
  action_button_profil_grafik<- eventReactive(input$action_button_profil, {
    data_filter<- read.csv('data/data profil prodi.csv')
    
    line_data <- data_filter[c('NAMA_UNIV', 'PROGRAM_STUDI', 'DAYA_TAMPUNG__2020', 'PEMINAT__2020',
                               'DAYA_TAMPUNG__2019', 'DAYA_TAMPUNG__2018', 'DAYA_TAMPUNG__2017', 
                               'PEMINAT__2019', 'PEMINAT__2018', 'PEMINAT__2017')]
    
    line_data<- line_data %>% gather("Metrik", "Total", c('DAYA_TAMPUNG__2020', 'PEMINAT__2020',
                                                          'DAYA_TAMPUNG__2019', 'DAYA_TAMPUNG__2018',
                                                          'DAYA_TAMPUNG__2017',
                                                          'PEMINAT__2019', 'PEMINAT__2018', 'PEMINAT__2017'))
    line_data<- as_tibble(line_data)
    line_data<- line_data %>% filter(NAMA_UNIV == input$profil_univ, PROGRAM_STUDI == input$ui_profil_prodi)
    line_data<- separate(line_data, Metrik, into = c("Metrik", "Tahun"), sep = "__")
    
    line_data$Tahun<- as.numeric(line_data$Tahun)
    
    kampus <- input$profil_univ
    prodi <- input$ui_profil_prodi
    AA<-line_data %>% ggplot(aes(x = Tahun, y = Total, group = Metrik)) + 
      geom_line(aes(color = Metrik)) +
      scale_color_manual(values=c('#ff7f50','#008080'))+
      labs(title = paste0(kampus, ": ", prodi), subtitle = "A subtitle") +
      ylab("Jumlah") +
      annotate("text", x = 2018, y = mean(line_data$Total[1:2]), label = "Peminat 2020 di Estimasi\n 
           dengan Triple Exponential Smoothing", size = 3) +
      geom_point(shape=21, fill="#69b3a2", size=3) + theme_grey()
    ggplotly(AA)
    
    
    
  })
  
  output$grafik_profil<- renderPlotly({
    action_button_profil_grafik()
  })
  
  output$lihat_data_pt<- DT::renderDataTable({
    data_sulawesi <- read.csv("data/Full.csv")
    data_sulawesi <- data_sulawesi[-c(1, 3291, 3292, 3293, 3294),]
    data_sulawesi<- data_sulawesi %>% 
      mutate(peluang = DAYA_TAMPUNG_2020/PEMINAT_2019*100) %>%
      mutate(peluang4 = round(peluang, 3))
    
    data_sulawesi$peluang1 <- numeric(length = length(data_sulawesi$REGION))
    
    for (i in 1:length(data_sulawesi$REGION)) {
      if(data_sulawesi$peluang4[i] == Inf){
        data_sulawesi$peluang1[i] <- "-"
      } else if(data_sulawesi$peluang4[i] > 100){
        data_sulawesi$peluang1[i] <- 100
      } else{
        data_sulawesi$peluang1[i] <- data_sulawesi$peluang4[i]
      }
    }
    
    data_sulawesi<- data_sulawesi %>%
      mutate(peluang2 = as.character(peluang1)) %>%
      mutate(PELUANG = paste0(peluang2, "%")) %>% mutate(PERSAINGAN_B = round(PEMINAT_2019/DAYA_TAMPUNG_2019)) %>% 
      mutate(PERSAINGAN = paste0("1:",PERSAINGAN_B))
    data_sulawesi<- data_sulawesi[c('REGION','NAMA_UNIV','PROGRAM_STUDI', 'DAYA_TAMPUNG_2020',
                                    'PEMINAT_2019', 'PELUANG', 'PERSAINGAN')]
    datatable(data_sulawesi, filter = 'top', options = list(
      pageLength = 5, autoWidth = F, scrollX = T))
  })
  
  output$radio_jumlah_perlembaga<- renderUI({
    data1<- read.csv("data/1 lembaga berdasarkan bentuk pendidikan.csv")
    selecti<- filter(data1, Provinsi != "Indonesia")
    pilih_daerah<- factor(data1$Provinsi, levels = c("Indonesia", unique(selecti$Provinsi)))
    selectizeInput("jumlah_per_lembaga","Pilih Daerah", 
                   choices = pilih_daerah,selected = "Indonesia",
                   width = '50%')
  })
  
  output$label_jumlah_per_lembaga<- renderUI({
    box(width = 12,title = paste0("Lembaga berdasarkan bentuk Pendidikan di ",input$jumlah_per_lembaga,
                             "\n Tahun 2018"), 
           withSpinner(plotlyOutput("bar_bentuk_lembaga")))
  })
  
  output$bar_bentuk_lembaga <- renderPlotly({
    data1<- read_csv("data/1 lembaga berdasarkan bentuk pendidikan.csv")
    if(input$jumlah_per_lembaga == "Indonesia"){
      data1<- data1 %>% filter(Provinsi == "Indonesia")
    } else{
      data1<- data1 %>%  filter(Provinsi == input$jumlah_per_lembaga)
    }
    data1<-data1[,-8]
    subtitle = paste0("Di ", input$jumlah_per_lembaga)
    
    data1<- data1 %>% 
      gather(key = "Bentuk Lembaga Pendidikan", value = "Jumlah", -1) %>%
      arrange(`Bentuk Lembaga Pendidikan`)
    
    fig <- plot_ly(data = data1,
                   x = ~`Bentuk Lembaga Pendidikan`,
                   y = ~Jumlah, text = ~Jumlah, textposition = 'auto',
                   name = "SF Zoo",
                   type = "bar",
                   marker = list(color = '#008080',
                                 line = list(color = 'rgb(8,48,107)',
                                             width = 1.5))
    ) 
    
    fig %>% layout(xaxis = list(title = "Bentuk Pendidikan"),
                   yaxis = list(title = "Jumlah"),
                   barmode = 'stack',
                   paper_bgcolor = 'rgba(245, 246, 249, 1)',
                   plot_bgcolor = 'rgba(245, 246, 249, 1)',
                   showlegend = FALSE)
    
  })
  
  output$bar_akreditasi_pt<- renderPlotly({
    data4<- read_csv("data/3 AKREDITASI PERGURUAN TINGGI.csv")
    Jumlah_Prodi<- input$pilih_akreditasi_prodi
    Jumlah_Prodi<- data4[[Jumlah_Prodi]]
    data4$Jumlah_Prodi<- Jumlah_Prodi
    data5<- data4[c('Provinsi', 'Jumlah_Prodi')]
    
    fig <- plot_ly(data5, x = ~Jumlah_Prodi, y = ~reorder(Provinsi, Jumlah_Prodi), type = 'bar', orientation = 'h',
                   text = ~Jumlah_Prodi, textposition = 'auto',
                   marker = list(color = '#008080',
                                 line = list(color = 'rgba(50, 171, 96, 1.0)', width = 1))) %>% 
      layout(xaxis = list(title = "Jumlah"),
             yaxis = list (title = "Provinsi"),
             paper_bgcolor = 'rgba(245, 246, 249, 1)',
             plot_bgcolor = 'rgba(245, 246, 249, 1)',
             showlegend = FALSE)
    
    fig
  })
  
  output$bar_akreditasi_prodi<- renderPlotly({
    data4<- read_csv("data/4 AKREDITASI PROGRAM STUDI.csv")
    Jumlah_Prodi<- input$pilih_akreditasi_prodi
    Jumlah_Prodi<- data4[[Jumlah_Prodi]]
    data4$Jumlah_Prodi<- Jumlah_Prodi
    data5<- data4[c('Provinsi', 'Jumlah_Prodi')]
    
    fig <- plot_ly(data5, x = ~Jumlah_Prodi, y = ~reorder(Provinsi, Jumlah_Prodi), type = 'bar', orientation = 'h',
                   text = ~Jumlah_Prodi, textposition = 'auto',
                   marker = list(color = '#008080',
                            line = list(color = 'rgba(50, 171, 96, 1.0)', width = 1))) %>% 
      layout(xaxis = list(title = "Jumlah"),
                     yaxis = list (title = "Provinsi"),
                     paper_bgcolor = 'rgba(245, 246, 249, 1)',
                     plot_bgcolor = 'rgba(245, 246, 249, 1)',
                    showlegend = FALSE)
    
    fig
  })
  
  output$bar_jenjang_prodi<- renderPlotly({
    data4<- read_csv("data/6 PROGRAM STUDI MENURUT JENJANG PROGRAM.csv")
    Jumlah_Prodi<- input$pilih_jenjang_prodi
    Jumlah_Prodi<- data4[[Jumlah_Prodi]]
    data4$Jumlah_Prodi<- Jumlah_Prodi
    data5<- data4[c('Provinsi', 'Jumlah_Prodi')]
    
    fig <- plot_ly(data5, x = ~Jumlah_Prodi, y = ~reorder(Provinsi, Jumlah_Prodi), type = 'bar', orientation = 'h',
                   text = ~Jumlah_Prodi, textposition = 'auto',
                   marker = list(color = '#008080',
                                 line = list(color = 'rgba(50, 171, 96, 1.0)', width = 1))) %>% 
      layout(xaxis = list(title = "Jumlah"),
             yaxis = list (title = "Provinsi"),
             paper_bgcolor = 'rgba(245, 246, 249, 1)',
             plot_bgcolor = 'rgba(245, 246, 249, 1)',
             showlegend = FALSE)
    
    fig
  })
  
  output$data_table_lembaga<- DT::renderDataTable({
    if(input$pilih_tabel_lembaga == "Bentuk Pendidikan"){
      data_lembaga<- read_csv("data/1 lembaga berdasarkan bentuk pendidikan.csv")
    } else if(input$pilih_tabel_lembaga == "Perkembangan Perguruan Tinggi"){
      data_lembaga<- read_csv("data/2 PERKEMBANGAN JUMLAH LEMBAGA PERGURUAN TINGGI.csv")
    } else if(input$pilih_tabel_lembaga == "Akreditas Perguruan Tinggi"){
      data_lembaga<- read_csv("data/3 AKREDITASI PERGURUAN TINGGI.csv")
    } else{
      data_lembaga<- read_csv("data/4 AKREDITASI PROGRAM STUDI.csv")
    }
    
    datatable(data_lembaga, filter = "top", options = list(pageLength = 5))
    
  })
  
  output$provinsi_dosen_tetap<- renderUI({
    data10<- read_csv("data/10 Jumlah dosen tetap DAN PENDIDIKAN TERTINGGI.csv")
    selectInput("provinsi_dostep", "Pilih Wilayah",choices = unique(data10$Wilayah))
  })
  
  output$pie_dostep<- renderPlotly({
    data10<- read_csv("data/10 Jumlah dosen tetap DAN PENDIDIKAN TERTINGGI.csv")
    
    wilayah_dostep<- input$provinsi_dostep
    data10<- data10 %>% filter(Wilayah == wilayah_dostep) %>%
      gather("Pendidikan Tertinggi", "Jumlah", -1) %>%
      mutate(Persentase = paste0(round(Jumlah / max(Jumlah) *100, 2)," %")) %>%
      mutate("Pendidikan Tertinggi (%)" = paste0(`Pendidikan Tertinggi`," (",Persentase,")"))
    
    data10<- data10[-7,]
    
    data10$`Pendidikan Tertinggi` = factor(data10$`Pendidikan Tertinggi`,
                                           levels = c("D1-D4", "S1", "S2", "S3", "Profesi","Spesialis"))
    jumlah_dostep<-data10 %>% summarise(sum(Jumlah))
    
    colors <- c('#008080','#5F9EA0', '#20B2AA','#66CDAA','#00CED1', '#E0FFFF', '#E0FFFF')
    
    fig <- plot_ly(data10, labels = ~`Pendidikan Tertinggi`, values = ~(Jumlah), type = 'pie',
                   textposition = 'outside',
                   textinfo = 'label+percent',
                   insidetextfont = list(color = '#FFFFFF'),
                   hoverinfo = 'text',
                   text = ~`Pendidikan Tertinggi (%)`,
                   marker = list(colors = colors,
                                 line = list(color = '#FFFFFF', width = 1)),
                   #The 'pull' attribute can also be used to create space between the sectors
                   showlegend = FALSE)
    fig <- fig %>% layout(#title = paste0("Persentase Jumlah Dosen Berdasarkan Pendidikan Tertinggi",
                           #              "Di ",wilayah_dostep, " 2018"),
                          xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE,
                                       text = "AAAAA"),
                          yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                          paper_bgcolor = 'rgba(245, 246, 249, 1)',
                          plot_bgcolor = 'rgba(245, 246, 249, 1)')
    
    fig
    
  })
  
  output$pie_prof<- renderChart2({
    data8<- read.csv("data/8 profesor menurut bidang ilmu.csv")
    
    data8<- data8 %>% mutate(Persentase = paste0(round(Jumlah / sum(Jumlah) *100,0)," %")) %>%
      mutate("Bidang.Ilmu.(%)" = paste0(Bidang.Ilmu," (",Persentase,")"))
    
    jumlah_prof<- data8 %>% summarise(sum(Jumlah)) 
    
    p1 = nPlot(x= "Bidang.Ilmu.(%)", y = "Jumlah",data = data8, type = 'pieChart')
    p1$print(include_assets=T)
    p1$chart(labelType= "percent")
    p1$chart(donut = T)
    p1$set(width = session$clientData$output_plot1_width)
    return(p1)
    p1
  })
  
  output$bar_rasio_prof<- renderPlotly({
    data9<- read.csv("data/9 Sebaran Dosen Profesor.csv")
    
    data9<- data9 %>% arrange(desc(Rasio.Sebaran.Professor))
    
    f <- list(
      family = "Courier New, monospace",
      size = 18,
      color = "#7f7f7f"
    )
    fig <- plot_ly(data9, x=~ Rasio.Sebaran.Professor, y =~ reorder(Wilayah, Rasio.Sebaran.Professor),
                   text = ~Rasio.Sebaran.Professor, textposition = 'auto',
                   type = 'bar', orientation = 'h', 
                   marker = list(color = 'rgba(50, 171, 96, 0.6)',
                                 line = list(color = 'rgba(50, 171, 96, 1.0)', width = 1))) %>%
      layout(#title = 'Household savings & net worth for eight OECD countries',
             font = list(size = 10),
             paper_bgcolor = 'rgba(245, 246, 249, 1)',
             plot_bgcolor = 'rgba(245, 246, 249, 1)',
             xaxis = list(
               title = "Rasio",
               titlefont = f
             ),
             yaxis= list(
               title = "Region",
               titlefont = f))
    
    fig
  })
  
  output$radio_dosen<- renderUI({
    data14<- read_csv("data/14 PERKEMBANGAN JUMLAH DOSEN PERGURUAN TINGGI.csv")
    pilih_daerah<- factor(data14$Provinsi)
    selectizeInput("provinsi_dosen","Pilih Daerah", 
                   choices = pilih_daerah,selected = "Indonesia",
                   width = '35%')
  })
  
  output$column_umur_dosen <- renderPlotly({
    data13<- read_csv("data/13 JUMLAH DOSEN MENURUT KELOMPOK UMUR.csv")
    data13<- data13 %>%  filter(Provinsi == input$provinsi_dosen)
    data13 = data13 %>% gather(key = "Kelompok_Umur", value = "Jumlah", -c(1:2))
    data13$Kelompok_Umur<- factor(data13$Kelompok_Umur,
                                  levels = c("<25 th","26-35 th", "36-45 th", "46-55 th",
                                             "56-65 th", ">65 th"))
    fig <- plot_ly(data13, x = ~Kelompok_Umur, y = ~Jumlah,
                   type = "bar",
                   text = ~Jumlah, textposition = 'auto',
                   marker = list(color = '#008080',
                                 line = list(color = 'rgb(8,48,107)',
                                             width = 1.5)))
    
    fig %>% 
      
      layout(title = list(size =1, text = "Jumlah Dosen Menurut\nKelompok Umur (2018)"),
             xaxis = list(title = "Kelompok Umur"),
             yaxis = list (title = "Jumlah"),
             autosize = T, margin = list(l=50, r=50, b=100, t=100, pad=4),
             paper_bgcolor = 'rgba(245, 246, 249, 1)',
             plot_bgcolor = 'rgba(245, 246, 249, 1)')
    
   
    
  })
  
  output$dosen_jabatan <- renderPlotly({
    data13<- read_csv("data/12 JUMLAH DOSEN MENURUT JABATAN FUNGSIONAL.csv")
    data13<- data13 %>%  filter(Provinsi == input$provinsi_dosen)
    data13 = data13 %>% gather(key = "Jabatan", value = "Jumlah", -1)
    fig <- plot_ly(data13, x = ~Jabatan, y = ~Jumlah,
                   type = "bar",
                   text = ~Jumlah, textposition = 'auto',
                   marker = list(color = '#008080',
                                 line = list(color = 'rgb(8,48,107)',
                                             width = 1.5)))
    
    fig %>% 
      
      layout(title = list(size =1, text = "Jumlah Dosen Menurut\nJabatan Fungsional (2018)"),
             xaxis = list(title = "Jabatan"),
             yaxis = list (title = "Jumlah"),
             autosize = T, margin = list(l=50, r=50, b=100, t=100, pad=4),
             paper_bgcolor = 'rgba(245, 246, 249, 1)',
             plot_bgcolor = 'rgba(245, 246, 249, 1)')
    
    
    
  })
  
  output$data_tabel_dosen <- DT::renderDataTable({
    
    tabel_dosen<- input$tabel_dosen
    #tabel_dosen<- "Pendidikan Tertinggi"
    if(tabel_dosen == "Bentuk Pendidikan") {
      tabel_data<- read_csv("data/7 JUMLAH DOSEN MENURUT BENTUK PENDIDIKAN.csv")
      datatable(tabel_data, filter = "top", options = list(pageLength = 5))
    } else if (tabel_dosen == "Kelompok Umur"){
      tabel_data<- read_csv("data/13 JUMLAH DOSEN MENURUT KELOMPOK UMUR.csv")
      datatable(tabel_data, filter = "top", options = list(pageLength = 5))
    } else {
      tabel_data<- read_csv("data/14 PERKEMBANGAN JUMLAH DOSEN PERGURUAN TINGGI.csv")
      datatable(tabel_data, filter = "top", options = list(pageLength = 5))
    }
    
  })
  
  output$do_bidang_ilmu<- renderPlotly({
    data15<- read_csv("data/15 Rasio DO Bidang Ilmu.csv")
    
    fig <- plot_ly(data15, x = ~Rasio, y = ~reorder(Bidang_Ilmu, Rasio), type = 'bar', orientation = 'h',
                   text = ~Rasio, textposition = 'auto',
                   marker = list(color = ~warna,
                                 line = list(color = 'rgba(50, 171, 96, 1.0)', width = 1))) %>% 
      layout(title = list(size =1, text = "Rasio Mahasiswa Putus Kuliah\nTahun 2018"),
             xaxis = list(title = "Kelompok Umur"),
             yaxis = list (title = "Jumlah"),
             autosize = T, margin = list(l=50, r=50, b=100, t=100, pad=4),
             paper_bgcolor = 'rgba(245, 246, 249, 1)',
             plot_bgcolor = 'rgba(245, 246, 249, 1)')
    fig
  })
  
  output$apk<- renderPlotly({
    data18<- read_csv("data/18 APK.csv")
    data18$Tahun<- as.factor(data18$Tahun)
    data18 %>%
      plot_ly(x = ~Tahun,
              text = ~APK,
              hoverinfo = 'text',
              showlegend = FALSE) %>%
      add_trace(y = ~APK,
                type = 'scatter',
                mode = 'lines+markers+text', 
                line = list(color = 'teal', 
                            width = 3),
                textposition = "bottom center", # here the text position
                marker = list(color = 'black', 
                              size = 8)) %>%
      layout(xaxis = list(title = "Tahun"),
             title = list(text = 'Angka Partisipasi Kasar\n(Umur 19-23)'),
             yaxis = list (title = "APK"),
             autosize = T, margin = list(l=50, r=50, b=100, t=100, pad=4),
             paper_bgcolor = 'rgba(245, 246, 249, 1)',
             plot_bgcolor = 'rgba(245, 246, 249, 1)',
             showlegend = FALSE)
  })
  
  output$do_jk<- renderEcharts4r({
    data17<- read_csv("data/17 DO menurut jenis kelamin.csv")
    
    data17 %>% 
      e_charts(`Jenis Kelamin`) %>% 
      e_pie(`Jumlah Putus Kuliah`) %>% 
      e_title("Pie","Persentase D.O Berdasarkan Jenis Kelamin (2018)") %>%
      e_color(color = c("teal", "coral")) %>%
      e_tooltip()
  })
  
  output$jumlah_mahasiswa <- renderPlotly({
    data16<- read_csv("data/16 mahasiswa.csv")
    data16<- data16[c('Wilayah', 'Jumlah Mahasiswa', 'Kode_Wilayah')]
    data16<- read_csv("data/16 mahasiswa.csv")
    data16<- data16[c('Wilayah', 'Jumlah Mahasiswa', 'Kode_Wilayah')]
    fig <- plot_ly(data16, x = ~`Jumlah Mahasiswa`, y = ~reorder(Wilayah, `Jumlah Mahasiswa`), 
                   type = 'bar', orientation = 'h',
                   text = ~`Jumlah Mahasiswa`, textposition = 'auto',
                   marker = list(color = ~Kode_Wilayah,
                                 line = list(color = 'rgba(50, 171, 96, 1.0)', width = 1))) %>% 
      layout(xaxis = list(title = "Jumlah Mahasiswa"),
             title = "Jumlah Mahasiswa Tahun 2018",
             autosize = T, margin = list(l=50, r=50, b=100, t=100, pad=4),
             yaxis = list (title = "Provinsi"),
             paper_bgcolor = 'rgba(245, 246, 249, 1)',
             plot_bgcolor = 'rgba(245, 246, 249, 1)',
             showlegend = FALSE)
    fig
  })
  
  output$putus_kuliah <- renderPlotly({
    data16<- read_csv("data/16 mahasiswa.csv")
    data16<- data16[c('Wilayah', 'Putus Kuliah', 'Kode_Wilayah')]
    
    fig <- plot_ly(data16, x = ~`Putus Kuliah`, y = ~reorder(Wilayah, `Putus Kuliah`), 
                   type = 'bar', orientation = 'h',
                   text = ~`Putus Kuliah`, textposition = 'auto',
                   marker = list(color = ~Kode_Wilayah,
                                 line = list(color = 'rgba(50, 171, 96, 1.0)', width = 1))) %>% 
      layout(xaxis = list(title = "Putus Kuliah"),
             title = "Jumlah Putus Kuliah Tahun 2018",
             autosize = T, margin = list(l=50, r=50, b=100, t=100, pad=4),
             yaxis = list (title = "Provinsi"),
             paper_bgcolor = 'rgba(245, 246, 249, 1)',
             plot_bgcolor = 'rgba(245, 246, 249, 1)',
             showlegend = FALSE)
  })
  
  output$lulusan <- renderPlotly({
    data16<- read_csv("data/16 mahasiswa.csv")
    data16<- data16[c('Wilayah', 'Lulusan', 'Kode_Wilayah')]
    
    fig <- plot_ly(data16, x = ~`Lulusan`, y = ~reorder(Wilayah, `Lulusan`), 
                   type = 'bar', orientation = 'h',
                   text = ~Lulusan, textposition = 'auto',
                   marker = list(color = ~Kode_Wilayah,
                                 line = list(color = 'rgba(50, 171, 96, 1.0)', width = 1))) %>% 
      layout(xaxis = list(title = "Lulusan"),
             title = "Jumlah Lulusan Tahun 2018",
             autosize = T, margin = list(l=50, r=50, b=100, t=100, pad=4),
             yaxis = list (title = "Provinsi"),
             paper_bgcolor = 'rgba(245, 246, 249, 1)',
             plot_bgcolor = 'rgba(245, 246, 249, 1)',
             showlegend = FALSE)
  })
  
  output$peta_akreditasi<- renderUI({
    selectInput("pilih_akreditasi", "Pilih Akreditasi", choices = c("A","B","C","Semua Akreditasi"),
                selected = "Semua Akreditasi")
  })
  
  output$prov_sebaran_dosen<- renderUI({
    data7<- read_csv("data/7 JUMLAH DOSEN MENURUT BENTUK PENDIDIKAN.csv")
    selectInput("pilih_bentuk_pendidikan", "Pilih Bentuk Pendidikan", choices = unique(names(data7[-1])),
                selected = "Semua Bentuk Pendidikan")
  })
  
 
}

shinyApp(ui, server)
