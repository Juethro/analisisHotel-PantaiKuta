library(shiny)
library(plotly)
library(ggplot2)
library(wesanderson)
library(VIM)
library(shinydashboard)
library(reshape2)
library(dplyr)

df <- read.csv(file = "Dataset Baru - Sheet1.csv")
head(df)

databaru <- df[-c(1,7)]
databaru
#normalisasi data numerik
minmax_norm <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}

datanorm <- as.data.frame(lapply(databaru[1:6], minmax_norm))
head(datanorm)

aggr_plot <- aggr(databaru, col=wes_palette(n=2, name='Royal2'), numbers=TRUE,
                  sortVars=TRUE, labels=names(databaru), cex.axis=.7,
                  gap=3, ylab=c("Missing Value","Pattern"))


d1 <- ggplot(data = datanorm, aes(x = Jarak, y = Harga)) +
  geom_point() +
  geom_smooth(method = lm) +
  ggtitle("Jarak")
d1
# Harga vs Total Tipe Kamar
d2 <- ggplot(data = datanorm, aes(x = Total.Tipe.Kamar , y = Harga)) +
  geom_point() +
  geom_smooth(method = lm) +
  ggtitle("Total Tipe Kamar")

# Harga vs Rating Penginapan
d3 <- ggplot(data = datanorm, aes(x = Rating.Penginapan, y = Harga)) +
  geom_point() +
  geom_smooth(method = lm) +
  ggtitle("Rating Penginapan")

# Harga vs Rata-Rata Diskon
d4 <- ggplot(data = datanorm, aes(x = Rata.rata.diskon, y = Harga)) +
  geom_point() +
  geom_smooth(method = lm) +
  ggtitle("Rata-Rata Diskon")


## Histogram dengan kurva distribusi normal


corr_mat <- round(cor(datanorm),2)
melted_corr_mat <- melt(corr_mat)

heatt <- ggplot(data = melted_corr_mat, aes(x=Var1, y=Var2,
                                   fill=value)) +
                geom_tile() +
                geom_text(aes(Var2, Var1, label = value),
                          color = "white", size = 4)

freq <- df$Tipe.Penginapan %>% as.data.frame %>%
  rename(Var='.') %>%
  group_by(Var) %>% summarise(N=n()) %>%
  ggplot(aes(x=Var,y=N,fill=Var))+
  geom_bar(stat = 'identity',color='black')+
  scale_y_continuous(labels = scales::comma_format(accuracy = 2))+
  geom_text(aes(label=N),vjust=-0.25,fontface='bold')

binn <- df$Bintang %>% as.data.frame %>%
  rename(Var='.') %>%
  group_by(Var) %>% summarise(N=n()) %>%
  ggplot(aes(x=Var,y=N,fill=Var))+
  geom_bar(stat = 'identity',color='black')+
  scale_y_continuous(labels = scales::comma_format(accuracy = 2))+
  geom_text(aes(label=N),vjust=-0.25,fontface='bold')



# Define UI for application that draws a histogram
ui <- fluidPage(
  dashboardPage(
    dashboardHeader(title = "Analisis Hotel"),
    
    dashboardSidebar(
      sidebarMenu(
        menuItem("Pendahuluan", tabName = "Pendahuluan", icon = icon("th")),
        menuItem("DashBoard", tabName = "Analisis", icon = icon("far fa-chart-bar")),
        menuItem("Kesimpulan", tabName = "Kesimpulan", icon = icon("fas fa-poll-h"))
      )
    ),
    
    dashboardBody(
      tabItems(
        tabItem(tabName = "Pendahuluan",
                tabBox(width = 12,
                       tabPanel(title = "Latar Belakang",
                                icon = icon('address-card'),
                                fluidRow(
                                  column(width = 6,img(src='https://www.rukita.co/stories/wp-content/uploads/2022/04/BeSS-Mansion-Hotel-Surabaya-3.jpg',
                                                       width=400),
                                         br(),
                                         a('Ini Hotel'),
                                         align = 'center'),
                                  column(width = 6,img(src='https://img.inews.co.id/media/822/files/inews_new/2021/10/06/IMG_06102021_163518__822_x_430_piksel_.jpg',
                                                       height = 227),
                                         br(),
                                         a('Ini Hotel Juga'),
                                         align = 'center')),
                                fluidRow(
                                  column(width = 12,br(),br(),
                                         p("Kelompok kami ingin memberikan rekomendasi kepada wisatawan yang ingin mencari tempat penginapan di 
                                           sekitar Pantai Kuta, Bali dengan menggunakan metode scraping berjudul “Analisis Penginapan di Sekitar Pantai Kuta, Bali 
                                           sebagai Rekomendasi untuk para Travellers”. Dengan rentang harga dari Rp. 0-1.000.000 terdapat berbagai macam tempat 
                                           penginapan seperti hotel, guest house, villa, homestay, dsb. Tujuan utama kami membuat analisis ini karena ingin 
                                           merekomendasikan tempat penginapan di sekitar area Pantai Kuta, Bali sesuai kebutuhan dan budget wisatawan.")
                                  ))
                                ),
                       
                       tabPanel(title = "Landasan Teori",
                                icon = icon('address-card'),
                                fluidRow(
                                  column(width = 12,
                                         h1('Penginapan'),
                                         p("Penginapan atau akomodasi saat bepergian atau liburan adalah jenis tempat tinggal dalam perjalanan di mana orang 
                                           yang harus tinggal jauh dari rumah lebih dari satu hari keperluan tempat untuk tidur, istirahat, keselamatan, tempat 
                                           berteduh dari suhu dingin atau hujan, penyimpanan barang, serta akses ke fungsi umum pada rumah tangga."),
                                         p("Penginapan dapat dilakukan pada hotel, resor, apartemen, hostel atau hostal, rumah pribadi (komersial, yaitu sebuah 
                                           tempat tamu untuk tidur yang mendapatkan sarapan pagi atau rumah sewa tempat liburan, yang non-komersial dengan 
                                           keanggotaan layanan keramahan atau tamu di rumah teman), dalam sebuah tenda saat berkemah (sering di perkemahan) 
                                           dengan termasuk masalah sampah."),br(),
                                         h1("Pantai"),
                                         p("Pantai atau pesisir adalah sebuah bentuk geografis yang terdiri dari pasir, dan terdapat di daerah pesisir laut. 
                                           Daerah pantai menjadi batas antara daratan dan perairan laut. Kawasan pantai berbeda dengan pesisir walaupun antara
                                           keduanya saling berkaitan. Panjang garis pantai diukur mengelilingi seluruh pantai yang merupakan daerah teritorial 
                                           suatu negara."),
                                         p("Menurut koreksi PBB tahun 2008, Indonesia merupakan negara dengan garis Pantai terpanjang keempat di dunia setelah 
                                           Amerika Serikat (USA), Kanada dan Rusia. Panjang garis pantai Indonesia tercatat sebesar 95.181 km."),br(),
                                         h1("Travellers"),
                                         p("Traveler merupakan istilah untuk seseorang yang sedang bepergian, arti kata bepergian memang sangat luas, sebagai 
                                           contoh jika kita pergi dari rumah menuju anggaplah sekolah maka hal tersebut juga termasuk kegiatan bepergian. 
                                           Namun dalam konteks traveler, bepergian bisa saja saat kita berkeliling kota Jakarta atau dalam jarak yang tidak 
                                           terlalu jauh pun sudah sah disebut sebagai traveler.")
                                         )
                                        )
                       
                       ),
                       tabPanel(title = "Sumber Data",
                                icon = icon('address-card'),
                                fluidRow(
                                  column(width = 12, 
                                         tableOutput('data')
                                         )
                                )
                                
                                
                       ),
                       tabPanel(title = "Metodologi",
                                icon = icon('address-card'),
                                fluidRow(
                                  column(width = 12,
                                         h1("Metodologi"),
                                         p("Pertama akan dilakukan data preprocessing, yakni visualisasi data dengan boxplot, 
                                           serta  mencari dan menangani outlier dan normalisasi. Kemudian dilanjutkan dengan 
                                           analisis deskriptif atau summary, yakni mengetahui mean, median Q1, Q3, IQR, dan 
                                           nilai minimal dan maksimal dari setiap variabel. Terakhir, dilakukan analisis 
                                           scatter plot untuk mencari korelasi antara variabel harga dengan variabel lainnya.")
                                         )
                                )
                                
                       )
                )),
        tabItem(tabName = "Analisis",
                
                tabBox(width = 12,
                       tabPanel(title = "Preprocessing",
                                fluidRow(
                                  column(width = 12,
                                         verbatimTextOutput('summary')
                                         )
                                ),br(),
                                fluidRow(
                                  column(width = 12,
                                         plotlyOutput('boxplot', height = 400)
                                         )
                                ),br(),
                                fluidRow(
                                  column(width = 6,
                                         plotOutput('miss')
                                         ),
                                  column(width = 6,
                                         verbatimTextOutput('missing')
                                         )
                                )
                                
                                ),
                       tabPanel(title = "Korelasi",
                                
                                fluidRow(
                                  column(width = 3,
                                         selectInput(
                                           inputId = "scatterr",
                                           label = "Atribut: ",
                                           choices = c(
                                             "Jarak" = "d1",
                                             "Jumlah Tipe Kamar" = "d2",
                                             "Rating Penginapan" = "d3",
                                             "Rata-rata Diskon" = "d4"
                                           )
                                         )),
                                  column(width = 9,
                                         plotOutput("scatter")
                                         )
                                ),
                                fluidRow(
                                  column( width = 12,
                                          plotOutput('heattt')
                                  )
                                ),
                                ),
                       
                       tabPanel(title = "Analisis",
                                fluidRow(
                                  column(width = 3,
                                         selectInput(
                                           inputId = "dist",
                                           label = "Atribut: ",
                                           choices = c(
                                             "Harga" = "Harga",
                                             "Jarak" = "Jarak",
                                             "Jumlah Tipe Kamar" = "Total.Tipe.Kamar",
                                             "Rating Penginapan" = "Rating.Penginapan",
                                             "Rata-rata Diskon" = "Rata.rata.diskon"
                                           )
                                         )
                                  ),
                                  column(width = 9,
                                         plotlyOutput("histoo", height = 380)
                                         )
                                ),
                                
                                fluidRow(
                                  column(width = 6,
                                         plotOutput("komposisi", height = 300)
                                         ),
                                  column(width = 6,
                                         plotOutput("buintang", height = 300)
                                         )
                                )
                                )
                
                      
                )),
        tabItem(tabName = "Kesimpulan",
                tabBox(width = 12,
                       h1("Kesimpulan"),
                       tags$ul(
                         tags$li("Pada dataset, ternyata tempat penginapan di sekitar Pantai Kuta, Bali didominasi oleh hotel, yaitu sebanyak 43 hotel, yang kedua guest house sebanyak 6, resort sebanyak 2, dan homestay sebanyak 1. Tempat penginapan tersebut berada pada rentang harga Rp. 0 - Rp. 1.000.000."), 
                         tags$li("Pada dataset, didapatkan rata-rata tempat penginapan di sekitar Pantai Kuta, Bali yaitu berbintang 3, yang berarti jumlah kamar standar minimal 30 kamar (luas minimal 24 m2) Memiliki minimal 2 kamar suite ( luas minimal 48 m2) dengan fasilitas kamar mandi, telepon, televisi, dan AC di dalam kamar."), 
                         tags$li("Pada data preprocessing yang dilakukan, tidak ditemukan adanya NA/missing value pada dataset. Terdapat outlier pada variabel harga, total tipe kamar, rating penginapan, rata-rata diskon, dan bintang."),
                         tags$li("Pada scatterplot yang diamati, tidak ada hubungan yang kuat antara variabel harga dengan peningkatan variabel jarak, total tipe kamar, rating penginapan, rata-rata-diskon, tipe penginapan, dan bintang penginapan."),
                         tags$li("Pada visualisasi histogram dapat ditarik kesimpulan :",
                           tags$ul(
                             tags$li("Pada variabel harga, distribusi miring ke kanan yaitu memiliki kemiringan positif, yang berarti distribusi harga pada tempat penginapan sekitar Pantai Kuta, Bali sebagian besar diantara harga Rp. 0 - Rp. 1.000.000, tetapi dengan kemiringan ke kanan, harga penginapan yang lain bisa lebih tinggi."),
                             tags$li("Pada variabel jarak, distribusi miring ke kiri yaitu memiliki kemiringan negatif, yang berarti distribusi jarak pada tempat penginapan sekitar Pantai Kuta, Bali sebagian besar berjarak 100 m - 800 m dari pantai, tetapi dengan kemiringan ke kiri, jarak tempat penginapan yang lain tidak ada yang kurang dari jarak tersebut."),
                             tags$li("Pada variabel jumlah tipe kamar, distribusi miring ke kanan yaitu memiliki kemiringan positif, yang berarti distribusi jumlah tipe kamar pada tempat penginapan sekitar Pantai Kuta, Bali sebagian besar terdapat 1-17 tipe kamar saja, tetapi dengan kemiringan ke kanan, jumlah tipe kamar  penginapan yang lain bisa lebih banyak."),
                             tags$li("Pada variabel rating penginapan, distribusi miring ke kiri yaitu memiliki kemiringan negatif, yang berarti distribusi rating penginapan pada tempat penginapan sekitar Pantai Kuta, Bali sebagian besar berada pada rating 7-9, tetapi dengan kemiringan ke kiri, rating tempat penginapan yang lain tidak ada yang kurang dari rating tersebut."),
                             tags$li("Pada variabel rata-rata diskon, diketahui bahwa terdistribusi secara normal, sehingga rata-rata diskon tempat penginapan di sekitar Pantai Kuta, Bali adalah 25%, tetapi dengan variabel yang berdistribusi normal berarti terdapat tempat penginapan yang mempunyai diskon lebih tinggi dan lebih rendah daripada rata-rata diskon.")
                           )
                         )
                       )
                       )
                )
        
      )
    )
  )

    
)

# Back End
server <- function(input, output) {
  
  #Sumber Data
  output$data <- renderTable(df)
  #summary
  output$summary <- renderPrint(summary(df))
  #boxplot
  output$boxplot <- renderPlotly({
    fig <- plot_ly(y = datanorm$Harga, type = "box", quartilemethod="linear", name="Harga")
    fig <- fig %>% add_trace(y = datanorm$Jarak , quartilemethod="linear", name="Jarak")
    fig <- fig %>% add_trace(y = datanorm$Total.Tipe.Kamar , quartilemethod="linear", name="Total Tipe Kamar")
    fig <- fig %>% add_trace(y = datanorm$Rating.Penginapan , quartilemethod="linear", name="Rating Penginapan")
    fig <- fig %>% add_trace(y = datanorm$Rata.rata.diskon , quartilemethod="linear", name="Diskon")
    fig <- fig %>% add_trace(y = datanorm$Bintang , quartilemethod="linear", name="Bintang")
    fig <- fig %>% layout(title = "Boxplot Outlier Dataset")
    
    fig
  })
  
  #missing data
  output$miss <- renderPlot(
    aggr(databaru, col=wes_palette(n=2, name='Royal2'), numbers=TRUE,
         sortVars=TRUE, labels=names(databaru), cex.axis=.7,
         gap=3, ylab=c("Missing Value","Pattern"))
  )
  output$missing <- renderPrint(summary(aggr_plot))
  
  #korelasi
  output$scatter <- renderPlot(get(input$scatterr))
  output$heattt <- renderPlot(heatt)
  
  #analisis (distribusi)
  output$histoo <- renderPlotly({
    gg <- ggplot(datanorm,aes(x = get(input$dist), color = 'density')) +  
      geom_histogram(aes(y = ..density..), bins = 7,  fill = '#67B7D1', alpha = 0.5) +  
      geom_density(color = '#67B7D1') +  
      geom_rug(color = '#67B7D1') + 
      ylab("") + 
      xlab("")  + theme(legend.title=element_blank()) +
      scale_color_manual(values = c('density' = '#67B7D1'))
    
    
    ggplotly(gg)%>% 
      layout(plot_bgcolor='#e5ecf6',   
             xaxis = list(   
               title="", 
               zerolinecolor = '#ffff',   
               zerolinewidth = 2,   
               gridcolor = 'ffff'),   
             yaxis = list(   
               title='', 
               zerolinecolor = '#ffff',   
               zerolinewidth = 2,   
               gridcolor = 'ffff')) 
  })
  
  #analisis bar chart frekuensi
  output$komposisi <- renderPlot(freq)
  output$buintang <- renderPlot(binn)

}

# Run the application 
shinyApp(ui = ui, server = server)
