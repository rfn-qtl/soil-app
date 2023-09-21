################
### Soil-App ###
################

library(shiny)
library(shinydashboard)
library(Matrix)
library(data.table)
library(lme4)
library(lmerTest)
library(agricolae)
library(corrgram)
library(car)
library(factoextra)
library(ggplot2)
library(tableHTML)
library(plotly)
library(qgraph)
library(cowplot)
library(corrr)

######################################################################################################################################################################################


header <- dashboardHeader(title = "Soil-App",
                          titleWidth = 250
                          #,
                          # dropdownMenu(
                          #   type = "notifications", 
                          #   icon = icon("exchange"),
                          #   badgeStatus = NULL,
                          #   headerText = "English Version:",
                          # notificationItem("Soil-App (Eng)", icon = icon("language"),
                          #                  href = "http://shiny.rstudio.com/"))
)

######################################################################################################################################################################################

sidebar <- dashboardSidebar(
  tags$head(tags$style(HTML("
                            @import url('//fonts.googleapis.com/css?family=Lobster|Cabin:400,700');
                            .main-header .logo {
                            font-family: 'Open Sans';
                            font-style: normal;
                            font-weight: bold;
                            font-size: 30px;
                            src: local('Lobster Regular'), local('Lobster-Regular'), url(http://fonts.gstatic.com/s/lobster/v20/RdfS2KomDWXvet4_dZQehvY6323mHUZFJMgTvxaG2iE.woff2) format('woff2');
                            unicode-range: U+0102-0103, U+0110-0111, U+1EA0-1EF9, U+20AB;
                            }
                            "))),
  width = 250,
  sidebarUserPanel("Menu:"),
  sidebarMenu(
    id = "tabs",
    menuItem("Início", tabName = "start",icon = icon("home")),
    menuItem("Conversão de Unidades",tabName = "aba11", icon = icon("angle-double-right")),
    menuItem("Análise de Solo", icon = icon("angle-double-right"),
             menuItem("Análise Física", icon = icon("angle-right"),tabName = "aba210"),
             menuItem("Análise Química", icon = icon("angle-right"),
                      menuSubItem("Nutrientes",tabName = "aba199"),
                      menuSubItem("Outros Parâmetros",tabName = "aba214"))),
    menuItem("Fertilizantes e Corretivos", icon = icon("angle-double-right"),
             menuItem("Fertilizantes", icon = icon("angle-right"),
                      menuSubItem("Recomendação", tabName = "aba23"),
                      menuSubItem("Fonte Simples", tabName = "aba1271"),
                      menuSubItem("Formulados", tabName = "aba1272"),
                      menuSubItem("Adubos Orgânicos", tabName = "aba1273")),
             menuItem("Calagem", icon = icon("angle-right"),
                      menuSubItem("Recomendação", tabName = "aba22"),
                      menuSubItem("Comparando Preços", tabName = "aba1261"),
                      menuSubItem("Atributos", tabName = "aba1262")),
             menuItem("Gessagem", icon = icon("angle-right"),tabName = "FR2211"),
             menuItem("Fosfatagem", icon = icon("angle-right"),tabName = "FR2213"),
             menuItem("Garantias Mínimas", icon = icon("angle-right"),
                      menuSubItem("Fertilizantes", tabName = "tabGA1"),
                      menuSubItem("Corretivos", tabName = "tabGA2"))),
    menuItem("Análise de Experimentos", icon = icon("angle-double-right"),
             menuItem("Delineamentos Experimentais", icon = icon("angle-right"),
                      menuSubItem("Conjunto de Dados", tabName = "aba121", icon = icon("th")),
                      menuSubItem("Estatística", tabName = "aba122"),
                      menuSubItem("Resultados", tabName = "aba1222")),
             menuItem("Análise Multivariada", icon = icon("angle-right"),
                      menuSubItem("Conjunto de Dados", tabName = "aba123", icon = icon("th")),
                      menuSubItem("Correlação", tabName = "aba124"),
                      menuSubItem("Componentes Principais", tabName = "aba125"))),
    menuItem("Informações", icon = icon("info-circle"),
             menuSubItem("Sobre Soil-App", tabName = "info1", icon = icon("angle-right")),
             menuSubItem("Contato", tabName = "info3", icon = icon("angle-right"))
    ),
    htmlTemplate("flagC.html",align = "center")))

# #####################################################################################################################################################################################
body <- dashboardBody(
  
  tabItems(
    # #####################################################################################################################################################################################
    
    tabItem("start", 
            shinyUI(fluidPage(
              tags$head(tags$style(HTML("
                                        #final_text {
                                        text-align: center;
                                        }
                                        div.box-header {
                                        text-align: center;
                                        }
                                        "))),
              box(status = "primary", solidHeader = TRUE,collapsible = TRUE, width = 12, align = "center",
                  title = "Para ser um agricultor de sucesso, é preciso primeiro conhecer a natureza do solo (Xenophon, Oeconomicus, 400 A.C).",
                  
                  a(id = "web_button", class = "btn action_button",
                    href= "http://www.genetica.esalq.usp.br/alogamas/R.html",
                    img(src = "logo1.jpg",align = "center",width = 700, height = 400)),
                  fluidRow(
                    infoBox(hr(), "ESALQ-USP",icon=icon("institution"), fill = TRUE, color = "olive",
                            href = "http://www4.esalq.usp.br/"),
                    infoBox(hr(), "Dpto. de Ciência do Solo",icon=icon("flask"), fill = TRUE, color = "olive",
                            href = "http://www.solos.esalq.usp.br/"),
                    infoBox(hr(), "Departamento de Genética",icon=icon("leaf"), fill = TRUE, color = "olive",
                            href = "http://www.genetica.esalq.usp.br/")
                  ))))),
    
    # #####################################################################################################################################################################################
    
    tabItem("aba11", 
            fluidPage(
              box(status = "primary",title = "Conversão de Unidades", solidHeader =T, width = "50%",
                  titlePanel(""),
                  sidebarLayout(
                    sidebarPanel( 
                      numericInput("res.aba11.1","Valor para Converter:",value = 100),
                      uiOutput("res.aba11.2")
                    ),
                    mainPanel(verbatimTextOutput("contents.aba11"))
                  )))),
    # #####################################################################################################################################################################################
    
    tabItem("aba121",
            fluidPage(
              box(status = "primary",title = "Carregue os Dados", solidHeader =T, width = "50%",
                  titlePanel(""),
                  sidebarLayout(
                    sidebarPanel(
                      fileInput('file1', 'Escolha o arquivo:',
                                accept=c('text/csv', 
                                         'text/comma-separated-values,text/plain', 
                                         '.csv',".txt")),
                      tags$hr(),
                      checkboxInput('header', 'Cabeçalho:', FALSE),
                      radioButtons('sep', 'Separador:',
                                   c(Comma =',',
                                     Semicolon=';',
                                     Tab ='\t'),
                                   ','),
                      radioButtons('quote', 'Aspas:',
                                   c(Nenhuma='',
                                     'Aspas Duplas'='"',
                                     'Aspas Simples'="'"),
                                   ''),
                      checkboxInput("info.11","Ajuda:", FALSE),
                      checkboxInput("ex.11","Exemplo:", TRUE)),
                    verbatimTextOutput("contents11")
                  )))),
    
    # #####################################################################################################################################################################################
    tabItem("aba122", 
            fluidPage(
              box(status = "primary",title = "Análise Estatística", solidHeader =T, width = "50%",
                  titlePanel(""),
                  sidebarLayout(
                    sidebarPanel( 
                      checkboxInput('res.12.name', 'Fontes de Variação:', FALSE),
                      textInput("model","Modelo estatístico:", width = 700,
                                value = "N~Bloco+Data+NBPT+Dose"),
                      actionButton("res.12.run","Rodar",icon = icon("hand-o-up")),
                      selectInput("res.12.1", "Escolha o resultado:",
                                  choices = c("ANOVA",
                                              "Estatistica Resumo"),width = 200),
                      textInput("name.txt.12","Escreva o nome do arquivo:"),
                      downloadButton('downloadData12', 'Download:'),
                      checkboxInput("info.12","Ajuda:", FALSE)
                    ),
                    mainPanel(verbatimTextOutput("contents12")
                              #,
                              #plotOutput("contents12.1")
                    )
                  )))),
    # #####################################################################################################################################################################################
    
    tabItem("aba1222",
            fluidPage(
              box(status = "primary",title = "Estatísticas (Resumo)", solidHeader =T, width = "50%",
                  titlePanel(""),
                  sidebarLayout(
                    sidebarPanel(
                      checkboxInput('res.122.name', 'Fontes de Variação:', FALSE),
                      textInput("model2","Escolha o parâmetro:", width = 700,
                                value = "N~NBPT"),
                      actionButton("res.122.run","Rodar",icon = icon("hand-o-up")),
                      # selectInput("res.122.1", "Método de ajuste do p-valor:",
                      #             choices = c("bonferroni","none","holm","hommel", 
                      #                         "hochberg", "BH", "BY", "fdr"),width = 200),
                      textInput("name.txt.122","Escreva o nome do arquivo:"),
                      downloadButton('downloadData122', 'Download:'),
                      checkboxInput("info.122","Ajuda:", FALSE)
                    ),
                    mainPanel(verbatimTextOutput("contents122"),
                              plotOutput("contents122.1"))
                  )))),
    
    # #####################################################################################################################################################################################
    
    tabItem("aba123",
            fluidPage(
              box(status = "primary",title = "Carregue os Dados", solidHeader =T, width = "50%",
                  titlePanel(""),
                  sidebarLayout(
                    sidebarPanel(
                      fileInput('file123', 'Escolha o arquivo:',
                                accept=c('text/csv', 
                                         'text/comma-separated-values,text/plain', 
                                         '.csv',".txt")),
                      tags$hr(),
                      checkboxInput('header123', 'Cabeçalho:', FALSE),
                      radioButtons('sep123', 'Separador:',
                                   c(Comma =',',
                                     Semicolon=';',
                                     Tab ='\t'),
                                   ','),
                      radioButtons('quote123', 'Aspas:',
                                   c(Nenhuma='',
                                     'Aspas Duplas'='"',
                                     'Aspas Simples'="'"),
                                   ''),
                      checkboxInput("info.123","Ajuda:", FALSE),
                      checkboxInput("ex.123","Exemplo:", TRUE)),
                    verbatimTextOutput("contents123"))))),
    
    # #####################################################################################################################################################################################
    tabItem("aba124", 
            fluidPage(
              box(status = "primary",title = "Análise de Correlação", solidHeader =T, width = "50%",
                  titlePanel(""),
                  sidebarLayout(
                    sidebarPanel(
                      selectInput("res.15.2", "Escolha o resultado:",
                                  choices = c("Correlacao",
                                              "p-Valor")),
                      textInput("name.txt.15","Escreva o nome do arquivo:"),
                      downloadButton('downloadData15', 'Download:'),
                      checkboxInput("info.152","Ajuda", FALSE)
                    ),
                    mainPanel(verbatimTextOutput("contents15.2"),
                              #plotOutput("contents15.3"),
                              plotOutput("contents15.333"))
                  )))),
    # #####################################################################################################################################################################################
    tabItem("aba125", 
            fluidPage(
              box(status = "primary",title = "Componentes Principais", solidHeader =T, width = "50%",
                  titlePanel(""),
                  sidebarLayout(
                    sidebarPanel(
                      selectInput("res.172", "Escolha o resultado:",
                                  choices = c("Estatistica Resumo","Scores de Tratamento","Scores de Características"),width = 200),
                      textInput("name.txt.172","Escreva o nome do arquivo:"),
                      downloadButton('downloadData172', 'Download:'),
                      checkboxInput("info.172","Ajuda", FALSE)),
                    mainPanel(verbatimTextOutput("contents.aba125.1"),
                              hr(),
                              plotOutput("contents.aba125.2"))
                  )))),
    # #####################################################################################################################################################################################
    tabItem("aba1261",
            fluidPage(
              box(status = "primary",title = "Calculando o Preço", solidHeader =T, width = "50%",
                  titlePanel(""),
                  sidebarLayout(
                    sidebarPanel(
                      numericInput("FR211.1","Área Total (ha)",min = 0,value = 200),
                      numericInput("FR211.2","Conteúdo de CaO (%)",min = 0,value = 36),
                      numericInput("FR211.3","Conteúdo de MgO (%)",min = 0,value = 20),
                      numericInput("FR211.4","Poder de Neutralização Total (PRNT)",min = 0,value = 85),
                      numericInput("FR211.5","Recomendação de calcário (t/ha)",min = 0,value = 3),
                      numericInput("FR211.6","Preço da tonelada de calcário (R$)",min = 0,value = 100)
                    ),
                    verbatimTextOutput("out.FR211")
                  )))),
    
    # #####################################################################################################################################################################################
    tabItem("aba1262",
            fluidPage(
              box(status = "primary",title = "Calculando os atributos do Calcário", solidHeader =T, width = "50%",
                  titlePanel(""),
                  sidebarLayout(
                    sidebarPanel(
                      selectInput("FR212.0", "Método do Poder de Neutralização (PN):",
                                  choices = c("Por Conteúdo de CaO e MgO","Por Reação Ácida")),
                      uiOutput("FR212.1"),
                      uiOutput("FR212.2"),
                      uiOutput("FR212.3"),
                      uiOutput("FR212.4"),
                      uiOutput("FR212.5"),
                      uiOutput("FR212.6"),
                      numericInput("FR212.7","Fração - Peneira ABNT 10 (>2.0mm)",min = 0,value =90.5),
                      numericInput("FR212.8","Fração - Peneira ABNT 20 (=>0.30mm e =<2.0mm)",min = 0,value =85.5),
                      numericInput("FR212.9","Fração - Peneira ABNT 50 (<0.30mm)",min = 0,value =70)
                    ),
                    mainPanel(
                    verbatimTextOutput("out.FR212"),
                    hr(),
                    img(src='prnt.png',align="c",width = "35%",height = "35%")
                  ))))),
    
    # #####################################################################################################################################################################################
    
    tabItem("aba22",
            fluidPage(
              box(status = "primary",title = "Recomendação", solidHeader =T, width = "50%",
                  titlePanel(""),
                  #plotOutput("aba22.999"),
                  column(3,
                         box(status = "primary",title = "Cultura", solidHeader =T, width = "50%",
                             uiOutput("aba22.1"),
                             hr(),
                             img(src='calagem1.png',width = "100%",height = "100%"))),
                  column(4,
                         box(status = "primary",title = "Análise de Solos", solidHeader =T, width = "50%",
                             numericInput("aba22.2","Conteúdo de Ca+2 (mmolc/dm3)",min = 0,value =8),
                             numericInput("aba22.3","Conteúdo de Mg+2 (mmolc/dm3)",min = 0,value =4),
                             numericInput("aba22.4","Conteúdo de Al+3 (mmolc/dm3)",min = 0,value =2),
                             numericInput("aba22.5","Capacidade de Troca Catiônica 'T' (mmolc/dm3)",min = 0,value =33),
                             numericInput("aba22.10","Capacidade de Troca Catiônica Efetiva 't' (mmolc/dm3)",min = 0,value =15),
                             numericInput("aba22.6","V (%)",min = 0,value =39),
                             numericInput("aba22.7","Porcentagem de Argila (%)",min = 0,max = 100,value =67),
                             numericInput("aba22.8","Fósforo Remanescente",min = 0,value =5),
                             verbatimTextOutput("aba22.9"),
                             strong("Obs: Cálculos considerando PRNT=100%."))),
                  column(4,
                         box(status = "primary",title = "Quantidade e Preço Total", solidHeader =T, width = "50%",
                             numericInput("aba222.2","Profundidade de incorporação (cm)",min = 0,value =20),
                             numericInput("aba222.3","Poder de Neutralização Total (PRNT)",min = 0,value =95),
                             numericInput("aba222.4","Superfície de aplicação (SA%)",min = 0,value =100),
                             numericInput("aba222.5","Área total de aplicação (ha)",min = 0,value =100),
                             numericInput("aba222.6","Preço por tonelada de calcário (R$)",min = 0,value =80),
                             numericInput("aba222.7","Custo do frete por tonelada de calcário (R$)",min = 0,max = 100,value =30),
                             verbatimTextOutput("aba222.8")))
              ))),
    
    # ######################################################################################################################################################################################
    tabItem("aba1271",
            fluidPage(
              box(status = "primary",title = "Recomendação com o uso de Fontes Simples", solidHeader =T, width = "50%",
                  titlePanel(""),
                  sidebarLayout(
                    sidebarPanel(
                      numericInput("FR11.11","Dose de Nitrogênio (N) (kg/ha)",min = 0,value = 10),
                      selectInput("FR11.12", "Escolha o Fertilizante (N):",
                                  choices = c("Sulfato de amonio","Ureia","Nitrato de calcio")),
                      numericInput("FR11.21","Dose de Fósforo (P2O5) (kg/ha)",min = 0,value = 40),
                      selectInput("FR11.22", "Escolha o Fertilizante (P):",
                                  choices = c("Superfosfato Triplo","Superfosfato Simples","Monoamonio fosfato (MAP)","Diamonio fosfato (DAP)")),
                      numericInput("FR11.31","Dose de Potássio (K2O) (kg/ha)",min = 0,value = 30),
                      selectInput("FR11.32", "Escolha o Fertilizante (K):",
                                  choices = c("Cloreto de Potassio","Sulfato de potassio","Nitrato de potassio"))
                    ),
                    verbatimTextOutput("out.FR11")
                  )))),
    # #####################################################################################################################################################################################
    tabItem("aba1272",
            fluidPage(
              box(status = "primary",title = "Cálculo para escolha de Formulados", solidHeader =T, width = "50%",
                  titlePanel(""),
                  sidebarLayout(
                    sidebarPanel(
                      numericInput("FR12.1","Dose de Nitrogênio (N) (kg/ha)",min = 0,value = 10),
                      numericInput("FR12.2","Dose de Fósforo (P2O5) (kg/ha)",min = 0,value = 40),
                      numericInput("FR12.3","Dose de Potássio (K2O) (kg/ha)",min = 0,value = 30),
                      numericInput("FR12.4","Fator de Multiplicação",min = 2,value = 2),
                      numericInput("FR12.5","Espaçamento entre linhas (m)",min = 2,value = 0.5)
                    ),
                    verbatimTextOutput("out.FR12")
                  )))),
    
    # #####################################################################################################################################################################################
    tabItem("aba1273",
            fluidPage(
              box(status = "primary",title = "Adubos Orgânicos", solidHeader =T, width = "50%",
                  titlePanel(""),
                  sidebarLayout(
                    sidebarPanel(
                      uiOutput("aba1273.1"),
                      numericInput("aba1273.2","Umidade do Composto:",min = 0,value = 40),
                      uiOutput("aba1273.per.2"),
                      uiOutput("aba1273.per.3"),
                      uiOutput("aba1273.per.4"),
                      uiOutput("aba1273.per.5"),
                      uiOutput("aba1273.per.6"),
                      uiOutput("aba1273.per.7")
                    ),
                    mainPanel(verbatimTextOutput("aba1273.3"),
                              strong("Obs: Apenas adubos organicos solidos devem ser utilizados."),
                              hr(),
                              img(src='umidade.png',width = "40%",height = "40%"))
                  )))),
    # ##############################################################################################################################################################################
    tabItem("aba210",
            fluidPage(
              box(status = "primary",title = "Análise Física", solidHeader =T, width = "50%",
                  titlePanel(""),
                  
                  column(4,
                         box(status = "primary",title = "", solidHeader =T, width = "50%",
                             uiOutput("aba210.0"))),
                  column(4,
                         box(status = "primary",title = "", solidHeader =T, width = "50%",
                             uiOutput("aba210.1"))),
                  column(4,
                         box(status = "primary",title = "", solidHeader =T, width = "50%",
                             uiOutput("aba210.2")))
              ))),
    
    # #####################################################################################################################################################################################
    
    tabItem("aba199",
            fluidPage(
              box(status = "primary",title = "Nutrientes", solidHeader =T, width = "50%",
                  titlePanel(""),
                  plotOutput("aba199.plot"),
                  column(4,
                         box(status = "primary",title = "Tipo de cultivo", solidHeader =T, width = "50%",
                             uiOutput("res.aba211.5"))),
                  column(4,
                         box(status = "primary",title = "Macronutrientes", solidHeader =T, width = "50%",
                             uiOutput("res.aba211.8"),
                             uiOutput("res.aba211.9"),
                             uiOutput("res.aba213.4"),
                             uiOutput("res.aba213.5"),
                             uiOutput("res.aba213.6"))),
                  column(4,
                         box(status = "primary",title = "Micronutrientes", solidHeader =T, width = "50%",
                             uiOutput("res.aba212.6"),
                             uiOutput("res.aba212.7"),
                             uiOutput("res.aba212.8"),
                             uiOutput("res.aba212.9"),
                             uiOutput("res.aba212.10")))
              ))),
    
    
    # tabItem("aba199",
    #         fluidPage(
    #           box(status = "primary",title = "Nutrientes", solidHeader =T, width = "50%",
    #               titlePanel(""),
    #               plotOutput("aba199.plot"),
    #               column(4,
    #                      box(status = "primary",title = "Tipo de cultivo", solidHeader =T, width = "50%",
    #                          uiOutput("res.aba211.5"))),
    #               column(4,
    #                      box(status = "primary",title = "Teor", solidHeader =T, width = "50%",
    #                          uiOutput("res.aba211.8"),
    #                          uiOutput("res.aba211.9"),
    #                          uiOutput("res.aba213.4"),
    #                          uiOutput("res.aba213.5"),
    #                          uiOutput("res.aba213.6"),
    #                          uiOutput("res.aba212.6"),
    #                          uiOutput("res.aba212.7"),
    #                          uiOutput("res.aba212.8"),
    #                          uiOutput("res.aba212.9"),
    #                          uiOutput("res.aba212.10"))),
    #               column(4,
    #                      box(status = "primary",title = "Classe", solidHeader =T, width = "50%",
    #                          uiOutput("res.aba211.11"),
    #                          uiOutput("res.aba211.12"),
    #                          uiOutput("res.aba213.7"),
    #                          uiOutput("res.aba213.8"),
    #                          uiOutput("res.aba213.9"),
    #                          uiOutput("res.aba212.11"),
    #                          uiOutput("res.aba212.12"),
    #                          uiOutput("res.aba212.13"),
    #                          uiOutput("res.aba212.14"),
    #                          uiOutput("res.aba212.15")))
    #           ))),
    
    # #####################################################################################################################################################################################
    
    tabItem("aba214",
            fluidPage(
              box(status = "primary",title = "Parâmetros Químicos", solidHeader =T, width = "50%",
                  titlePanel(""),
                  
                  column(6,
                         box(status = "primary",title = "Valor", solidHeader =T, width = "50%",
                             uiOutput("res.aba214.1"),
                             uiOutput("res.aba214.2"))),
                  column(6,
                         box(status = "primary",title = "Classe", solidHeader =T, width = "50%",
                             uiOutput("res.aba214.3"),
                             uiOutput("res.aba214.4")))
              ))),
    # #####################################################################################################################################################################################
    
    tabItem("aba23", 
            fluidPage(
              box(status = "primary",title = "Recomendação de Adubação", solidHeader =T, width = "50%",
                  titlePanel(""),
                  sidebarLayout(
                    sidebarPanel( 
                      uiOutput("res.aba23.0"),
                      uiOutput("res.aba23.2"),
                      uiOutput("res.aba23.3"),
                      uiOutput("res.aba23.4"),
                      uiOutput("res.aba23.5"),
                      actionButton("res.aba23.6","Rodar",icon = icon("hand-o-up")),
                      downloadButton("report", "Gerar relatório")
                    ),
                    mainPanel(verbatimTextOutput("contents.aba23"),
                              verbatimTextOutput("contents.aba23.1"))
                  )))),
    
    # #####################################################################################################################################################################################
    
    #################################
    #### Garantias Fertilizantes ####
    #################################
    
    tabItem("tabGA1",
            fluidPage(
              box(status = "primary",title = "Garantias de Fertilizantes", solidHeader =T, width = "50%",
                  titlePanel(""),
                  column(3,
                         box(status = "primary",title = "", solidHeader =T, width = "50%",
                             uiOutput("tabGA1.1"))),
                  column(3,
                         box(status = "primary",title = "", solidHeader =T, width = "50%",
                             uiOutput("tabGA1.2"),
                             uiOutput("tabGA1.5"),
                             uiOutput("tabGA1.10"),
                             uiOutput("tabGA1.12"),
                             uiOutput("tabGA1.15"),
                             uiOutput("tabGA1.18"))),
                  column(3,
                         box(status = "primary",title = "", solidHeader =T, width = "50%",
                             uiOutput("tabGA1.3"),
                             uiOutput("tabGA1.6"),
                             uiOutput("tabGA1.7"),
                             uiOutput("tabGA1.11"),
                             uiOutput("tabGA1.13"),
                             uiOutput("tabGA1.16"),
                             uiOutput("tabGA1.19"))),
                  column(3,
                         box(status = "primary",title = "", solidHeader =T, width = "50%",
                             uiOutput("tabGA1.4"),
                             uiOutput("tabGA1.8"),
                             uiOutput("tabGA1.9"),
                             uiOutput("tabGA1.14"),
                             uiOutput("tabGA1.17")))
              ))),
    
    # #####################################################################################################################################################################################
    
    ##############################
    #### Garantias Corretivos ####
    ##############################
    
    #tabGA2
    
    tabItem("tabGA2",
            fluidPage(
              box(status = "primary",title = "Garantias de Corretivos", solidHeader =T, width = "50%",
                  titlePanel(""),
                  column(3,
                         box(status = "primary",title = "", solidHeader =T, width = "50%",
                             uiOutput("tabGA2.1"))),
                  column(3,
                         box(status = "primary",title = "", solidHeader =T, width = "50%",
                             uiOutput("tabGA2.2"),
                             uiOutput("tabGA2.6"),
                             uiOutput("tabGA2.12"),
                             uiOutput("tabGA2.17"))),
                  column(3,
                         box(status = "primary",title = "", solidHeader =T, width = "50%",
                             uiOutput("tabGA2.3"),
                             uiOutput("tabGA2.7"),
                             uiOutput("tabGA2.8"),
                             uiOutput("tabGA2.9"),
                             uiOutput("tabGA2.13"),
                             uiOutput("tabGA2.14"),
                             uiOutput("tabGA2.18"),
                             uiOutput("tabGA2.19"))),
                  column(3,
                         box(status = "primary",title = "", solidHeader =T, width = "50%",
                             uiOutput("tabGA2.4"),
                             uiOutput("tabGA2.5"),
                             uiOutput("tabGA2.10"),
                             uiOutput("tabGA2.11"),
                             uiOutput("tabGA2.15"),
                             uiOutput("tabGA2.16"),
                             uiOutput("tabGA2.20"),
                             uiOutput("tabGA2.21"),
                             uiOutput("tabGA2.22")))
              ))),
    
    # #####################################################################################################################################################################################
    
    tabItem("FR2211",
            fluidPage(
              box(status = "primary",title = "Gessagem", solidHeader =T, width = "50%",
                  titlePanel(""),
                  sidebarLayout(
                    sidebarPanel(
                      box(status = "primary",title = "Análise de Solo", solidHeader =T, width = "50%",
                          numericInput("FR221.2","Porcentagem de Argila",min = 0,value =30),
                          numericInput("FR221.3","Fósforo Remanescente (mg/L)",min = 0,value =10),
                          numericInput("FR221.4","Capacidade de Troca Catiônica (CTC) (mmolc/dm3)",min = 0,value =80),
                          numericInput("FR221.5","Conteúdo de Ca+2 (mmolc/dm3)",min = 0,value =60))
                    ),
                    mainPanel(
                    verbatimTextOutput("FR221.6"),
                    hr(),
                    img(src='gessagem.png',width = "60%",height = "60%")
                  ))))),
    # #####################################################################################################################################################################################
    
    tabItem("FR2213",
            fluidPage(
              box(status = "primary",title = "Fosfatagem", solidHeader =T, width = "50%",
                  titlePanel(""),
                  sidebarLayout(
                    sidebarPanel(
                      box(status = "primary",title = "Análise de Solo", solidHeader =T, width = "50%",
                          uiOutput("FR223.2"),
                          uiOutput("FR223.3"),
                          numericInput("FR223.4","Fósforo no Solo (mg/L)",min = 0,value =10))
                    ),
                    mainPanel(
                      verbatimTextOutput("FR223.5"),
                      hr(),
                      img(src='fosfatagem.png',width = "35%",height = "35%")
                  ))))),
    
    
    # tabItem("FR221",
    #         fluidPage(
    #           titlePanel("Gessagem"),
    #           sidebarLayout(
    #             sidebarPanel(
    #               selectInput("FR221.0", "Choose Gessagem Recomendação method:",
    #                           choices = c("Porcentagem de Calagem","Fósforo Remanescente","Porcentagem de Argila","Saturação por Bases")),
    #               uiOutput("FR221.01"),
    #               uiOutput("FR221.02"),
    #               uiOutput("FR221.03"),
    #               uiOutput("FR221.04"),
    #               uiOutput("FR221.05"),
    #               uiOutput("FR221.06"),
    #               numericInput("FR221.1","Poder de Neutralização Total (PRNT)",min = 0,value = 90),
    #               numericInput("FR221.2","Calagem Necessidade (t/ha)",min = 0,value = 0.8,step = 0.01),
    #               numericInput("FR221.3","Correction profile size (cm)",min = 0,value = 0.28,step = 0.01),
    #               numericInput("FR221.4","Porcentagem de coverage area (%)",min = 0,value = 0.9,step = 0.01),
    #               numericInput("FR221.5","Porcentagem de Argila",min = 0,value = 30)
    #             ),
    #             verbatimTextOutput("out.FR221")
    #           ))),
    
    # #####################################################################################################################################################################################      
    # Information
    # #####################################################################################################################################################################################      
    
    tabItem("info1",         
            fluidPage(
              titlePanel(""),
              fluidRow(
                box(status = "primary",title = "Sobre Soil-App", solidHeader =T, width = "50%",
                    br(),
                    pre(includeText("FertSoilAbout2.txt")))
              ))),
    tabItem("info3", 
            fluidPage(
              titlePanel(""),
              fluidRow(
                box(status = "primary",title = "Contato Soil-App", solidHeader =T, width = "50%",
                    br(),
                    pre(includeText("FertSoilContact2.txt"))))))
    # #####################################################################################################################################################################################
            ))
# #####################################################################################################################################################################################

shinyApp(
  ui = dashboardPage(header, sidebar, body,
                     tags$head(tags$style(HTML('
                                               /* logo */
                                               .skin-blue .main-header .logo {
                                               background-color:  #572b0c;
                                               }
                                               
                                               /* logo when hovered */
                                               .skin-blue .main-header .logo:hover {
                                               background-color: sienna;
                                               }
                                               
                                               /* navbar (rest of the header) */
                                               .skin-blue .main-header .navbar {
                                               background-color: #7a3c11;
                                               }        
                                               
                                               /* main sidebar */
                                               .skin-blue .main-sidebar {
                                               background-color: dodgerblue4;
                                               }
                                               
                                               
                                               /* other links in the sidebarmenu */
                                               .skin-blue .main-sidebar .sidebar .sidebar-menu a{
                                               background-color: dodgerblue4;
                                               color: write;
                                               }
                                               
                                               /* other links in the sidebarmenu when hovered */
                                               .skin-blue .main-sidebar .sidebar .sidebar-menu a:hover{
                                               background-color: sienna;
                                               }
                                               /* toggle button when hovered  */                    
                                               .skin-blue .main-header .navbar .sidebar-toggle:hover{
                                               background-color: sienna;
                                               }
                                               ')))),
  server = function(input, output) { 
    
    # #####################################################################################################################################################################################
    # Geral
    options(shiny.maxRequestSize=50*1024^2)
    # #####################################################################################################################################################################################
    # Aprendendo e Analizando
    # #####################################################################################################################################################################################
    
    #aba11
    
    data.aba11<-reactive({
      data<-read.table(file = "unidades.txt",header = T,sep = "\t")
      return(data)
    })
    
    
    output$res.aba11.2<-renderUI({
      data<-data.aba11()
      escolha<- paste("De: (",data[,1], ") Para: (", data[,2], ")",sep = "")
      selectInput("res.aba11.21", "Escolha a Conversão:",
                  choices = c(as.character(escolha)))
    })
    
    output$contents.aba11<-renderPrint({
      data<-data.aba11()
      escolha<- paste("De: (",data[,1], ") Para: (", data[,2], ")",sep = "")
      pos<-as.character(escolha)==as.character(input$res.aba11.21)
      valor<-as.numeric(as.character(input$res.aba11.1))*as.numeric(as.character(data[pos,3]))
      return(paste(input$res.aba11.1," ",data[pos,1]," = ",round(valor,3)," ",data[pos,2],sep = ""))
    })
    
    # #####################################################################################################################################################################################
    
    # aba121
    out.11.1 <- reactive({
      if(input$ex.11!=FALSE){
        TABE.1<-read.table("DadosExperimentoUreia.txt",header = TRUE, sep = "\t",na.strings = F)
        return(TABE.1)}
      inFile <- input$file1
      if (is.null(inFile))
        return(NULL)
      TABE<-read.table(inFile$datapath, header=input$header, sep=input$sep, 
                       quote=input$quote)
      return(TABE)
    })
    
    output$contents11 <- renderPrint({
      if(input$info.11!=TRUE)
        return (out.11.1())
      else 
        includeScript("help-dados.txt")})
    # #####################################################################################################################################################################################
    # aba1262
    
    output$FR212.1<-renderUI({
      if(input$FR212.0=="Por Conteúdo de CaO e MgO"){
        numericInput("FR212.1.1","Conteúdo de CaO (%)",min = 0,value = 36)
      }})
    output$FR212.2<-renderUI({
      if(input$FR212.0=="Por Conteúdo de CaO e MgO"){
        numericInput("FR212.2.1","Conteúdo de MgO (%)",min = 0,value = 20)
      }})
    output$FR212.3<-renderUI({
      if(input$FR212.0=="Por Reação Ácida"){
        numericInput("FR212.3.1","Concentração de HCl na solução (mol/L)",min = 0,value = 0.5105,step = 0.001)
      }})
    output$FR212.4<-renderUI({
      if(input$FR212.0=="Por Reação Ácida"){
        numericInput("FR212.4.1","Concentração de NaOH na solução (mol/L)",min = 0,value = 0.2489,step = 0.001)
      }})
    output$FR212.5<-renderUI({
      if(input$FR212.0=="Por Reação Ácida"){
        numericInput("FR212.5.1","Volume de NaOH usad na titulação (ml)",min = 0,value = 20.45,step = 0.001)
      }})
    output$FR212.6<-renderUI({
      if(input$FR212.0=="Por Reação Ácida"){
        numericInput("FR212.6.1","Peso Inicial da Amostra (g)",min = 0,value = 1.1005,step = 0.001)
      }})
    
    react.FR212 <- reactive({
      
      CaO<-input$FR212.1.1
      MgO<-input$FR212.2.1
      mol.CaCO3 = 40 + 12 + 3*16
      mol.CaO = 40 + 16 
      relacao.CaCO3.CaO = mol.CaCO3 / mol.CaO 
      mol.MgO = 24 + 16  
      relacao.CaCO3.MgO = 100 / mol.MgO 
      
      if(input$FR212.0=="Por Conteúdo de CaO e MgO"){
        if(CaO==0&MgO==0){return(NULL)}
        PN = relacao.CaCO3.CaO*CaO + relacao.CaCO3.MgO*MgO
      }
      
      M1 = input$FR212.3.1
      M2 = input$FR212.4.1
      Vb = input$FR212.5.1
      massa = input$FR212.6.1
      
      if(input$FR212.0=="Por Reação Ácida"){
        if(M1==0&M2==0&Vb==0&massa==0){return(NULL)}
        PN = (10*(25*M1)-(Vb*M2))/massa
      }
      
      peneira.10=input$FR212.7
      peneira.20=input$FR212.8
      peneira.50=input$FR212.9
      
      RE = 0.2*(peneira.10-peneira.20) + 0.6*(peneira.20-peneira.50) + 1*peneira.50
      
      PRNT = PN * RE /100 
      
      if(PRNT>=45&PRNT<=60){tipo.cal=paste(round(PRNT,3), c(" %, Classe A"),sep="")}
      if(PRNT>=60.1&PRNT<=75){tipo.cal=paste(round(PRNT,3), c(" %, Classe B"),sep="")}
      if(PRNT>=75.1&PRNT<=90){tipo.cal=paste(round(PRNT,3), c(" %, Classe C"),sep="")}
      if(PRNT>90){tipo.cal=paste(round(PRNT,3), c(" %, Classe D"),sep="")}
      
      saida<-as.matrix(rbind(paste(round(PN,3)," %",sep = ""),paste(round(RE,3)," %",sep = ""),tipo.cal))
      colnames(saida)<-"Atributos do Calcário:"
      rownames(saida)<-c("Poder de Neutralização (PN)","Reatividade (RE)","Poder de Neutralização Total (PRNT)")
      return(saida)
    })
    
    output$out.FR212 <- renderPrint({
      return(react.FR212())
    })
    
    # #####################################################################################################################################################################################
    # aba22
    
    data.aba22<-reactive({
      data1<-read.table(file = "calagem.txt",header = T,sep = "\t")
      LISTA<-list(data1)
      return(LISTA)
    })
    
    
    output$aba22.1<-renderUI({
      data1<-data.aba22()[[1]]
      data1<-as.matrix(data1)
      escolha<-unique(as.character(data1[,1]))
      selectInput("aba222.cul", "Escolha a Cultura:",
                  choices = c(as.character(escolha)))
    })
    
    
    aba222.100<-reactive({
      data1<-data.aba22()[[1]]
      data1<-as.matrix(data1)
      v1<-as.numeric(input$aba22.6)
      v2<-as.numeric(data1[as.character(data1[,1])==as.character(input$aba222.cul),2])
      mt<-as.numeric(data1[as.character(data1[,1])==as.character(input$aba222.cul),3])
      ca<-as.numeric(input$aba22.2)
      mg<-as.numeric(input$aba22.3)
      al<-as.numeric(input$aba22.4)
      ctc<-as.numeric(input$aba22.5)
      y<-as.numeric(input$aba22.7)
      x<-as.numeric(data1[as.character(data1[,1])==as.character(input$aba222.cul),4])
      t<-as.numeric(input$aba22.10)
      
      DI<-as.numeric(input$aba222.2)
      PRNT<-as.numeric(input$aba222.3)
      SA<-as.numeric(input$aba222.4)
      TA<-as.numeric(input$aba222.5)
      PL<-as.numeric(input$aba222.6)
      FC<-as.numeric(input$aba222.7)
      
      # Metodo da Saturacao por Bases (Boletim 100)
      #NC11 = paste((as.matrix(((v2-v1)*ctc)/1000))," t/ha",sep = "")
      NC1 = as.matrix(((v2-v1)*ctc)/1000)
      
      #if(NC1<0){NC1<-0}
      
      rownames(NC1)="NC (t/ha) ="
      colnames(NC1)=""
      
      #QC1 = paste((NC1*(100/PRNT)*(SA/100)*(DI/20))," t/ha",sep = "")
      QC1 = as.matrix(NC1*(100/PRNT)*(SA/100)*(DI/20))
      CT1 = as.matrix(TA*(PL + FC)*QC1)
      
      #Metodo da neutralizacao do Al3+ e Elevacao dos teores de Ca2+ e Mg2+ ( 5 Aproximacao)
      #NC22 =paste((as.matrix((y *(al - (mt/100)) + x - (ca + mg))/10))," t/ha",sep = "")
      y1<-2
      
      NC2.a<-(y1 *(al - (mt*t/100)))
      if(NC2.a<0){NC2.a<-0.001}
      NC2.b<-(x - (ca + mg))
      if(NC2.b<0){NC2.b<-0.001}
      NC2 = (as.matrix((NC2.a + NC2.b)/10))
      rownames(NC2)="NC(t/ha) ="
      colnames(NC2)=""
      
      #QC2 = paste((NC2*(100/PRNT)*(SA/100)*(DI/20))," t/ha",sep = "")
      QC2 = as.matrix(NC2*(100/PRNT)*(SA/100)*(DI/20))
      CT2 = as.matrix(TA*(PL + FC)*QC2)
      
      LISTA1<-list(NC1,NC2)
      names(LISTA1)<-c("Metodo da Saturacao por Bases (Boletim 100)","Metodo da neutralizacao do Al3+ e Elevacao dos teores de Ca2+ e Mg2+ ( 5 Aproximacao)")
      
      LISTA2<-as.matrix(c(QC1,CT1))
      rownames(LISTA2)=c("QC  (t/ha) = ","Preço (R$) = ")
      colnames(LISTA2)=""
      
      LISTA3<-as.matrix(c(QC2,CT2))
      rownames(LISTA3)=c("QC  (t/ha) = ","Preço (R$) = ")
      colnames(LISTA3)=""
      
      LISTA4<-list(LISTA2,LISTA3)
      names(LISTA4)<-c("Metodo da Saturacao por Bases (Boletim 100)","Metodo da neutralizacao do Al3+ e Elevacao dos teores de Ca2+ e Mg2+ ( 5 Aproximacao)")
      
      LISTA<-list(LISTA1,LISTA4)
      
      return(LISTA)
    })
    
    # output$aba22.999<-renderPlot({
    #   data1<-data.aba22()[[1]]
    #   data1<-as.matrix(data1)
    #   v1<-as.numeric(input$aba22.6)
    #   v2<-as.numeric(data1[as.character(data1[,1])==as.character(input$aba222.cul),2])
    #   mt<-as.numeric(data1[as.character(data1[,1])==as.character(input$aba222.cul),3])
    #   ca<-as.numeric(input$aba22.2)
    #   mg<-as.numeric(input$aba22.3)
    #   al<-as.numeric(input$aba22.4)
    #   ctc<-as.numeric(input$aba22.5)
    #   y<-as.numeric(input$aba22.7)
    #   x<-as.numeric(data1[as.character(data1[,1])==as.character(input$aba222.cul),4])
    #   t<-as.numeric(input$aba22.10)
    #   
    #   DI<-as.numeric(input$aba222.2)
    #   PRNT<-as.numeric(input$aba222.3)
    #   SA<-as.numeric(input$aba222.4)
    #   TA<-as.numeric(input$aba222.5)
    #   PL<-as.numeric(input$aba222.6)
    #   FC<-as.numeric(input$aba222.7)
    #   
    #   # Metodo da Saturacao por Bases (Boletim 100)
    #   #NC11 = paste((as.matrix(((v2-v1)*ctc)/1000))," t/ha",sep = "")
    #   NC1 = as.matrix(((v2-v1)*ctc)/1000)
    #   
    #   #if(NC1<0){NC1<-0}
    #   
    #   rownames(NC1)="NC (t/ha) ="
    #   colnames(NC1)=""
    #   
    #   #QC1 = paste((NC1*(100/PRNT)*(SA/100)*(DI/20))," t/ha",sep = "")
    #   QC1 = as.matrix(NC1*(100/PRNT)*(SA/100)*(DI/20))
    #   CT1 = as.matrix(TA*(PL + FC)*QC1)
    #   
    #   #Metodo da neutralizacao do Al3+ e Elevacao dos teores de Ca2+ e Mg2+ ( 5 Aproximacao)
    #   #NC22 =paste((as.matrix((y *(al - (mt/100)) + x - (ca + mg))/10))," t/ha",sep = "")
    #   y1<-2
    #   
    #   NC2.a<-(y1 *(al - (mt*t/100)))
    #   if(NC2.a<0){NC2.a<-0.001}
    #   NC2.b<-(x - (ca + mg))
    #   if(NC2.b<0){NC2.b<-0.001}
    #   NC2 = (as.matrix((NC2.a + NC2.b)/10))
    #   rownames(NC2)="NC(t/ha) ="
    #   colnames(NC2)=""
    #   
    #   NC1.fun = function(v2,v1,x){as.matrix(((v2-v1)*x)/1000)}
    #   
    #   NC2.fun<-function(x=al,mt,t,x1,ca,mg){
    #     y1<-2
    #     NC2.a<-(y1 *(x - (mt*t/100)))
    #     if(NC2.a<0){NC2.a<-0.001}
    #     NC2.b<-(x1 - (ca + mg))
    #     if(NC2.b<0){NC2.b<-0.001}
    #     as.matrix((NC2.a + NC2.b)/10)
    #   }
    #   
    #   ctc1=c(50:100)
    #   
    #   dat<-data.frame(FPR=NC1.fun(v2,v1,x=ctc1),TPR=ctc)
    #   
    #   p11<-ggplot(dat,aes(FPR,TPR))+
    #     geom_line(size = 2, alpha = 0.7)+
    #     labs(title= "Metodo da Saturacao por Bases (Boletim 100)",
    #          x = "Capacidade de Troca Catiônica 'T' (mmolc/dm3)",
    #          y = "Necessidade de Calcário (kg/ha)") + 
    #     scale_colour_manual(values=colors) + 
    #     scale_linetype_manual(values=ltys)
    #   
    #   ggplotly(p11)
    # })
    
    output$aba22.9<-renderPrint({
      return(aba222.100()[[1]])
    })
    
    output$aba222.8<-renderPrint({
      return(aba222.100()[[2]])
    })
    
    # #####################################################################################################################################################################################
    # FR22
    
    output$FR221.6<-renderPrint({
      PC<-as.numeric(input$FR221.2)
      RP<-as.numeric(input$FR221.3)
      CEC<-as.numeric(input$FR221.4)
      Ca<-as.numeric(input$FR221.5)
      
      # Recomendacao em funcao da Textura do solo (Boletim 100)
      NG1 = as.matrix(PC * 60)
      rownames(NG1)="NG  (kg/ha) ="
      colnames(NG1)=""
      
      # Recomendacao baseada na textura do Solo/P-rem ( 5 Aproximacao)
      NG2<-as.matrix((0.00034 - 0.002445*PC^0.5 + 0.0338886*PC - 0.00176366*PC^1.5) * 1000)
      if(NG2<0){NG2<-0}
      rownames(NG2)="NG  (kg/ha) ="
      colnames(NG2)=""
      
      NG2.2<-as.matrix((315.8 - 25.5066*RP^0.5 - 5.70675*RP + 0.485335*RP^1.5)*1000/(Ca))
      #if(NG2.2<0){NG2.2<-0.01}
      rownames(NG2.2)="NG  (kg/ha) ="
      colnames(NG2.2)=""
      
      # Recomendacao Embrapa
      NG3.1 = as.matrix(50 *PC) 
      rownames(NG3.1)="NG para culturas anuais (kg/ha) ="
      colnames(NG3.1)=""
      
      NG3.2 = as.matrix(75 *PC) 
      rownames(NG3.2)="NG para culturas perenes (kg/ha) ="
      colnames(NG3.2)=""
      
      #Recomendacao para Elevacao da saturação por Ca na CTC efetiva
      NG4.a<-0.6*CEC
      NG4.b<-NG4.a-Ca
      if(NG4.b<0){NG4.b<-0}
      NG4<-as.matrix((NG4.b)*640)
      rownames(NG4)="NG (kg/ha) ="
      colnames(NG4)=""
      
      Lista<-list(NG1, NG2, NG2.2,NG3.1,NG3.2,NG4)
      names(Lista)<-c("Recomendacao em funcao da Textura do solo (Boletim 100)",
                      "Recomendacao baseada na textura do Solo( 5 Aproximacao)",
                      "Recomendacao baseada no P-rem ( 5 Aproximacao)",
                      "Recomendacao Embrapa Anual",
                      "Recomendacao Embrapa Perene",
                      "Recomendacao para Elevacao da saturação por Ca na CTC efetiva")
      
      return(Lista)
      
    })
    
    # #####################################################################################################################################################################################
    # FR23
    data.aba2213<-reactive({
      data1<-read.table(file = "fosfatagem.txt",header = T,sep = "\t")
      LISTA<-list(data1)
      return(LISTA)
    })
    
    output$FR223.2<-renderUI({
      data1<-data.aba2213()[[1]]
      data1<-as.matrix(data1)
      escolha<-unique(as.character(data1[,1]))
      selectInput("FR223.2.1", "Metodologia:",
                  choices = c(as.character(escolha)))
    })
    
    output$FR223.3<-renderUI({
      data1<-data.aba2213()[[1]]
      data1<-as.matrix(data1)
      escolha<-unique(as.character(data1[,2]))
      selectInput("FR223.2.2", "Porcentagem de Argila:",
                  choices = c(as.character(escolha)))
    })
    
    output$FR223.5<-renderPrint({
      data1<-data.aba2213()[[1]]
      NC<-data1[as.character(data1[,2])==as.character(input$FR223.2.2)&as.character(data1[,1])==as.character(input$FR223.2.1),3]
      if(as.numeric(as.character(NC))<as.numeric(input$FR223.4))
        return("Não há necessidade de aplicação")
      if(as.numeric(input$FR223.4)<0)
        return("Fósforo do solo deve ser >= 0")
      TAM<-data1[as.character(data1[,2])==as.character(input$FR223.2.2)&as.character(data1[,1])==as.character(input$FR223.2.1),4]
      NF<-(as.numeric(as.character(NC))-as.numeric(input$FR223.4))*as.numeric(as.character(TAM))
      if(NF<0){NF<-0}
      return(paste("Recomendação de Fosfatagem: ",NF," kg/ha de P2O5",sep = ""))
    })
    # #####################################################################################################################################################################################
    # aba1271
    
    react.FR11 <- reactive({
      
      datadata<-read.table("fontes_nova.txt",header = T,sep="\t")
      
      n<-input$FR11.11 
      fonte.n<-input$FR11.12 
      
      p<-input$FR11.21 
      fonte.p<-input$FR11.22 
      
      k <-input$FR11.31
      fonte.k<-input$FR11.32
      
      if(n==0&p==0&k==0){return(NULL)}
      
      k2o<-k*1 
      fonte.k1<-as.numeric(as.character(datadata[datadata[,1]==fonte.k,4]))
      fonte.k1.N<-as.numeric(as.character(datadata[datadata[,1]==fonte.k,2]))
      
      k.fim = (k2o*100)/fonte.k1 
      
      out.k<-paste(round(k.fim,3)," kg/ha de ",fonte.k,sep = "")
      
      p2o5<-p*1 
      
      fonte.p1<-as.numeric(as.character(datadata[datadata[,1]==fonte.p,3]))
      fonte.p1.N<-as.numeric(as.character(datadata[datadata[,1]==fonte.p,2]))
      
      p.fim = (p2o5*100)/fonte.p1 
      out.p<-paste(round(p.fim,3)," kg/ha de ",fonte.p,sep = "")
      
      
      nec.N<-(as.numeric(n)-(k.fim*fonte.k1.N/100+p.fim*fonte.p1.N/100))
      
      if(nec.N<0){
        n.fim = 0
      }
      
      if(nec.N>0){
        fonte.n1<-as.numeric(as.character(datadata[datadata[,1]==fonte.n,2]))
        n.fim = (nec.N*100)/fonte.n1 
      }
      
      out.n <- paste(round(n.fim,3)," kg/ha de ",fonte.n,sep = "")
      
      saida<-as.matrix(rbind(out.n,out.p,out.k))
      
      colnames(saida)<-"Recomendação:"
      rownames(saida)<-c("N","P","K")
      return(saida)
    })
    
    output$out.FR11 <- renderPrint({
      return(react.FR11())
    })    
    # #####################################################################################################################################################################################
    # aba1272
    
    react.FR12 <- reactive({
      n<-input$FR12.1
      
      p<-input$FR12.2
      
      k <-input$FR12.3
      
      nivel<-input$FR12.4
      
      if(n==0&p==0&k==0){return(NULL)}
      
      k2o<-k
      p2o5<-p
      
      npk<-c(round(n,0),round(p2o5,0),round(k2o,0))
      
      npk.1<-npk/min(npk) 
      
      npk.2<-round(npk.1*nivel,0)
      
      formulado<-paste("Formulado: ",paste(npk.2,collapse = "-"),sep = "") 
      
      quant.form<-(npk/npk.2)*100 
      
      quant.form.fim<-round(mean(quant.form),3)
      
      saida<-as.matrix(paste(quant.form.fim," kg/ha de ",formulado," = soma(",sum(npk.2),") Obs: Somatório deve ser 21 < (N-P-K) < 60",sep = "")) 
      
      colnames(saida)<-" "
      rownames(saida)<-c("N-P-K")
      
      #dif.nut<-round((outer(quant.form, quant.form, '-')/quant.form.fim)*100,2)
      dif.nut<-round(outer(quant.form, quant.form, '-'),2)
      
      colnames(dif.nut)<-c("N","P","K")
      rownames(dif.nut)<-c("N","P","K")
      
      espacamento_metros<-input$FR12.5
      
      g.m.linear = (quant.form.fim*espacamento_metros)/10
      
      g.m.linear.1 = as.matrix(paste(round(mean(g.m.linear),3)," g/m.linear de ",formulado,sep = "")) 
      
      colnames(g.m.linear.1)<-" "
      rownames(g.m.linear.1)<-c("N-P-K")
      
      #saida1<-list(saida,g.m.linear.1,dif.nut)
      #names(saida1)<-c("Recomendação","Recomendação por metro linear","Diferença entre fontes (kg/ha)")
      
      saida1<-list(saida,g.m.linear.1)
      names(saida1)<-c("Recomendação","Recomendação por metro linear")
      return(saida1)
      
    })
    
    output$out.FR12 <- renderPrint({
      return(react.FR12())
    })
    
    # #####################################################################################################################################################################################
    #aba1273
    
    data.aba1273<-reactive({
      data<-read.table(file = "ORGANICO.txt",header = T,sep = "\t")
      return(data)
    })
    
    output$aba1273.1<-renderUI({
      data<-data.aba1273()
      escolha<-unique(as.character(data[,1]))
      selectInput("aba1273.11", "Escolha o Composto:",
                  choices = c(as.character(escolha)))
    })
    
    output$aba1273.per.2<-renderUI({
      if(input$aba1273.11=="Personalizado"){
        numericInput("aba1273.out.1","Conteúdo de Nitrogênio (N):",min = 0,value = 10,step = 1)
      }})
    
    output$aba1273.per.3<-renderUI({
      if(input$aba1273.11=="Personalizado"){
        numericInput("aba1273.out.2","Conteúdo de Fósforo (P):",min = 0,value = 20,step = 1)
      }})
    
    output$aba1273.per.4<-renderUI({
      if(input$aba1273.11=="Personalizado"){
        numericInput("aba1273.out.3","Conteúdo de Potássio (K):",min = 0,value = 30,step = 1)
      }})
    
    output$aba1273.per.5<-renderUI({
      if(input$aba1273.11=="Personalizado"){
        numericInput("aba1273.out.4","Conteúdo de Cálcio (Ca):",min = 0,value = 5,step = 1)
      }})
    
    output$aba1273.per.6<-renderUI({
      if(input$aba1273.11=="Personalizado"){
        numericInput("aba1273.out.5","Conteúdo de Magnésio (Mg):",min = 0,value = 2,step = 1)
      }})
    
    output$aba1273.per.7<-renderUI({
      if(input$aba1273.11=="Personalizado"){
        numericInput("aba1273.out.6","Conteúdo de Enxofre (S):",min = 0,value = 20,step = 1)
      }})
    
    output$aba1273.3<-renderPrint({
      data<-data.aba1273()
      if(input$aba1273.11=="Personalizado"){
        saida<-round(as.matrix(cbind(input$aba1273.out.1,2.29*input$aba1273.out.2,1.205*input$aba1273.out.3,
                                 input$aba1273.out.4,input$aba1273.out.5,input$aba1273.out.6)*(100-input$aba1273.2)/100),2)
        rownames(saida)<-paste("Concentração em kg/t (",input$aba1273.11,"):  ",sep = "")
        colnames(saida)<-c("N",	"P2O5",	"K2O",	"Ca",	"Mg",	"S")
        return(saida)
      }
      if(input$aba1273.11!="Personalizado"){
      data1<-data[as.character(data[,1])==as.character(input$aba1273.11),-1]
      saida<-round(as.matrix(data1*(100-input$aba1273.2)/100),2)
      rownames(saida)<-paste("Concentração em kg/t (",input$aba1273.11,"):  ",sep = "")
      colnames(saida)<-c("N",	"P2O5",	"K2O",	"Ca",	"Mg",	"S")
      return(saida)}
    })  
    
    # #####################################################################################################################################################################################
    # aba122
    
    out.12.1 <- reactive({
      if(is.null(out.11.1()))
        return(NULL)
      data11<-out.11.1()
      
      carac<-do.call(c,strsplit(input$model,split ="~"))[1]
      
      data12<-data11
      for(i in 1:dim(data11)[2]){
        if(as.character(names(data11)[i])==as.character(carac))
        {data12[,i]<-as.numeric(as.character(data11[,i]))}
        if(as.character(names(data12)[i])!=as.character(carac))
        {data12[,i]<-as.factor(data11[,i])}
      }
      
      out.mod<-reactive({
        if(input$res.12.run){
          formula(isolate(input$model))}
        else
          NULL})
      
      if(is.null(out.mod()))
        return(NULL)
      modelo<-out.mod()
      mod.12<-lm(modelo, data=data12, na.action =  na.omit)
      mod.122<-aov(modelo, data=data12, na.action =  na.omit)
      LISTA<-list(mod.12,mod.122)
      return(LISTA)
    })
    
    
    output$contents12 <- renderPrint({
      if(input$info.12!=TRUE){
        if(input$res.12.name!=FALSE){
          return(names(out.11.1()))}
        if(input$res.12.run){
          switch(input$res.12.1,
                 "Estatistica Resumo"= summary(out.12.1()[[1]]),
                 "ANOVA"= anova(out.12.1()[[1]]))}
        else{}}
      else
        includeScript("help-estatistica.txt")})
    
    
    # output$contents12.1 <- renderPlot({
    #   if(input$info.12!=TRUE)
    #     if(input$res.12.run){
    #     qqPlot(out.12.1()[[1]])}
    #   else{}
    #   else
    #     includeScript("BeBreederEstatisticalInformation.txt")})
    
    output$downloadData12 <- downloadHandler(
      filename = function() { 
        paste(input$name.txt.12,'.txt', sep = "") 
      },
      content = function(file) {
        write.table(out.12.1()[[1]], file, sep="\t")})
    # #####################################################################################################################################################################################
    # aba122
    
    out.aba122<-reactive({
      parametro<-do.call(c,strsplit(input$model2,split = "~"))[2]
      #out <- LSD.test(out.12.1()[[2]],parametro, p.adj=input$res.122.1)
      out <- HSD.test(out.12.1()[[2]],parametro)
      return(out)})
    
    output$contents122 <- renderPrint({
      if(input$info.122!=TRUE){
        if(input$res.122.name!=FALSE){
          return(names(out.11.1()))}
        if(input$res.12.run){
          out.aba122()
        }
        else{}}
      else
        includeScript("help-resultados.txt")})
    
    
    output$contents122.1 <- renderPlot({
      if(input$info.122!=TRUE)
        if(input$res.122.run){
          if(is.null(out.11.1()))
            return(NULL)
          data12<-out.11.1()
          
          carac<-do.call(c,strsplit(input$model,split ="~"))[1]
          
          for(i in 1:dim(data12)[2]){
            if(names(data12)[i]==carac)
            {data12[,i]<-as.numeric(as.character(data12[,i]))}
            else
              data12[,i]<-as.factor(data12[,i])
          }
          geral.nam<-do.call(c,strsplit(as.character(input$model2),split = "~"))
          boxplot(formula(isolate(input$model2)),col="gray", data=data12,ylab = geral.nam[1],xlab = geral.nam[2])
        }
      else{}
      else
        includeScript("BeBreederEstatisticalInformation.txt")})
    
    output$downloadData122 <- downloadHandler(
      filename = function() { 
        paste(input$name.txt.122,'.txt', sep = "") 
      },
      content = function(file) {
        write.table(out.12.1()[[1]], file, sep="\t")})
    
    # ##############################################################################################################################################################################
    # aba123
    out.123.1 <- reactive({
      if(input$ex.123!=FALSE){
        TABE.1<-read.table("DadosExperimentoUreia2.txt",header = TRUE, sep = "\t")
        return(TABE.1)}
      inFile <- input$file123
      if (is.null(inFile))
        return(NULL)
      TABE<-read.table(inFile$datapath, header=input$header123, sep=input$sep123, 
                       quote=input$quote123)
      return(TABE)
    })
    
    output$contents123 <- renderPrint({
      if(input$info.123!=TRUE)
        return (out.123.1())
      else 
        includeScript("help-dados-multi.txt")})
    # ##############################################################################################################################################################################
    # aba124
    
    out.15.1<-reactive({
      if(is.null(out.123.1())){
        return(NULL)}
      data15<-as.matrix(out.123.1())
      name15<-names(out.123.1())
      data15.1<-data15[,-c(which(name15=="Tratamento"))]
      data15.2<-apply(data15.1,2,function(x){
        as.numeric(as.character(x))
      })
      COR.T<-correlation(data15.2,method ="pearson")
      LISTA<-COR.T
      names(LISTA)<-c("Correlacao","p-Valor","Num.Obs")
      return(LISTA)
    })
    
    
    output$contents15.2<- renderPrint({
      if(is.null(out.15.1())){
        return(NULL)}
      if(input$info.152!=TRUE){
        switch(input$res.15.2,
               "Correlacao"= round(out.15.1()[[1]],3),
               "p-Valor"=round(out.15.1()[[2]],3))
      }
      else 
        includeScript("help-correlacao.txt")})
    
    output$contents15.3<- renderPlot({
      if(is.null(out.15.1())){
        return(NULL)}
      if(input$info.152!=TRUE){
        #corrgram(out.15.1()[[1]], type = "cor", lower.panel = panel.shade, upper.panel = NULL)
        panel.shadeNtext <- function (x, y, corr = NULL, col.regions, ...) 
        {
          if (is.null(corr)) 
            1#corr <- cor(x, y, use = "pair",method = "pearson")
          corr <- cor(x, y, method = "pearson")
          ncol <- 14
          pal <- col.regions(ncol)
          col.ind <- as.numeric(cut(corr, breaks = seq(from = -1, to = 1, 
                                                       length = ncol + 1), include.lowest = TRUE))
          usr <- par("usr")
          rect(usr[1], usr[3], usr[2], usr[4], col = pal[col.ind], 
               border = NA)
          box(col = "lightgray")
          on.exit(par(usr))
          par(usr = c(0, 1, 0, 1))
          r <- formatC(corr, digits = 2, format = "f")
          cex.cor <- .5/strwidth("-X.xx")
          text(0.5, 0.5, r, cex = cex.cor)
        }
        
        panel.signif <-  function (x, y, corr = NULL, col.regions, digits = 2, cex.cor, 
                                   ...) {
          usr <- par("usr")
          on.exit(par(usr))
          par(usr = c(0, 1, 0, 1))
          results <- cor.test(x, y, alternative = "two.sided")
          est <- results$p.value
          stars <- ifelse(est < 5e-4, "***", 
                          ifelse(est < 5e-3, "**", 
                                 ifelse(est < 5e-2, "*", "NS")))
          cex.cor <- 0.2/strwidth(stars)
          text(0.5, 0.5, stars, cex = cex.cor)
        }
        corrgram(out.15.1()[[1]], type="data", lower.panel=panel.shadeNtext,
                 upper.panel=panel.signif)
        
      }
      else 
        includeScript("BeBreederDataPathAnalyse.txt")})
    
    output$downloadData15 <- downloadHandler(
      filename = function() { 
        paste(input$name.txt.15,'.txt',sep="") 
      },
      content = function(file) {
        write.table(capture.output(out.15.1()), file, sep="\t")
      })
    
    output$contents15.333<- renderPlot({
      if(is.null(out.15.1())){
        return(NULL)}
      if(input$info.152!=TRUE){
        #corrgram(out.15.1()[[1]], type = "cor", lower.panel = panel.shade, upper.panel = panel.pie)
        qgraph(out.15.1()[[1]], shape="square", layout="groups",curveAll=TRUE,
               vsize=5,label.cex=2,border.width=2, esize = 10)
      }
      else 
        includeScript("BeBreederDataPathAnalyse.txt")})
    
    output$downloadData15 <- downloadHandler(
      filename = function() { 
        paste(input$name.txt.15,'.txt',sep="") 
      },
      content = function(file) {
        write.table(capture.output(out.15.1()), file, sep="\t")
      })
    # ##############################################################################################################################################################################
    # aba125
    
    out.17.1<-reactive({
      if(is.null(out.123.1()))
        return(NULL)
      data171<-out.123.1()
      data17<-as.matrix(data171[,-1])
      rownames(data17)<-data171[,1]
      SUMMARY<- summary(prcomp(data17,scale = TRUE))
      SCORES.GEN<- summary(prcomp(data17,scale = TRUE))$x
      SCORES.ENV<- summary(prcomp(data17,scale = TRUE))$rotation
      
      LIST.17<-list(SUMMARY,SCORES.GEN,SCORES.ENV)
      return(LIST.17)})
    
    output$contents.aba125.2<- renderPlot({
      if(is.null(out.123.1()))
        return(NULL)
      data171<-out.123.1()
      # data17<-as.matrix(data171[,-1])
      # rownames(data17)<-data171[,1]
      # name17<-colnames(data17)
      # 
      # CP<-(summary(prcomp(data17,scale = TRUE))[[1]])^2
      # CP1<-paste("PC 1 (",(round(CP[1]/sum(CP),3))*100,"%",")", sep="")
      # CP2<-paste("PC 2 (",(round(CP[2]/sum(CP),3))*100,"%",")", sep="")
      # 
      # biplot(prcomp(data17,scale = TRUE),
      #        ylab=CP2,xlab=CP1)
      # abline(v=0,h=0)
      
      data.fim<-data171[,-1]
      data.fim1<-apply(data.fim,2,function(x){
        as.numeric(as.character(x))
      })
      rownames(data.fim1)<-data171[,1]
      
      res.pca <- prcomp(data.fim1,  scale = TRUE)
      
      fviz_pca_biplot(res.pca, label="all",pointshape = 16,
                      addEllipses=TRUE, ellipse.level=0.6,pointsize = 4,arrowsize = 1,labelsize = 5)+theme(legend.position = "none",
                                                                                                           legend.direction = "vertical",
                                                                                                           legend.text = element_text(color="black",size=18),
                                                                                                           legend.title = element_text(color="black",size=18),
                                                                                                           axis.text.y = element_text(color="black",size=18),
                                                                                                           axis.title = element_text(color="black",size=18),
                                                                                                           axis.text.x = element_text(color="black",size=18),
                                                                                                           strip.text = element_text(color="black",size=18),
                                                                                                           strip.background = element_rect(fill="white"))
      
    })
    
    out.17.3<-reactive({
      switch(input$res.172,
             "Estatistica Resumo"= out.17.1()[[1]],
             "Scores de Tratamento"= out.17.1()[[2]],
             "Scores de Características"= out.17.1()[[3]])})
    
    output$contents.aba125.1<- renderPrint({
      if(input$info.172!=TRUE)
        return(out.17.3())
      else 
        includeScript("help-pca.txt")
    })
    # #####################################################################################################################################################################################
    # aba1261
    
    react.FR211 <- reactive({
      area<-input$FR211.1
      CaO<-input$FR211.2
      MgO<-input$FR211.3
      PRNT<-input$FR211.4
      calagem<-input$FR211.5
      preco.ton<-input$FR211.6
      
      if(area==0&CaO==0&MgO==0&PRNT==0&calagem==0&preco.ton==0){return(NULL)}
      
      Ca.fornecido<-round(CaO*0.01784,3)
      Mg.fornecido<-round(MgO*0.02481,3) 
      Ca.for.rec<-round(Ca.fornecido*calagem,3) 
      Mg.for.rec<-round(Mg.fornecido*calagem,3) 
      correcao.para.100<- round((calagem * 100) / PRNT,3)  
      total.cal<- round(area*correcao.para.100,3) 
      custo.total<- round(total.cal*preco.ton,3)
      
      saida<-as.matrix(rbind(Ca.fornecido,Mg.fornecido,Ca.for.rec,Mg.for.rec,correcao.para.100,total.cal,custo.total))
      colnames(saida)<-"Atributos do Calcário"
      rownames(saida)<-c("Conteúdo de Ca no Calcário (cmolc/dm3)","Conteúdo de Mg no Calcário (cmolc/dm3)",
                         "Total Ca fornecido (cmolc)","Total Mg fornecido (cmolc)","Correção do PRNT=100% (t/ha)",
                         "Total de calcário necessário (t)","Preço Total (R$)")
      
      return(saida)
      
    })
    
    output$out.FR211 <- renderPrint({
      return(react.FR211())
    })      
    
    # #####################################################################################################################################################################################
    #aba210
    
    data.aba2111<-reactive({
      data1<-read.table(file = "macro.txt",header = T,sep = "\t")
      data2<-read.table(file = "micro.txt",header = T,sep = "\t")
      data3<-read.table(file = "outros.txt",header = T,sep = "\t")
      data4<-read.table(file = "ph.txt",header = T,sep = "\t")
      data5<-read.table(file = "fisica.txt",header = T,sep = "\t")
      LISTA<-list(data1,data2,data3,data4,data5)
      return(LISTA)
    })
    
    output$aba210.0<-renderUI({
      data<-data.aba2111()[[5]]
      data<-as.matrix(data)
      escolha<-unique(data[,1])
      selectInput("a.210.0", "Conteúdo de Argila (g/kg):",
                  choices = c(as.character(escolha)))
    })
    
    output$aba210.1<-renderUI({
      data<-data.aba2111()[[5]]
      data<-as.matrix(data)
      escolha<-data[unique(data[,1])==input$a.210.0,2]
      textInput("a.210.1","Porcentagem de Argila (%):", value = escolha)
    })
    
    output$aba210.2<-renderUI({
      data<-data.aba2111()[[5]]
      data<-as.matrix(data)
      escolha<-data[unique(data[,1])==input$a.210.0,3]
      textInput("a.210.2","Textura do Solo:", value = escolha)
    })
    
    # #####################################################################################################################################################################################
    #aba211
    
    
    output$res.aba211.5<-renderUI({
      data<-data.aba2111()[[1]]
      escolha<-unique(data[as.character(data[,1])=="P",3])
      selectInput("res.aba2111.5", "Escolha a Cultura:",
                  choices = c(as.character(escolha)))
    })
    
    output$res.aba211.8<-renderUI({
      data<-data.aba2111()[[1]]
      escolha<-unique(data[as.character(data[,1])=="P"&as.character(data[,3])==input$res.aba2111.5,4])
      selectInput("res.aba2111.8", "Conteúdo de Fósforo (mmolc/dm3):",
                  choices = c(as.character(escolha)))
    })
    
    output$res.aba211.9<-renderUI({
      data<-data.aba2111()[[1]]
      escolha<-unique(data[as.character(data[,1])=="K",4])
      selectInput("res.aba2111.9", "Conteúdo de Potássio (mmolc/dm3):",
                  choices = c(as.character(escolha)))
    })
    
    reac.P<-reactive({
      data<-data.aba2111()[[1]]
      escolha<-unique(data[as.character(data[,1])=="P"&as.character(data[,3])==input$res.aba2111.5&as.character(data[,4])==input$res.aba2111.8,5])
      escolha
    })
    
    output$res.aba211.11<-renderUI({
      selectInput("a.2112","Classe de Fósforo (P):", choices = reac.P())
    })
    
    reac.K<-reactive({
      data<-data.aba2111()[[1]]
      escolha<-unique(data[as.character(data[,1])=="K"&as.character(data[,4])==input$res.aba2111.9,5])
      escolha
    })
    
    output$res.aba211.12<-renderUI({
      selectInput("a.2113","Classe de Potássio (K):", choices = reac.K())
    })
    
    # #####################################################################################################################################################################################
    #aba212
    
    output$res.aba212.6<-renderUI({
      data<-data.aba2111()[[2]]
      escolha<-unique(data[as.character(data[,1])=="B",3])
      selectInput("res.aba2121.6", "Conteúdo de Boro (mg/dm3):",
                  choices = c(as.character(escolha)))
    })
    output$res.aba212.7<-renderUI({
      data<-data.aba2111()[[2]]
      escolha<-unique(data[as.character(data[,1])=="Cu",3])
      selectInput("res.aba2121.7", "Conteúdo de Cobre (mg/dm3):",
                  choices = c(as.character(escolha)))
    })
    output$res.aba212.8<-renderUI({
      data<-data.aba2111()[[2]]
      escolha<-unique(data[as.character(data[,1])=="Fe",3])
      selectInput("res.aba2121.8", "Conteúdo de Ferro (mg/dm3):",
                  choices = c(as.character(escolha)))
    })
    output$res.aba212.9<-renderUI({
      data<-data.aba2111()[[2]]
      escolha<-unique(data[as.character(data[,1])=="Mn",3])
      selectInput("res.aba2121.9", "Conteúdo de Manganês (mg/dm3):",
                  choices = c(as.character(escolha)))
    })
    output$res.aba212.10<-renderUI({
      data<-data.aba2111()[[2]]
      escolha<-unique(data[as.character(data[,1])=="Zn",3])
      selectInput("res.aba2121.10", "Conteúdo de Zinco (mg/dm3):",
                  choices = c(as.character(escolha)))
    })
    
    reac.B<-reactive({
      data<-data.aba2111()[[2]]
      escolha<-unique(data[as.character(data[,1])=="B"&as.character(data[,3])==input$res.aba2121.6,4])
      escolha
    })
    
    output$res.aba212.11<-renderUI({
      selectInput("a.2121","Classe de Boro (B):", choices = reac.B())
    })
    
    reac.Cu<-reactive({
      data<-data.aba2111()[[2]]
      escolha<-unique(data[as.character(data[,1])=="Cu"&as.character(data[,3])==input$res.aba2121.7,4])
      escolha
    })
    
    output$res.aba212.12<-renderUI({
      selectInput("a.2122","Classe de Cobre (Cu):", choices = reac.Cu())
    })

    reac.Fe<-reactive({
      data<-data.aba2111()[[2]]
      escolha<-unique(data[as.character(data[,1])=="Fe"&as.character(data[,3])==input$res.aba2121.8,4])
      escolha
    })
    
    output$res.aba212.13<-renderUI({
      selectInput("a.2123","Classe de Ferro (Fe):", choices = reac.Fe())
    })
    
    reac.Mn<-reactive({
      data<-data.aba2111()[[2]]
      escolha<-unique(data[as.character(data[,1])=="Mn"&as.character(data[,3])==input$res.aba2121.9,4])
      escolha
    })
    
    output$res.aba212.14<-renderUI({
      selectInput("a.2124","Classe de Manganês (Mn):", choices = reac.Mn())
    })
    
    reac.Zn<-reactive({
      data<-data.aba2111()[[2]]
      escolha<-unique(data[as.character(data[,1])=="Zn"&as.character(data[,3])==input$res.aba2121.10,4])
      escolha
    })
    
    output$res.aba212.15<-renderUI({
      selectInput("a.2125","Classe de Zinco (Zn):", choices = reac.Zn())
    })
    
    # #####################################################################################################################################################################################
    #aba213
    
    output$res.aba213.4<-renderUI({
      data<-data.aba2111()[[3]]
      escolha<-unique(data[as.character(data[,1])=="Ca",3])
      selectInput("res.aba2131.4", "Conteúdo de Cálcio (mmolc/dm3):",
                  choices = c(as.character(escolha)))
    })
    output$res.aba213.5<-renderUI({
      data<-data.aba2111()[[3]]
      escolha<-unique(data[as.character(data[,1])=="Mg",3])
      selectInput("res.aba2131.5", "Conteúdo de Magnésio (mmolc/dm3):",
                  choices = c(as.character(escolha)))
    })
    output$res.aba213.6<-renderUI({
      data<-data.aba2111()[[3]]
      escolha<-unique(data[as.character(data[,1])=="S",3])
      selectInput("res.aba2131.6", "Conteúdo de Enxofre (mg/dm3):",
                  choices = c(as.character(escolha)))
    })
    
    reac.Ca<-reactive({
      data<-data.aba2111()[[3]]
      escolha<-unique(data[as.character(data[,1])=="Ca"&as.character(data[,3])==input$res.aba2131.4,4])
      escolha
    })
    
    output$res.aba213.7<-renderUI({
      selectInput("a.2131","Classe de Cálcio (Ca):", choices = reac.Ca())
    })
    
    reac.Mg<-reactive({
      data<-data.aba2111()[[3]]
      escolha<-unique(data[as.character(data[,1])=="Mg"&as.character(data[,3])==input$res.aba2131.5,4])
      escolha
    })
    
    output$res.aba213.8<-renderUI({
      selectInput("a.2132","Classe de Magnésio (Mg):", choices = reac.Mg())
    })
    
    reac.S<-reactive({
      data<-data.aba2111()[[3]]
      escolha<-unique(data[as.character(data[,1])=="S"&as.character(data[,3])==input$res.aba2131.6,4])
      escolha
    })
    
    output$res.aba213.9<-renderUI({
      selectInput("a.2133","Classe de Enxofre (S):", choices = reac.S())
    })
    
    # #####################################################################################################################################################################################
    
    plot199<-reactive({
      nivel<-c(as.character(reac.P()),
               as.character(reac.K()),
               as.character(reac.Ca()),
               as.character(reac.Mg()),
               as.character(reac.S()),
               as.character(reac.B()),
               as.character(reac.Cu()),
               as.character(reac.Fe()),
               as.character(reac.Mn()),
               as.character(reac.Zn()))
      elem<-c("P","K","Ca","Mg","S","B","Cu","Fe","Mn","Zn")
      
      col.niv<-gsub("Muito alto","darkolivegreen4",as.character(nivel))
      col.niv<-gsub("Alto","darkolivegreen3",col.niv)
      col.niv<-gsub("Medio","darkgoldenrod1",col.niv)
      col.niv<-gsub("Baixo","brown3",col.niv)
      col.niv<-gsub("Muito baixo","brown4",col.niv)
      
      Num.niv<-gsub("Muito alto",5,as.character(nivel))
      Num.niv<-gsub("Alto",4,Num.niv)
      Num.niv<-gsub("Medio",3,Num.niv)
      Num.niv<-gsub("Baixo",2,Num.niv)
      Num.niv<-gsub("Muito baixo",1,Num.niv)
      
      dados<-cbind(nivel,elem,col.niv,Num.niv)
      dados1<-as.matrix(dados)
      return(dados1)
    })
    
    output$aba199.plot<-renderPlot({
      
      dados22<-as.matrix(plot199())
      dados<-as.data.frame(dados22)
      colnames(dados)<-c("nivel","elem","col.niv","Num.niv")
      dados$Num.niv<-as.numeric(as.character(dados$Num.niv))
      dados$nivel<-as.factor(dados$nivel)
      #dados$elem<-as.factor(dados$elem)
      dados$col.niv<-as.character(dados$col.niv)
      dados$elem1<-factor(dados$elem,levels = c("P","K","Ca","Mg","S","B","Cu","Fe","Mn","Zn"))
      
      col.niv<-as.character(dados$col.niv)
      nivel<-as.character(dados$nivel)
      
      # my_bar<- barplot(as.vector(dados$Num.niv), border=F , names.arg=dados$elem , las=1 , col=as.character(as.vector(dados$col.niv)), main="Perfil da Análise de Solo" )
      # text(my_bar, as.numeric(dados$Num.niv)+0.4 , dados$nivel ,cex=1)
      # 
      # my_bar
      
      # mycolor2<-c("Muito alto","Alto","Medio","Baixo","Muito baixo")
      # mycolor<-c("Muito alto"="darkolivegreen4","Alto"="darkolivegreen3",
      #            "Medio"="darkgoldenrod1","Baixo"="brown3","Muito baixo"="brown4")[mycolor2%in%col.niv]
      # mycolor3<-c("Muito alto","Alto","Medio","Baixo","Muito baixo")[mycolor2%in%col.niv]
      
      ggplot(dados, aes(x = elem1 , y = Num.niv,fill=col.niv))+
        geom_bar(stat = "identity",
                 position = 'dodge') +
        #scale_fill_manual(c())+
        #scale_fill_manual(values=c("darkolivegreen3","brown3","darkgoldenrod1","darkolivegreen4","brown4"))+
        scale_fill_grey(start = 0.2, end = 0.9)+
        #scale_fill_gradient2(low="red", mid="orange",high="green")
        geom_text(aes(label=as.character(nivel),mapping=F), position=position_dodge(width=0.9), vjust=-0.20,size=5)+
        scale_y_continuous(breaks=seq(0,6,by=1), limits = c(0, 5))+
        theme_bw()+
        xlab("")+
        ylab("Perfil da Análise de Solo")+
        theme(legend.position = "rigth",
              legend.title = element_text(color="black",size=18),
              axis.text.y = element_blank(),
              axis.title = element_text(color="black",size=18),
              axis.text.x = element_text(color="black",size=16,angle = 0, hjust = 0.5,vjust = 0.5),
              strip.text = element_text(color="black",size=18),
              strip.background = element_rect(fill="white"))
      
    })
    
    # #####################################################################################################################################################################################
    #aba214
    
    output$res.aba214.1<-renderUI({
      data<-data.aba2111()[[4]]
      escolha<-unique(data[as.character(data[,1])=="pH",2])
      selectInput("res.aba2141.1", "Valor do pH em CaCl2:",
                  choices = c(as.character(escolha)))
    })
    output$res.aba214.2<-renderUI({
      data<-data.aba2111()[[4]]
      escolha<-unique(data[as.character(data[,1])=="V",2])
      selectInput("res.aba2141.2", "Saturação por Bases (V%):",
                  choices = c(as.character(escolha)))
    })
    output$res.aba214.3<-renderUI({
      data<-data.aba2111()[[4]]
      escolha<-unique(data[as.character(data[,1])=="pH"&as.character(data[,2])==input$res.aba2141.1,3])
      textInput("a.2141","Classe do pH em CaCl2:", value = escolha)
    })
    output$res.aba214.4<-renderUI({
      data<-data.aba2111()[[4]]
      escolha<-unique(data[as.character(data[,1])=="V"&as.character(data[,2])==input$res.aba2141.2,3])
      textInput("a.2142","Classe de Saturação por Bases (V%):", value = escolha)
    })
    # #####################################################################################################################################################################################
    #aba23
    data.aba23<-reactive({
      data<-read.table(file = "recomendacao_2.txt",header = T,sep = "\t")
      return(data)
    })
    
    output$res.aba23.0<-renderUI({
      data<-data.aba23()
      escolha<-unique(as.character(data[,1]))
      selectInput("res.aba23.1", "Escolha a Cultura:",
                  choices = c(as.character(escolha)))
    })
    
    output$res.aba23.2<-renderUI({
      data<-data.aba23()
      escolha<-unique(data[as.character(data[,1])==as.character(input$res.aba23.1),3])
      if(input$res.aba23.1%in%c("Eucalyptus","Pinus")){
        return(
          selectInput("res.aba23.21", "Teor de Argila (g/kg):",
                      choices = c(as.character(escolha))))
      }
      if(!input$res.aba23.1%in%c("Eucalyptus","Pinus")){
        selectInput("res.aba23.21", "Produtividade Esperada (t/ha):",
                    choices = c(as.character(escolha)))}
    })
    
    output$res.aba23.3<-renderUI({
      data<-data.aba23()
      escolha<-unique(data[as.character(data[,1])==as.character(input$res.aba23.1)&as.character(data[,2])=="N",4])
      if(input$res.aba23.1%in%c("Eucalyptus","Pinus")){
        return(
          selectInput("res.aba23.31", "Conteúdo de M.O. (g/dm³):",
                      choices = c(as.character(escolha)))
        )}
      if(!input$res.aba23.1%in%c("Eucalyptus","Pinus")){
        selectInput("res.aba23.31", "Conteúdo de Nitrogênio (N):",
                    choices = c(as.character(escolha)))}
    })
    
    output$res.aba23.4<-renderUI({
      data<-data.aba23()
      escolha<-unique(data[as.character(data[,1])==as.character(input$res.aba23.1)&as.character(data[,2])=="P",4])
      selectInput("res.aba23.41", "Conteúdo de Fósforo (P mg/dm3):",
                  choices = c(as.character(escolha)))
    })
    
    output$res.aba23.5<-renderUI({
      data<-data.aba23()
      escolha<-unique(data[as.character(data[,1])==as.character(input$res.aba23.1)&as.character(data[,2])=="K",4])
      selectInput("res.aba23.51", "Conteúdo de Potássio (K mmolc/dm3):",
                  choices = c(as.character(escolha)))
    })
    
    output$contents.aba23<-renderPrint({
      data<-data.aba23()
      if(input$res.aba23.6){
        N<-data[as.character(data[,1])==as.character(input$res.aba23.1)&as.character(data[,1])==as.character(input$res.aba23.1)&as.character(data[,3])==input$res.aba23.21&as.character(data[,4])==input$res.aba23.31&as.character(data[,2])=="N",5]
        P<-data[as.character(data[,1])==as.character(input$res.aba23.1)&as.character(data[,1])==as.character(input$res.aba23.1)&as.character(data[,3])==input$res.aba23.21&as.character(data[,4])==input$res.aba23.41&as.character(data[,2])=="P",5]
        K<-data[as.character(data[,1])==as.character(input$res.aba23.1)&as.character(data[,1])==as.character(input$res.aba23.1)&as.character(data[,3])==input$res.aba23.21&as.character(data[,4])==input$res.aba23.51&as.character(data[,2])=="K",5]
        
        saida<- rbind(paste("Aplicar ",N," kg/ha de N",sep = ""),
                      paste("Aplicar ",P," kg/ha de P2O5",sep = ""),
                      paste("Aplicar ",K," kg/ha de K2O",sep = ""))
        colnames(saida)<-paste("Recomendação para ",input$res.aba23.1,sep = "")
        rownames(saida)<-c("N","P","K")
        return(saida)}
    })
    
    out.aba23.rel<-reactive({
      data<-data.aba23()
        N<-data[as.character(data[,1])==as.character(input$res.aba23.1)&as.character(data[,1])==as.character(input$res.aba23.1)&as.character(data[,3])==input$res.aba23.21&as.character(data[,4])==input$res.aba23.31&as.character(data[,2])=="N",5]
        P<-data[as.character(data[,1])==as.character(input$res.aba23.1)&as.character(data[,1])==as.character(input$res.aba23.1)&as.character(data[,3])==input$res.aba23.21&as.character(data[,4])==input$res.aba23.41&as.character(data[,2])=="P",5]
        K<-data[as.character(data[,1])==as.character(input$res.aba23.1)&as.character(data[,1])==as.character(input$res.aba23.1)&as.character(data[,3])==input$res.aba23.21&as.character(data[,4])==input$res.aba23.51&as.character(data[,2])=="K",5]
        
        saida<- rbind(paste("Aplicar ",N," kg/ha de N",sep = ""),
                      paste("Aplicar ",P," kg/ha de P2O5",sep = ""),
                      paste("Aplicar ",K," kg/ha de K2O",sep = ""))
        colnames(saida)<-paste("Recomendação para ",input$res.aba23.1,sep = "")
        rownames(saida)<-c("N","P","K")
        return(saida)
    })
    
    output$contents.aba23.1 <- renderPrint({
      if(input$res.aba23.6){
        if(as.character(input$res.aba23.1)=="Soja")
        {return (includeScript("Soja.txt"))}
        if(as.character(input$res.aba23.1)=="Milho Cobertura")
        {return (includeScript("MilhoCobertura.txt"))}
        if(as.character(input$res.aba23.1)=="Milho Plantio")
        {return (includeScript("MilhoPlantio.txt"))}
        if(as.character(input$res.aba23.1)=="Eucalyptus")
        {return (includeScript("EucalyptusPlantio.txt"))}
        # if(as.character(input$res.aba23.1)=="")
        # {return (includeScript("PastagemPlantio.txt"))}
        if(as.character(input$res.aba23.1)=="Saccharum spp - Plantio")
        {return (includeScript("SaccarumLplantio.txt"))}
        if(as.character(input$res.aba23.1)=="Saccharum spp - Soqueira")
        {return ("Em construção")}
        if(as.character(input$res.aba23.1)=="Pinus")
        {return (includeScript("PinusPlantio.txt"))}
        else
          #return ("Em construção")
          return (includeScript("PastagemPlantio.txt"))
        return (NULL)}
    })
    
    output$report <- downloadHandler(
      filename = "report.pdf",
      content = function(file) {
        withProgress(message = 'Gerando o relatório', value = 0, {
          incProgress(1/2, detail = paste("Parte", 1))
        tempReport <- file.path(tempdir(), "report.Rmd")
        file.copy("report.Rmd", tempReport, overwrite = TRUE)
        params <- list(n = as.matrix(out.aba23.rel()))
        rmarkdown::render(tempReport, output_file = file,
                          params = params,
                          envir = new.env(parent = globalenv())
        )
        incProgress(2/2, detail = paste("Parte", 2))})
        }
      )
    
    # #####################################################################################################################################################################################
    
    ###############################
    ### Garantias Fertilizantes ###
    ###############################
    
    data.GA1<-reactive({
      data1<-read.table(file = "opcoes_legis_fert.txt",header = F,sep = ";")
      data2<-read.table(file = "legis_natureza_fisica.txt",header = F,sep = "\t")
      data3<-read.table(file = "legis_formula_simples.txt",header = F,sep = "\t")
      data4<-read.table(file = "legis_Mistos.txt",header = F,sep = "\t")
      data5<-read.table(file = "legis_Def.txt",header = F,sep = "\t")
      data6<-read.table(file = "legis_Exc.txt",header = F,sep = "\t")
      data7<-read.table(file = "legis_Fert.txt",header = F,sep = "\t")
      LISTA<-list(data1,data2,data3,data4,data5,data6,data7)
      return(LISTA)
    })
    
    output$tabGA1.1<-renderUI({
      data<-data.GA1()[[1]]
      escolha<-c(as.character(unique(data[,1])))
      selectInput("res.GA1.1", "Escolha o Tipo:",
                  choices = c(as.character(escolha)))
    })
    
    ### Fisica
    
    output$tabGA1.2<-renderUI({
      type.G<-input$res.GA1.1
      if(as.character(type.G)!="Natureza fisica")
        return("")
      data<-data.GA1()[[2]]
      data<-as.matrix(data)
      escolha<-unique(data[,1])
      selectInput("res.GA1.2", "Tipo:",
                  choices = c(as.character(escolha)))
    })
    
    output$tabGA1.3<-renderUI({
      type.G<-input$res.GA1.1
      if(type.G!="Natureza fisica")
        return("")
      type.G.1<-input$res.GA1.2
      data<-data.GA1()[[2]]
      data<-as.matrix(data)
      escolha<-unique(data[as.character(data[,1])==as.character(type.G.1),2])
      selectInput("res.GA1.3", "Granulometria:",
                  choices = c(as.character(escolha)))
    })
    
    output$tabGA1.4<-renderUI({
      type.G<-input$res.GA1.1
      if(type.G!="Natureza fisica")
        return("")
      type.G.1<-input$res.GA1.2
      type.G.2<-input$res.GA1.3
      data<-data.GA1()[[2]]
      data<-as.matrix(data)
      escolha<-unique(data[as.character(data[,1])==as.character(type.G.1)&as.character(data[,2])==as.character(type.G.2),3])
      textInput("saida.GA.fisica","Partículas passantes:", value = escolha)
    })
    
    ### Simples
    
    output$tabGA1.5<-renderUI({
      type.G<-input$res.GA1.1
      if(as.character(type.G)!="Fontes simples")
        return("")
      data<-data.GA1()[[3]]
      data<-as.matrix(data)
      escolha<-unique(data[,1])
      selectInput("res.GA1.4", "Tipo:",
                  choices = c(as.character(escolha)))
    })
    
    output$tabGA1.6<-renderUI({
      type.G<-input$res.GA1.1
      if(type.G!="Fontes simples")
        return("")
      type.G.1<-input$res.GA1.4
      data<-data.GA1()[[3]]
      data<-as.matrix(data)
      escolha<-unique(data[as.character(data[,1])==as.character(type.G.1),2])
      selectInput("saida.GA.fisica", "Teor e forma do nutriente:",
                  choices = c(as.character(escolha)))
      #textInput("saida.GA.fisica","Teor e forma do nutriente:", value = escolha)
    })
    
    output$tabGA1.7<-renderUI({
      type.G<-input$res.GA1.1
      if(type.G!="Fontes simples")
        return("")
      type.G.1<-input$res.GA1.4
      data<-data.GA1()[[3]]
      data<-as.matrix(data)
      escolha<-unique(data[as.character(data[,1])==as.character(type.G.1),3])
      selectInput("saida.GA.fisica", "Solubilidade do nutriente / granulometria:",
                  choices = c(as.character(escolha)))
      #textInput("saida.GA.fisica","Solubilidade do nutriente / granulometria", value = escolha)
    })
    
    output$tabGA1.8<-renderUI({
      type.G<-input$res.GA1.1
      if(type.G!="Fontes simples")
        return("")
      type.G.1<-input$res.GA1.4
      data<-data.GA1()[[3]]
      data<-as.matrix(data)
      escolha<-unique(data[as.character(data[,1])==as.character(type.G.1),4])
      selectInput("saida.GA.fisica", "Obtenção:",
                  choices = c(as.character(escolha)))
      #textInput("saida.GA.fisica","Obtenção:", value = escolha)
    })
    
    output$tabGA1.9<-renderUI({
      type.G<-input$res.GA1.1
      if(type.G!="Fontes simples")
        return("")
      type.G.1<-input$res.GA1.4
      data<-data.GA1()[[3]]
      data<-as.matrix(data)
      escolha<-unique(data[as.character(data[,1])==as.character(type.G.1),5])
      selectInput("saida.GA.fisica", "Observação:",
                  choices = c(as.character(escolha)))
      #textInput("saida.GA.fisica","Observação:", value = escolha)
    })
    
    ### MIstos
    
    output$tabGA1.10<-renderUI({
      type.G<-input$res.GA1.1
      if(as.character(type.G)!="Fertilizantes mistos")
        return("")
      data<-data.GA1()[[4]]
      data<-as.matrix(data)
      escolha<-unique(data[,1])
      selectInput("res.GA1.5", "Tipo:",
                  choices = c(as.character(escolha)))
    })
    
    output$tabGA1.11<-renderUI({
      type.G<-input$res.GA1.1
      if(type.G!="Fertilizantes mistos")
        return("")
      type.G.1<-input$res.GA1.5
      data<-data.GA1()[[4]]
      data<-as.matrix(data)
      escolha<-unique(data[as.character(data[,1])==as.character(type.G.1),2])
      selectInput("saida.GA.fisica", "Soma dos macronutrientes (% em peso - p.p.):",
                  choices = c(as.character(escolha)))
      #textInput("saida.GA.fisica","Soma dos macronutrientes (% em peso - p.p.)", value = escolha)
    })
    
    ### Def
    
    output$tabGA1.12<-renderUI({
      type.G<-input$res.GA1.1
      if(as.character(type.G)!="Tolerancias quanto a deficiencia")
        return("")
      data<-data.GA1()[[5]]
      data<-as.matrix(data)
      escolha<-unique(data[,1])
      selectInput("res.GA1.6", "Teores Garantidos (Tg) em %",
                  choices = c(as.character(escolha)))
    })
    
    output$tabGA1.13<-renderUI({
      type.G<-input$res.GA1.1
      if(type.G!="Tolerancias quanto a deficiencia")
        return("")
      type.G.1<-input$res.GA1.6
      data<-data.GA1()[[5]]
      data<-as.matrix(data)
      escolha<-unique(data[as.character(data[,1])==as.character(type.G.1),2])
      textInput("saida.GA.fisica","Tolerância (T) Para Fertilizantes Minerais Simples e Complexos", value = escolha)
    })
    
    output$tabGA1.14<-renderUI({
      type.G<-input$res.GA1.1
      if(type.G!="Tolerancias quanto a deficiencia")
        return("")
      type.G.1<-input$res.GA1.6
      data<-data.GA1()[[5]]
      data<-as.matrix(data)
      escolha<-unique(data[as.character(data[,1])==as.character(type.G.1),3])
      textInput("saida.GA.fisica","Tolerância (T) Para Fertilizantes Minerais Mistos", value = escolha)
    })
    
    ### Exes
    
    output$tabGA1.15<-renderUI({
      type.G<-input$res.GA1.1
      if(as.character(type.G)!="Tolerancias quanto a excesso")
        return("")
      data<-data.GA1()[[6]]
      data<-as.matrix(data)
      escolha<-unique(data[,1])
      selectInput("res.GA1.6", "Nutriente",
                  choices = c(as.character(escolha)))
    })
    
    output$tabGA1.16<-renderUI({
      type.G<-input$res.GA1.1
      if(type.G!="Tolerancias quanto a excesso")
        return("")
      type.G.1<-input$res.GA1.6
      data<-data.GA1()[[6]]
      data<-as.matrix(data)
      escolha<-unique(data[as.character(data[,1])==as.character(type.G.1),2])
      textInput("saida.GA.fisica","Tolerância máxima quando comercializado em mistura", value = escolha)
    })
    
    output$tabGA1.17<-renderUI({
      type.G<-input$res.GA1.1
      if(type.G!="deficiências quanto a excesso")
        return("")
      type.G.1<-input$res.GA1.6
      data<-data.GA1()[[6]]
      data<-as.matrix(data)
      escolha<-unique(data[as.character(data[,1])==as.character(type.G.1),3])
      textInput("saida.GA.fisica","Tolerância máxima quando comercializado individualmente", value = escolha)
    })
    
    ### Fertir
    
    output$tabGA1.18<-renderUI({
      type.G<-input$res.GA1.1
      if(as.character(type.G)!="Aplicacao via fertirrigacao, foliar, hidroponia e semente")
        return("")
      data<-data.GA1()[[7]]
      data<-as.matrix(data)
      escolha<-unique(data[,1])
      selectInput("res.GA1.7", "Teor do elemento (%):",
                  choices = c(as.character(escolha)))
    })
    
    output$tabGA1.19<-renderUI({
      type.G<-input$res.GA1.1
      if(type.G!="Aplicacao via fertirrigacao, foliar, hidroponia e semente")
        return("")
      type.G.1<-input$res.GA1.7
      data<-data.GA1()[[7]]
      data<-as.matrix(data)
      escolha<-unique(data[as.character(data[,1])==as.character(type.G.1),2])
      textInput("saida.GA.fisica","Tolerância:", value = escolha)
    })
    
    # #####################################################################################################################################################################################    
    #tabGA2
    
    data.GA2<-reactive({
      data1<-read.table(file = "opcoes_legis_Cor.txt",header = F,sep = ";")
      data2<-read.table(file = "legis_natureza_fisica_Cor.txt",header = T,sep = "\t")
      data3<-read.table(file = "legis_acid_Cor.txt",header = T,sep = "\t")
      data4<-read.table(file = "legis_alca_Cor.txt",header = T,sep = "\t")
      data5<-read.table(file = "legis_sod_Cor.txt",header = T,sep = "\t")
      LISTA<-list(data1,data2,data3,data4,data5)
      return(LISTA)
    })
    
    output$tabGA2.1<-renderUI({
      data<-data.GA2()[[1]]
      escolha<-c(as.character(unique(data[,1])))
      selectInput("res.GA2.1", "Escolha o Tipo:",
                  choices = c(as.character(escolha)))
    })
    
    ### Fisica
    
    output$tabGA2.2<-renderUI({
      type.G<-input$res.GA2.1
      if(as.character(type.G)!="Natureza fisica")
        return("")
      data<-data.GA2()[[2]]
      data<-as.matrix(data)
      escolha<-unique(data[,1])
      selectInput("res.GA2.2", "Peneira:",
                  choices = c(as.character(escolha)))
    })
    
    output$tabGA2.3<-renderUI({
      type.G<-input$res.GA2.1
      if(type.G!="Natureza fisica")
        return("")
      type.G.1<-input$res.GA2.2
      data<-data.GA2()[[2]]
      data<-as.matrix(data)
      escolha<-unique(data[as.character(data[,1])==as.character(type.G.1),2])
      textInput("saida.GA.fisica","Abertura (mm):", value = escolha)
    })
    
    output$tabGA2.4<-renderUI({
      type.G<-input$res.GA2.1
      if(type.G!="Natureza fisica")
        return("")
      type.G.1<-input$res.GA2.2
      data<-data.GA2()[[2]]
      data<-as.matrix(data)
      escolha<-unique(data[as.character(data[,1])==as.character(type.G.1),3])
      textInput("saida.GA.fisica","Passante mínimo (%):", value = escolha)
    })
    
    output$tabGA2.4<-renderUI({
      type.G<-input$res.GA2.1
      if(type.G!="Natureza fisica")
        return("")
      type.G.1<-input$res.GA2.2
      data<-data.GA2()[[2]]
      data<-as.matrix(data)
      escolha<-unique(data[as.character(data[,1])==as.character(type.G.1),4])
      textInput("saida.GA.fisica","Tolerância  para deficiência:", value = escolha)
    })
    
    output$tabGA2.5<-renderUI({
      type.G<-input$res.GA2.1
      if(type.G!="Natureza fisica")
        return("")
      type.G.1<-input$res.GA2.2
      data<-data.GA2()[[2]]
      data<-as.matrix(data)
      escolha<-unique(data[as.character(data[,1])==as.character(type.G.1),5])
      textInput("saida.GA.fisica","Tolerância  para excesso:", value = escolha)
    })
    
    ### acidez
    
    output$tabGA2.6<-renderUI({
      type.G<-input$res.GA2.1
      if(as.character(type.G)!="Corretivos de acidez")
        return("")
      data<-data.GA2()[[3]]
      data<-as.matrix(data)
      escolha<-unique(data[,1])
      selectInput("res.GA2.3", "Material corretivo de acidez:",
                  choices = c(as.character(escolha)))
    })
    
    output$tabGA2.7<-renderUI({
      type.G<-input$res.GA2.1
      if(type.G!="Corretivos de acidez")
        return("")
      type.G.1<-input$res.GA2.3
      data<-data.GA2()[[3]]
      data<-as.matrix(data)
      escolha<-unique(data[as.character(data[,1])==as.character(type.G.1),2])
      textInput("saida.GA.fisica","Poder de neutralizacao PN (% equivalente CaCO3) mínimo:", value = escolha)
    })
    
    output$tabGA2.8<-renderUI({
      type.G<-input$res.GA2.1
      if(type.G!="Corretivos de acidez")
        return("")
      type.G.1<-input$res.GA2.3
      data<-data.GA2()[[3]]
      data<-as.matrix(data)
      escolha<-unique(data[as.character(data[,1])==as.character(type.G.1),3])
      textInput("saida.GA.fisica","Soma %CaO + %MgO mínimo:", value = escolha)
    })
    
    output$tabGA2.9<-renderUI({
      type.G<-input$res.GA2.1
      if(type.G!="Corretivos de acidez")
        return("")
      type.G.1<-input$res.GA2.3
      data<-data.GA2()[[3]]
      data<-as.matrix(data)
      escolha<-unique(data[as.character(data[,1])==as.character(type.G.1),4])
      textInput("saida.GA.fisica","PRNT (%) mínimo:", value = escolha)
    })
    
    output$tabGA2.10<-renderUI({
      type.G<-input$res.GA2.1
      if(type.G!="Corretivos de acidez")
        return("")
      type.G.1<-input$res.GA2.3
      data<-data.GA2()[[3]]
      data<-as.matrix(data)
      escolha<-unique(data[as.character(data[,1])==as.character(type.G.1),5])
      textInput("saida.GA.fisica","Tolerância  para excesso:", value = escolha)
    })
    
    output$tabGA2.11<-renderUI({
      type.G<-input$res.GA2.1
      if(type.G!="Corretivos de acidez")
        return("")
      type.G.1<-input$res.GA2.3
      data<-data.GA2()[[3]]
      data<-as.matrix(data)
      escolha<-unique(data[as.character(data[,1])==as.character(type.G.1),6])
      selectInput("saida.GA.fisica", "Tolerância  para deficiência:",
                  choices = c(as.character(escolha)))
    })
    
    ### alcal
    
    output$tabGA2.12<-renderUI({
      type.G<-input$res.GA2.1
      if(as.character(type.G)!="Corretivos de alcalinidade")
        return("")
      data<-data.GA2()[[4]]
      data<-as.matrix(data)
      escolha<-unique(data[,1])
      selectInput("res.GA2.4", "Material corretivo de sodicidade:",
                  choices = c(as.character(escolha)))
    })
    
    output$tabGA2.13<-renderUI({
      type.G<-input$res.GA2.1
      if(type.G!="Corretivos de alcalinidade")
        return("")
      type.G.1<-input$res.GA2.4
      data<-data.GA2()[[4]]
      data<-as.matrix(data)
      escolha<-unique(data[as.character(data[,1])==as.character(type.G.1),2])
      selectInput("saida.GA.fisica", "Garantia mínima:",
                  choices = c(as.character(escolha)))
    })
    
    output$tabGA2.14<-renderUI({
      type.G<-input$res.GA2.1
      if(type.G!="Corretivos de alcalinidade")
        return("")
      type.G.1<-input$res.GA2.4
      data<-data.GA2()[[4]]
      data<-as.matrix(data)
      escolha<-unique(data[as.character(data[,1])==as.character(type.G.1),3])
      selectInput("saida.GA.fisica", "Características:",
                  choices = c(as.character(escolha)))
    })
    
    output$tabGA2.15<-renderUI({
      type.G<-input$res.GA2.1
      if(type.G!="Corretivos de alcalinidade")
        return("")
      type.G.1<-input$res.GA2.4
      data<-data.GA2()[[4]]
      data<-as.matrix(data)
      escolha<-unique(data[as.character(data[,1])==as.character(type.G.1),4])
      selectInput("saida.GA.fisica", "Obtenção:",
                  choices = c(as.character(escolha)))
    })
    
    output$tabGA2.16<-renderUI({
      type.G<-input$res.GA2.1
      if(type.G!="Corretivos de alcalinidade")
        return("")
      type.G.1<-input$res.GA2.4
      data<-data.GA2()[[4]]
      data<-as.matrix(data)
      escolha<-unique(data[as.character(data[,1])==as.character(type.G.1),5])
      selectInput("saida.GA.fisica", "Tolerância  para deficiência:",
                  choices = c(as.character(escolha)))
    })
    
    ### Corretivos de sodicidade
    
    output$tabGA2.17<-renderUI({
      type.G<-input$res.GA2.1
      if(as.character(type.G)!="Corretivos de sodicidade")
        return("")
      data<-data.GA2()[[5]]
      data<-as.matrix(data)
      escolha<-unique(data[,1])
      selectInput("res.GA2.5", "Material corretivo de sodicidade:",
                  choices = c(as.character(escolha)))
    })
    
    output$tabGA2.18<-renderUI({
      type.G<-input$res.GA2.1
      if(type.G!="Corretivos de sodicidade")
        return("")
      type.G.1<-input$res.GA2.5
      data<-data.GA2()[[5]]
      data<-as.matrix(data)
      escolha<-unique(data[as.character(data[,1])==as.character(type.G.1),2])
      selectInput("saida.GA.fisica", "Garantia mínima:",
                  choices = c(as.character(escolha)))
    })
    
    output$tabGA2.19<-renderUI({
      type.G<-input$res.GA2.1
      if(type.G!="Corretivos de sodicidade")
        return("")
      type.G.1<-input$res.GA2.5
      data<-data.GA2()[[5]]
      data<-as.matrix(data)
      escolha<-unique(data[as.character(data[,1])==as.character(type.G.1),3])
      selectInput("saida.GA.fisica", "Características:",
                  choices = c(as.character(escolha)))
    })
    
    output$tabGA2.20<-renderUI({
      type.G<-input$res.GA2.1
      if(type.G!="Corretivos de sodicidade")
        return("")
      type.G.1<-input$res.GA2.5
      data<-data.GA2()[[5]]
      data<-as.matrix(data)
      escolha<-unique(data[as.character(data[,1])==as.character(type.G.1),4])
      selectInput("saida.GA.fisica", "Obtenção:",
                  choices = c(as.character(escolha)))
    })
    
    output$tabGA2.21<-renderUI({
      type.G<-input$res.GA2.1
      if(type.G!="Corretivos de sodicidade")
        return("")
      type.G.1<-input$res.GA2.5
      data<-data.GA2()[[5]]
      data<-as.matrix(data)
      escolha<-unique(data[as.character(data[,1])==as.character(type.G.1),5])
      selectInput("saida.GA.fisica", "Observação:",
                  choices = c(as.character(escolha)))
    })
    
    output$tabGA2.22<-renderUI({
      type.G<-input$res.GA2.1
      if(type.G!="Corretivos de sodicidade")
        return("")
      type.G.1<-input$res.GA2.5
      data<-data.GA2()[[5]]
      data<-as.matrix(data)
      escolha<-unique(data[as.character(data[,1])==as.character(type.G.1),6])
      selectInput("saida.GA.fisica", "Tolerância  para deficiência:",
                  choices = c(as.character(escolha)))
    })
    
    # #####################################################################################################################################################################################
    
    # FR221
    
    output$FR221.01<-renderUI({
      if(input$FR221.0=="Porcentagem de Calagem"){
        numericInput("FR221.1.1","Proportion de Calagem Recomendação (%)",min = 0,value = 25)}})
    output$FR221.02<-renderUI({
      if(input$FR221.0=="Fósforo Remanescente"){
        numericInput("FR221.2.1","Proportion de CaO on calcário (%)",min = 0,value = 18.75,step = 0.01)}})
    output$FR221.03<-renderUI({
      if(input$FR221.0=="Fósforo Remanescente"){
        numericInput("FR221.3.1","Conteúdo de Fósforo Remanescente (mg/L)",min = 0,value = 10)}})
    output$FR221.04<-renderUI({
      if(input$FR221.0=="Saturação por Bases"){
        numericInput("FR221.4.1","Soil saturation value (V1%)",min = 0,value = 40)}})
    output$FR221.05<-renderUI({
      if(input$FR221.0=="Saturação por Bases"){
        numericInput("FR221.5.1","Saturation value to raise (V2%)",min = 0,value = 70)}})
    output$FR221.06<-renderUI({
      if(input$FR221.0=="Saturação por Bases"){
        numericInput("FR221.6.1","Capacidade de Troca Catiônica (CTC) cmolc/dm3",min = 0,value = 6.85,step = 0.01)}})
    
    
    react.FR221 <- reactive({
      
      V2 = input$FR221.5.1 
      V1 = input$FR221.4.1
      ctc = input$FR221.6.1 
      PRNT = input$FR221.1
      f = PRNT/100 
      NC<-input$FR221.2
      rec<-input$FR221.1.1
      EC=input$FR221.3
      SC=input$FR221.4
      Tca<-input$FR221.2.1
      Prem<- input$FR221.3.1
      Teor.arg<- input$FR221.5
      
      if(input$FR221.0=="Porcentagem de Calagem"){
        NG1 =NC * rec/100
        NG = paste( "Gessagem Recomendação: ",round(NG1,3)," (t/ha)",sep = "") 
        QG = paste( "Gessagem Necessidade: ",round(NG1*(EC/20)*(SC/100),5)," (t)",sep = "")
        saida<-as.matrix(rbind(NG,QG))
        colnames(saida)<-"Recomendação"
        rownames(saida)<-c("Recomendação","Necessidade")
        return(saida)}
      
      if(Prem>=0&Prem<4){Prem.max=0;Prem.min=4;Ca.max<-315;Ca.min<-250;NG.max<-1.680;NG.min<-1.333}
      if(Prem>=4&Prem<10){Prem.max=4;Prem.min=10;Ca.max<-250;Ca.min<-190;NG.max<-1.333;NG.min<-1.013} 
      if(Prem>=10&Prem<19){Prem.max=10;Prem.min=19;Ca.max<-190;Ca.min<-135;NG.max<-1.013;NG.min<-0.720}
      if(Prem>=19&Prem<30){Prem.max=19;Prem.min=30;Ca.max<-135;Ca.min<-85;NG.max<-0.720;NG.min<-0.453}
      if(Prem>=30&Prem<44){Prem.max=30;Prem.min=44;Ca.max<-85;Ca.min<-40;NG.max<-0.453;NG.min<-0.213}
      if(Prem>=44&Prem<60){Prem.max=44;Prem.min=60;Ca.max<-40;Ca.min<-0;NG.max<-0.213;NG.min<-0}
      if(input$FR221.0=="Fósforo Remanescente"){
        Ca.est<-Ca.max-((Tca-Prem.min)*(Ca.max-Ca.min)/(Prem.max-Prem.min))
        #NG1<-NG.max-((Tca-Prem.min)*(NG.max-NG.min)/(Prem.max-Prem.min))
        #NG = paste( "Gessagem Recomendação: ",round(NG1,3)," (t/ha)",sep = "")
        NG1=(Ca.est/Tca*10)
        NG = paste( "Gessagem Recomendação: ",round(NG1,3)," (t/ha)",sep = "") 
        QG = paste( "Gessagem Necessidade: ",round(NG1*(EC/20)*(SC/100),5)," (t)",sep = "")
        saida<-as.matrix(rbind(NG,QG))
        colnames(saida)<-"Recomendação"
        rownames(saida)<-c("Recomendação","Necessidade")
        return(saida)}
      
      if(Teor.arg>=0&Teor.arg<15){Teor.arg.max=0;Teor.arg.min=15;NG.max<-0.4;NG.min<-0}
      if(Teor.arg>=15&Teor.arg<35){Teor.arg.max=15;Teor.arg.min=35;NG.max<-0.8;NG.min<-0.4} 
      if(Teor.arg>=35&Teor.arg<60){Teor.arg.max=35;Teor.arg.min=60;NG.max<-1.2;NG.min<-0.8}
      if(Teor.arg>=60&Teor.arg<100){Teor.arg.max=60;Teor.arg.min=100;NG.max<-1.6;NG.min<-1.2}
      
      if(input$FR221.0=="Porcentagem de Argila"){
        NG1<-NG.min+((Teor.arg-Teor.arg.min)*(NG.max-NG.min)/(Teor.arg.max-Teor.arg.min))
        NG = paste( "Gessagem Recomendação: ",round(NG1,3)," (t/ha)",sep = "")
        QG = paste( "Gessagem Necessidade: ",round(NG1*(EC/20)*(SC/100),5)," (t)",sep = "")
        saida<-as.matrix(rbind(NG,QG))
        colnames(saida)<-"Recomendação"
        rownames(saida)<-c("Recomendação","Necessidade")
        return(saida)}
      
      if(input$FR221.0=="Saturação por Bases"){
        NG1<-(V2 - V1)*ctc/PRNT
        NC = paste( "Calagem Necessidade: ",round(NG1,3)," (t/ha)",sep = "")
        QG = paste( "Gessagem Necessidade: ",round(NG1*(EC/20)*(SC/100),5)," (t)",sep = "")
        saida<-as.matrix(rbind(NC,QG))
        colnames(saida)<-"Recomendação"
        rownames(saida)<-c("Recomendação","Necessidade")
        return(saida)}})
    
    output$out.FR221 <- renderPrint({
      return(react.FR221())
    })
    
    # #####################################################################################################################################################################################      
    
  })
