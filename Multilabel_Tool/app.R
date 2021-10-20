rm(list=ls()) 
#options(warn = -1)
options(shiny.autoreload = TRUE)
options(shiny.sanitize.errors = TRUE)
options(shiny.maxRequestSize=300*1024^2)
options(shiny.error = NULL)
#Sys.setlocale(locale = "portuguese")
tags$style(type="text/css",
           ".shiny-output-error { visibility:hidden;}",
           ".shiny-output-error:before {visibility: hidden;}")


source('Bibliotecas.R')

ui <- navbarPage(
    title = "Multi-Label Classifiers Tool",
    theme = shinytheme("united"),
    position = 'static-top',
    
    
    
    ######### INICIO - ABA INICIO #########
    
    tabPanel("Home",
             icon = icon('home'),
             withMathJax(includeMarkdown('Inicio.Rmd'))),
    
    ######### FIM - ABA INICIO #######
    
    navbarMenu(
        title = 'About classifiers',
        icon = icon('code'),
        tabPanel(
            'Model Overview',
            includeMarkdown('Modelos.Rmd')
            
        ),
        tabPanel(
            'Data preprocessing',
            includeMarkdown('preprocess.Rmd')
            
        ),
        tabPanel(
            'Hyperparameters tuning',
            includeMarkdown("Hiperparametros.Rmd")
        ),
        tabPanel(
            'Resampling and validation',
            includeMarkdown("Metodos_Treinamento.Rmd")
        ),
        tabPanel(
            'Model Evaluation & Binary Relevance + Infogain',
            includeMarkdown("Resultados.Rmd")
        )
        
    ),
    
    
    navbarMenu(
        "Models - Classifiers",
        icon = icon('th-large'),
        
        
#####################################################################################################   
############################Inicio tabPanel BR#######################################################
#####################################################################################################
        tabPanel(
            "Build & Prediction",
            tabsetPanel(
                id = "tabs_BR",
                
                tabPanel("Build Model",
                         icon = icon('tasks'),
                         
                         sidebarPanel( width = 3,
                                       fileInput(
                                           "arquivo_BR",
                                           "Input your Dataset:",
                                           accept = c("text/csv",
                                                      "text/comma-separated-values,text/plain",
                                                      ".csv")
                                       ),
                                       materialSwitch("mostrar_BR", "Show Dataset", FALSE),
                                       uiOutput('XBR'),
                                       uiOutput('YBR'),
                                       
                                       hr(),
                                       
                                       selectInput('modelo_escolhido',
                                                   'Choose the model:',
                                                   choices = c('Binary Relevance + Random Forest'='BR','Classifier Chain + Random Forest'='CC','Multivariate Random Forest'='MRF'),
                                                   # 'Kappa'),multilabel.subset01, multilabel.hamloss, multilabel.acc,multilabel.f1, timepredict
                                                   selected ='BR'), 
                                       
                                       
                                       hr(),
                                       
                                       numericInput('treinar_BR', 'Split Rate (Train):', value = 0.8),
                                       
                                       hr(),
                                       
                                       selectInput('parametros',
                                                   'Hyperparameter tuning:',
                                                   choices = c('Ntree'='ntree','Nodesize'='nodesize','Mtry'='mtry','All'='all'),
                                                   # 'Kappa'),multilabel.subset01, multilabel.hamloss, multilabel.acc,multilabel.f1, timepredict
                                                   selected ='ntree'), 
                                                     #c('ntree','nodesize','mtry','all')),
                                       hr(),
                                       radioButtons(
                                         inputId = "opcao_CV_BR",
                                         label = "Cross-validation method:",
                                         choices = c("k-fold CV" = "CV",
                                                     "Repeated k-fold CV" = "RepCV"),
                                         selected = "CV",
                                         inline = T
                                       ),
                                       
                                       hr(),
                                       selectInput(
                                         "numero_folds",
                                         "Number of folds:",
                                         choices = c(3,5,7,10),
                                         selected = 10
                                       ),
                                       
                                       uiOutput('opcoes_treinar_BR'),
                                       
                                       hr(),
                                       selectInput('metrica_BR1',
                                                   'Performance Measure:',
                                                   choices = c('Accuracy'='multilabel.acc','HammingLoss'='multilabel.hamloss','Subset01'='multilabel.subset01','F1'='multilabel.f1'),
                                                   # 'Kappa'),multilabel.subset01, multilabel.hamloss, multilabel.acc,multilabel.f1, timepredict
                                                   selected = 'multilabel.hamloss'),
                                                     #c('multilabel.acc','multilabel.hamloss','multilabel.subset01','multilabel.f1')),
                                       
                                       hr(),
                                       actionButton("computar_BR", "Build Model", class =
                                                        'btn-primary'),
                                       downloadButton("baixar_BR", "Download Model", class = 'btn-primary')
                         ),
                         
                         
                         mainPanel(
                           fluidRow(column(
                             12,
                             h4('Imported Dataset'),
                             h5(
                               'Enable the Show Dataset option to view the first 5 rows of your imported file'
                             ),
                             DT::dataTableOutput("importacao_BR")
                             %>% withSpinner(type = 8, size = 0.5)
                           ))  
                          ,
                          
                           hr(),
                           fluidRow(column(
                            12,
                            h4('Information Gain - BR+IG method'),
                            plotlyOutput("graficos_BR")
                            %>% withSpinner(type = 8, size = 0.5)
                            
                          )),
                          # column(6,
                          #        plotOutput("erro_BR")
                          #        %>% withSpinner(type = 8, size = 0.5))),
                          # 
                           hr(),
                            fluidRow(column(
                            12,
                            h4('Performance measures'),
                            DT::dataTableOutput("estratificacao")
                             %>% withSpinner(type = 8, size = 0.5),
                            uiOutput('mensagem_erro_BR')
                          ))
                         )
                         
                ),
                
                
          
                tabPanel("Make Prediction",
                         icon = icon('cogs'),
                         
                         
                         sidebarPanel(
                           width = 3,
                           fileInput(
                             "arquivo_teste_BR",
                             "Input your Dataset:",
                             accept = c("text/csv",
                                        "text/comma-separated-values,text/plain",
                                        ".csv")
                           ),
                           
                           materialSwitch("mostrar_teste_BR", "Show Dataset", TRUE),
                           checkboxInput('opcao_prever_BR',
                                         'Import a Model?',
                                         value = F),
                           
                           uiOutput('importar_BR1'),
                           
                           actionButton("prever_BR", "Make Prediction", class = 'btn-primary')
                           
                         ),
                         
                         
                         mainPanel(
                           DT::dataTableOutput('importacao_teste_BR'),
                           uiOutput('resultados_obtidos_previsoes_BR'),
                           DT::dataTableOutput('resultado_previsoes_BR'),
                           downloadButton('download_BR', "Download Results", class = 'btn-primary')
                           
                         ))            
               
                
                
                
                
                
                
                 
   )
)),



#####################################################################################################   
#############################Final tabPanel BR#######################################################
#####################################################################################################



# 
# 
# 
# # #####################################################################################################   
# # ############################Inicio tabPanel CC#######################################################
# # #####################################################################################################
# tabPanel(
#   "Classifier Chain + Random Forest",
#   tabsetPanel(
#     id = "tabs_CC",
# 
#     tabPanel("Build Model",
#              icon = icon('tasks'),
# 
#              sidebarPanel( width = 3,
#                            fileInput(
#                              "arquivo_CC",
#                              "Input your Dataset:",
#                              accept = c("text/csv",
#                                         "text/comma-separated-values,text/plain",
#                                         ".csv")
#                            ),
#                            materialSwitch("mostrar_CC", "Show Dataset", FALSE),
#                            uiOutput('XCC'),
#                            uiOutput('YCC'),
# 
#                            hr(),
# 
#                            numericInput('treinar_CC', 'Split Rate (Train):', value = 0.8),
# 
#                            hr(),
# 
#                            selectInput('parametros',
#                                        'Hyperparameter tuning:',
#                                        choices = c('Ntree'='ntree','Nodesize'='nodesize','Mtry'='mtry','All'='all'),
#                                        # 'Kappa'),multilabel.subset01, multilabel.hamloss, multilabel.acc,multilabel.f1, timepredict
#                                        selected ='ntree'),
#                            #c('ntree','nodesize','mtry','all')),
#                            hr(),
#                            radioButtons(
#                              inputId = "opcao_CV_CC",
#                              label = "Cross-validation method:",
#                              choices = c("k-fold CV" = "CV",
#                                          "Repeated k-fold CV" = "RepCV"),
#                              selected = "CV",
#                              inline = T
#                            ),
# 
#                            hr(),
#                            selectInput(
#                              "numero_folds",
#                              "Number of folds:",
#                              choices = c(3,5,7,10),
#                              selected = 10
#                            ),
# 
#                            uiOutput('opcoes_treinar_CC'),
# 
#                            hr(),
#                            selectInput('metrica_CC1',
#                                        'Performance Measure:',
#                                        choices = c('Accuracy'='multilabel.acc','HammingLoss'='multilabel.hamloss','Subset01'='multilabel.subset01','F1'='multilabel.f1'),
#                                        # 'Kappa'),multilabel.subset01, multilabel.hamloss, multilabel.acc,multilabel.f1, timepredict
#                                        selected = 'multilabel.hamloss'),
#                            #c('multilabel.acc','multilabel.hamloss','multilabel.subset01','multilabel.f1')),
# 
#                            hr(),
#                            actionButton("computar_CC", "Build Model", class =
#                                           'btn-primary'),
#                            downloadButton("baixar_CC", "Download Model", class = 'btn-primary')
#              ),
# 
# 
#              mainPanel(
#                fluidRow(column(
#                  12,
#                  h4('Imported Dataset'),
#                  h5(
#                    'Enable the Show Dataset option to view the first 5 rows of your imported file'
#                  ),
#                  DT::dataTableOutput("importacao_CC")
#                  %>% withSpinner(type = 8, size = 0.5)
#                ))
#                ,
# 
#                hr(),
#                fluidRow(column(
#                  12,
#                  h4('Information Gain - BR+IG method'),
#                  plotlyOutput("graficos_CC")
#                  %>% withSpinner(type = 8, size = 0.5)
# 
#                )),
#                # column(6,
#                #        plotOutput("erro_CC")
#                #        %>% withSpinner(type = 8, size = 0.5))),
#                #
#                hr(),
#                fluidRow(column(
#                  12,
#                  h4('Performance measures'),
#                  DT::dataTableOutput("estratificacao")
#                  %>% withSpinner(type = 8, size = 0.5),
#                  uiOutput('mensagem_erro_CC')
#                ))
#              )
# 
#     ),
# 
# 
# 
#     tabPanel("Make Prediction",
#              icon = icon('cogs'),
# 
# 
#              sidebarPanel(
#                width = 3,
#                fileInput(
#                  "arquivo_teste_CC",
#                  "Input your Dataset:",
#                  accept = c("text/csv",
#                             "text/comma-separated-values,text/plain",
#                             ".csv")
#                ),
# 
#                materialSwitch("mostrar_teste_CC", "Show Dataset", TRUE),
#                checkboxInput('opcao_prever_CC',
#                              'Import a Model?',
#                              value = F),
# 
#                uiOutput('importar_CC1'),
# 
#                actionButton("prever_CC", "Make Prediction", class = 'btn-primary')
# 
#              ),
# 
# 
#              mainPanel(
#                DT::dataTableOutput('importacao_teste_CC'),
#                uiOutput('resultados_obtidos_previsoes_CC'),
#                DT::dataTableOutput('resultado_previsoes_CC'),
#                downloadButton('download_CC', "Download Results", class = 'btn-primary')
# 
#              ))
# 
# 
# 
# 
# 
# 
# 
# 
#   )
# )),
# 
# 
# 
# #####################################################################################################
# #############################Final tabPanel CC#######################################################
# #####################################################################################################





























tabPanel('Information',
         icon = icon('info'),
         includeMarkdown('information.Rmd'))

)





server <- function(input, output, session) {
    

  
  
  #####################################################################################################   
  ##############################Inicio Server BR#######################################################
  #####################################################################################################  
  
     
     dados_BR1 <- reactive({
        inFile <- input$arquivo_BR
        
        if (is.null(inFile))
            return(NULL)
        
        a<-NULL
        a<-read.csv(inFile$datapath, header = T, sep = ";")
        if (is.null(a)==TRUE){
          a<-read.csv(inFile$datapath, header = T, sep = ",")
        }
        a
        
    })
    
    
    dados_BR <- reactive({
        req(dados_BR1())
        dados_BR1()[complete.cases(dados_BR1()),]
    })
    
    output$importacao_BR <- DT::renderDataTable({
        if (input$mostrar_BR == TRUE) {
            DT::datatable(dados_BR(), options = list(pageLength =5, scrollX = TRUE,  searching = FALSE))
            
        }
    })
    
    
    output$XBR <- renderUI({
        nomes <- names(dados_BR())
        pickerInput(
            'XBR2',
            'Independent Variables (X)',
            nomes,
            options = list(
                `actions-box` = TRUE,
                size = 10,
                `selected-text-format` = "count > 3"
            ),
            multiple = TRUE
        )
    })
    
    output$YBR <- renderUI({
        nomes <- names(dados_BR())
        #selectInput('YBR2', 'Variável Dependente (y) - Alvo:', nomes)
        pickerInput(
            'YBR2',
            'Dependent Variables (Y)',
            nomes,
            options = list(
                `actions-box` = TRUE,
                size = 10,
                `selected-text-format` = "count > 3"
            ),
            multiple = TRUE
        )
        
    })
    
    
    
    
    dados_BR_folds <- reactive({
      req(dados_BR())
       dados_BR() %>%
         mutate(n = row_number())
    }) 
    
    
    
    split_BR <- eventReactive(input$computar_BR,
                              {  req(input$YBR2,dados_BR_folds())
                                   set.seed(123)
                                   y <- input$YBR2
                                  # #createDataPartition(dados_BR()[, y], p = input$treinar_BR, list =
                                  # #                       FALSE)
                                  #  dados <- dados_BR() %>%
                                  #      mutate(n = row_number()) 
                                  # # #%>% #create row number if you dont have one
                                   #   select(n, everything()) # put 'n' at the front of the dataset
                                   #dados_BR()[, colnames(dados_BR()) %in% y]
                                   dados<-as.data.frame(stratified(dados_BR_folds(),y, size = input$treinar_BR))
                                  #dados =stratified(dados,y, size = 0.8)
                                  dados[, c("n")]
                                  })
    
    
    
    treino_BR1 <- eventReactive(input$computar_BR,
                                 {req(dados_BR_folds(),split_BR())
                                     dados_BR_folds()[split_BR(), !(colnames(dados_BR_folds()) %in% c("n")) ]

                               })

    teste_BR1 <- eventReactive(input$computar_BR,
                               {req(dados_BR_folds(),split_BR())
                                dados_BR_folds()[-split_BR(), !(colnames(dados_BR_folds()) %in% c("n")) ]
      
                               })

    treino_BR <- reactive({
        temp <- treino_BR1()
        temp[,input$YBR2] <- mutate_if(temp[,input$YBR2], is.numeric, ~ as.logical((.x)))
        temp[,input$XBR2] <- mutate_if(temp[,input$XBR2], is.character, ~ as.factor((.x)))
        temp
    })
    
    teste_BR <- reactive({
        req(teste_BR1())
        temp <- teste_BR1()
        temp[,input$YBR2] <- mutate_if(temp[,input$YBR2], is.numeric, ~ as.logical((.x)))
        temp[,input$XBR2] <- mutate_if(temp[,input$XBR2], is.character, ~ as.factor((.x)))
        temp
    })
    
    
    metrica_BR <- reactive({
      if(input$metrica_BR1=='multilabel.acc'){
        multilabel.acc
      }else{
       if(input$metrica_BR1=='multilabel.hamloss'){   
          multilabel.hamloss
       }else{
        if(input$metrica_BR1=='multilabel.subset01'){
           multilabel.subset01
        }else{
          multilabel.f1
        } 
      } 
    } 
      
    })
    
    hyperparameters_BR<-reactive({
      if(input$parametros=='ntree'){
        makeParamSet(makeIntegerParam("ntree",lower = 50, upper = 1000))
      }else{
        if(input$parametros=='mtry'){   
          makeParamSet(makeIntegerParam("mtry", lower = 3, upper = 30))
        }else{
          if(input$parametros=='nodesize'){
            makeParamSet(makeIntegerParam("nodesize", lower = 10, upper = 50))
          }else{
            makeParamSet(makeIntegerParam("ntree",lower = 50, upper = 500), makeIntegerParam("mtry", lower = 3, upper = 10), makeIntegerParam("nodesize", lower = 10, upper = 50))
          } 
        } 
      } 
    })


    
    output$opcoes_treinar_BR <- renderUI({
      req(input$opcao_CV_BR)
      if (input$opcao_CV_BR == "RepCV") {
        selectInput(
          "repeticoes_BR",
          "Times",
          choices = c(3, 5, 7),
          selected = 3
        )
      }
      
    })
    
    
    cross_validation_BR <- reactive({
      if(input$opcao_CV_BR=='CV'){
        makeResampleDesc("CV",iters = as.integer(input$numero_folds) )
      }else{
        makeResampleDesc("RepCV",reps= as.integer(input$repeticoes_BR), folds = as.integer(input$numero_folds) )
      }  
    })    
        
        

    fit_BR <- eventReactive(input$computar_BR, {

        tryCatch(
            withProgress(message = 'Building Model, please wait ...', value =1, {

                    labels<-input$YBR2
                    base.task <- makeMultilabelTask(id = "multi", data =treino_BR(), target= labels)
                    
                    
                    if(input$modelo_escolhido=='BR'){ 
                     lrn.br  <-  makeLearner ("classif.randomForestSRC" , predict.type = "response", par.vals = list(ntree = 200, mtry = 3))
                     lrn.br$par.vals = list(importance = TRUE)
                     lrn.br<-makeDownsampleWrapper(lrn.br, dw.stratify = TRUE)
                     lrn.br <- makeMultilabelBinaryRelevanceWrapper(lrn.br)
                    
                     lrn.br_param <- hyperparameters_BR()
                     rancontrol <- makeTuneControlRandom(maxit = 5L)
                     set_cv <- cross_validation_BR()
                     lrn.br_tune <- tuneParams(learner = lrn.br, resampling = set_cv,  task =base.task, par.set = lrn.br_param, control = rancontrol, measures = list(metrica_BR()))
                     lrn.br.tree <- setHyperPars(lrn.br, par.vals = lrn.br_tune$x)
                    }else{
                      if(input$modelo_escolhido=='CC'){
                      lrn.br  <-  makeLearner ("classif.randomForestSRC" , predict.type = "response", par.vals = list(ntree = 200, mtry = 3))
                      lrn.br$par.vals = list(importance = TRUE)
                      lrn.br<-makeDownsampleWrapper(lrn.br, dw.stratify = TRUE)
                      lrn.br <- makeMultilabelClassifierChainsWrapper(lrn.br)
                      
                      lrn.br_param <- hyperparameters_BR()
                      rancontrol <- makeTuneControlRandom(maxit = 5L)
                      set_cv <- cross_validation_BR()
                      lrn.br_tune <- tuneParams(learner = lrn.br, resampling = set_cv,  task =base.task, par.set = lrn.br_param, control = rancontrol, measures = list(metrica_BR()))
                      lrn.br.tree <- setHyperPars(lrn.br, par.vals = lrn.br_tune$x)  
                      }else{
                        lrn.br  <-  makeLearner ("multilabel.randomForestSRC" , predict.type = "response", par.vals = list(ntree = 200, mtry = 3))
                        lrn.br$par.vals = list(importance = TRUE)
                        
                        lrn.br_param <- hyperparameters_BR()
                        rancontrol <- makeTuneControlRandom(maxit = 5L)
                        set_cv <- cross_validation_BR()
                        lrn.br_tune <- tuneParams(learner = lrn.br, resampling = set_cv,  task =base.task, par.set = lrn.br_param, control = rancontrol, measures = list(metrica_BR()))
                        lrn.br.tree <- setHyperPars(lrn.br, par.vals = lrn.br_tune$x) 
                      } 
                    }

                   mod<-mlr:: train(lrn.br.tree, base.task)
                   mod
                   #pred<-predict(mod,newdata=teste_BR(),type=prob)
                   #perf=as.data.frame(performance(pred, measures = list(multilabel.subset01, multilabel.hamloss, multilabel.acc,multilabel.f1, timepredict))
                   #pred[["data"]]
            }), error= function(e){return(NA)})

    })
    
    
    
    info_BR <- reactive({
      req(input$XBR2,input$YBR2,treino_BR())
      x<-input$XBR2
      y<-input$YBR2
      
      if (length(x) > 1) {
        info_gain_BR<-rep(0,length(x))
        
        for(i in 1:length(y)){
          dependente<-y[i]
          independente<-x
          formula_BR <-reformulate(independente, response = dependente)
          info<-information_gain(formula_BR, data = treino_BR())
          info_gain_BR<-info_gain_BR+info[,2]
        }
        info_gain_BR=info_gain_BR/length(x)
        
        data.frame(features=info[,1],info_gain=info_gain_BR)
      }
    })
    

    output$graficos_BR <- renderPlotly({
      
       info_BR2<-arrange(info_BR(), by=info_gain)
       
       ggplot(info_BR2, aes(x = features, y = info_gain , fill = features  )) +
         geom_col(position = "stack")

    })
    
    
    
    
    
    
    performance_BR <- eventReactive(input$computar_BR, {
      
      pred<-predict(fit_BR(),newdata=teste_BR(),type=prob)
      perf<-as.data.frame(performance(pred,measures = list(multilabel.subset01, multilabel.hamloss, multilabel.acc,multilabel.f1, timepredict)))
      
      colnames(perf)<-"value"
      round(perf,digits = 3)
                         
      })   
    
    output$estratificacao <- DT::renderDataTable({ 
      req(performance_BR())
       performance_BR()
    })
      
    
    
    output$baixar_BR <- downloadHandler(

        filename = function() {
          "multilabel_model.rds"
        },
        content = function(fname) {
          withProgress(message = 'Downloading Model, please wait ...', value=1, {
            saveRDS(fit_BR(), fname)
          })
        }
      )
    
    
    
    ########################################
    ###Comeca a aba realizar predicoes######
    ########################################

    dados_teste_BR2 <- reactive({
      inFile <- input$arquivo_teste_BR

      if (is.null(inFile))
        return(NULL)

      #read.csv(inFile$datapath, header = T,sep=";",dec=".")
      a<-NULL
      a<-read.csv(inFile$datapath, header = T, sep = ";")
      if (is.null(a)==TRUE){
        a<-read.csv(inFile$datapath, header = T, sep = ",")
      }
      a
    })

    
    dados_teste_BR1 <- reactive({
      req(dados_teste_BR2())
      dados_teste_BR2()[complete.cases(dados_teste_BR2()),]
    })
    
    
    
    output$importacao_teste_BR <- DT::renderDataTable(
      if (input$mostrar_teste_BR== TRUE) {
        DT::datatable(dados_teste_BR1(), options = list(pageLength =5, scrollX = TRUE,  searching = FALSE))

      })
    

    
    
    dados_teste_BR <- reactive({
      req(dados_teste_BR1())
      temp <- dados_teste_BR1()
      temp <- mutate_if(temp, is.character, ~ as.factor((.x)))
      temp
    })
    
    
    output$importar_BR1 <- renderUI({
      if(input$opcao_prever_BR == TRUE)
        tagList(
          fileInput(
            "importar_BR",
            "Input your modelo:",
            accept = c(".rds")
          ),
          helpText("Make sure that your new data has the columns used to generate the imported model")
        )
      
    })

    
    fit_importado_BR <- reactive({
      inFile <- input$importar_BR
      
      if (is.null(inFile))
        return(NULL)
      
      readRDS(inFile$datapath)
      
    })
    
    
    
    
    
   
    
     previsao_BR1 <- reactive({
      req(fit_importado_BR())
       
       tryCatch(
         previsao<-predict(object=fit_importado_BR(), newdata = dados_teste_BR())
         ,error = function(e) { return(rep('NA', nrow(dados_teste_BR()))) }
       )

    })
     
     
     
    previsao_BR <- reactive({
      req(previsao_BR1())
      if (is.data.frame(previsao_BR1()[[2]])==TRUE){
       dados<-previsao_BR1()[[2]]
       dados<-ifelse(dados==FALSE,0,1)
       dados
      }else{
        previsao_BR1()
      }
    })


    tabela_previsoes_BR <- eventReactive(input$prever_BR, {
      if(input$opcao_prever_BR == TRUE){
          Resultado_Modelo <- previsao_BR()
          cbind(dados_teste_BR(), Resultado_Modelo)
      }else{
         pred<- predict(object=fit_BR(), newdata=dados_teste_BR())
         Resultado_Modelo<-ifelse(pred[[2]]==FALSE,0,1)
         cbind(dados_teste_BR(), Resultado_Modelo)
      }
    })
    # 
    # 
    #
    
    
    

  output$resultados_obtidos_previsoes_BR <- renderUI({
      req(tabela_previsoes_BR())
      h3("Prediction Results")
      
    })
    
    output$resultado_previsoes_BR <- DT::renderDataTable(
      DT::datatable(tabela_previsoes_BR(), options = list(pageLength =5, scrollX = TRUE,  searching = FALSE))
      
    )
    
    
    
    output$download_BR <- downloadHandler(
      filename = function() {
        "resultados_previsoes.xlsx"
      },
      content = function(fname) {
        write.xlsx(tabela_previsoes_BR(), fname)
      }
    )
    
    
    output$mensagem_erro_BR <- renderUI({
          if(is.na(fit_BR()))
            includeMarkdown('Mensagem_Erro.Rmd')
        })

                            #           
                        


#####################################################################################################   
###############################Final Server BR#######################################################
#####################################################################################################


    
    
# 
# #####################################################################################################
# ##############################Inicio Server CC#######################################################
# #####################################################################################################
# 
# 
#     dados_CC1 <- reactive({
#       inFile <- input$arquivo_CC
# 
#       if (is.null(inFile))
#         return(NULL)
# 
#       a<-NULL
#       a<-read.csv(inFile$datapath, header = T, sep = ";")
#       if (is.null(a)==TRUE){
#         a<-read.csv(inFile$datapath, header = T, sep = ",")
#       }
#       a
# 
#     })
# 
# 
#     dados_CC <- reactive({
#       req(dados_CC1())
#       dados_CC1()[complete.cases(dados_CC1()),]
#     })
# 
#     output$importacao_CC <- DT::renderDataTable({
#       if (input$mostrar_CC == TRUE) {
#         DT::datatable(dados_CC(), options = list(pageLength =5, scrollX = TRUE,  searching = FALSE))
# 
#       }
#     })
# 
# 
#     output$XCC <- renderUI({
#       nomes <- names(dados_CC())
#       pickerInput(
#         'XCC2',
#         'Independent Variables (X)',
#         nomes,
#         options = list(
#           `actions-box` = TRUE,
#           size = 10,
#           `selected-text-format` = "count > 3"
#         ),
#         multiple = TRUE
#       )
#     })
# 
#     output$YCC <- renderUI({
#       nomes <- names(dados_CC())
#       #selectInput('YCC2', 'Variável Dependente (y) - Alvo:', nomes)
#       pickerInput(
#         'YCC2',
#         'Dependent Variables (Y)',
#         nomes,
#         options = list(
#           `actions-box` = TRUE,
#           size = 10,
#           `selected-text-format` = "count > 3"
#         ),
#         multiple = TRUE
#       )
# 
#     })
# 
# 
# 
# 
#     dados_CC_folds <- reactive({
#       req(dados_CC())
#       dados_CC() %>%
#         mutate(n = row_number())
#     })
# 
# 
# 
#     split_CC <- eventReactive(input$computar_CC,
#                               {  req(input$YCC2,dados_CC_folds())
#                                 set.seed(123)
#                                 y <- input$YCC2
#                                 # #createDataPartition(dados_CC()[, y], p = input$treinar_CC, list =
#                                 # #                       FALSE)
#                                 #  dados <- dados_CC() %>%
#                                 #      mutate(n = row_number())
#                                 # # #%>% #create row number if you dont have one
#                                 #   select(n, everything()) # put 'n' at the front of the dataset
#                                 #dados_CC()[, colnames(dados_CC()) %in% y]
#                                 dados<-as.data.frame(stratified(dados_CC_folds(),y, size = input$treinar_CC))
#                                 #dados =stratified(dados,y, size = 0.8)
#                                 dados[, c("n")]
#                               })
# 
# 
# 
#     treino_CC1 <- eventReactive(input$computar_CC,
#                                 {req(dados_CC_folds(),split_CC())
#                                   dados_CC_folds()[split_CC(), !(colnames(dados_CC_folds()) %in% c("n")) ]
# 
#                                 })
# 
#     teste_CC1 <- eventReactive(input$computar_CC,
#                                {req(dados_CC_folds(),split_CC())
#                                  dados_CC_folds()[-split_CC(), !(colnames(dados_CC_folds()) %in% c("n")) ]
# 
#                                })
# 
#     treino_CC <- reactive({
#       temp <- treino_CC1()
#       temp[,input$YCC2] <- mutate_if(temp[,input$YCC2], is.numeric, ~ as.logical((.x)))
#       temp[,input$XCC2] <- mutate_if(temp[,input$XCC2], is.character, ~ as.factor((.x)))
#       temp
#     })
# 
#     teste_CC <- reactive({
#       req(teste_CC1())
#       temp <- teste_CC1()
#       temp[,input$YCC2] <- mutate_if(temp[,input$YCC2], is.numeric, ~ as.logical((.x)))
#       temp[,input$XCC2] <- mutate_if(temp[,input$XCC2], is.character, ~ as.factor((.x)))
#       temp
#     })
# 
# 
#     metrica_CC <- reactive({
#       if(input$metrica_CC1=='multilabel.acc'){
#         multilabel.acc
#       }else{
#         if(input$metrica_CC1=='multilabel.hamloss'){
#           multilabel.hamloss
#         }else{
#           if(input$metrica_CC1=='multilabel.subset01'){
#             multilabel.subset01
#           }else{
#             multilabel.f1
#           }
#         }
#       }
# 
#     })
# 
#     hyperparameters_CC<-reactive({
#       if(input$parametros=='ntree'){
#         makeParamSet(makeIntegerParam("ntree",lower = 50, upper = 1000))
#       }else{
#         if(input$parametros=='mtry'){
#           makeParamSet(makeIntegerParam("mtry", lower = 3, upper = 30))
#         }else{
#           if(input$parametros=='nodesize'){
#             makeParamSet(makeIntegerParam("nodesize", lower = 10, upper = 50))
#           }else{
#             makeParamSet(makeIntegerParam("ntree",lower = 50, upper = 500), makeIntegerParam("mtry", lower = 3, upper = 10), makeIntegerParam("nodesize", lower = 10, upper = 50))
#           }
#         }
#       }
#     })
# 
# 
# 
#     output$opcoes_treinar_CC <- renderUI({
#       req(input$opcao_CV_CC)
#       if (input$opcao_CV_CC == "RepCV") {
#         selectInput(
#           "repeticoes_CC",
#           "Times",
#           choices = c(3, 5, 7),
#           selected = 3
#         )
#       }
# 
#     })
# 
# 
#     cross_validation_CC <- reactive({
#       if(input$opcao_CV_CC=='CV'){
#         makeResampleDesc("CV",iters = as.integer(input$numero_folds) )
#       }else{
#         makeResampleDesc("RepCV",reps= as.integer(input$repeticoes_CC), folds = as.integer(input$numero_folds) )
#       }
#     })
# 
# 
# 
#     fit_CC <- eventReactive(input$computar_CC, {
# 
#       tryCatch(
#         withProgress(message = 'Building Model, please wait ...', value =1, {
# 
#           labels<-input$YCC2
#           base.task <- makeMultilabelTask(id = "multi", data =treino_CC(), target= labels)
# 
#           lrn.CC  <-  makeLearner ("classif.randomForestSRC" , predict.type = "response", par.vals = list(ntree = 200, mtry = 3))
#           lrn.CC$par.vals = list(importance = TRUE)
#           lrn.CC<-makeDownsampleWrapper(lrn.CC, dw.stratify = TRUE)
#           lrn.CC <- makeMultilabelClassifierChainsWrapper(lrn.CC)
# 
#           lrn.CC_param <- hyperparameters_CC()
#           rancontrol <- makeTuneControlRandom(maxit = 5L)
#           set_cv <- cross_validation_CC()
#           lrn.CC_tune <- tuneParams(learner = lrn.CC, resampling = set_cv,  task =base.task, par.set = lrn.CC_param, control = rancontrol, measures = list(metrica_CC()))
#           lrn.CC.tree <- setHyperPars(lrn.CC, par.vals = lrn.CC_tune$x)
# 
# 
# 
#           mod<-mlr:: train(lrn.CC.tree, base.task)
#           mod
#           #pred<-predict(mod,newdata=teste_CC(),type=prob)
#           #perf=as.data.frame(performance(pred, measures = list(multilabel.subset01, multilabel.hamloss, multilabel.acc,multilabel.f1, timepredict))
#           #pred[["data"]]
#         }), error= function(e){return(NA)})
# 
#     })
# 
# 
# 
#     info_CC <- reactive({
#       req(input$XCC2,input$YCC2,treino_CC())
#       x<-input$XCC2
#       y<-input$YCC2
# 
#       if (length(x) > 1) {
#         info_gain_CC<-rep(0,length(x))
# 
#         for(i in 1:length(y)){
#           dependente<-y[i]
#           independente<-x
#           formula_CC <-reformulate(independente, response = dependente)
#           info<-information_gain(formula_CC, data = treino_CC())
#           info_gain_CC<-info_gain_CC+info[,2]
#         }
#         info_gain_CC=info_gain_CC/length(x)
# 
#         data.frame(features=info[,1],info_gain=info_gain_CC)
#       }
#     })
# 
# 
#     output$graficos_CC <- renderPlotly({
# 
#       info_CC2<-arrange(info_CC(), by=info_gain)
# 
#       ggplot(info_CC2, aes(x = features, y = info_gain , fill = features  )) +
#         geom_col(position = "stack")
# 
#     })
# 
# 
# 
# 
# 
# 
#     performance_CC <- eventReactive(input$computar_CC, {
# 
#       pred<-predict(fit_CC(),newdata=teste_CC(),type=prob)
#       perf<-as.data.frame(performance(pred,measures = list(multilabel.subset01, multilabel.hamloss, multilabel.acc,multilabel.f1, timepredict)))
# 
#       colnames(perf)<-"value"
#       round(perf,digits = 3)
# 
#     })
# 
#     output$estratificacao <- DT::renderDataTable({
#       req(performance_CC())
#       performance_CC()
#     })
# 
# 
# 
#     output$baixar_CC <- downloadHandler(
# 
#       filename = function() {
#         "modelo_CC.rds"
#       },
#       content = function(fname) {
#         withProgress(message = 'Downloading Model, please wait ...', value=1, {
#           saveRDS(fit_CC(), fname)
#         })
#       }
#     )
# 
# 
# 
#     ########################################
#     ###Comeca a aba realizar predicoes######
#     ########################################
# 
#     dados_teste_CC2 <- reactive({
#       inFile <- input$arquivo_teste_CC
# 
#       if (is.null(inFile))
#         return(NULL)
# 
#       #read.csv(inFile$datapath, header = T,sep=";",dec=".")
#       a<-NULL
#       a<-read.csv(inFile$datapath, header = T, sep = ";")
#       if (is.null(a)==TRUE){
#         a<-read.csv(inFile$datapath, header = T, sep = ",")
#       }
#       a
#     })
# 
# 
#     dados_teste_CC1 <- reactive({
#       req(dados_teste_CC2())
#       dados_teste_CC2()[complete.cases(dados_teste_CC2()),]
#     })
# 
# 
# 
#     output$importacao_teste_CC <- DT::renderDataTable(
#       if (input$mostrar_teste_CC== TRUE) {
#         DT::datatable(dados_teste_CC1(), options = list(pageLength =5, scrollX = TRUE,  searching = FALSE))
# 
#       })
# 
# 
# 
# 
#     dados_teste_CC <- reactive({
#       req(dados_teste_CC1())
#       temp <- dados_teste_CC1()
#       temp <- mutate_if(temp, is.character, ~ as.factor((.x)))
#       temp
#     })
# 
# 
#     output$importar_CC1 <- renderUI({
#       if(input$opcao_prever_CC == TRUE)
#         tagList(
#           fileInput(
#             "importar_CC",
#             "Input your modelo:",
#             accept = c(".rds")
#           ),
#           helpText("Make sure that your new data has the columns used to generate the imported model")
#         )
# 
#     })
# 
# 
#     fit_importado_CC <- reactive({
#       inFile <- input$importar_CC
# 
#       if (is.null(inFile))
#         return(NULL)
# 
#       readRDS(inFile$datapath)
# 
#     })
# 
# 
# 
# 
# 
# 
# 
#     previsao_CC1 <- reactive({
#       req(fit_importado_CC())
# 
#       tryCatch(
#         previsao<-predict(object=fit_importado_CC(), newdata = dados_teste_CC())
#         ,error = function(e) { return(rep('NA', nrow(dados_teste_CC()))) }
#       )
# 
#     })
# 
# 
# 
#     previsao_CC <- reactive({
#       req(previsao_CC1())
#       if (is.data.frame(previsao_CC1()[[2]])==TRUE){
#         dados<-previsao_CC1()[[2]]
#         dados<-ifelse(dados==FALSE,0,1)
#         dados
#       }else{
#         previsao_CC1()
#       }
#     })
# 
# 
#     tabela_previsoes_CC <- eventReactive(input$prever_CC, {
#       if(input$opcao_prever_CC == TRUE){
#         Resultado_Modelo <- previsao_CC()
#         cbind(dados_teste_CC(), Resultado_Modelo)
#       }else{
#         pred<- predict(object=fit_CC(), newdata=dados_teste_CC())
#         Resultado_Modelo<-ifelse(pred[[2]]==FALSE,0,1)
#         cbind(dados_teste_CC(), Resultado_Modelo)
#       }
#     })
#     #
#     #
#     #
# 
# 
# 
# 
#     output$resultados_obtidos_previsoes_CC <- renderUI({
#       req(tabela_previsoes_CC())
#       h3("Prediction Results")
# 
#     })
# 
#     output$resultado_previsoes_CC <- DT::renderDataTable(
#       DT::datatable(tabela_previsoes_CC(), options = list(pageLength =5, scrollX = TRUE,  searching = FALSE))
# 
#     )
# 
# 
# 
#     output$download_CC <- downloadHandler(
#       filename = function() {
#         "resultados_previsoes.xlsx"
#       },
#       content = function(fname) {
#         write.xlsx(tabela_previsoes_CC(), fname)
#       }
#     )
# 
# 
#     output$mensagem_erro_CC <- renderUI({
#       if(is.na(fit_CC()))
#         includeMarkdown('Mensagem_Erro.Rmd')
#     })
# 
#     #
# 
# 
# 
# #####################################################################################################
# ###############################Final Server CC#######################################################
# #####################################################################################################


} 

shinyApp(ui = ui, server = server)


#incluir BR information gain
#fazer dowload exportar modelo
#fazer previsoes
