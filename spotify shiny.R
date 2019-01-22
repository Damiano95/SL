library(plotly)
library(ggplot2)
library(corrplot)
library("caret")
library(DT)
library(Metrics)
library(shiny)
library("shinythemes")

audio <- read.csv("https://Damiano95.github.io/SL/audio_features.txt", sep="")
numeric = audio[,sapply(audio, is.numeric)]
integer = audio[,sapply(audio, is.integer)]
var_num = cbind(numeric, integer)

lasso <- read.table("https://Damiano95.github.io/SL/lasso.txt", sep="")
randomf <- read.table("https://Damiano95.github.io/SL/rf.txt", sep="")
xgb <- read.table("https://Damiano95.github.io/SL/xgb.txt", sep="")
glm <- read.table("https://Damiano95.github.io/SL/glm.txt", sep="")

ui <- fluidPage(theme = shinytheme("cerulean"),
  navbarPage("", # Con navbarPage posso aggiungere diverse pagine, in modo da dividere per argomenti
                 # all'interno di ogni pagina poi potrò inserire diverse schede per ordinare tutto il lavoro
    tabPanel("1-Introduzione",        
             tabsetPanel(        
             # Prima pagina
    tabPanel("1.a Titolo", mainPanel(div(style="text-align:center",h1("Guess which are my songs")))),
    tabPanel("1.b Obiettivi",
             mainPanel(div(style="text-align:center",h1("Obiettivi:"),br(),br(),br(),br(),br(), h4("1) Eseguire un'analisi utilizzando i dati di spotify per riconoscere quali canzoni sono di gradimento dell'uno o dell'altro"),h4("2) Creare un'app shiny in cui implementare l'analisi per renderla veloce e interattiva", h4("3) Fornire un esempio di utilizzo dell'app")))
             )),
    tabPanel("1.c Descrizione delle variabili",
             mainPanel(h1("Variabili dataset"),
                       h3("Track_name"),
                       print("Nome della traccia"),
                       h3("Artist_name"),
                       print("Nome dell'artista"),
                       h3("Album_name"),
                       print("Nome dell'album"),
                       h3("Danceability"),
                       print("Quanto una traccia è adeguata per essere ballata, basandosi su una combinazione di elementi musicali che includono tempo, stabilità del ritmo, potenza dei beat e regolarità complessiva. 0 non ballabile, mentre 1 è il valore massimo"),
                       h3("Energy"),
                       print("Misura compresa tra 0 e 1 che rappresenta la percezione di intensità e azione. Tipicamente, le canzoni energiche sono veloci, forti e rumorose"),
                       h3("Key"),
                       print("La tonalità complessiva stimata del brano"),
                       h3("Loudness"),
                       print("La forza complessiva di una traccia in decibel. I valori di rumorosità sono calcolati in media lungo l’intera traccia e sono utili per comparare la rumorosità di canzoni. La variabile assume valori tra -60 e 0 db"),
                       h3("Mode"),
                       print("Modalità (maggiore o minore ) della traccia, il tipo di scala dal quale il contenuto melodico è derivato. Maggiore è rappresentato da 1 e minore da 0"),
                       h3("Speechiness"),
                       print("Determina la presenza di parole nella traccia. Più una traccia è 'parlata', più il valore si avvicina a 1. Un valore oltre 0.66 descrive tracce interamente parlate, come audiolibri. Tracce con un valore compreso tra 0.33 e 0.66 sono riferiti a canzoni composte sia da musica che da testo."),
                       h3("Acousticness"),
                       print("Una misura di confidenza compreso tra 0 e 1, dove 1 indica una canzone è acustica con un alto livello di confidenza"),
                       h3("Instrumentalness"),
                       print("Ci dice se una traccia non contiene voci. Suoni come  “ooh” e “aah” sono trattati come strumentali in questo contesto. Rap e canzoni parlate sono chiaramente 'vocali'. Più vicino il valore è a 1, più verosimilmente la traccia non conterrà voci. Valori intorno 0.5 si suppone rappresentino brani strumentali, ma la probabilità di esserlo aumenta quando il valore si avvicina a 1"),
                       h3("Liveness"),
                       print("Indaga sulla presenza di pubblico durante la registrazione. Alti valori di liveness rappresentano alta probabilità che la tracccia sia live"),
                       h3("Valence"),
                       print("Una misura da 0 a 1 che descrive la positività trasmessa da una traccia. Brani con un alta valenza sembrano più positivi, mentre pezzi con valori più bassi sembrano più negativi"),
                       h3("Tempo"),
                       print("Il tempo stimato complessivo di una traccia calcolato in beat per minuto (BPM). In musica, il tempo è la velocità o il ritmo di un certo pezzo e deriva direttamente dalla durata medie dei beat"),
                       h3("Track_uri"),
                       print("Un HTTP URL per accedere alla analisi completa della traccia"),
                       h3("Duration_ms"),
                       print("La durata della traccia in millisecondi"),
                       h3("Time_signature"),
                       print("E' una stima della metrica generale della traccia. La metrica è la notazione convenzionale per specificare il numero di beat per battuta"),
                       h3("Key_mode"),
                       print("Una stima complessiva della tonalità del brano e della sua modalità"),
                       h3("Class"),
                       print("Variabile di interesse: 1 indica che la canzone è stata scelta da Damiano, 0 invece da Giacomo")
      )
    ))),
    tabPanel("2-Divisione Dataset e Analisi Esplorativa",
             
             # Seconda pagina
             tabsetPanel(
               
               # Primo tab della seconda pagina
               tabPanel("2.a Creazione Dataset di lavoro",
                       sidebarPanel(
                       h3("Training e Test set"),
                       numericInput("seme", "Inserisci il numero del seme", value=123,min = 0),
                       sliderInput("pertest", "Percentuale di osservazioni da inserire nel test set", min=1, max=100, value=30),
                       radioButtons("elVar", "Vuoi escludere qualche variabile dal dataset?", choices = c("Si","No"), selected = "No"),
                       conditionalPanel("input.elVar == 'Si'", checkboxGroupInput("VarDaEscl","Quali variabili vuoi escludere?", choices = names(audio)[-19], selected = names(audio)[15])),
                       actionButton("go", "Crea"),
                       hr()
                          ),
             mainPanel(# Conditional panel mi permette di inserire qualcosa al verificarsi di un particolare evento
               # in questo caso le scritte compariranno solo se avr? cliccato almeno una volta su "crea"
               conditionalPanel("input.go == 0", "Dividere il dataset tra train e test prima di proseguire. L'analisi proposta è svolta utilizzando come seme '123', il 30% delle osservazioni del dataset come test set ed escludendo la variabile 'track_uri'."),
               conditionalPanel("input.go != 0",
               h3("Dimensioni Test Set"),
               verbatimTextOutput("dimTest"),
               h3("Dimensioni Training Set"),
               verbatimTextOutput("dimTraining"),
               hr(),
               h3("Distribuzione var Class nel Training Set"),
               verbatimTextOutput("tabClass"))
             )
       ),
      # Statistiche Descrittive
    tabPanel("2.b Statistiche Descrittive",
               sidebarPanel(
                selectInput("listaVarNum", "Scegli la variabile di interesse",choices=colnames(audio), selected = colnames(audio)[1]),
                conditionalPanel("input.StatDes == 'correlazione'", selectInput("varcor", "Scegli una seconda variabile",choices=colnames(var_num), selected = "Class")),
                hr(),
                 radioButtons("StatDes", "Scegli una funzione da applicare", choices = c("summary","levels","correlazione","correlazione con Class >0.5"), selected = "summary")),
                
               # Main panel - Statistiche descrittive
               
               conditionalPanel("input.StatDes == 'summary' || input.StatDes == 'levels'", mainPanel(verbatimTextOutput("RstatDes"))), #se si vogliono inserire pi? condizioni (a,b), vanno inserite all'interno delle "a && b"
               conditionalPanel("input.StatDes == 'correlazione'", mainPanel(verbatimTextOutput("RstatDes2"))),                
               conditionalPanel("input.StatDes == 'correlazione con Class >0.5'", mainPanel(htmlOutput("plotcorr"))) #plotOutput
               
               # Main panel - Grafici
               ),
      # Grafici
   tabPanel("2.c Grafici",
                sidebarPanel(
                  selectInput("vargraf", "Scegli la variabile di interesse",choices=colnames(var_num), selected = colnames(var_num)["Class"]),
                  conditionalPanel("input.graf == 'Box Plot condizionati' || input.graf == 'Grafico a dispersione'", selectInput("vargraf2", "Scegli la seconda variabile di interesse", choices = colnames(var_num), selected = colnames(var_num)[1])),
                  conditionalPanel("input.graf == 'Importanza delle variabili'", selectInput("importanzavar", "Scegli la funzione da cui generare il grafico", choices = c("Random Forest","Lasso","Xgboost"), selected = "Random Forest")),
                  hr(),
                  radioButtons("graf", "Scegli il grafico di interesse", choices = c("Box Plot", "Box Plot condizionati", "Grafico a dispersione", "Importanza delle variabili"), selected = "Box Plot")),
                
      mainPanel(conditionalPanel("input.graf != 'Importanza delle variabili'",plotlyOutput("Ograf", height = "400px"),
                                 verbatimTextOutput("info")),
                conditionalPanel("input.graf == 'Importanza delle variabili'",htmlOutput("imp")))
   )    
      
      )
    ),
   
   # Terza pagina
   
   tabPanel("3-Analisi",
            
            # Traincontrol
            tabsetPanel(
            tabPanel("3.a TrainControl",
                     sidebarPanel( "Prima di implementare il modello:",
                                  numericInput("modseme", "Inserisci il numero del seme", value=321,min = 0),
                                  hr(),
                                  radioButtons("trmethod", "Che metodo di esecuzione vuoi usare?", choices = c("Nessuno", "cv", "repeatedcv"), selected = "Nessuno"),
                                  conditionalPanel("input.trmethod != 'Nessuno'", numericInput("number", "Inserisci il numero di iterazioni", value= 10, min=1)),
                                  conditionalPanel("input.trmethod == 'repeatedcv'", numericInput("ncv", "Inserisci il numero di cv da effettuare", value = 2, min=1)))
                     ),
            
            tabPanel("3.b Implementazione modelli",
                     
          # Sidepanel
                     
            
          sidebarPanel( radioButtons("model","Seleziona il modello da utilizzare", choices = c("Random Forest", "Lasso", "Glm", "Xgboost"), selected = "Xgboost"),
              selectInput("outputmod","Cosa vuoi visualizzare?", choices = c("Matrice di confusione, AUC e previsione", "Summary del modello")),
              checkboxGroupInput("varmodel", "Seleziona le esplicative del modello", choices = names(audio)[-19],selected = names(audio)[-19]),
              actionButton("gorf", "Random Forest"),
              actionButton("golasso", "Lasso"),
              actionButton("goglm", "Glm"),
              actionButton("goxgb", "Xgboost"),
              hr(),
              actionButton("gomodel","Calcola")
            ),
          
          # Main panel
          
                                                                                                  mainPanel(conditionalPanel("input.gomodel == 0", "Per stimare un modello clicca sul pulsante 'Calcola'. Puoi anche utilizzare uno dei modelli già preimpostati cliccando sul pulsante del modello corrispondente prima di eseguire la stima"),
                                                                                                        conditionalPanel("input.outputmod == 'Matrice di confusione, AUC e previsione'",verbatimTextOutput("tab"),
                                                                                                        verbatimTextOutput("auc"),
                                                                                                        verbatimTextOutput("prev")),
                                                                                                        conditionalPanel("input.outputmod == 'Summary del modello'",verbatimTextOutput("summary"))

            )))),#

  tabPanel("4-Conclusioni",
           tabsetPanel(
             tabPanel("4.a Visualizzazione risultati",
           sidebarPanel(
             radioButtons("visual", "Quali canzoni vuoi visualizzare?", choices = c("Tutte quelle classificate come 'Giacomo'","Tutte quelle classificate come 'Damiano'",
                                                                                    "Quelle classificate correttamente come 'Giacomo'", "Quelle classificate correttamente come 'Damiano'",
                                                                                    "Quelle erroneamente classificate come 'Giacomo'", "Quelle erroneamente classificate come 'Damiano'"), selected = "Tutte quelle classificate come 'Giacomo'"),
             hr(),
             checkboxGroupInput("Vartabella","Quali variabili vuoi visualizzare?", choices = names(audio), selected = names(audio)[c(1:3,19)])),
            mainPanel(DT::dataTableOutput("tabellafin"))
           #mainPanel(verbatimTextOutput("tabellafin"))
  ),
  tabPanel("4.b Conclusioni finali",
           div(style="text-align:center",h1("Conlcusioni finali:"),br(),br(),br(),br(),br(), h4("- Risultati soddisfacenti dal punto di vista previsivo"),h4("- Possibilità di utilizzare l'analisi con uno scopo differente da quello presentato", h4("- Applicazione shiny facile da utilizzare e funzionale allo scopo")))
  ))
           )
))
server <- function(input, output, session) {
  
  # Dividere train e test
  
  oss.test <- eventReactive(input$go, {set.seed(input$seme)
                                      sample(1:nrow(audio), size = nrow(audio)*input$pertest/100, replace=F)})
 
  # creo la variabile contenente le variabili da escludere usando un if
  
   vari <- eventReactive(input$go,{if(input$elVar == "Si")  names(audio) %in% input$VarDaEscl
    else  rep(F,length(names(audio)))})
  
  # creo test e train e imposto gli output per la parte superiore
   
   test <- eventReactive( input$go,{
      naudio <- audio[oss.test(),!vari()]
      naudio$Class <- NA
      naudio})
  
  training <- eventReactive(input$go,{
    audio[-oss.test(),!vari()]})
  output$dimTest <- renderPrint({dim(test())})
  output$dimTraining <- renderPrint({dim(training())})
  output$tabClass <- renderPrint({table(training()$Class)})
  
  newaudio <- eventReactive(input$go,{ndati <- data.frame(as.factor(audio[oss.test(),"Class"]))[,1]
                                      levels(ndati) <- c("Giacomo", "Damiano")
                                      ndati})
  
  graftraining <- eventReactive(input$go,{graf <- audio[-oss.test(),!vari()]
  graf$Class <- as.factor(audio$Class[-oss.test()])
  levels(graf$Class) <- c("Giacomo", "Damiano")
  graf})
  
  
  tabaudio <- audio
  tabaudio$Class <- as.factor(audio$Class)
  levels(tabaudio$Class) <- c("Giacomo", "Damiano")
  # Variabili da eliminare
  variabili_definitive <- eventReactive(input$go,{colnames(audio)[!vari()]})
  nnum <- eventReactive(input$go,{numeric = training()[,sapply(training(), is.numeric)]
                                  integer = training()[,sapply(training(), is.integer)]
                                  var_num = cbind(numeric, integer)})
  
  esplicative_definitive <- reactive({nvari <- vari()
  nvari[19]=T
  colnames(audio)[!nvari]
  })
  
  # aggiorno le selezioni 

observeEvent(input$go,{updateCheckboxGroupInput(session, "varmodel", label = "Scegli le esplicative del modello", choices = esplicative_definitive(), selected = esplicative_definitive())}) # go

observeEvent(input$gorf,{updateCheckboxGroupInput(session, "varmodel", label = "Scegli le esplicative del modello", choices = esplicative_definitive(), selected = names(audio)[-c(1:3,15,18,19)]) # Random Forest
                         updateRadioButtons(session,"trmethod", "Che metodo di esecuzione vuoi usare?", choices = c("Nessuno", "cv", "repeatedcv"), selected = "Nessuno")
                         updateNumericInput(session, "modseme", "Inserisci il numero del seme", value=321,min = 0)
                         updateRadioButtons(session,"model","Seleziona il modello da utilizzare", choices = c("Random Forest", "Lasso", "Glm", "Xgboost"), selected = "Random Forest")
                         })

observeEvent(input$golasso,{updateCheckboxGroupInput(session, "varmodel", label = "Scegli le esplicative del modello", choices = esplicative_definitive(), selected = names(audio)[c(4,5,9,11,12,13)]) # Lasso
  updateRadioButtons(session,"trmethod", "Che metodo di esecuzione vuoi usare?", choices = c("Nessuno", "cv", "repeatedcv"), selected = "cv")
  updateNumericInput(session,"modseme", "Inserisci il numero del seme", value=321,min = 0)
  updateRadioButtons(session,"model","Seleziona il modello da utilizzare", choices = c("Random Forest", "Lasso", "Glm", "Xgboost"), selected = "Lasso")
  updateNumericInput(session,"number", "Inserisci il numero di iterazioni", value= 10, min=1)
})

observeEvent(input$goglm,{updateCheckboxGroupInput(session, "varmodel", label = "Scegli le esplicative del modello", choices = esplicative_definitive(), selected = names(audio)[c(4,5,9,11,13)]) # Glm
  updateRadioButtons(session,"trmethod", "Che metodo di esecuzione vuoi usare?", choices = c("Nessuno", "cv", "repeatedcv"), selected = "cv")
  updateNumericInput(session,"modseme", "Inserisci il numero del seme", value=321,min = 0)
  updateRadioButtons(session,"model","Seleziona il modello da utilizzare", choices = c("Random Forest", "Lasso", "Glm", "Xgboost"), selected = "Glm")
  updateNumericInput(session,"number", "Inserisci il numero di iterazioni", value= 10, min=1)
})

observeEvent(input$goxgb,{updateCheckboxGroupInput(session, "varmodel", label = "Scegli le esplicative del modello", choices = esplicative_definitive(), selected = names(audio)[-c(1:3,8,15,16,18,19)]) # Xgboost
  updateRadioButtons(session,"trmethod", "Che metodo di esecuzione vuoi usare?", choices = c("Nessuno", "cv", "repeatedcv"), selected = "cv")
  updateNumericInput(session,"modseme", "Inserisci il numero del seme", value=321,min = 0)
  updateRadioButtons(session,"model","Seleziona il modello da utilizzare", choices = c("Random Forest", "Lasso", "Glm", "Xgboost"), selected = "Xgboost")
  updateNumericInput(session,"number", "Inserisci il numero di iterazioni", value= 10, min=1)
})
  



observe({updateSelectInput(session, "listaVarNum", label = "Scegli una variabile di interesse", choices = variabili_definitive(), selected = variabili_definitive()[4])
  updateSelectInput(session, "varcor", "Scegli una seconda variabile", choices = names(nnum()), selected = names(nnum())[length(names(nnum()))])
  updateSelectInput(session, "vargraf", label = "Scegli una variabile di interesse", choices = variabili_definitive(), selected = variabili_definitive()[4])
  updateSelectInput(session, "vargraf2", label = "Scegli una seconda variabile di interesse", choices = variabili_definitive(), selected = variabili_definitive()[length(variabili_definitive())])
  #
  })

# Per eseguire Statistiche descrittive 
  
 output$RstatDes <- renderPrint({stat <- switch(input$StatDes,summary = summary,
                                        cor = cor, levels=levels)
 output$RstatDes2 <- renderPrint({cor(training()[,input$listaVarNum], training()[,input$varcor])})
   stat(training()[,input$listaVarNum])})


 # immagine corr
 src_corr = "https://Damiano95.github.io/SL/corr_var.png"
 output$plotcorr <- renderText({c('<img src="',src_corr,'">')})
 
 # immagine importanza variabili
 
 src_rf = "https://Damiano95.github.io/SL/var_imp_rf.png"
 src_lasso = "https://Damiano95.github.io/SL/var_imp_lasso.png"
 src_xgb = "https://Damiano95.github.io/SL/var_imp_xgb.png"

 output$imp <- renderText({if (input$importanzavar == "Random Forest") c('<img src="',src_rf,'">')
                            else if (input$importanzavar == "Lasso") c('<img src="',src_lasso,'">')
                              else c('<img src="',src_xgb,'">')
   })
   
 
 # Grafici

 output$Ograf <- renderPlotly({if (input$graf == "Box Plot") {p <- ggplot(training(),aes_string( y=input$vargraf)) + theme_minimal() + labs(y = input$vargraf) + geom_boxplot(fill="grey") 
                                                                ggplotly(p) %>% 
                                                                  layout(height = 400, autosize=F)}
                                else if (input$graf == "Box Plot condizionati") {if (input$vargraf2 =="Class") {p<- ggplot(graftraining(),aes_string( y=input$vargraf, x=input$vargraf2, fill=input$vargraf2)) + theme_minimal() + geom_boxplot() +scale_x_discrete(labels=c("Giacomo","Damiano"))
                                p +scale_fill_brewer(palette="Blues")} 
                                  else {p <-ggplot(training(),aes_string( y=input$vargraf, x=input$vargraf2)) + theme_minimal() + labs( y = input$vargraf) + geom_boxplot() + scale_x_continuous(input$var2, limits = c(-0.5,1.5))
                                ggplotly(p)}}
                                else if (input$graf == "Grafico a dispersione"){
                                  p<-ggplot(training(), aes_string(x = input$vargraf, y = input$vargraf2, col = graftraining()[,"Class"],label=training()[,"track_name"], text=training()[,"artist_name"])) + 
                                    geom_point() +theme_minimal() 
                                  ggplotly(p,tooltip = c("label","text"))
                                }

   
   })

 # Analisi
 tr <- reactive({if (input$trmethod == "Nessuno") trainControl(summaryFunction = twoClassSummary,classProbs = TRUE)
        else if (input$trmethod == "cv") trainControl(method= "cv", number = input$number, summaryFunction = twoClassSummary,classProbs = TRUE)
        else trainControl(method= "repeatedcv", number = input$number,repeats = input$ncv, summaryFunction = twoClassSummary,classProbs = TRUE)
   })
 formula_mod <- reactive({as.formula(paste("Class",paste(input$varmodel, collapse="+"), sep="~"))})
 formula_rf <- reactive({if (length(input$varmodel) == 13) {if (sum(names(audio)[-c(1:3,15,18,19)] == input$varmodel)== length(input$varmodel)) T
                                                              else F}
                          else F})
 
 formula_lasso <- reactive({if (length(input$varmodel) == 6) {if (sum(names(audio)[c(4,5,9,11,12,13)] == input$varmodel)== length(input$varmodel)) T
   else F}
   else F})
 
 formula_glm <- reactive({if (length(input$varmodel) == 5) {if (sum(names(audio)[c(4,5,9,11,13)] == input$varmodel)== length(input$varmodel)) T
   else F}
   else F})
 
 formula_xgb <- reactive({if (length(input$varmodel) == 11) {if (sum(names(audio)[-c(1:3,8,15,16,18,19)] == input$varmodel)== length(input$varmodel)) T
   else F}
   else F})
 
 formula_corr <- reactive({if (length(esplicative_definitive())==length(input$varmodel)) {if (sum(esplicative_definitive() == input$varmodel)== length(input$varmodel)) T
                                                                                          else F}
                              else F})


 newtraining <- reactive({new <-as.data.frame(training())
 new[,"Class"]=as.factor(new[,"Class"])
 levels(new[,"Class"]) <- c("Giacomo","Damiano")
 new})
 

modello <- eventReactive(input$gomodel, {set.seed(input$modseme)
            if (input$model == "Random Forest" ) {if (input$modseme == 321 && formula_rf() == T && input$trmethod == 'Nessuno') prev <- list(randomf,T,"I dati relativi a questo modello sono stati importati, per questo motivo non è possibile eseguire un summary del modello che li ha generati")
                                                  else {phat <- train(formula_mod(),
                                                      newtraining(),
                                                      method = "rf",
                                                      preProcess=c("center","scale"),
                                                      trControl=tr(),
                                                      metric="ROC")
                                                  
                                                  prev <- list(predict(phat, newdata=test(),type="prob")[,2], phat,summary(phat))}}
            else if (input$model == "Lasso" ) {if (input$modseme == 321 && formula_lasso() == T  && input$trmethod == 'cv' && input$number == 10) prev <- list(lasso,T,"I dati relativi a questo modello sono stati importati, per questo motivo non è possibile eseguire un summary del modello che li ha generati")
                                                  else {phat <- train(formula_mod(),
                                                             newtraining(),
                                                             method = "glmnet",
                                                             preProcess=c("center","scale"),
                                                             trControl=tr(),
                                                             tuneGrid=expand.grid(alpha=1,lambda=seq(0.001, 1, by = 0.001)),
                                                             family="binomial",
                                                             metric = "ROC")
                                                    
                                                  prev <- list(predict(phat, newdata=test(),type="prob")[,2], phat,summary(phat))}}
            else if (input$model == "Glm" ) {if (input$modseme == 321 && formula_glm() == T  && input$trmethod == 'cv' && input$number == 10) prev <- list(glm,T,"I dati relativi a questo modello sono stati importati, per questo motivo non è possibile eseguire un summary del modello che li ha generati")
                                                 else {phat <- train(formula_mod(),
                                                              newtraining(),
                                                              method = "glm",
                                                              preProcess=c("center","scale"),
                                                              trControl=tr(),
                                                              family="binomial",
                                                              metric = "ROC")
                              
                                                  prev <- list(predict(phat, newdata=test(),type="prob")[,2], phat,summary(phat))}}
            else if (input$model == "Xgboost" )   {if (input$modseme == 321 && formula_xgb() == T && input$trmethod == 'cv' && input$number == 10) prev <- list(xgb,T,"I dati relativi a questo modello sono stati importati, per questo motivo non è possibile eseguire un summary del modello che li ha generati")
                                                  else {my_grid<-expand.grid(nrounds=500, eta=0.02, max_depth=5, gamma=5,
                                                                            min_child_weight=3, subsample=0.74, colsample_bytree=0.57 )
                                                  phat <- train(formula_mod(),
                                                             newtraining(),
                                                             method = "xgbTree",
                                                             preProcess=c("center","scale"),
                                                             trControl=tr(),
                                                             tuneGrid=my_grid,
                                                             family="binomial",
                                                             metric = "ROC")
                                                  
                                                  prev <- list(predict(phat, newdata=test(),type="prob")[,2], phat, summary(phat))}
}
            prev})

# Impostazione variabili modello

 previsioni <- reactive({prev <- ifelse(modello()[[1]]<0.5, "Giacomo", "Damiano")
 prev <- factor(prev, levels=c("Giacomo", "Damiano"))
 levels(prev) <- c("Giacomo","Damiano")
 as.data.frame(prev)[,1]
 })
 
  output$tab <- renderPrint({ 
    table(previsioni(),newaudio())
})
  output$auc <- renderPrint({cat("AUC: ",auc(audio[oss.test(),"Class"],modello()[[1]]))})
  output$prev <- renderPrint({cat("Percentuale di previsioni corrette: ",sum(diag(table(previsioni(),newaudio())))*100/length(newaudio()))})
  output$summary <- renderPrint({print(modello()[[3]])}) 
  
  
  # Tabella finale
  pos <- reactive({ if (input$visual=="Tutte quelle classificate come 'Giacomo'") {oss.test()[which(previsioni() == "Giacomo")]}
                      else if (input$visual=="Tutte quelle classificate come 'Damiano'") {oss.test()[which(previsioni() =="Damiano")]}
                        else if (input$visual=="Quelle classificate correttamente come 'Giacomo'") {oss.test()[which(previsioni() == "Giacomo" & newaudio() == "Giacomo")]}
                          else if (input$visual=="Quelle classificate correttamente come 'Damiano'") {oss.test()[which(previsioni() == "Damiano" & newaudio() == "Damiano")]}
                            else if (input$visual=="Quelle erroneamente classificate come 'Giacomo'") {oss.test()[which(previsioni() == "Giacomo" & newaudio() == "Damiano")]}
                              else  {oss.test()[which(previsioni() == "Damiano" & newaudio() == "Giacomo")]}})
    output$tabellafin <- DT::renderDataTable({tabaudio[pos(),input$Vartabella]})

    
}

shinyApp(ui, server)
