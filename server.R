library()
server <- function(input, output) {
  df23 <- readRDS('data/FiveWords.rds')
  inputr <- eventReactive(input$submit, 
                          { trimws( gsub( '\\s+' ,' ', tolower(input$inputWords) ) ) }
                          )
  
  # Render output based on input
  output$outTable  <-  renderTable(  { 
 
  ou <-  arrange( df23 [df23$X == inputr(),  ]                                          , desc(csum ) ) 
  ou ['csumLag']<- lead(ou['csum'], n=1 ) 
  ou[ dim(ou)[1] , 'csumLag'] <- 0
  # predict based on randNum
  # set.seed(15)
  randNum <- runif(1) 
  # print(randNum)
  prediction <- ou[ ou$csum  > randNum & ou$csumLag <= randNum
                         , c("X", "y") ]
  for (x in 1:2) {
    randNum <- runif(1) 
    pred <- ou[ ou$csum  > randNum & ou$csumLag <= randNum
                     , c("X", "y") ]
    prediction <- rbind(prediction, pred)
  }
  prediction
  } ) 


  output$outputWords <- renderText ( { 
    inputr()
    } )
}