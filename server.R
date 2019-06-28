library(shiny)
library(DT)
library(caret)
library(doParallel)
library(pls)
library(nnet)
library(glmnet)
library(cubit)
library(elasticnet)
library(MASS)
library(Cubist)

clus <- makeCluster(detectCores(all.tests = FALSE, logical = TRUE))
registerDoParallel(clus)  # this will work on windows
trControl <- trainControl("cv", number = 10)  # shared cross validation specification

server <- shinyServer(function(input, output, session) {

  getData <- reactive({
    data <- read.csv(file="/Users/shunli/Desktop/Ass4Data.csv")
    rownames(data) <- data$ID
    data$ID <- NULL
    data
  })
  
  output$BoxPlots <- renderPlot({
    d <- getData()
    numeric <- sapply(d, FUN = is.numeric)
    boxplot(d[,numeric], outline=TRUE, main="Boxplot using multiplier of 1.5")
  })
  
  output$DataSummary <- renderPrint({
    str(getData())
  })

  output$Table <- DT::renderDataTable({
    d <- getData()
    numeric <- c(FALSE, sapply(d, is.numeric)) # never round rownames which are the first column (when shown)
    DT::datatable(d) %>%
      formatRound(columns = numeric, digits = 3)
  })
  
  
  ############################################################################## 
  getSplit <- reactive({
    createDataPartition(y = getData()$Y, p = input$Split, list = FALSE)
  })
  
  getTrainData <- reactive({
    getData()[getSplit(),]
  })
  
  getTestData <- reactive({
    getData()[-getSplit(),]
  })
  
  output$SplitSummary <- renderPrint({
    cat(paste("Training observations:", nrow(getTrainData()), "\n", "Testing observations:", nrow(getTestData())))
  })

  
  ##############################################################################  
  getGlmModels <- reactive({
    method <- "glmnet"
    showNotification(id = method, paste("Optimising", method, "hyper-parameters using cross validation"), session = session, duration = NULL)
    mods <- caret::train(Y ~ ., data = getTrainData(), method = method, metric = "RMSE",
                  trControl = trControl,
                  tuneGrid = expand.grid(alpha = seq(0,1, 0.1), lambda = seq(0.1, 10, by = 0.1))
    ) # note glmnet does not support parameter "allowParallel"
    removeNotification(id=method)
    mods
  })
  
  output$GlmModelSummary1 <- renderTable({
    mods <- getGlmModels()
    as.data.frame(mods$bestTune)
  })  
  
  output$GlmModelPlots <- renderPlot({
    mods <- getGlmModels()
    plot(mods$finalModel)
  })     
  
  output$GlmModelSummary2 <- renderPrint({
    print(getGlmModels())
  })
  
  
  ##############################################################################
  getPlsModels <- reactive({
    method <- "pls"
    showNotification(id = method, paste("Optimising", method,"hyper-parameters using cross validation"), session = session, duration = NULL)
    mods <- caret::train(Y ~ ., data = getTrainData(), method = method, metric = "RMSE", scale = FALSE, 
                  trControl = trControl,
                  tuneGrid = expand.grid(ncomp = seq(1, 10, by = 1)),
                  allowParallel = TRUE
                  )
    
    removeNotification(id=method)
    mods
  })
  
  output$PlsModelSummary1 <- renderTable({
    mods <- getPlsModels()
    as.data.frame(mods$bestTune)
  })  
  
  output$PlsModelPlots <- renderPlot({
    plot(getPlsModels())
  })     
  
  output$PlsModelSummary2 <- renderPrint({
    mods <- getPlsModels()
    summary(mods$finalModel)
  })
  
  
  
  ##############################################################################
  getAnnModels <- reactive({
    method <- "nnet"
    showNotification(id = method, paste("Optimising", method, "hyper-parameters using cross validation"), session = session, duration = NULL)
    mods <- caret::train(Y ~ ., data = getTrainData(), method = method, metric = "RMSE", maxit = 1000, trace = F, linout = 1,
                  trControl = trControl,
                  tuneGrid = expand.grid(.decay = seq(0.3, 0.6, by=0.1), .size = seq(4, 8, by=1)),
                  allowParallel = TRUE
                  ) 
    removeNotification(id=method)
    mods
  })
  
  output$AnnModelSummary1 <- renderTable({
    mods <- getAnnModels()
    as.data.frame(mods$bestTune)
  })  
  
  output$AnnModelPlots <- renderPlot({
    plot(getAnnModels())
  })     
  
  output$AnnModelSummary2 <- renderPrint({
    mods <- getAnnModels()
    print(mods$finalModel)
  })
  

  ##############################################################################
  getCubistModels <- reactive({
    method <- "cubist"
    showNotification(id = method, paste("Optimising", method,"hyper-parameters using cross validation"), session = session, duration = NULL)
    mods <- caret::train(Y ~ ., data = getTrainData(), method = method, metric = "RMSE",
                  trControl = trControl,
                  tuneGrid = expand.grid(committees = c(1, 5, 10, 15, 20),neighbors = c(0, 3, 5, 7,9)),
                  allowParallel = TRUE
    )
    
    removeNotification(id=method)
    mods
  })
  
  output$CubistModelSummary1 <- renderTable({
    mods <- getCubistModels()
    as.data.frame(mods$bestTune)
  })  
  
  output$CubistModelPlots <- renderPlot({
    plot(getCubistModels())
  })     
  
  output$CubistModelSummary2 <- renderPrint({
    mods <- getCubistModels()
    summary(mods$finalModel)
  })
  
  
  
  ############################################################################## 
  
  getElasticnetModels <- reactive({
    method <- "enet"
    showNotification(id = method, paste("Optimising", method,"hyper-parameters using cross validation"), session = session, duration = NULL)
    mods <- caret::train(Y ~ ., data = getTrainData(), method = method, metric = "RMSE", trace = F,intercept = T,
                  trControl = trControl,
                  tuneGrid = expand.grid(fraction = seq(0.05, 1, by= 0.05),lambda = c(0, 10 ^ seq(-1, -4, by= -1))),
                                                                      # lambda = 0 performs the Lasso fit
                  allowParallel = TRUE
    )
    
    removeNotification(id=method)
    mods
  })
  
  output$ElasticnetModelSummary1 <- renderTable({
    mods <- getElasticnetModels()
    as.data.frame(mods$bestTune)
  })  
  
  output$ElasticnetModelPlots <- renderPlot({
    plot(getElasticnetModels())
  })     
  
  output$ElasticnetModelSummary2 <- renderPrint({
    mods <- getElasticnetModels()
    print(mods$finalModel)
  })
  
  
  
  ##############################################################################
 
  getRobustLinearModels <- reactive({
    method <- "rlm"
    showNotification(id = method, paste("Optimising", method,"hyper-parameters using cross validation"), session = session, duration = NULL)
    mods <- caret::train(Y ~ ., data = getTrainData(), method = method, metric = "RMSE", maxit = 20, contrasts= NULL, 
                  trControl = trControl,
                  tuneGrid = expand.grid(intercept=c(TRUE, FALSE),psi=c("psi.huber", "psi.hampel", "psi.bisquare")),
                          # the psi function is specified by this argument.
                          #  
                  allowParallel = TRUE
    )
    
    removeNotification(id=method)
    mods
  })
  
  output$RobustLinearModelSummary1 <- renderTable({
    mods <- getRobustLinearModels()
    as.data.frame(mods$bestTune)
  })  
  
  output$RobustLinearModelPlots <- renderPlot({
    plot(getRobustLinearModels())
  })     
  
  output$RobustLinearModelSummary2 <- renderPrint({
    mods <- getRobustLinearModels()
    print(mods$finalModel)
  }) 
  
  
  
  
  
  ##############################################################################  
  getAllModels <- reactive({
    list(GLMnet=getGlmModels(), PLS=getPlsModels(), ANN=getAnnModels(),Cubist=getCubistModels(), Elasticnet=getElasticnetModels(),RobustLinear=getRobustLinearModels())  
  })
  
  output$SelectionSummary <- renderPrint({
    results <- resamples(getAllModels())
    summary(results)
  })
  
  output$SelectionBoxPlot <- renderPlot({
    results <- caret::resamples(getAllModels())
    bwplot(results, notch=input$Notch)
  })
  
  
  ############################################################################## 
  output$Title <- renderUI({
    tags$h3(paste("Unseen data results for chosen model:", input$Choice))
  })

  getTestResults <- reactive({
    test <- getTestData()
    mod <- getAllModels()[input$Choice]
    predictions <- predict(mod, newdata=test)
    d <- data.frame(test$Y, predictions)
    colnames(d) <- c("obs", "pred")
    d
  })
  
  output$TestSummary <- renderPrint({
    caret::defaultSummary(getTestResults())
  })

  output$TestPlot <- renderPlot({
    plot(getTestResults(), main="Predicted versus Observed")
    abline(a = 0, b=1)
  })
  
})

shinyApp(ui = ui, server = server)
