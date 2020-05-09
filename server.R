#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(caret)
# X_train = readRDS("gene_fs.rds")
y = readRDS("class.rds")
pca = readRDS("pca.rds")

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    
    
    X = reactive({
        n = input$fs
        X = pca[,1:n]
        # X = cbind(X, y)
        return(X)})
    
   plot =  eventReactive(input$plot, {
       
       validate(need(input$metric, "Choose metrics"))
        set.seed(100)
       
        cv_acc = cv_f1 = cv_bacc = c()
        for(i in 1:50){
            cvSets = cvTools::cvFolds(nrow(X()), 5)
            acc = f1 = bacc = c()
            for(j in 1:5){
                test_id = cvSets$subsets[cvSets$which == j]
                y_test = y[test_id]
                y_train = y[-test_id]
                X_test = X()[test_id,]
                X_train = X()[-test_id,]
                
                
                svm_res <- e1071::svm(x = X_train, y = as.factor(y_train))
                fit <- predict(svm_res, X_test)
                cm = confusionMatrix(factor(fit, levels = c("Yes", "No")),  
                                     factor(y_test, levels = c("Yes", "No")))
                acc[j] = cm$overall[1] %>% round(2)
                fscore = cm$byClass["F1"]
                f1[j] = ifelse(is.na(fscore), 0, round(fscore, 2))
                bacc[j] = cm$byClass["Balanced Accuracy"] %>% round(2)
            }
            cv_acc <- append(cv_acc, mean(acc))
            cv_f1 <- append(cv_f1, mean(f1))
            cv_bacc <- append(cv_bacc, mean(bacc))
        }
        l = list()
        if("Accuracy" %in% input$metric){
            l[["Accuracy"]] = cv_acc
        }
        if("F1 Score" %in% input$metric){
            l[["F1_Score"]] = cv_f1
        }
        if("Balanced Accuracy" %in% input$metric){
            l[["Balanced_Accuracy"]] = cv_bacc
        }
        
       plot = data.frame(l)%>%
            reshape2::melt(id.vars = c(), variable.name = "Metrics", value.name = "Value") %>% 
            ggplot(aes(x = Metrics, y = Value)) + 
            geom_boxplot() + 
            labs(title = "Performance Evaluation", 
                 caption = "data retrieved from  https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=GSE120396")
        
       # plot =  boxplot(l)
       return(plot)
    })
   
   cm = eventReactive(input$CM,{
       set.seed(100)
       t = 1
       vals = c(0,0,0,0)
       cvSets = cvTools::cvFolds(nrow(X()), 5)
       for(j in 1:5){
           test_id = cvSets$subsets[cvSets$which == j]
           y_test = y[test_id]
           y_train = y[-test_id]
           X_test = X()[test_id,]
           X_train = X()[-test_id,]
           
           
           svm_res <- e1071::svm(x = X_train, y = as.factor(y_train))
           fit <- predict(svm_res, X_test)
           cmat = confusionMatrix(fit, factor(y_test, levels = c("Yes", "No")))
           if(t == 1){
               t = cmat$table
           }else{
               t = t + cmat$table
           }
           vals = vals + as.vector(cmat$table)
       }
       m = matrix(vals, byrow =F , nrow = 2)
       colnames(m) = c("Yes", "No")
       rownames(m) = c("Yes", "No")
       return(t)
       
   })
    
    

    output$plot <- renderPlot({
        plot()

    })

    output$table <- renderPrint({
        cm()
    })

})
