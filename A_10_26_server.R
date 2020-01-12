library(shiny)
library(shinydashboard)
library(DT)
library(party)
library(randomForest)
library(caTools)
library(rpart)
library(rpart.plot)

data=read.csv("processed.cleveland.data")

names(data)<-c("age", "sex", "cp", "trestbps", "choi", 
               
               "fbs", "restecg", "thalach", "exang", "oldpeak", 
               
               "slope", "ca", "thai", "num")

data$num[data$num > 1]<-1


data <- transform(
  data,
  age=as.integer(age),
  
  sex=as.factor(sex),
  
  cp=as.factor(cp),
  
  trestbps=as.integer(trestbps),
  
  choi=as.integer(choi),
  
  fbs=as.factor(fbs),
  
  restecg=as.factor(restecg),
  
  thalach=as.integer(thalach),
  
  exang=as.factor(exang),
  
  oldpeak=as.numeric(oldpeak),
  slope=as.factor(slope),
  
  ca=as.factor(ca),
  
  thai=as.factor(thai),
  
  num=as.factor(num)
)

tree = ctree(num ~ ., data=data)

tab1 = table(predict(tree),data$num)

#train<=subset(data)

shinyServer(function(input, output) 
{
  output$p1 = renderPlot({
    
    
    plot(input$v1,input$v2)
  })
  
  
  output$table =renderDataTable({
    
    file =read.csv("processed.cleveland.data")
    
    file
    
    
    datatable(file,
              
              rownames=T,
              
              filter="top",
              
              h3("All students Information"),
              
              options=list(pageLength=10)
    )
    
    
    
  })
  output$tableA=renderDataTable({
    
    data=read.csv("processed.cleveland.data")
    
    
    # val$num[val$num > 1]<-1
    
    
    names(data)<-c("age", "sex", "cp", "trestbps", "choi", 
                   
                   "fbs", "restecg", "thalach", "exang", "oldpeak", 
                   
                   "slope", "ca", "thai", "num")
    
    data$num[data$num > 1]<-1
    
    
    data <- transform(
      data,
      
      age=as.integer(age),
      sex=as.factor(sex),
      
      cp=as.factor(cp),
      trestbps=as.integer(trestbps),
      
      choi=as.integer(choi),
      fbs=as.factor(fbs),
      restecg=as.factor(restecg),
      
      thalach=as.integer(thalach),
      exang=as.factor(exang),
      oldpeak=as.numeric(oldpeak),
      
      slope=as.factor(slope),
      ca=as.factor(ca),
      thai=as.factor(thai),
      num=as.factor(num)
      
    )
    
    summary(data)
    
    png(filename = "tree.png")
    
    tree<-randomForest( num~ .,data=data)
    
    #tree<-ctree( num~ .,data=data)
    
    plot(tree)
    
    dev.off()
    
    
    data=read.csv("processed.cleveland.data")
    
    data
    
    
    datatable(data,
              
              rownames=T,
              
              filter="top",
              
              h3("Information"),
              
              options=list(pageLength=10)
    )
    
    
    
  })
  output$image=renderImage({
    
    list(src="tree.png",contentType="image/png")
    
  },
  deleteFile = FALSE)
  
  
  
  output$tableB=renderDataTable({
    
    
    
    DecisionTreeModel <- function(training.set,test.set) {
      
      #####Extract Training Data#####
      
      training.set
      str(training.set)
      
      
      #####Creating Decision Tree Model#####
      
      tree <- rpart(Class ~ XB + XC + XD + XE + XF + XG + XH + XI + XJ +
                      XK + XL + XM + XN + XO + XP + XQ + XR + XS + XT + XU,
                    data=training.set,method="class",parms = list(split = 'information'), 
                    minsplit=2,minbucket=1)
      
      
      #####Print summary of model#####
      
      summary(tree)
      
      
      ######Plotting the decision tree######
      
      par(mar=rep(0.1,4))
      
      plot(tree,uniform=T,compress = T)
      
      text(tree)
      
      
      ######Pruning the tree#####
      
      plotcp(tree)
      
      printcp(tree)
      
      prunedTree <- prune(tree, 
                          cp=(tree$cptable[which.min(tree$cptable[, "xerror"]), "CP"]))
      
      par(mar=rep(0.1,4))
      
      plot(prunedTree,uniform=T,compress = T)
      
      text(prunedTree)
      
      
      #####Testing the Model using test set#####
      
      str(test.set)
      
      prediction <- predict(prunedTree,test.set,type = "class")
      
      predTable <- table(prediction,test.set$Class)
      
      
      #####Calculating accuracy#####
      
      accuracy <- sum(diag(predTable))/sum(predTable)
      accuracy*100
    }
    
    training.set = read.csv("processed.cleveland.data")
    
    test.set = read.csv("processed.cleveland.data")
    
    DecisionTreeModel(training.set, test.set)
    
    tree2<-ctree(class~ .,data=train)
  })
  
  
  output$image2=renderImage({
    list(src="test.png",contentType="image/png")
  },
  deleteFile = FALSE)
  
  output$tableC=renderTable({
    
    paste0(tab1)
  })
  output$p1 = renderPlot(
    {
      plot(tree, main="Tree for C4.5")
    }
  )
  
  output$p2 = renderPlot(
    {
      plot(tree1, main="Tree for id3")
    }
  )
})