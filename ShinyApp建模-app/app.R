if(!require(shiny)) install.packages("shiny");library(shiny)
if(!require(tidyverse))install.packages("tidyverse");library(tidyverse)
if(!require("psych")) install.packages("psych");library(psych)     #describe
if(!require("DT")) install.packages("DT");library(DT)
if(!require("rpart")) install.packages("rpart");library(rpart)
if(!require("rpart.plot")) install.packages("rpart.plot");library(rpart.plot)
if(!require("MASS")) install.packages("MASS");library(MASS)
data=read.csv("Fatality-task2.csv")
data$Rate=factor(data$Rate)
data$jaild=factor(data$jaild)

ui <- navbarPage(title = "Task 2",
                 #########
                 # Tab 1 #
                 #########
                   tabPanel("Data Exploration",
                            sidebarPanel(
                              
                              #Select a variable for summary
                              selectInput(inputId = "sm_var",label = "Choose a variable for its summary statistics",
                              choices= as.list(names(data[,1:5]))),
                              
                              #Whether do summary by Rate
                              selectInput(inputId = "BY",label="See summary statistics by Rate",
                                          choices = list("No","Yes")),
                              
                              #Select a variable for plotting
                              selectInput(inputId = "plot_var",label = "Choose a varible for plotting",
                                          choices= as.list(names(data[,1:5])))
                            ),
                            
                            mainPanel(
                              h4(htmlOutput("txt1"),style = "color:blue"),
                              tableOutput("summary"),
                              h4(htmlOutput("txt2"),style = "color:blue"),
                              plotOutput('plot')
                            )
                          ),
                 #########
                 # Tab 2 #
                 #########
                   tabPanel("Classification tools",
                            sidebarPanel(
                              h4(strong("1.Classification Tree")),
                              #Select a proportion of data as training data set
                              sliderInput(inputId = "train_percent",label = "Select the proportion of data for the training data set",
                                          min=0.4,max=0.8,value = 0.6,step=0.1),
                              #View pruned tree or Unpruned tree
                              radioButtons(inputId = "View_tree",label="The tree to be displayed:",
                                           choices = list("View pruned tree","View unpruned tree")),
                              helpText("*Note the cp value for pruning correspondes with the smallest xerror"),
                              h4(strong("2.Input observed values for prediction")),
                              selectInput("jaild",label="jaild",choices = levels(data$jaild),selected = "no"),
                              numericInput("beertax",label="beertax",value = mean(data$beertax)),
                              numericInput("vmiles",label="vmiles",value = mean(data$vmiles)),
                              numericInput("unrate",label="unrate",value = mean(data$unrate)),
                              numericInput("perinc",label="perinc",value = mean(data$perinc))
                              
                              ),
                            mainPanel(#Notify how much data for training and validation
                              h3(htmlOutput("txt3")),
                              plotOutput("tree"),
                              DT::dataTableOutput("comparison"),   #the table for comparison
                              h4(em("Note: The `best` classification method with the highest CCR on validation is highlighted in", span(strong("green"),style = "color:lightgreen"))),
                              h3("The state with input values is predicted by the `best` model whether to have fatality rate above the US average. (0=No,1=Yes)"),
                              h3(htmlOutput("prediction")),
                              h3(htmlOutput("warning"))
                              ),
                            )
                   
                   
)

  
  server=function(input,output){
    ########
    # Tab1 #
    ########
    #Modularize the reactive data for the first step
    reactiveData=reactive({
      data%>%
        dplyr::select(input$sm_var)
  })
    
    #Text reminder
    output$txt1=renderText({
      if(input$BY=="Yes"){
        if(input$sm_var!="jaild")
        paste("The summary statistics of chosen variable-",strong(input$sm_var,"by Rate"))
        else "Counts of <B>Jaild</B> by Rate"}
      else{
        if(input$sm_var!="jaild")
          paste("The summary statistics of chosen variable-",strong(input$sm_var))
        else "Counts of <B>Jaild</B>"}
      
  })
    output$txt2=renderText({
      paste("Visualize the relationship between",strong(input$plot_var),"and Rate")
    })

    # 1,2 Summary 
    output$summary= renderTable({
      if(input$BY=="Yes"){
      #If-else allows for different plots for both numeric and character column
        if(input$sm_var!="jaild"){
        describeBy(reactiveData(), 
               group=data$Rate,
               quant=c(.25,.75),
               mat = T)%>%
        as_tibble()%>%
        dplyr::select(group1,n,mean,sd,min,Q0.25,median,Q0.75,max)%>%
        rename(Rate=group1,counts=n,Q1=Q0.25,Q3=Q0.75)
        }else{
          table(data$jaild,data$Rate)%>%
          as.data.frame()%>%
          rename(jaild=Var1,Rate=Var2,Counts=Freq)}
      }else{
        if(input$sm_var!="jaild")
          describe(reactiveData(),
                     quant=c(.25,.75),
                     )%>%
          as_tibble()%>%
          dplyr::select(n,mean,sd,min,Q0.25,median,Q0.75,max)%>%
          rename(counts=n,Q1=Q0.25,Q3=Q0.75)
        else {
          table(data$jaild)%>%
          as.data.frame()%>%
          rename(jaild=Var1,Counts=Freq)
          }}
  })
      
      # 3,4 Plot
      output$plot= renderPlot({
        if(input$plot_var!="jaild")
        ggplot(data,aes_string(x=input$plot_var,fill="Rate"))+
          geom_density(alpha=0.4)
        else ggplot(data,aes_string(x=input$plot_var,fill="Rate"))+
          geom_bar(position = "dodge",color="white")
  })
      ########
      #Tab 2 #
      ########
      
      
      #Divide data into training and validation
      train_index=reactive({
        train_size=as.numeric(input$train_percent)*nrow(data)
        sample(replace = FALSE,x=1:nrow(data),size = train_size)
})    
      train=reactive({data[train_index(),]})
      
      validation=reactive({data[setdiff(1:nrow(data),train_index()),]})

      
      # Create a list-like container to save all we need- Models, corresponding CCR,MCR, best method name.
      # tree & cp_min_xerror & prunred ccr.tree
      # lda.model & lda.ccr
      # best_method
      container=reactiveValues()
      observe({
          #tree, 
          #cp_min_xerror, pruned, rpart.plot call plot and return a list, it can't be displayed as a plot when called later
          container$tree=rpart(Rate~.,data = train())
          cp_mat=printcp(container$tree)%>%
            as.data.frame()
          container$cp_min_xerror=cp_mat$CP[which.min(cp_mat$xerror)]
          container$pruned = rpart(data=train(),Rate~.,cp=container$cp_min_xerror)
          container$tree.ccr = mean(predict(container$pruned,newdata=validation(),type="class")==validation()$Rate)
          # container$tree.plot = rpart.plot(container$tree)
          # container$pruned.plot = rpart.plot(container$pruned)
          #LDA
          container$lda.model = lda(data=train(),Rate~.)
          container$lda.ccr=mean(predict(container$lda.model,validation())$class==validation()$Rate)
          
          #best_method-Pruned tree","LDA"
          container$best_method=
            if(container$tree.ccr>container$lda.ccr){"Pruned tree"
            } else if(container$tree.ccr<container$lda.ccr) {"LDA"
                 } else {"tie"}
        })
      
      
      ##############################
      # requirement 2 - Call Plots #
      ##############################
       
      output$tree=shiny::renderPlot({
        if(input$View_tree=="View pruned tree")
        {rpart.plot(container$pruned)
        }else {rpart.plot(container$tree)}
      })
      #############################################
      # requirement 3 - a reactive table using DT #
      #############################################

      
      #The Correct-classification Rates, Miss-classification Rates Table on validation
      output$comparison=DT::renderDataTable({
        obj=as.data.frame(matrix(NA,nrow=2,ncol=3))
        names(obj)=c("Method","Correct-classification Rate(%)","Miss-classification Rate(%)")
        #Assemble the table with items alreay in container
        obj$Method=c("Pruned tree","LDA")
        obj$`Correct-classification Rate(%)`=round(c(container$tree.ccr,container$lda.ccr)*100,digits = 4)
        obj$`Miss-classification Rate(%)`=round((c(1,1)-c(container$tree.ccr,container$lda.ccr))*100,digits = 4)
        # to highlight
        competition = round(c(container$tree.ccr,container$lda.ccr)*100,digits = 4)
        DT::datatable(obj,options = list(dom = 't')) %>% formatStyle(
          'Correct-classification Rate(%)',
          target = 'row',
          backgroundColor = styleEqual(c(max(competition),min(competition)), c('lightgreen', 'white'))
        )
      })
       
      #################
      # requirement 4 #
      #################
      
      #Input as a dataframe
      user_defined=reactive({
      #stop if input is unavailable
      req(input$beertax,input$vmiles,input$unrate,input$perinc)
      #a NAMED vector
      user_defined_vector=c(input$beertax,input$jaild,input$vmiles,input$unrate,input$perinc)
      names(user_defined_vector)=names(data)[-6]
      #Newdata
      #Caution: silent transformation of data type can happen "as.list(user_defined))"
      data.frame(lapply(user_defined_vector,type.convert))})
        
      
      
      #predict 
      output$prediction=shiny::renderText({
        result=if(container$best_method=="Pruned tree"){
                 predict(container$pruned,newdata=user_defined(),type="class")
               }else if(container$best_method=="LDA"){
                 predict(container$lda.model,newdata=user_defined())$class
                    }else if(container$best_method=="tie"){
                      #Be careful when u combine two factor type values! They will be transformed to numeric values!
                      c(as.character(predict(container$pruned,newdata=user_defined(),type="class")),
                      as.character(predict(container$lda.model,newdata=user_defined())$class))}
          if(container$best_method!="tie"){           
           paste("The best method is ",span(strong(container$best_method),style="color:lightgreen"),", its prediction based on input values is ",strong(result),sep = "")
          }else paste(seq="",span(strong("The two methods tie"),style="color:lightgreen"),"in performance test using validation data, and the prediction of pruned tree is ",strong(result[1]),"while the prediction of LDA is ",strong(result[2]))
        })
      
      
        #Warning
        #check whether input is in observed range one by one
        output$warning=renderText({
          #customise a function to check whether an input is within a range, if it goes over a limit, return TRUE
          check.range=function(x,var,Data){
          if(x<range(data[,var])[1]|x>range(data[,var])[2])
            return(T) #True out of range
          else return(F) #Not out of range
          }
          #check multiple values with mapply, give_warning=T if any input is out of the observed range
          checklist= mapply(check.range,x=user_defined()[-2],var=names(user_defined()[,-2]),
                            MoreArgs = list(Data=data[,-c(2,6)]))
          give_warning = checklist%>%
          any()
         warning.names=names(checklist[checklist==T])
         #There could be several values out of range, I want multiple lines to specify the variables causing extrapolations
        if(give_warning){
          if(length(warning.names)==1){
            paste(span(strong("Warning:"),style="color:red"),"You are extrapolating,",span(strong(warning.names),style="color:red"),"is out of the observed range")
          } else{
            do.call(cbind,lapply(warning.names,
                                      function(x){paste(span(strong("Warning:"),style="color:red"),"You are extrapolating,",span(strong(x),style="color:red"),"is out of the range in observed data!","<br>")}
            ))
          }
        }else {""}
      
  })
}
  

shinyApp(ui,server)


