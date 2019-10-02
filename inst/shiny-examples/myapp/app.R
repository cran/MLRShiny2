library(shiny)
library(shinyAce)
library(dplyr)
library(psych)
library(QuantPsyc)
library(forecast)
library(corrgram)
# Define UI for application that draws a histogram
ui <- fluidPage(
  h6(" Authored by:", tags$img(src ="K.JPG", height=100, width=100)),
  verbatimTextOutput("preface"),
  tags$img(src = "T.png"),
  br(),
  br(),
  br(),
  sliderInput("Choice", label = "Choose 1 for reading from the Editor else Choose 0 for reading from the uploaded file",
              min = 0, max = 1, value = 1, step = 1),
  fileInput("file","Upload the *.csv file"),

  helpText("Copy paste data with variable names from Excel"),
  aceEditor("text", value=
              "q1	q2	q3	q4	q5	q6	q7	q8	q9	q10
            2	2	2	2	2	2	2	2	2	2
            3	3	4	2	2	3	3	4	3	3
            3	3	3	3	3	3	3	3	3	2
            3	1	3	3	3	3	3	2	3	3
            3	3	3	3	3	3	3	3	3	3
            4	3	3	2	3	3	4	4	3	4
            2	3	4	3	3	3	4	5	5	4
            4	4	4	4	3	3	4	4	4	4
            3	3	4	3	3	3	4	4	3	3
            3	3	4	3	3	3	4	4	4	4
            1	2	4	1	3	3	3	3	2	2
            4	4	4	3	3	3	3	3	4	4
            5	5	4	5	3	4	4	3	2	2
            3	3	5	4	3	4	4	4	4	4
            5	5	5	3	3	4	5	5	5	5
            3	4	5	2	3	3	5	5	5	5
            1	1	1	4	4	4	5	5	5	4
            1	1	2	3	4	2	3	3	2	2
            2	4	2	3	4	2	5	4	4	4
            5	4	2	4	4	4	5	4	4	3
            4	4	3	3	4	4	5	4	5	4
            3	1	3	4	4	3	3	3	3	3
            4	4	3	4	4	4	3	4	4	4
            3	3	3	4	4	4	4	4	4	4
            4	4	3	3	4	3	3	3	2	3
            3	5	3	4	4	4	4	3	4	4
            4	4	3	4	4	4	4	5	5	5
            3	3	3	4	4	4	5	5	5	5
            2	3	3	4	4	4	4	4	4	4
            3	3	4	4	4	3	4	4	3	4
            5	5	4	4	4	4	4	5	5	4
            5	3	4	4	4	4	5	5	5	5
            4	4	4	4	4	4	4	4	4	4
            5	5	4	3	4	4	4	5	5	4
            4	4	4	4	4	4	4	4	4	4
            4	4	4	4	4	4	4	4	4	4
            5	4	4	3	4	4	4	4	4	4
            3	3	4	4	4	4	4	4	4	3
            2	3	4	3	4	4	4	4	4	4
            5	3	4	3	4	4	5	5	5	5
            5	5	4	4	4	3	5	5	4	4
            4	4	4	4	4	4	4	4	4	4
            4	4	4	4	4	3	4	4	4	4
            4	4	4	4	4	4	4	4	4	4
            3	4	4	4	4	4	4	4	4	4
            3	3	4	4	4	4	4	4	4	4
            4	4	4	4	4	4	4	4	4	4
            4	4	4	4	4	4	4	4	4	4
            3	4	4	4	4	4	4	4	4	4
            4	3	4	4	4	4	4	4	4	4
            4	4	4	4	4	4	4	4	4	4
            3	4	4	4	4	4	5	5	5	4
            4	4	4	4	4	4	3	4	4	4
            5	4	4	4	4	4	4	5	5	5
            2	3	4	4	4	4	5	5	5	4
            3	3	4	5	4	3	4	4	4	4
            4	4	4	4	4	5	4	4	4	4
            4	3	4	4	4	4	4	4	4	4
            2	2	4	2	4	4	3	3	3	4
            4	4	4	4	4	5	4	4	4	4
            3	3	4	4	4	4	5	4	4	4
            4	4	4	4	4	4	4	4	4	4
            5	5	4	3	4	3	3	5	4	5
            3	4	4	3	4	3	4	4	4	4
            2	5	5	4	4	5	3	5	5	5
            5	5	5	4	4	4	4	4	4	4
            3	5	5	5	4	4	5	5	5	4
            4	5	5	4	4	3	4	4	4	3
            5	5	5	4	4	4	5	5	5	5
            4	5	5	4	4	4	4	4	2	4
            5	5	5	5	4	5	5	5	5	5
            4	4	5	4	4	4	4	4	4	4
            5	4	5	4	4	4	5	5	5	4
            3	3	5	5	4	4	5	5	5	4
            5	5	5	4	4	4	5	5	4	5
            5	5	5	4	4	4	4	4	3	4
            5	5	5	4	4	4	5	5	4	5
            5	5	5	3	4	5	4	5	5	4
            4	5	5	4	4	4	4	4	4	4
            3	5	5	4	4	4	5	5	5	4
            5	5	5	4	4	4	5	4	4	5
            1	1	2	4	5	4	4	4	4	4
            5	4	2	4	5	4	5	5	5	5
            5	5	2	5	5	5	5	5	4	5
            5	5	3	5	5	5	5	5	5	5
            5	5	3	5	5	5	5	5	5	5
            4	4	3	5	5	5	5	5	5	5
            3	4	3	4	5	5	4	4	4	3
            1	1	3	5	5	5	5	5	4	4
            4	4	3	5	5	5	5	5	4	4
            3	3	3	5	5	5	5	5	5	5
            4	5	3	5	5	5	5	5	5	4
            5	4	4	4	5	4	5	4	3	4
            1	1	4	5	5	5	5	5	5	4
            4	3	4	4	5	2	4	4	3	4
            3	4	4	5	5	5	5	5	5	5
            3	3	4	5	5	4	4	4	5	5
            5	5	4	4	5	4	5	5	5	5
            5	4	4	5	5	5	2	5	5	3
            4	4	4	4	5	5	4	5	5	5
            4	3	4	4	5	5	4	5	5	4
            4	4	4	5	5	5	5	5	5	4
            4	4	4	4	5	5	4	5	4	5
            4	4	4	5	5	4	4	4	4	4
            5	4	4	5	5	4	5	5	5	4
            5	1	4	5	5	5	5	5	4	5
            4	4	4	5	5	5	5	5	4	5
            4	4	4	4	5	5	3	3	4	4
            4	3	4	4	5	4	4	4	3	3
            4	4	4	5	5	4	5	5	5	4
            3	4	4	4	5	4	4	3	3	4
            5	5	4	4	5	5	5	5	5	5
            4	4	4	5	5	4	5	5	5	5
            4	4	4	5	5	5	4	4	3	4
            4	4	4	4	5	4	5	5	5	5
            4	4	4	4	5	5	5	5	5	5
            4	4	4	4	5	4	4	4	4	4
            5	5	4	4	5	4	5	5	5	5
            4	4	4	4	5	5	4	4	3	3
            3	4	4	4	5	5	4	4	4	4
            5	5	4	5	5	5	4	5	5	5
            4	2	4	4	5	3	4	5	5	4
            4	5	4	4	5	4	4	5	5	4
            4	5	4	4	5	5	4	4	4	4
            4	4	4	5	5	5	5	5	5	5
            4	4	4	5	5	5	5	5	4	5
            3	3	4	5	5	5	4	4	4	3
            4	4	4	5	5	5	4	5	5	4
            5	4	4	4	5	5	4	5	5	4
            4	4	4	4	5	5	5	5	5	5
            4	4	4	5	5	5	4	5	5	4
            3	3	4	5	5	5	5	5	5	5
            5	4	4	5	5	4	5	5	5	5
            4	4	4	5	5	4	4	5	4	5
            4	4	4	5	5	5	4	5	5	5
            4	4	4	4	5	5	4	4	4	4
            4	4	4	5	5	5	5	5	5	5
            5	5	4	5	5	5	5	5	5	5
            4	4	4	5	5	5	5	5	5	5
            4	4	4	4	5	4	4	5	4	5
            4	4	4	5	5	5	5	5	5	5
            4	4	4	5	5	5	5	5	5	5
            4	4	4	5	5	4	5	5	5	5
            4	4	4	5	5	5	5	4	5	4
            4	3	5	4	5	5	5	5	4	5
            4	5	5	5	5	4	5	5	5	4
            5	5	5	5	5	5	5	5	5	5
            5	2	5	5	5	3	4	3	3	3
            4	4	5	5	5	5	5	5	5	5
            3	5	5	4	5	4	5	5	5	5
            4	4	5	3	5	4	5	5	5	5
            5	4	5	5	5	5	5	5	5	5
            4	4	5	5	5	5	4	4	4	4
            5	5	5	5	5	5	4	5	5	5
            5	4	5	4	5	3	5	5	4	5
            2	4	5	4	5	5	4	4	5	5
            5	5	5	5	5	5	5	5	5	4
            2	5	5	4	5	5	5	5	5	5
            5	5	5	4	5	4	5	5	5	4
            5	5	5	5	5	5	5	5	5	5
            4	5	5	5	5	5	5	5	5	5
            5	5	5	5	5	4	3	4	3	4
            4	5	5	5	5	5	5	5	5	5
            5	5	5	5	5	5	5	5	5	5
            5	5	5	5	5	4	5	5	5	5
            5	5	5	5	5	5	5	5	5	5
            4	4	5	5	5	3	5	5	5	5
            5	5	5	4	5	5	5	5	5	5
            5	5	5	5	5	5	5	5	5	5
            5	5	5	4	5	4	5	5	5	5
            5	5	5	5	5	4	5	5	5	5
            5	5	5	5	5	4	5	5	4	4
            5	5	5	5	5	5	5	5	5	4
            5	5	5	5	5	5	5	5	5	5
            4	4	5	5	5	4	4	5	5	5
            4	4	5	4	5	4	4	4	4	4
            5	5	5	5	5	5	3	4	4	4
            5	5	5	5	5	5	5	5	5	5
            5	5	5	5	5	5	5	5	5	5
            3	5	5	5	5	5	5	5	5	5
            5	5	5	5	5	4	2	4	4	4
            5	5	5	5	5	5	5	5	5	4
            5	5	5	5	5	5	5	5	5	5
            4	4	5	3	5	4	4	4	4	4
            5	5	5	5	5	5	5	5	5	5
            3	3	5	5	5	5	2	5	5	5
            5	5	5	5	5	5	5	5	5	5
            4	5	5	4	5	4	3	5	2	5
            5	5	5	5	5	5	5	5	5	5
            5	4	5	5	5	5	5	5	5	5
            5	5	5	4	5	4	5	5	5	5
            4	4	5	4	5	4	5	5	5	5
            5	5	5	5	5	4	5	5	5	5
            5	5	5	4	5	5	4	4	5	4
            5	5	5	5	5	5	2	5	5	5
            3	3	5	5	5	4	4	4	4	4
            4	4	5	5	5	2	2	5	1	5
            5	5	5	5	5	5	5	5	5	5
            5	5	5	4	5	5	4	4	4	5
            5	5	5	5	5	5	5	5	5	5
            3	3	5	5	5	5	5	5	5	5
            4	5	5	5	5	5	5	5	5	5
            5	5	5	5	5	5	5	5	5	5
            5	5	5	5	5	5	5	5	5	5
            5	5	5	5	5	5	5	5	5	5
            5	5	5	4	5	4	4	4	4	4
            5	5	5	5	5	4	5	5	5	5
            4	4	5	5	5	5	5	5	2	5
            4	4	5	5	5	4	5	5	5	5
            5	5	5	5	5	5	5	5	5	5
            4	4	5	5	5	5	5	5	5	5
            5	4	5	5	5	5	5	5	2	5
            5	5	5	5	5	5	5	5	5	5
            5	5	5	5	5	5	5	5	5	5
            5	5	5	5	5	5	5	5	5	5
            4	4	5	4	5	5	5	5	4	4
            5	5	5	4	5	5	5	5	5	5
            5	5	5	5	5	5	5	5	5	5
            4	5	5	5	5	5	5	5	5	5
            5	5	5	5	5	5	4	4	4	4
            5	4	5	5	5	5	5	5	5	4
            5	5	5	5	5	5	5	5	5	5
            5	5	5	5	5	5	5	5	3	5
            4	4	5	5	5	5	5	5	5	5
            4	5	5	5	5	4	5	5	5	4
            4	5	5	4	5	5	4	4	4	4
            5	5	5	5	5	5	4	4	5	5
            3	5	5	5	5	5	5	5	5	5
            4	4	5	5	5	5	5	5	5	5
            5	5	5	5	5	5	5	5	5	5
            5	2	5	5	5	5	5	5	5	5
            5	4	5	5	5	5	5	5	5	5
            5	5	5	5	5	5	5	5	5	5
            5	5	5	5	5	5	5	5	5	5
            5	5	5	5	5	5	5	5	5	5
            4	4	5	5	5	5	4	5	4	4
            5	5	5	5	5	5	5	5	5	5
            5	5	5	5	5	5	5	5	5	5
            5	5	5	5	5	5	5	5	5	5
            4	5	5	5	5	5	5	5	5	5
            4	4	5	5	5	5	5	5	5	5
            4	5	5	5	5	4	5	5	4	5
            5	5	5	5	5	4	5	5	5	5
            4	4	5	5	5	5	4	5	5	5
            4	5	5	5	5	4	5	5	5	5
            5	5	5	5	5	5	5	5	5	5
            5	5	5	5	5	5	4	4	5	5
            5	5	5	4	5	5	5	5	2	2
            3	5	5	5	5	5	4	5	5	5
            4	5	5	5	5	5	4	4	5	5
            ",  mode="r", theme="white"),
 plotOutput("Pout"),
 textAreaInput("model", "Regression Model Specification(Dependent var ~ Independent variables).You can put simply . on the Right hand side in case all the variables in the dataset except dependent variable are to be considered as independent variables", value = 'q10 ~ q1 + q4'),
  sliderInput("train_num", label = "Enter the proportion of training dataset:",
              min = 0.6, max = 1, value = 0.6, step = 0.01),
  verbatimTextOutput("printpaper1")


  )

# Define server logic required to draw a histogram
server <- function(input, output) {

  output$preface <-renderPrint({

    cat(sprintf("\nDr.  Kartikeya Bolar\n"))
    cat(sprintf("\nAssociate Professor and Area  Co-Chair\n"))
    cat(sprintf("\nOperations and Information Science\n"))
    cat(sprintf("\nT A  Pai Management Institute\n"))

  })

  output$Pout <-renderPlot({
    if(input$Choice > 0)
    {
      get.text <- reactive({input$text})
    if(is.null(get.text())){return ()}
    dataframe<- read.table(text = get.text(),header = TRUE)
    }
    if(input$Choice < 1)
    {file1 <- input$file
    if(is.null(file1)){return()}
    data= read.table(file=file1$datapath, sep=",", header = TRUE)
    if(is.null(data())){return ()}
    dataframe = data

    }
    corrgram(dataframe,order = TRUE,lower.panel = panel.shade,upper.panel = panel.pie,main ="Corrgram")
  })




  output$printpaper1 <-renderPrint({
   # print(input$Var1)

    modelmydata = input$model
    if(input$Choice > 0)
    {
    get.text <- reactive({input$text})
    if(is.null(get.text())){return ()}
    dataframe<- read.table(text = get.text(),header = TRUE)
    }
    if(input$Choice < 1)
    {file1 <- input$file
    if(is.null(file1)){return()}
    data= read.table(file=file1$datapath, sep=",", header = TRUE)
    if(is.null(data())){return ()}
    dataframe = data

    }
    prop = input$train_num
    #index1 = grep(input$Var1,colnames(dataframe))
    #print(index1)
    set.seed(1)
    train.rows = sample(row.names(dataframe),dim(dataframe)[1]*prop)
    dataframet = dataframe[train.rows,]
    valid.rows = setdiff(row.names(dataframe),train.rows)
    dataframev = dataframe[valid.rows,]
    mydatamodel= lm(modelmydata,data = data.frame(dataframet))
    terms =  mydatamodel$terms
    temp = data.frame(attr(terms,"dataClasses"))
    temp$variables  = row.names(temp)
    dependent = temp$variables[1]
    options(scipen = 99999)
    cat(sprintf("Data Analysis and Interpretation\n"))
    sum =  summary(mydatamodel)
    d = data.frame( t(data.frame(sum$fstatistic)))
    samplesizetrain =  d$numdf + d$dendf + 1
    samplesizevalid = nrow(dataframe) - samplesizetrain
    a = sum$coefficients
    b = data.frame(t(a))
    b$X.Intercept.=NULL
    a = data.frame(t(b))
    a$Std..Error =NULL
    a$IndVar =row.names(a)
    betas = lm.beta(mydatamodel)
    row.names(a) = 1:nrow(a)
    a$StdEst = round(betas,2)
    a$Estimate =NULL
    a$t_stat=  round(a$t.value,2)
    a$t.value = NULL
    a$P_value = round(a$Pr...t..,4)
    a$Pr...t.. =NULL
    a = a[order(a$StdEst,decreasing = TRUE),]
    cat(sprintf("\n The sample size of the training data set is %g\n",samplesizetrain))
    cat(sprintf("\n The sample size of the validation data set is %g\n",samplesizevalid))
    cat(sprintf("\n The Dependent variable is %s",dependent))
    cat(sprintf("\nThe independent variables are as follows :"))
    print(a$IndVar)
    cat(sprintf("\n 1. Goodness of Fit of the Model\n"))

    cat(sprintf(" It is given by RSquare(Coefficient of Determination) which is %g\n",sum$r.squared))

    rsqp = round(sum$r.squared *100,2)
    cat(sprintf("It means  %g independent variables together are able to explain  %g percent of the changes in the dependent variable\n",d$numdf,rsqp))

    cat(sprintf("\n 2. Hypothesis testing at 5 percent level of significance\n"))
    cat(sprintf("\nResearcher's Hypothesis(Alternate): The independent variables influence(are related to) dependent variable\n"))
    print(anova(mydatamodel))
    cat(sprintf("\n To test for the overall existence/significance of the model(relationships) we need to check the F-statistic which is %g\n" , d$value))
    cat(sprintf("\n After checking the F-statistic value we can conclude that the model is %s",ifelse(d$value > 4,"significant","insignificant")))
    #cat(sprintf("\n To test for the relationship of independent variables with the dependent variable individually we need to check the individual t-statistic or p-value\n"))
    cat(sprintf("\nSummary of the regression model with standardized estimates in decreasing\n"))
    row.names(a)= 1:nrow(a)
    print(a)
    cat(sprintf("\n Summary of the regression model with unstandardized estimates\n"))
    print(sum)

    if(prop < 1)
    {
      cat(sprintf("\n 3. Accuracy(Predictive Power) of the Model\n"))
      prediction = predict.lm(mydatamodel,newdata = dataframev)
      terms =  mydatamodel$terms
      temp = data.frame(attr(terms,"dataClasses"))
      temp$variables  = row.names(temp)
      dependent = temp$variables[1]
      indexdependent= grep(dependent, colnames(dataframev))
    cat(sprintf("\n The details of accuracy of the model in the validation data set are are as follows\n"))
    acc = data.frame(accuracy(prediction,dataframev[,indexdependent[1]]))
    psuedoRSquare =  cor(dataframev[,indexdependent[1]],prediction)^2
    print(acc)
    cat(sprintf("\n The Psuedo RSquare indicated the accuracy of the model is %g",psuedoRSquare))
    }




  })










}

# Run the application
shinyApp(ui = ui, server = server)

