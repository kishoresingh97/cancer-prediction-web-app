

data = read.csv("breast-cancer-wisconsin.csv")
colnames(data)= c("id","Clump_Thickness","Uniformity_of_Cell_Size",
                  "Uniformity_of_Cell_Shape","Marginal_Adhesion","Single_Epithelial_Cell_Size",
                  "Bare_Nuclei","Bland_Chromatin","Normal_Nucleoli","Mitoses","Class")
cdata=  data[,2:11]
cdata$Class = factor( cdata$Class, levels = c("2","4"), labels = c("0","1"))

str(cdata)

library(caTools)

split= sample.split(cdata$Class, SplitRatio = 0.8)

train= subset(cdata, split==T)
test= subset(cdata, split==F)
library(rpart)

classifier = rpart(Class~., data = train, method = "class",control=rpart.control(minsplit = 1) )

summary(classifier)

Y_pred=  predict(classifier, newdata = test[-10], type = "class")

cm = table( Y_pred, test$Class)


#web app
library(shiny)
library(shinythemes)


ui= fluidPage( theme = shinytheme("yeti"),
  titlePanel("Predict Breast cancer "),
  sidebarLayout(
    sidebarPanel( width = 2,
      sliderInput("a","Clump Thickness", min(cdata$Clump_Thickness), max(cdata$Clump_Thickness),""),
      sliderInput("b","Uniformity of Cell Size", min(cdata$Uniformity_of_Cell_Size), max(cdata$Uniformity_of_Cell_Size),""),
      sliderInput("c", "Uniformity of Cell Shape",min(cdata$Uniformity_of_Cell_Shape), max(cdata$Uniformity_of_Cell_Shape),""),
      sliderInput("d","Marginal Adhesion", min(cdata$Marginal_Adhesion), max(cdata$Marginal_Adhesion),""),
      sliderInput("e","Single Epithelial Cell Size", min(cdata$Single_Epithelial_Cell_Size), max(cdata$Single_Epithelial_Cell_Size),""),
      
      selectInput("f","Bare Nuclei",choices = unique(cdata$Bare_Nuclei)),
      
      sliderInput("g","Bland Chromatin", min(cdata$Bland_Chromatin), max(cdata$Bland_Chromatin),""),
      sliderInput("h","Normal Nucleoli", min(cdata$Normal_Nucleoli), max(cdata$Normal_Nucleoli),""),
      sliderInput("i","Mitoses", min(cdata$Mitoses), max(cdata$Mitoses),""),
      actionButton("submit","submit")
    ),
    mainPanel(
      DT:: dataTableOutput ("class"),
      tags$h3("Report"),
      textOutput("msg"),
      tags$h3("Training dataset"),
      downloadButton("download","Download"),
      DT:: dataTableOutput("testtable")
    )
  )
)


server = function(input, output){
  
  df = eventReactive(input$submit,
                     {
    data.frame(
     Clump_Thickness=input$a,
     Uniformity_of_Cell_Size= input$b,
    Uniformity_of_Cell_Shape= input$c,
    Marginal_Adhesion= input$d,
    Single_Epithelial_Cell_Size= input$e,
    Bare_Nuclei= input$f,
    Bland_Chromatin= input$g,
    Normal_Nucleoli= input$h,
    Mitoses= input$i
    ) } )
  category = reactive( predict(classifier, newdata = df(), type="class"))
  report = reactive(ifelse(category()==1, "Positive","Negative") )
 
   output$class = DT:: renderDataTable ({ cbind(df(),category())})

  output$msg = renderPrint({ cat("The report came out ",report())})
  
  output$download = downloadHandler(  filename =  function()( paste("Test data",".csv", sep = "")), 
                                     content= function(file){write.csv(cdata , file) }
                                     )
  output$testtable = DT:: renderDataTable({cdata})
  
  #output$class = renderTable({ as.table( df())})
  }

shinyApp(ui=ui, server= server)












