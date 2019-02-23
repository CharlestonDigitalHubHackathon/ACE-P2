library(plotly)
library(shiny)
#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

#import the Data
source("Source_Clean.R")
Data$Clean.Score<-Data$CO2/Data$KWH
inds.excludeT<-which(Data$CATEGORY_ROLLUP=="Total")
inds.exclude<-which(Data$CATEGORY_ROLLUP=="Exclude")
Data.ex<-Data[-c(inds.exclude,inds.excludeT),]
Country_Choice<-list()
un.count<-as.character(unique(Data$COUNTRY))
for(i in 1:length(unique(Data$COUNTRY))){
  Country_Choice[[un.count[i]]]<-un.count[i]
  
}

Category_Choice<-list()
un.cat<-as.character(unique(Data$CATEGORY_ROLLUP))
for(i in 1:length(unique(Data$CATEGORY_ROLLUP))){
  Category_Choice[[un.cat[i]]]<-un.cat[i]
  
}


library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

   # Application title
   titlePanel("Energy Consumption and Forecast"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        selectInput("Country","Select Country: ",Country_Choice),
        selectInput("Category","Select Category",Category_Choice),
        selectInput("Visualize","Select Visualization Technique:",c("Raw Scores"="RawScores","Score Ratio"="ScoreRatio"))
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
        tabsetPanel(type = "tabs",
                    tabPanel("outputs",
                             plotlyOutput("distPlot"),
                             plotOutput("distPlot2"),
                             tableOutput("tab1")
                    ),
                    tabPanel("Summary",
                             HTML('<p><img src="my_img.jpg"/></p>')
                             )
                    
        ))
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   #total outputs
  
   output$distPlot <- renderPlotly({
      # generate bins based on input$bins from ui.R
      x    <-  Data$YEAR[which(Data$COUNTRY==input$Country & Data$CATEGORY_ROLLUP==input$Category & Data$KWH !=0)]
      
      if(input$Visualize=="ScoreRatio"){
        y    <- Data$Clean.Score[which(Data$COUNTRY==input$Country & Data$CATEGORY_ROLLUP==input$Category & Data$KWH !=0)]
      }else{
        y    <- Data$KWH[which(Data$COUNTRY==input$Country & Data$CATEGORY_ROLLUP==input$Category & Data$KWH !=0)]
      }
      data.cur<-data.frame("Year"=x,"Consumption"=y)
      
      
      # draw the histogram with the specified number of bins
      p<-plot_ly(data.cur,x=~Year,y=~Consumption,mode="lines",trace="scatter",type = "scatter")
      if(input$Visualize=="Score Ratio"){
        p<-p %>% layout(yaxis = list(range = c(0,1)))
      }else{
        p<-p
      }
      
      p
      
  })
   
   output$tab1<-renderTable(
     Data[which(Data$COUNTRY==input$Country),]
   )
   
   
   output$distPlot2 <- renderPlot({
     x    <-Data.ex$YEAR[which(Data.ex$COUNTRY==input$Country & Data.ex$KWH!=0)]
     if(input$Visualize=="ScoreRatio"){
       y    <-Data.ex$Clean.Score[which(Data.ex$COUNTRY==input$Country & Data.ex$KWH!=0)]
     }else{
       y  <-Data.ex$KWH[which(Data.ex$COUNTRY==input$Country & Data.ex$KWH!=0)]
     }
     z    <-Data.ex$CATEGORY_ROLLUP[which(Data.ex$COUNTRY==input$Country & Data.ex$KWH!=0)]
     cat.cur<-unique(z)
     x.new<-x[which(z==cat.cur[1])]
     y.new<-y[which(z==cat.cur[1])]
     cur.data<-data.frame("Year"=x.new,"Consumption"=y.new)
     plot(cur.data$Year,cur.data$Consumption,xlim = c(1985,2030),ylim = c(0,max(y)),type = 'l')
     legend("topright","(x,y)",cat.cur,lty=rep(1,length(cat.cur)),lwd=rep(2.5,length(cat.cur)),col=seq(1:length(cat.cur)))
     p<-plot_ly(cur.data, x = ~Year, y = ~Consumption, type = 'scatter', mode = 'lines',name = cat.cur[1])
     for(j in 2:length(cat.cur)){
       x.new<-x[which(z==cat.cur[j])]
       y.new<-y[which(z==cat.cur[j])]
       cur.data<-data.frame("Year"=x.new,"Consumption"=y.new)
       points(cur.data$Year,cur.data$Consumption,type='l',col=j)
}
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

