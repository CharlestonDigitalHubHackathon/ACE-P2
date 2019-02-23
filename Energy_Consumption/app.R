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
source("C:/Users/598522/Documents/Rprojects/Hackathon/Energy_Consumption/Source_Clean.R")
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
   titlePanel("Old Faithful Geyser Data"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        selectInput("Country","Select Country: ",Country_Choice),
        selectInput("Category","Select Category",Category_Choice)
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotlyOutput("distPlot"),
         plotlyOutput("distPlot2"),
         tableOutput("tab1")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$distPlot <- renderPlotly({
      # generate bins based on input$bins from ui.R
      x    <-  Data$YEAR[which(Data$COUNTRY==input$Country & Data$CATEGORY_ROLLUP==input$Category)]
      y    <- Data$KWH[which(Data$COUNTRY==input$Country & Data$CATEGORY_ROLLUP==input$Category)]
      data.cur<-data.frame("Year"=x,"Consumption"=y)
      
      
      # draw the histogram with the specified number of bins
      p<-plot_ly(data.cur,x=~Year,y=~Consumption,mode="lines",trace="scatter")
      p
      
  })
   
   output$tab1<-renderTable(
     Data[which(Data$COUNTRY==input$Country),]
   )
   
   
   output$distPlot2 <- renderPlotly({
     x    <-Data$YEAR[which(Data$COUNTRY==input$Country)]
     y    <-Data$KWH[which(Data$COUNTRY==input$Country)]
     z    <-Data$CATEGORY_ROLLUP[which(Data$COUNTRY==input$Country)]
     cat.cur<-unique(z)
     x.new<-x[which(z==cat.cur[1])]
     y.new<-y[which(z==cat.cur[1])]
     cur.data<-data.frame("Year"=x.new,"Consumption"=y.new)
     
     p<-plot_ly(cur.data, x = ~Year, y = ~Consumption, type = 'scatter', mode = 'lines',name = cat.cur[1])
     for(j in 2:length(cat.cur)){
       x.new<-x[which(z==cat.cur[j])]
       y.new<-y[which(z==cat.cur[j])]
       
       p<-add_trace(p, y=~y.new, x=~x.new, mode="lines",name=cat.cur[j],trace="scatter")
       
     }
     
     # a <- list(
     #   autotick = FALSE,
     #   ticks = "outside",
     #   tick0 = 0,
     #   dtick = 0.25,
     #   ticklen = 5,
     #   tickwidth = 2,
     #   tickcolor = toRGB("blue")
     # )
     p<-p %>%
       layout(yaxis=list(range=c(0,max(y))))
     p
     
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

