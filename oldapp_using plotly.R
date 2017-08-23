#problem with this is it doesn't keep axis names! plotly doesn't currently support expressions

library(ggplot2);library(scales);require(plotly);require(shinythemes)
library(Cairo)   # For nicer ggplot2 output when deployed on Linux
birds<-read.csv("freqdata.csv")
ui <- fluidPage(
  #modify CSS style 
  theme=shinytheme("spacelab"),
  tags$head(
    tags$style(HTML("hr {border-top: 1px solid #000000;}"))),
     
  titlePanel("Exploring how vocal frequency scales with body size"),
  hr(),
  verbatimTextOutput(paste(" ",sep="\n")),
  radioButtons("ax","Label Format:",c("Exponential"="exp","Integer"="int")),
        
          column(6,plotlyOutput("plot1", height = 300))
        
      )

server <- function(input, output) {

  output$plot1 <- renderPlotly({

  g0<-ggplot(birds,aes(y=log10Hz,x=log10mass))+geom_point()+geom_smooth(method="lm")+xlab("Body Mass (g)")+ylab("Vocal Frequency (Hz)")+scale_x_log10(breaks=trans_breaks('log10',function(x) 10^x),labels=trans_format('log10',math_format(10^.x,format=identity)))+scale_y_continuous(breaks=seq(0,4,1),labels=c("1","10","10^2","10^3","10^4")+coord_fixed())
 
ggplotly(g1,dynamicTicks=F)%>%layout(margin=list(l=50,r=50,t=50,pad=0))
    })

 

}

shinyApp(ui, server)