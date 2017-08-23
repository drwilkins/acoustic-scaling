#not using plotly...doesn't support expressions in labels

library(ggplot2);library(scales)
library(Cairo)   # For nicer ggplot2 output when deployed on Linux
birds<-read.csv("freqdata.csv")
fams<-as.character(unique(birds$fam))
ui <- fluidPage(
  #modify CSS style 
  tags$head(
    tags$style(HTML("hr {border-top: 1px solid #000000;}"))),
     
  titlePanel("Exploring how vocal frequency scales with body size"),
  hr(),
#>User toggles
   fluidRow(
    column(1,radioButtons("axlab","Label Format:",c("Exponential"="exp","Integer"="int"))),
    column(2,selectInput("highlight","Which family to highlight?",choices=c("none",fams),selected="none")),
     column(6,
       p("Selected Point(s):"),
     tableOutput("click_info"))
     
    ),#end fluidRow 1
##############
#> Graphical Output 
   fluidRow(
          column(5,
            plotOutput("plot1", height = 500,width=500,click="plot1click")),
     verbatimTextOutput("clue")#main MTE plot
        
      )#end fluidRow 2
)#End ui


#*********************************************************************
server <- function(input, output) {
#get user point click
clicked<-NA

output$click_info<-renderTable({clk<-nearPoints(df=birds,coordinfo=input$plot1click,xvar="log10mass",yvar="log10Hz",addDist=F,threshold=3)[,c("Taxon","family","fam","F_Hz","body_mass")] 
  names(clk)<-c("Genus_species","Scientific Fam", "Common Fam","Freq (Hz)","Mass (g)")
  clk
})

  output$plot1 <- renderPlot({
    
    g0<-ggplot(birds,aes(y=log10Hz,x=log10mass))+geom_point()+geom_smooth(method="lm")+xlab("Body Mass (g)")+ylab("Vocal Frequency (Hz)")+theme_linedraw()+theme(axis.text=element_text(size=16),axis.title=element_text(size=16,face="bold"))

  #if user selects Exponential Label format, change axis labels accordingly, else display as integers
if(input$axlab=="exp"){
  g0<-g0+scale_y_continuous(breaks=seq(0,4,1),labels=math_format(10^.x)(seq(0,4,1)),limits=c(2.,4))+scale_x_continuous(breaks=seq(0,4.1,1),labels=math_format(10^.x)(seq(0,4,1)),limits=c(0,4.25))
  }else{
  g0<-g0+scale_y_continuous(breaks=seq(0,4,1),labels=sapply(seq(0,4,1),function(x)10^x),limits=c(2.,4))+scale_x_continuous(breaks=seq(0,4.1,1),labels=sapply(seq(0,4.1,1),function(x)10^x),limits=c(0,4.25))}  
  
if(input$highlight!="none"){#if a family is selected, make those points red diamonds
  highlighted=subset(birds,fam==input$highlight)
  g1=g0+geom_point(data=highlighted,aes(x=log10mass,y=log10Hz),col="#FF0F1B",size=4,shape=18)}else{g1<-g0}
  


# If point clicked, distinguish that point

clk2<-isolate(nearPoints(df=birds,coordinfo=input$plot1click,xvar="log10mass",yvar="log10Hz",addDist=F,threshold=3)[,c("Taxon","family","fam","F_Hz","body_mass")] )
output$clue<-renderPrint(clk2)

#  if(nrow(clicked)>0){
  #   clicked2<-birds[which(clicked$Genus_species%in%birds$Taxon),]
  #   g1<-g1+geom_point(df=clicked2,aes(x=log10mass,y=log10Hz),col="yellow")
  #   }
  return(g1)
  
    }) #end renderPlot
  

  
  }#end server

shinyApp(ui, server)