library(ggplot2);library(scales);require(ape);require(geiger);require(png)
library(Cairo)  # For nicer ggplot2 output when deployed on Linux
birds<-read.csv("freqdata.csv")
birdtree<-read.tree("singletree.tre")
fams<-sort(as.character(unique(birds$fam)))
ui <- fluidPage(
  titlePanel(windowTitle="Bird Vocal Scaling",h1("Exploring how vocal frequency scales with body size",style="font-family: 'Courier New';color: #444444;")),
  p("Each data point represents the average body mass and song pitch for a different bird species (n=795 total species)",style="font-family: 'Courier New';color: #444444;"),
  
  #modify CSS style 
  tags$head(
    tags$style(HTML("hr {border-top: 1px solid #444444;}"))),
  hr(),
#>User toggles
   fluidRow(style="background-color: #e3e3e1;",
    column(2,radioButtons("axlab","Label Format:",c("Exponent"="exp","Integer"="int"))),
    column(3,selectInput("highlight","Which family to highlight?",choices=c("none",fams),selected="none")),
     column(2,checkboxInput("mainline","Fit Overall Line?",FALSE),
       checkboxInput("famline","Fit Family Line?",FALSE),
       checkboxInput("phyl","Show Phylogeny?",FALSE))
    ),#end fluidRow 1
##############
#> Graphical Output 
   fluidRow(
      column(12,column(5,plotOutput("plot1",click="plot1click")), #main MTE plot
      column(6,plotOutput("phylogeny"))) ),#output phylogeny; end fluidRow2 
    fluidRow(
      column(6,
      h5("Selected Point(s):"),
      tableOutput("click_info")))#end fluidRow 3
)#End ui


#*********************************************************************
server <- function(input, output) {

#store reactive list to access outside reactive functions
vals<-reactiveValues(clicked=data.frame())
  
  output$plot1 <- renderPlot({
    
#if a family is selected, make those points red diamonds
if(input$highlight!="none"){
  highlighted=subset(birds,fam==input$highlight)
  unhighlighted=subset(birds,fam!=input$highlight)
}else{highlighted=NA;unhighlighted=birds}
    
#Initial plot definition (gets modified by user input)
  g0<-ggplot(unhighlighted,aes(y=log10Hz,x=log10mass))+geom_point(size=3,stroke=1.5,shape=21,alpha=.75)+xlab("Body Mass (g)")+ylab("Vocal Frequency (Hz)")+theme_linedraw()+theme(axis.text=element_text(size=16),axis.title=element_text(size=16,face="bold"),plot.title=element_text(hjust=.5))+ggtitle("Click a point for detailed info")
  if(input$mainline==T){g0<-g0+geom_smooth(method="lm",col="#6C58EF",fill="#6C58EF")}
  
  #Add highlighted points if a family selected
  if(length(highlighted)>1){g0<-g0+geom_point(data=highlighted,aes(x=log10mass,y=log10Hz),col="#FF0F1B",size=3,shape=5,stroke=1.5,alpha=.75)}
  
  #if user selects Exponential Label format, change axis labels accordingly, else display as integers
if(input$axlab=="exp"){
  g1<-g0+scale_y_continuous(breaks=seq(0,4,1),labels=math_format(10^.x)(seq(0,4,1)),limits=c(2.,4))+scale_x_continuous(breaks=seq(0,4.1,1),labels=math_format(10^.x)(seq(0,4,1)),limits=c(0,4.25))
  }else{
  g1<-g0+scale_y_continuous(breaks=seq(0,4,1),labels=sapply(seq(0,4,1),function(x)10^x),limits=c(2.,4))+scale_x_continuous(breaks=seq(0,4.1,1),labels=sapply(seq(0,4.1,1),function(x)10^x),limits=c(0,4.25))}  
  


  #If point selected, distinguish that point
  if(nrow(vals$clicked)>0){
    clickeddf<-birds[which(birds$Taxon%in%vals$clicked[,"Taxon"]),]
    vals$clickedspp<-clickeddf$Taxon
    shp<-rep(16,nrow(clickeddf))
    #for taxa that are in the selected family, make them a filled diamond
    if(input$highlight!="none"){
    highclick<-which(clickeddf$Taxon%in%highlighted[,"Taxon"])
    if(is.numeric(highclick)){shp[highclick]<-18}
    }
    g2<-g1+geom_point(data=clickeddf,aes(x=log10mass,y=log10Hz),col="#E9CE2C",size=3,shape=shp)
  }else{g2<-g1}
  
  #Add family trend line to selected family
  if(input$highlight!="none" & input$famline==T)
  {g2<-g2+geom_smooth(method="lm",data=highlighted,aes(x=log10mass,y=log10Hz),col="#FF0F1B",fill="#FF0F1B")}
  return(g2+theme(aspect.ratio = 1))
  
    }) #end renderPlot

#get user point click
observeEvent(input$plot1click, {
  vals$clicked<-nearPoints(birds,coordinfo=input$plot1click,xvar="log10mass",yvar="log10Hz",addDist=F,threshold=5)[,c("Taxon","phyloname2","family","fam","F_Hz","body_mass")]
  clk<-vals$clicked[,-1]#assigned to reactiveValue object
  #render Table & rename columns for output
output$click_info<-renderTable({
  names(clk)<-c("Genus species","Scientific Fam", "Common Fam","Freq (Hz)","Mass (g)")
clk})
})


#ONLY output phylogeny if checked box{
output$phylogeny<-renderPlot({
  if(input$phyl==T){
  edgecols<-rep("black",length(birdtree$edge))
  #if family selected, make edges red & add family pic
  if(input$highlight!="none"){
    #find out which edges connect to the species in the family of interest & replace colors
   edgecols[which.edge(birdtree,as.character(birds$Taxon[which(birds$fam==input$highlight)]))]<-"#FF0F1B"
   
    ###Look up appropriate family pic
    #Does pic exist for this fam?
   
   #Not run
   #Which missing?
   #unique(birds$family)[-which(unique(birds$family)%in%sapply(piclist,tools::file_path_sans_ext))]
   
    currfam<-as.character(birds$family[which(birds$fam==input$highlight[1])[1]])
    piclist<-list.files("www/")
    fampicindx<-match(currfam,sapply(piclist,tools::file_path_sans_ext))
    if(!is.na(fampicindx)){
      fampic<-readPNG(paste0("www/",piclist[fampicindx]))
       aspect<-dim(fampic)[2]/dim(fampic)[1]
       #if wider than tall, scale image to max width; else to max height
        if(aspect>=1.5){
          width=2
          height=2/aspect
          x1=.25
        }else{x1=.5;height=1.5;width=1.5*aspect}
    }
}#end family highlighting code
  par(mar=c(0,0,2,0))
  plot.phylo(birdtree,show.tip.label=F, root.edge=T,label.offset=3,edge.color=edgecols,main="Phylogeny (Evolutionary family tree)")
  
#### Highlight phylogeny tips of clicked points  
  #if something has been clicked, put a yellow tip label
 if(nrow(vals$clicked)>0){
  spprows<-match(vals$clickedspp,birdtree$tip.label)
  tiplabels(text="<",col="#E9CE2C",tip=spprows,adj=c(-.1,.5),bg="gray40",cex=2.)
 }else{tiplabels(text="",col="transparent",tip=NA,frame="none")}#don't add tiplabels if nothing clicked
  
    #plot family image when selected & pic exists
  if(input$highlight!="none"&&!is.na(fampicindx)){
     par(usr=c(0,10,0,10))#rescale plot device to add pic in a sensible way
     rect(x1-.2,8.25-.2,x1+width+.2,8.25+height+.2,col="#FF0F1B",density=70)
     rasterImage(fampic,x1,8.25,x1+width,8.25+height)
    }#End fam pic generation
  
  
#if user didn't want the phylogeny (default), just plot blank
  }else{par(mar=c(0,0,2,0))
    plot(1,1, type="n", axes=F, xlab="hello", ylab="")}
  
})#end phylogeny backend


  }#end server

shinyApp(ui, server)