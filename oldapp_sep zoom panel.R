library(ggplot2);library(scales);require(plotly)
library(Cairo)   # For nicer ggplot2 output when deployed on Linux
birds<-read.csv("freqdata.csv")
ui <- fluidPage(
  fluidRow(
        column(width = 8, class = "well",
      h4("Left plot controls right plot"),
      fluidRow(
        column(width = 6,
          plotOutput("plot1", height = 300,
            brush = brushOpts(
              id = "plot2_brush",
              resetOnNew = TRUE
            )
          )
        ),
        column(width = 6,
          plotOutput("plot2", height = 300)
        )
      )
    )

  )
)

server <- function(input, output) {

  # -------------------------------------------------------------------
  # Single zoomable plot (on left)
  ranges <- reactiveValues(x = NULL, y = NULL)

  # output$plot1 <- renderPlot({
  #   ggplot(mtcars, aes(wt, mpg)) +
  #     geom_point() +
  #     coord_cartesian(xlim = ranges$x, ylim = ranges$y, expand = FALSE)
  # })
  # 
  # # When a double-click happens, check if there's a brush on the plot.
  # # If so, zoom to the brush bounds; if not, reset the zoom.
  # observeEvent(input$plot1_dblclick, {
  #   brush <- input$plot1_brush
  #   if (!is.null(brush)) {
  #     ranges$x <- c(brush$xmin, brush$xmax)
  #     ranges$y <- c(brush$ymin, brush$ymax)
  # 
  #   } else {
  #     ranges$x <- NULL
  #     ranges$y <- NULL
  #   }
  # })

  # -------------------------------------------------------------------
  # Linked plots (middle and right)
  ranges2 <- reactiveValues(x = NULL, y = NULL)

  output$plot1 <- renderPlot({
    ggplot(birds,aes(y=pk_freq*1000,x=body_mass))+geom_point()+geom_smooth(method="lm")+scale_x_log10(breaks=trans_breaks('log10',function(x) 10^x),labels=trans_format('log10',math_format(10^.x)))+scale_y_log10(breaks=trans_breaks('log10',function(x) 10^x),labels=trans_format('log10',math_format(10^.x)))+xlab("Body Mass (g)")+ylab("Vocal Frequency (Hz)")+ggtitle("Full View")
  })

  output$plot2 <- renderPlot({
   gzoom<-ggplot(birds,aes(y=pk_freq*1000,x=body_mass))+geom_point()+geom_smooth(method="lm")+scale_x_log10(breaks=trans_breaks('log10',function(x) 10^x),labels=trans_format('log10',math_format(10^.x)))+scale_y_log10(breaks=trans_breaks('log10',function(x) 10^x),labels=trans_format('log10',math_format(10^.x)))+xlab("Body Mass (g)")+ylab("Vocal Frequency (Hz)")+ggtitle("Zoomed View")+
      coord_cartesian(xlim = ranges2$x, ylim = ranges2$y, expand = FALSE)
 gzoom
    # ggplotly(gzoom)
  })

  # When a double-click happens, check if there's a brush on the plot.
  # If so, zoom to the brush bounds; if not, reset the zoom.
  observe({
    brush <- input$plot2_brush
    if (!is.null(brush)) {
      ranges2$x <- c(brush$xmin, brush$xmax)
      ranges2$y <- c(brush$ymin, brush$ymax)

    } else {
      ranges2$x <- NULL
      ranges2$y <- NULL
    }
  })

}

shinyApp(ui, server)