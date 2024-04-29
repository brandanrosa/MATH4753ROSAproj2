library(shiny)
library(purrr)
library(rootSolve)

spruce.df = read.csv("SPRUCE.csv")

d = spruce.df$BHDiameter

coeff2 <- function(xk){
  df=within(spruce.df,
            {
              X <-(BHDiameter-xk)*(BHDiameter>xk)
            }
  )
  lmp <- lm(Height ~ BHDiameter + X , data=df)
  tmp <- summary(lmp)
  coef(lmp)
}

myf3 = function(x, xk, coef){
  coef[1]+coef[2]*(x) + coef[3]*(x-xk)*(x-xk>0)
}

ploty <- function(xk) {
  with(spruce.df,plot(BHDiameter,
                      Height,
                      pch = 21,
                      cex=1.5,
                      bg="hotpink",
                      col = "darkgreen",
                      main="Piecewise Regression"))

  cf <- coeff2(xk)
  curve(myf3(x, xk, coef = cf), add = TRUE, lwd=3, col="darkgreen")
}
##############################################################################

# Define UI for application that draws a histogram
ui <- fluidPage(

  # title
  titlePanel("Piecewise Regression Analysis"),

  # Sidebar
  sidebarLayout(
    sidebarPanel(
      sliderInput("xk",
                  "Choose knot:",
                  min = min(d),
                  max = max(d),
                  value = 17.44165,
                  step=0.01),

      sliderInput("intervalroot",
                  "choose L and U for root interval:",
                  min = min(d),
                  max = max(d),
                  value = c(15,17.55),
                  step=0.01)

    ),

    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("regressPlot",
                 click = "plotclick"),
      plotOutput("R2"),
      tableOutput("root"),
      # table of data
      tableOutput("tab"),
      plotOutput("allroots")
    )
  )
)

# Server
server <- function(input, output, session) {

  observeEvent(input$plotclick, {
    updateSliderInput(session, "xk", value = input$plotclick$x)
  })

  output$tab <- renderTable(spruce.df)

  output$regressPlot <- renderPlot({
    ploty(input$xk)

    knotstuff.df=within(spruce.df, X<-(BHDiameter-input$xk)*(BHDiameter>input$xk))
    lmp = lm(Height ~ BHDiameter + X, data = knotstuff.df)
    tmp=summary(lmp)

    points(input$xk,myf(input$xk,input$xk,coef=tmp$coefficients[,"Estimate"] ),col="black",pch=21,bg="red",cex=2)

    points(uroot()$root,myf(uroot()$root,uroot()$root,coef=tmp$coefficients[,"Estimate"] ),col="black",pch=21,bg="navy",cex=2)

    text(25,10, paste("R sq.=",round(tmp$r.squared,4) ))
  })

  uroot = reactive({
    intv = input$intervalroot
    uniroot(f=rsqdash, interval=intv, h=0.001,data=spruce.df, extendInt = "yes" )
  })

  urootall = reactive({
    intv = input$intervalroot
    uniroot.all(f=rsqdash, interval=intv, h=0.001,data=spruce.df )
  })

  output$R2 <- renderPlot({
    dsmooth = seq(min(d),max(d),length=1000)
    r2=map_dbl(dsmooth, ~rsq(.x,data=spruce.df))
    plot(dsmooth,r2,pch=21,bg="blue",
         ylab = expression(R^2 ),
         xlab = "knot",
         cex=0.5,
         main="Which Knot",
         type="p",
         ylim = c(min(r2),1.1*max(r2)))
    intv = input$intervalroot
    rts=uroot()
    r2=rsq(rts$root,data=spruce.df)
    abline(v=seq(floor(min(d)), ceiling(max(d)), by=1),lwd=0.5,col="pink")
    abline(v=rts$root,h=rsq(rts$root,data=spruce.df))
    text(rts$root, r2*1.05,paste("knot is:", rts$root))
    axis(3, round(rts$root,4), col = "maroon")
    axis(4, round(r2,4),col="maroon")
  })

  output$root<-renderTable({

    intv = input$intervalroot
    rts=uroot()
    as.data.frame(rts)
  })
}

# Run the application
shinyApp(ui = ui, server = server)

