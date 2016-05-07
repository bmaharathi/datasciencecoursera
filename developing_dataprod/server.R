library(shiny)
data("mtcars")
library(GGally)
library(ggplot2)

shinyServer(
    function(input, output) {
        output$compplot <- renderPlot({
            cols <- as.numeric(input$Par1)
            if (NROW(cols)<1) {
                #ggpairs(mtcars[,c(1,2,3,4,9)], title = "please select atleast 1 parameters")
                plot(1,cex=0.001,axes=FALSE, frame.plot=FALSE,xlab = '',ylab='')
                text(c(1,1),"Please select atleast one parameter to compare",cex=2,col='red')
                
            }
            else{
                ggpairs(mtcars[,c(1,cols)], title = "MPG performance comparison with selected parameter")
            }
        })
            
            
            
        #output$oPar1 <- renderPrint({NROW(as.numeric(input$Par1))})
        output$oid2 <- renderPrint({input$id2})
        
        output$carlist <- renderDataTable({
            
            dat <- mtcars[mtcars$mpg> as.numeric(input$mpg) & mtcars$cyl> as.numeric(input$cyl) & 
                       mtcars$disp> as.numeric(input$disp) & mtcars$hp > as.numeric(input$hp) & 
                       mtcars$am == as.numeric(input$am),c(1,2,3,4,9)]
            dat$carname <-row.names(dat)
            dat
        })
    }
)