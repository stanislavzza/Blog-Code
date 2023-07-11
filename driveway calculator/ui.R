#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# Define UI for application that draws a histogram
fluidPage(

    # Application title
    titlePanel("Driveway calculator"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
          
          #----  position ------
          sliderInput("position",
                      "Car position:",
                      min = 30,
                      max = 70,
                      step = 1,
                      value = 50),
            
            hr(), # ---------- car dims --------------
            selectInput("model", "Car model", choices = car_choices$Model),
            sliderInput("wheelbase",
                        "Wheelbase:",
                        min = 90,
                        max = 110,
                        step = .5,
                        value = 106),
            sliderInput("height",
                        "Ground clearance:",
                        min = 5,
                        max = 9,
                        step = .1,
                        value = 7.8),
            hr(), #------- driveway dims ----------------

            sliderInput("angle",
                        "Driveway angle:",
                        min = 0,
                        max = 20,
                        step = .1,
                        value = 15.4),
            sliderInput("bevel",
                        "Bevel radius:",
                        min = 0,
                        max = 20,
                        step = 1,
                        value = 3),
            hr(), #-------- bump -------------------
            sliderInput("bump_x",
                        "Bump position:",
                        min = -70,
                        max = -30,
                        step = 1,
                        value = -50),
            sliderInput("bump_width",
                        "Bump width:",
                        min = 3,
                        max = 30,
                        step = 1,
                        value = 6),
            sliderInput("bump_height",
                        "Bump height:",
                        min = 0,
                        max = 3,
                        step = .5,
                        value = 0)
        ),


        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("animationPlot"),
            plotOutput("clearancePlot")
            
        )
    )
)
