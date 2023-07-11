#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# Define server logic required to draw a histogram
function(input, output, session) {
  

   # the selector sets values for height and length
  observe({
    updateSliderInput(
      session, 'wheelbase',
      value = car_choices$Wheelbase[car_choices$Model == input$model]
    )
  })
  
  observe({
    updateSliderInput(
      session, 'height',
      value = car_choices$Clearance[car_choices$Model == input$model]
    )
  })

   measures <- reactive({
     
     # generate angle graphs
     theta_g    =  input$angle *pi / 180  # ground angle in radians
     wheelbase  =  input$wheelbase
     bevel      =  input$bevel
     height     =  input$height
     
     # independent variable is x, the distance from the back wheel to the original apex
     # horizontal position markers are from outside garage inwards:
     #  [front wheel]------y-------[bevel edge]-----[apex]-----------x---------[back wheel]
    
     df <- tibble(x = seq(wheelbase/5, 4*wheelbase/5, .1)) %>% 
       mutate( drop = x * tan(theta_g), # drop of rear wheels
                
               theta_car = asin(drop / wheelbase),
               y         = cos(theta_car)*wheelbase - x - bevel, # horizontal distance from front wheel to bevel start
               theta_max = atan(height/y), # max angle before grounding (not used)
               
               
               # clearance of apex closest to front wheel
               Clearance_front = height - y * tan(theta_car),
               
               # clearance of apex closest to back wheel
               Clearance_back  = height - (y + 2*bevel) * tan(theta_car) + # angle of car over that distance
                                 bevel*tan(theta_g), # drop of driveway 
               
               Clearance = pmin(Clearance_front, Clearance_back),
               
               # location of idealized wheel hubs
               # x runs left to right, so we'll have to reverse the x-axis on the plot
               BackWheel_x = x,
               BackWheel_y  = height - drop, # driveway drop plus height to axle 
               FrontWheel_x = x - cos(theta_car)* wheelbase,
               FrontWheel_y = height, # sitting on the garage floor
               radius       = height,  # for the geom_circle function
           
       )
     
       # adjust for bump if necessary
       if (input$bump_height > 0){
         
         tdf <- df %>% 
           filter(abs( FrontWheel_x - input$bump_x) <= input$bump_width/2) %>% 
           mutate(BumpFrontWheel_y = FrontWheel_y + input$bump_height,
                 arc_car_bump = input$bump_height / wheelbase, # because front wheel is on it
                 
                 # approx with r*theta
                 front_bonus = arc_car_bump*(x + bevel),
                 back_bonus  = arc_car_bump*(x - bevel),
                 Clearance_front = Clearance_front + front_bonus,
                 Clearance_back = Clearance_back + back_bonus,
                 BumpClearance = pmin(Clearance_front, Clearance_back)) %>% 
           select(x, BumpClearance, BumpFrontWheel_y) 
         
         df <- df %>% 
           left_join(tdf) %>% 
           mutate(Clearance = if_else(!is.na(BumpClearance), BumpClearance, Clearance),
                  FrontWheel_y = if_else(!is.na(BumpFrontWheel_y),BumpFrontWheel_y,FrontWheel_y))
       }
     
       return(df)
  })

    output$clearancePlot <- renderPlot({
      
        pos_clearance <- measures() %>% 
          filter(abs(x - input$position) < .05) %>% 
          select(Clearance) %>% 
          pull() %>% 
          round(1)
        
        pos_clearance_label <- tibble(x = input$position,
                                      y = 2,
                                      label = pos_clearance)
          

        measures() %>% 
          ggplot(aes(x = x, y = Clearance)) +
          geom_line(size = 1) +
  #        geom_vline(xintercept = wheelbase/2, linetype = "dashed") +
          geom_hline(yintercept = 0, color = "red") +
          geom_vline(xintercept = input$position, color = "green") +
          geom_label(aes(label = label, x = x, y = y), data = pos_clearance_label, color = "darkgreen") +
          theme_bw() +
          ylim(-1, 4) +
          xlab("") +
          ylab("inches") +
          ggtitle("Minimum clearance")
    })
    
    output$animationPlot <- renderPlot({
      
      # get the data for current position
      rdf <- measures() %>% 
          filter(abs(x - input$position) < .05)
      
      # data for the cross section of the surface
      bevel <- input$bevel
      theta_g    =  input$angle *pi / 180
      rate <- tan(theta_g)
      height <- input$height
      bump_left <- input$bump_x - input$bump_width/2 
      bump_right <- input$bump_x + input$bump_width/2
      floor_height <- rdf$FrontWheel_y - height
      bump_height <- input$bump_height 
      
      # from slope to flat
      surface_x <- c(rdf$BackWheel_x,  bevel, -bevel, bump_right,  bump_right, bump_left, bump_left, bump_left - 10)
      surface_y <- c(rdf$BackWheel_y - height, -bevel*rate, 0, 0, bump_height, bump_height, 0,0)
      
      surface <- tibble(x = surface_x, y = surface_y, x0= NA, y0=NA, radius = NA) 
      
      # plot the wheels and ground
      rdf %>% 
        select(Position = x, contains("Wheel"), radius) %>% 
        gather(Var, Value, -Position, -radius) %>% 
        separate(Var, sep = "_", into = c("Wheel","Dim")) %>% 
        spread(Dim, Value) %>% 
        ggplot(aes(x0 = x, y0 = y, x = x, y = y, r = radius)) +
        geom_circle(fill = "steelblue") +
        geom_line() +
        geom_path(aes(x = x, y = y),color = "red", data = surface) +
        geom_vline(xintercept = input$position, color = "green") +
        theme_bw() +
        coord_fixed() +
        ylim(-20,20) +
        xlim(-90,90) +
        xlab("Horizontal distance from rear wheel to peak.") +
        ggtitle("Car position")
      
    })
    

}
