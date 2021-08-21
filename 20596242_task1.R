
    #Final
    ---------------------------#Packages ---------------------------
    library(shiny)
    library(shinydashboard)
    library(ggmap)
    library(plotly)
    
    
    
    #---------------------------Load the data--------------------------------
    #crime_data<-reas.csv(file="C:/Users/dell/OneDrive - LA TROBE UNIVERSITY/Documents/crime_data.csv")
    crime_data<-read.csv(file = "crime_data.csv")
    #View(crime_data)
    
    lon<-crime_data$lon
    lat<-crime_data$lat
    offense<- as.factor(crime_data$offense)
    
    date <- as.Date(crime_data$date,
                    format = "%m/%d/%y")
    
    # ----------------------------UI---------------------------------

    ui<-dashboardPage(
      dashboardHeader(title="Houston Crime Data Analysis",titleWidth = 400), 
      
      dashboardSidebar(
        
        sidebarMenu(
        h4(selectInput("plot1", "Select Offense type:",
                                c("Murder Offense"=1, "Rape Offense"=2,	
                                  "Aggravated assault"= 3,"Robbery Offense"=4 ,"Burglary Offense"=5,"Auto theft"=6)) ,
           radioButtons("month", "Select Month",
                        choices = list("January" = 1,
                                       "February" = 2,
                                       "March" = 3,
     "April" = 4,"May" = 5, "June"= 6, "July"= 7,"August" = 8),selected = 1),
           
         ))),
        
        dashboardBody(
          tabBox(title = "Tab Box",id = "tabset1",width=1000,
         tabPanel("Task 1(a)", plotlyOutput(outputId = "map1",width=800)),
          tabPanel("Task 1(b)", plotlyOutput(outputId = "map2",width=1000))
        ))
     
    )
     
  #---------------------------------Server------------------------------------ 
    server <- function(input, output) {
      
      
#------------------------------------Task 1(a)----------------------------------    
    
      output$map1<-renderPlotly({
        
  ggplotly({
    
 map <- get_stamenmap(zoom = 10, source="stamen",size = c(500, 200), scale = 2)
        
          if(input$plot1==1)
        {
           
       ggmap(map)+ stat_density_2d(
          data = subset(crime_data, offense =="murder"),
          aes(x = lon, y = lat, fill = ..level.., alpha = ..level..), geom = 'polygon') +
           scale_fill_gradient(low = "yellow", high = "red") +
  scale_alpha_continuous(range = c(0, 0.8)) + xlab("longitude")+ylab("latitude")+
  geom_point(data = subset(crime, offense == 'murder'),
  aes(x = lon, y = lat), size = 0.5) + guides(fill = FALSE, alpha = FALSE)+
              ggtitle("Density plot for Murder offense")+
              theme(plot.title = element_text(size = 13,face = "bold")) 
       
        }
  
      else  if(input$plot1==2)
  
        {     
          ggmap(map)+ stat_density_2d(
       data = subset(crime_data, offense =="rape"),
       aes(x = lon, y = lat, fill = ..level.., alpha = ..level..), geom = 'polygon') +
       scale_fill_gradient(low = "yellow", high = "red") + xlab("longitude")+ylab("latitude")+
       scale_alpha_continuous(range = c(0, 0.8)) + geom_point(data = subset(crime, offense == 'rape'),
         aes(x = lon, y = lat), size = 0.5) + guides(fill = FALSE, alpha = FALSE)+ 
            ggtitle("Density plot for Rape offense")+theme(plot.title = element_text(size = 13,face = "bold")) 
        }
          
    else if(input$plot1==3)
        {
          ggmap(map)+ stat_density_2d(
            data = subset(crime_data, offense =="aggravated assault"),
            aes(x = lon, y = lat, fill = ..level.., alpha = ..level..), geom = 'polygon') +
            scale_fill_gradient(low = "yellow", high = "red") + xlab("longitude")+ylab("latitude")+
            scale_alpha_continuous(range = c(0, 0.8))+ ggtitle("Density plot for Aggravated Assault")+
        theme(plot.title = element_text(size = 13,face = "bold"))+guides(fill = FALSE, alpha = FALSE) 
    }      
 
 else if(input$plot1==4)
 {
   ggmap(map)+ stat_density_2d(
     data = subset(crime_data, offense =="robbery"),
     aes(x = lon, y = lat, fill = ..level.., alpha = ..level..), geom = 'polygon') +
     scale_fill_gradient(low = "yellow", high = "red") + xlab("longitude")+ylab("latitude")+
     scale_alpha_continuous(range = c(0, 0.8))+ ggtitle("Density plot for Robbery Offense")+
     theme(plot.title = element_text(size = 13,face = "bold"))+guides(fill = FALSE, alpha = FALSE) 
 }  
        
      else if(input$plot1==5)
        {
          ggmap(map)+ stat_density_2d(
            data = subset(crime_data, offense =="burglary"),
            aes(x = lon, y = lat, fill = ..level.., alpha = ..level..), geom = 'polygon') +
            scale_fill_gradient(low = "yellow", high = "red") +
            scale_alpha_continuous(range = c(0, 0.8)) + xlab("longitude")+ylab("latitude")+
            guides(fill = FALSE, alpha = FALSE)+theme(plot.title = element_text(size = 13,face = "bold"))+
          ggtitle("Density plot for Burglary offense") +guides(fill = FALSE, alpha = FALSE)
        }      
        
     else if(input$plot1==6)
        {
         
         ggmap(map)+ stat_density_2d(
           data = subset(crime_data, offense =="auto theft"),
           aes(x = lon, y = lat, fill = ..level.., alpha = ..level..), geom = 'polygon') +
           scale_fill_gradient(low = "yellow", high = "red") +
           scale_alpha_continuous(range = c(0, 0.8)) + xlab("longitude")+ylab("latitude")+
           guides(fill = FALSE, alpha = FALSE)+ggtitle("Density plot for Auto Theft")+
         theme(plot.title = element_text(size = 13,face = "bold"))+guides(fill = FALSE, alpha = FALSE)
           
     } 
        })
      })
  #----------------------------------------------Task 1 (b)---------------------------------------------------------
      
      
     
      
     # --------Data Filtered Month wise--------------------
      jandata<-filter(crime_data,month =="january")
      febdata<-filter(crime_data,month =="february")
      marchdata<-filter(crime_data,month =="march")
      aprildata<-filter(crime_data,month =="april")
      maydata<-filter(crime_data,month =="may")
      junedata<-filter(crime_data,month =="june")
      julydata<-filter(crime_data,month =="july")
      augdata<-filter(crime_data,month =="august")
      
  
      output$map2<-renderPlotly({
        
        ggplotly({
        
        map <- get_stamenmap(zoom = 10, source="stamen",
                             size = c(500, 200), scale = 2)
        
        if (input$month==1)
        {
          
          ggmap(map) +
            geom_point(aes(x=lon, y=lat, color=offense), 
                       data=subset(jandata,offense!="theft"))+
          #theme(legend.background=element_blank())+
          ggtitle("Crimes in month of January")+xlab("longitude")+ylab("latitude")+
            theme(plot.title = element_text(size = 13,face = "bold"))
          
        }
        else if (input$month==2)
        {
          
          ggmap(map) +
            geom_point(aes(x=lon, y=lat, color=offense), 
                       data=subset(febdata,offense!="theft")) + 
            #theme(legend.background=element_blank())+
            ggtitle("Crimes in month of February")+xlab("longitude")+ylab("latitude")+
            theme(plot.title = element_text(size = 13,face = "bold"))
          
        }
        
        else if (input$month==3)
        {
          
          ggmap(map) +
            geom_point(aes(x=lon, y=lat, color=offense), 
                       data=subset(marchdata,offense!="theft")) + 
            #theme(legend.background=element_blank())+
            ggtitle("Crimes in month of March")+xlab("longitude")+ylab("latitude")+
            theme(plot.title = element_text(size = 13,face = "bold"))
          
        }
        else if (input$month==4)
        {
          
          ggmap(map) +
            geom_point(aes(x=lon, y=lat, color=offense), 
                       data=subset(aprildata,offense!="theft"))+ 
            #theme(legend.background=element_blank())+
            ggtitle('Crimes in month of April')+xlab("longitude")+ylab("latitude")+
            theme(plot.title = element_text(size = 13,face = "bold"))
          
        }
        else if (input$month==5)
        {
          
          ggmap(map) +
            geom_point(aes(x=lon, y=lat, color=offense), 
                       data=subset(maydata,offense!="theft"))+ 
            #theme(legend.background=element_blank())+
            ggtitle('Crimes in month of May')+xlab("longitude")+ylab("latitude")+
            theme(plot.title = element_text(size = 13,face = "bold"))
          
        }
        
        else if (input$month==6)
        {
          
          ggmap(map) +
            geom_point(aes(x=lon, y=lat, color=offense), 
                       data=subset(junedata,offense!="theft")) + 
            #theme(legend.background=element_blank())+
            ggtitle('Crimes in month of June')+xlab("longitude")+ylab("latitude")+
            theme(plot.title = element_text(size = 13,face = "bold"))
          
        }
        else if (input$month==7)
        {
          
          ggmap(map) +
            geom_point(aes(x=lon, y=lat, color=offense), 
                       data=subset(julydata,offense!="theft")) + 
            #theme(legend.background=element_blank())+
            ggtitle('Crimes in month of July')+xlab("longitude")+ylab("latitude")+
            theme(plot.title = element_text(size = 13,face = "bold"))
          
        }
      else if (input$month==8)
        {
          
          ggmap(map) +
            geom_point(aes(x=lon, y=lat, color=offense), 
                       data=subset(augdata,offense!="theft")) + 
            #theme(legend.background=element_blank())+
            ggtitle('Crimes in month of August')+xlab("longitude")+ylab("latitude")+
          theme(plot.title = element_text(size = 13,face = "bold"))
          
        }
    })
    
      })
   #  -------------------------Run App----------------------------  
    }
    
    shinyApp(ui, server)
    
    
    
