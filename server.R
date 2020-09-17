#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#    code is modified from this source:https://learnr.wordpress.com/2009/03/29/ggplot2_marimekko_mosaic_chart/

library(shiny)

# Define server logic required to draw a Marimekko chart
shinyServer(function(input, output) {

    output$MosaicPlot <- renderPlot({
        

        # generate Marimekko chart based on input from ui.R
        inFile <- input$file1
        data<-read.csv(inFile$datapath)
        if(input$caltype=='Manufacturer'){
            library(dplyr)
            df2=data %>%group_by(Segment) %>%summarise(agg=sum(MAT.TY)) %>%mutate(segpct = agg / sum(agg)*100)
            #drop agg
            df2=df2%>%select(-agg)
            #Pivot table of manufacturer and segment
            temp_table=data%>%group_by(Manufacturer,Segment)%>%summarise(agg2=sum(MAT.TY))%>%group_by(Segment)%>%mutate(percent=agg2/sum(agg2)*100)%>%select(-agg2)%>%tidyr::spread(Segment,percent)
            #Transpose of it
            temp_table_t = setNames(data.frame(t(temp_table[,-1])),temp_table[,1])
            colnames(temp_table_t)=temp_table$Manufacturer
            #merge with df2
            finaldata=cbind(df2,temp_table_t)
            finaldata=finaldata %>% mutate_if(is.numeric, round, digits=1)
            df=finaldata
            #calculate growth rate of each segment
            
            
        }else if (input$caltype=='Brand'){
            library(dplyr)
            df3=data %>%group_by(Segment) %>%summarise(agg=sum(MAT.TY)) %>%mutate(segpct = agg / sum(agg)*100)
            #drop agg
            df3=df3%>%select(-agg)
            #Pivot table of manufacturer and segment
            temp_table2=data%>%group_by(Brand,Segment)%>%summarise(agg2=sum(MAT.TY))%>%group_by(Segment)%>%mutate(percent=agg2/sum(agg2)*100)%>%select(-agg2)%>%tidyr::spread(Segment,percent)
            #Transpose of it
            temp_table_t2 = setNames(data.frame(t(temp_table2[,-1])),temp_table2[,1])
            colnames(temp_table_t2)=temp_table2$Brand
            #merge with df2
            finaldata2=cbind(df3,temp_table_t2)
            finaldata2=finaldata2 %>% mutate_if(is.numeric, round, digits=1)
            df=finaldata2
        }
        
        df3=data %>%group_by(Segment) %>%summarise(a=sum(MAT.TY),b=sum(MAT.LY))%>%mutate(growth =((a/b)-1) *100)
        df$xmax <- cumsum(df$segpct)
        df$xmin <- df$xmax - df$segpct
        df$segpct <- NULL
        
        library(tidyr)
        dfm<-gather(df,key="variable",value,-Segment,-xmin,-xmax)
        dfm[is.na(dfm)] = 0
        dfm1<-do.call(rbind,by(dfm,dfm$Segment,function(Segments){transform(Segments,ymax=cumsum(value))}))
        dfm1<-do.call(rbind,by(dfm1,dfm1$Segment,function(Segment2){transform(Segment2,ymin=ymax-value)}))
        dfm1$xtext <- with(dfm1, xmin + (xmax - xmin)/2)
        dfm1$ytext <- with(dfm1, ymin + (ymax - ymin)/2)
        #vlookup growth rate in df3 to dfm1
        dfm1 <- (merge(dfm1, df3, by = 'Segment'))
        dfm1$growth=round(dfm1$growth,1)
        
        
        library(ggplot2)
        p2=ggplot(dfm1, aes(ymin = ymin, ymax = ymax,xmin = xmin, xmax = xmax, fill = variable,order=-ymin))+ geom_rect(colour = I("white"))+ theme_minimal()+ theme(panel.grid.major = element_blank(),panel.grid.minor =element_blank(),legend.position = "bottom",legend.key.size=unit(0.3, "cm"))+ scale_fill_manual(values=c("#00aeef", "#b21dac", "#ffb100","#8dc63f","#dd0014"))+ labs(x = NULL, y = NULL,fill = NULL)+geom_text(data=subset(dfm1,value!=0),aes(x = xtext, y = ytext,label = ifelse(Segment == "A", paste(variable," - ", value, "%", sep = ""), paste(value,"%", sep = ""))), size = 2.5,color="white")
        p3 = p2 + geom_text(aes(x = xtext, y = -5,label = stringr::str_wrap(paste(Segment),0.2)),vjust=-0.001,size = 2.5)
        #label growth rate
        p4=p3 + annotate("text", x =-4 ,y=103,label = "Growth rate (%)",size=2.5)
        #annotate the plot with the growth rate on top of chart
        p4+ geom_text(aes(x = xtext, y = 103,label = paste(growth)),size = 2.5)   
        
    }, height=500, width=700)

})
