
library(shiny)
library(tidyverse)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(reshape)
library(jsonlite)
library(tidyverse)
library(purrr)
library(tidyr)
library(plotly)
library("plyr")

source("parse.R")

source("dataframe.R")

#----------------------------------------------------------
main="SRS4818563_SRX5897024"
inputs=c("SRS4818567_SRX5897028","SRS4809062_SRX5886762")

server <- shinyServer(
  function(input, output) {
    #----------------------------------------
    output$N_1 <- renderPlotly({
      req(input$db_id)
      #p <- ggplot(data[data$read_type ==1, ] %>% filter(db_id %in% input$db_id),
      #            aes(x=b, y=n*100, color=db_id))
      p <- ggplot(data[data$read_type ==1, ] %>% filter(db_id %in% input$db_id), 
                  aes(x=b, y=n, color=db_id))
      
      p <- p + geom_point()+ facet_wrap(~db_id)+
        scale_x_continuous(breaks = scales::pretty_breaks(n = 10))+ 
        labs(x = "Base", y= "Percentage N-count")+
        ggtitle("read type: 1")+ theme(legend.position = "none") 
      ggplotly(p)
    })
    output$N_1.5 <- renderPlotly({
      req(input$db_id)
      data1=data[data$read_type ==1, ]
      main=data1[data1$db_id == input$main, ]
      
      #p <- ggplot(data[data$read_type ==1, ] %>% filter(db_id %in% input$db_id),
      #            aes(x=b, y=n*100, color=db_id))
      #p <- ggplot(data[data$read_type ==1, ] %>% filter(db_id %in% input$db_id), 
      #            aes(x=b, y=n, color=db_id))
      p=ggplot() + aes(x=b, y=n) + geom_point(data = data1[data1$db_id == input$main, ],aes(color=db_id))+ 
        geom_boxplot(data = data1[data1$db_id %in% input$db_id, ],aes(group=b))
      p <- p +
        scale_x_continuous(breaks = scales::pretty_breaks(n = 10))+ 
        labs(x = "Base", y= "Percentage N-count")+
        ggtitle("read type: 1")+ theme(legend.position = "none") 
      ggplotly(p)
    })
    output$N_2.5 <- renderPlotly({
      req(input$db_id)
      data1=data[data$read_type ==2, ]
      main=data1[data1$db_id == input$main, ]
      
      #p <- ggplot(data[data$read_type ==1, ] %>% filter(db_id %in% input$db_id),
      #            aes(x=b, y=n*100, color=db_id))
      #p <- ggplot(data[data$read_type ==1, ] %>% filter(db_id %in% input$db_id), 
      #            aes(x=b, y=n, color=db_id))
      p=ggplot() + aes(x=b, y=n) + geom_point(data = data1[data1$db_id == input$main, ],aes(color=db_id))+ 
        geom_boxplot(data = data1[data1$db_id %in% input$db_id, ],aes(group=b))
      p <- p +
        scale_x_continuous(breaks = scales::pretty_breaks(n = 10),
                           limits=c(min(main$b), max(main$b)))+ 
        labs(x = "Base", y= "Percentage N-count")+
        ggtitle("read type: 2")+ theme(legend.position = "none") 
      ggplotly(p)
    })
    output$N_2 <- renderPlotly({
      req(input$db_id)
      p <- ggplot(data[data$read_type == 2, ] %>% filter(db_id %in% input$db_id),
                  aes(x=b, y=n,color=db_id))
      p <- p + geom_line(aes(color=db_id))+
        scale_x_continuous(breaks = scales::pretty_breaks(n = 10))+ 
        labs(x = "Base", y= "Percentage N-count")+
        ggtitle("read type: 2")+ theme(legend.position = "none") 
      ggplotly(p)
    })
    #----------------------------------------
    output$SC_1 <- renderPlotly({
      req(input$db_id)
      p <- ggplot(data_s[data_s$read_type==1,] %>% filter(db_id %in% input$db_id), 
                  aes(x=b))
      p <- p + geom_line(aes(y = a),color="red")+
        geom_line(aes(y = c),color="blue")+
        geom_line(aes(y = g),color="green")+
        geom_line(aes(y = t),color="black")+
        scale_colour_manual("",
                            breaks = c("% A", "% C", "% G", "% T"),
                            values = c("red","blue", "green", "black"))+
        scale_x_continuous(breaks = scales::pretty_breaks(n = 10))+ 
        labs(x = "Base", y= "% Read")+
        ggtitle(label = "read type: 1",subtitle = input$db_id)+
        facet_wrap(~db_id,dir="v")
      #+theme(strip.text=element_blank())
      ggplotly(p)
    })
    output$SC_2 <- renderPlotly({
      req(input$db_id)
      p <- ggplot(data_s[data_s$read_type==2,] %>% filter(db_id %in% input$db_id), 
                  aes(x=b))
      p <- p + geom_line(aes(y = a),color="red")+
        geom_line(aes(y = c),color="blue")+
        geom_line(aes(y = g),color="green")+
        geom_line(aes(y = t),color="black")+
        scale_colour_manual("",
                            breaks = c("% A", "% C", "% G", "% T"),
                            values = c("red","blue", "green", "black"))+
        scale_x_continuous(breaks = scales::pretty_breaks(n = 10))+ 
        labs(x = "Base", y= "% Read")+
        ggtitle(label = "read type: 2",subtitle = input$db_id)+
        facet_wrap(~db_id,dir="v")
      ggplotly(p)
    })
    #----------------------------------------
    output$GC_0.15 <- renderPlotly({
      req(input$db_id)
      data_g1=data_g[data_g$read_type ==1, ]
      main=data_g1[data_g1$db_id == input$main, ]
      
      #p <- ggplot(data[data$read_type ==1, ] %>% filter(db_id %in% input$db_id),
      #            aes(x=b, y=n*100, color=db_id))
      #p <- ggplot(data[data$read_type ==1, ] %>% filter(db_id %in% input$db_id), 
      #            aes(x=b, y=n, color=db_id))
      p=ggplot() + aes(x=g, y=c) + geom_point(data = data_g1[data_g1$db_id == input$main, ],aes(color=db_id))+ 
        geom_boxplot(data = data_g1[data_g1$db_id %in% input$db_id, ],aes(group=g))
      p <- p +
        scale_x_continuous(breaks = scales::pretty_breaks(n = 10),
                           limits=c(min(main$b), max(main$b)))+ 
        labs(x = "%GC", y= "count")+
        ggtitle("read type: 1")+ theme(legend.position = "none") 
      ggplotly(p)
    })
    output$GC_0.25 <- renderPlotly({
      req(input$db_id)
      data_g1=data_g[data_g$read_type ==2, ]
      main=data_g1[data_g1$db_id == input$main, ]
      
      #p <- ggplot(data[data$read_type ==1, ] %>% filter(db_id %in% input$db_id),
      #            aes(x=b, y=n*100, color=db_id))
      #p <- ggplot(data[data$read_type ==1, ] %>% filter(db_id %in% input$db_id), 
      #            aes(x=b, y=n, color=db_id))
      p=ggplot() + aes(x=g, y=c) + geom_point(data = data_g1[data_g1$db_id == input$main, ],aes(color=db_id))+ 
        geom_boxplot(data = data_g1[data_g1$db_id %in% input$db_id, ],aes(group=g))
      p <- p +
        scale_x_continuous(breaks = scales::pretty_breaks(n = 10),
                           limits=c(min(main$g), max(main$g)))+ 
        labs(x = "%GC", y= "count")+
        ggtitle("read type: 1")+ theme(legend.position = "none") 
      ggplotly(p)
    })
    output$GC_1 <- renderPlotly({
      req(input$db_id)
      p <- ggplot(data_g[data_g$read_type==1,] %>% filter(db_id %in% input$db_id), 
                  aes(x=g, y=c , color=db_id))
      
      p <- p + geom_line(aes(color=db_id))+
        scale_x_continuous(breaks = scales::pretty_breaks(n = 10))+ 
        labs(x = "%GC", y= "Count")+
        ggtitle("read type: 1") + theme(legend.position = "none") 
      ggplotly(p)
    })
    
    output$GC_1.5 <- renderPlotly({
      req(input$db_id)
      data_read=data_g[data_g$read_type==1,]
      q_unique=c()
      for (db in input$db_id){
        q_unique <- append(q_unique,data_read[data_read$db_id==db,]$g)
      }
      q_unique = unique(q_unique)
      avg=c()
      minl=c()
      maxl=c()
      for (q in q_unique){
        total=0
        n=0
        min=0
        max=0
        for (db in input$db_id){
          data_temp=data_read[data_read$db_id==db,]
          if (q %in% data_temp$g){
            c_cur=data_temp[data_temp$g==q,]$c
            total = total + c_cur
            n=n+1
            if (min==0){
              min=c_cur
            }
            if(c_cur<min){
              min=c_cur
            }
            if (c_cur>max){
              max=c_cur
            }
          }
        }
        avg <- append(avg, total/n)
        minl <- append(minl, min)
        maxl <- append(maxl, max)
      }
      g <- q_unique
      c <- avg
      df_mean <- data.frame(g, c)
      df_minmax <- data.frame(g,minl,maxl)
      
      p=ggplot() + aes(x=g,y=c)+
        geom_ribbon(data=df_minmax,aes(ymin = minl, ymax = maxl), fill = "#CCCCCC") +
        geom_line(data=data_read[data_read$db_id==input$main,],aes(color="main"))+
        geom_line(data=df_mean,linetype = "dashed",aes(color="mean"))
      p <- p +
        scale_x_continuous(breaks = scales::pretty_breaks(n = 10))+ 
        labs(x = "%GC", y= "Count")+
        scale_color_manual(name="",values = c("main" = "#FF6666", "mean" = "#000000"))+
        ggtitle("read type: 1") 
      ggplotly(p)
    })
    
    output$GC_2.5 <- renderPlotly({
      req(input$db_id)
      data_read=data_g[data_g$read_type==2,]
      q_unique=c()
      minl=c()
      maxl=c()
      for (db in input$db_id){
        q_unique <- append(q_unique,data_read[data_read$db_id==db,]$g)
      }
      q_unique = unique(q_unique)
      avg=c()
      for (q in q_unique){
        total=0
        n=0
        min=0
        max=0
        for (db in input$db_id){
          data_temp=data_read[data_read$db_id==db,]
          if (q %in% data_temp$g){
            c_cur=data_temp[data_temp$g==q,]$c
            total = total + c_cur
            n=n+1
            if (min==0){
              min=c_cur
            }
            if(c_cur<min){
              min=c_cur
            }
            if (c_cur>max){
              max=c_cur
            }
          }
        }
        avg <- append(avg, total/n)
        minl <- append(minl, min)
        maxl <- append(maxl, max)
      }
      g <- q_unique
      c <- avg
      df_mean <- data.frame(g, c)
      df_minmax <- data.frame(g,minl,maxl)
      
      p=ggplot() + aes(x=g,y=c)+
        geom_ribbon(data=df_minmax,aes(ymin = minl, ymax = maxl), fill = "#CCCCCC") +
        geom_line(data=data_read[data_read$db_id==input$main,],aes(color="main"))+
        geom_line(data=df_mean,linetype = "dashed",aes(color="mean"))
      p <- p +
        scale_x_continuous(breaks = scales::pretty_breaks(n = 10))+ 
        labs(x = "%GC", y= "Count")+
        scale_color_manual(name="",values = c("main" = "#3399FF", "mean" = "#000000"))+
        ggtitle("read type: 2")
      ggplotly(p)
    })
    
    
    output$GC_2 <- renderPlotly({
      req(input$db_id)
      p <- ggplot(data_g[data_g$read_type==2,] %>% filter(db_id %in% input$db_id), 
                  aes(x=g, y=c , color=db_id))
      
      p <- p + geom_line(aes(color=db_id))+
        scale_x_continuous(breaks = scales::pretty_breaks(n = 10))+ 
        labs(x = "%GC", y= "Count")+
        ggtitle("read type: 2") + theme(legend.position = "none") 
      ggplotly(p)
    })
    #----------------------------------------
    
    output$QS_1 <- renderPlotly({
      req(input$db_id)
      p <- ggplot(data_sq[data_sq$read_type==1,] %>% filter(db_id %in% input$db_id), 
                  aes(x=q, y=c , color=db_id))
      p <- p + geom_line(aes(color=db_id))+
        scale_x_continuous(breaks = scales::pretty_breaks(n = 10))+ 
        labs(x = "sequence quality", y= "Count")+
        ggtitle("read type: 1") + theme(legend.position = "none") 
      ggplotly(p)
    })
    
    output$QS_1.5 <- renderPlotly({
      req(input$db_id)
      data_read=data_sq[data_sq$read_type==1,]
      q_unique=c()
      for (db in input$db_id){
        q_unique <- append(q_unique,data_read[data_read$db_id==db,]$q)
      }
      q_unique = unique(q_unique)
      avg=c()
      for (q in q_unique){
        total=0
        n=0
        for (db in input$db_id){
          data_temp=data_read[data_read$db_id==db,]
          if (q %in% data_temp$q){
            total = total + data_temp[data_temp$q==q,]$c
            n=n+1
          }
        }
        avg <- append(avg, total/n)
      }
      q <- q_unique
      c <- avg
      df_mean <- data.frame(q, c)
      
      p=ggplot() + aes(x=q,y=c)+
        geom_line(data=data_read[data_read$db_id==input$main,],aes(color="main"))+
        geom_line(data=df_mean,linetype = "dashed",aes(color="mean"))
      p <- p +
        scale_x_continuous(breaks = scales::pretty_breaks(n = 10))+ 
        labs(x = "sequence quality", y= "Count")+
        scale_color_manual(name="",values = c("main" = "#FF6666", "mean" = "#000000"))+
        ggtitle("read type: 1")
      ggplotly(p)
    })
    
    output$QS_2.5 <- renderPlotly({
      req(input$db_id)
      data_read=data_sq[data_sq$read_type==2,]
      q_unique=c()
      for (db in input$db_id){
        q_unique <- append(q_unique,data_read[data_read$db_id==db,]$q)
      }
      q_unique = unique(q_unique)
      avg=c()
      for (q in q_unique){
        total=0
        n=0
        for (db in input$db_id){
          data_temp=data_read[data_read$db_id==db,]
          if (q %in% data_temp$q){
            total = total + data_temp[data_temp$q==q,]$c
            n=n+1
          }
        }
        avg <- append(avg, total/n)
      }
      q <- q_unique
      c <- avg
      df_mean <- data.frame(q, c)
      
      p=ggplot() + aes(x=q,y=c)+
        geom_line(data=data_read[data_read$db_id==input$main,],aes(color="main"))+
        geom_line(data=df_mean,linetype = "dashed",aes(color="mean"))
      p <- p +
        scale_x_continuous(breaks = scales::pretty_breaks(n = 10))+ 
        labs(x = "sequence quality", y= "Count")+
        scale_color_manual(name="",values = c("main" = "#FF6666", "mean" = "#000000"))+
        ggtitle("read type: 2")
      ggplotly(p)
    })
    
    output$QS_2 <- renderPlotly({
      req(input$db_id)
      p <- ggplot(data_sq[data_sq$read_type==2,] %>% filter(db_id %in% input$db_id), 
                  aes(x=q, y=c , color=db_id))
      p <- p + geom_line(aes(color=db_id))+
        scale_x_continuous(breaks = scales::pretty_breaks(n = 10))+ 
        labs(x = "sequence quality", y= "Count")+
        ggtitle("read type: 2") + theme(legend.position = "none") 
      ggplotly(p)
    })
    #----------------------------------------
    output$MQS_1 <- renderPlotly({
      req(input$db_id)
      p <- ggplot(data_q[data_q$read_type==1,] %>% filter(db_id %in% input$db_id), 
                  aes(x=b, y=m , color=db_id))
      p <- p + geom_line(aes(color=db_id))+
        scale_x_continuous(breaks = scales::pretty_breaks(n = 10))+ 
        labs(x = "base", y= "score")+
        ggtitle("read type: 1") + theme(legend.position = "none") 
      ggplotly(p)
    })
    
    output$MQS_1.5 <- renderPlotly({
      req(input$db_id)
      data_read=data_q[data_q$read_type==1,]
      q_unique=c()
      for (db in input$db_id){
        q_unique <- append(q_unique,data_read[data_read$db_id==db,]$b)
      }
      q_unique = unique(q_unique)
      avg=c()
      for (q in q_unique){
        total=0
        n=0
        for (db in input$db_id){
          data_temp=data_read[data_read$db_id==db,]
          if (q %in% data_temp$b){
            total = total + data_temp[data_temp$b==q,]$m
            n=n+1
          }
        }
        avg <- append(avg, total/n)
      }
      b <- q_unique
      m <- avg
      df_mean <- data.frame(b, m)
      
      p=ggplot() + aes(x=b,y=m)+
        geom_line(data=data_read[data_read$db_id==input$main,],aes(color="main"))+
        geom_line(data=df_mean,linetype = "dashed",aes(color="mean"))
      p <- p +
        scale_x_continuous(breaks = scales::pretty_breaks(n = 10))+
        scale_color_manual(name="",values = c("main" = "#FF6666", "mean" = "#000000"))+
        labs(x = "base", y= "score")+
        ggtitle("read type: 1")
      ggplotly(p)
      
    })
    
    output$MQS_2.5 <- renderPlotly({
      req(input$db_id)
      data_read=data_q[data_q$read_type==2,]
      q_unique=c()
      for (db in input$db_id){
        q_unique <- append(q_unique,data_read[data_read$db_id==db,]$b)
      }
      q_unique = unique(q_unique)
      avg=c()
      for (q in q_unique){
        total=0
        n=0
        for (db in input$db_id){
          data_temp=data_read[data_read$db_id==db,]
          if (q %in% data_temp$b){
            total = total + data_temp[data_temp$b==q,]$m
            n=n+1
          }
        }
        avg <- append(avg, total/n)
      }
      b <- q_unique
      m <- avg
      df_mean <- data.frame(b, m)
      
      p=ggplot() + aes(x=b,y=m)+
        geom_line(data=data_read[data_read$db_id==input$main,],aes(color="main"))+
        geom_line(data=df_mean,linetype = "dashed",aes(color="mean"))
      p <- p +
        scale_x_continuous(breaks = scales::pretty_breaks(n = 10))+
        scale_color_manual(name="",values = c("main" = "#FF6666", "mean" = "#000000"))+
        labs(x = "base", y= "score")+
        ggtitle("read type: 2")
      ggplotly(p)
      
    })
    
    
    output$MQS_2 <- renderPlotly({
      req(input$db_id)
      p <- ggplot(data_q[data_q$read_type==2,] %>% filter(db_id %in% input$db_id), 
                  aes(x=b, y=m , color=db_id))
      p <- p + geom_line(aes(color=db_id))+
        scale_x_continuous(breaks = scales::pretty_breaks(n = 10))+ 
        labs(x = "base", y= "score")+
        ggtitle("read type: 2") + theme(legend.position = "none") 
      ggplotly(p)
    })
  }
)



