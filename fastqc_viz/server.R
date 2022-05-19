
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
all_va = unique(c(data$db_id,data_s$db_id,
                  data_g$db_id,data_sq$db_id,
                  data_q$db_id))
all_meta = c("library_strategy","library_source","library_selection",
           "platform","instrument_model","taxon_id","scientific_name")
meta_fields <- c("meta1","meta2","meta3","meta4","meta5","meta6","meta7")
choice_fields <- c("level1", "level2","level3","level4","level5","level6","level7")
button_fields <- c("button1", "button2","button3","button4","button5","button6","button7")
pull_fields <- c("pull1", "pull2","pull3","pull4","pull5","pull6","pull7")
radio_fields <- c("radio1", "radio2","radio3","radio4","radio5","radio6","radio7")

server <- shinyServer(
  function(input, output,session) {
    values <- reactiveValues()
    values$nof <- 1
    
    metaData <- reactive({
      meta_inputs <- sapply(meta_fields, function(x) input[[x]])
      meta_inputs
    })
    choiceData <- reactive({
      choice_inputs <- sapply(choice_fields, function(x) input[[x]])
      choice_inputs
    })
    
    observeEvent(input$button1,{
      shinyjs::hide("button1")
      shinyjs::show("pull1")
      shinyjs::show("radio1")
      if(values$nof==1){
        output$pull1 <- renderUI({
          selectInput(inputId="meta1",
                      label= strong("Filter by:"),
                      width = "70%",
                      choices=all_meta,
                      multiple = FALSE
          )
        })
        
        output$radio1 <- renderUI({
          req(input$meta1)
          tep_meta = data_meta[data_meta["library_layout"]==input$layout,]
          values_1 <- unique(tep_meta[,input$meta1])
          radioButtons(inputId = "level1",
                       label = NULL,
                       choices = values_1,
                       selected = values_1[1])
        })
        values$nof <- 2
      }
      shinyjs::show("button2")
    })
    
    observeEvent(input$button2,{
      shinyjs::hide("button2")
      shinyjs::show("pull2")
      shinyjs::show("radio2")
      if(values$nof==2){
        output$pull2 <- renderUI({
          req(input$level1)
          meta_choice2 = all_meta
          meta_choice2 <- meta_choice2[!(meta_choice2 %in% c(input$meta1))]
          selectInput(inputId="meta2",
                      label= NULL,
                      width = "70%",
                      choices=meta_choice2,
                      multiple = FALSE
          )
        })
        
        output$radio2 <- renderUI({
          req(input$meta2,input$level1)
          tep_meta = data_meta[data_meta["library_layout"]==input$layout,]
          data_meta2 = tep_meta[tep_meta[input$meta1]==input$level1,]
          values_2 <- unique(data_meta2[,input$meta2])
          radioButtons(inputId = "level2",
                       label = NULL,
                       choices = values_2,
                       selected = values_2[1])
        })
        values$nof <- 3
      }
      shinyjs::show("button3")
    })
    
    observeEvent(input$button3,{
      shinyjs::hide("button3")
      shinyjs::show("pull3")
      shinyjs::show("radio3")
      if(values$nof==3){
      output$pull3 <- renderUI({
        req(input$level2)
        meta_choice3 = all_meta
        meta_choice3 <- meta_choice3[!(meta_choice3 %in% c(input$meta1,input$meta2))]
        
        selectInput(inputId="meta3",
                    label= NULL,
                    width = "70%",
                    choices=meta_choice3,
                    multiple = FALSE
        )
      })
      output$radio3 <- renderUI({
        req(input$meta3,input$level2)
        tep_meta = data_meta[data_meta["library_layout"]==input$layout,]
        for(i in 1:2){
          tep_meta = tep_meta[tep_meta[input[[meta_fields[i]]]]==input[[choice_fields[i]]],]
        }
        values_3 <- unique(tep_meta[,input$meta3])
        radioButtons(inputId = "level3",
                     label = NULL,
                     choices = values_3,
                     selected = values_3[1])
      })
      values$nof <- 4
      shinyjs::show("button4")
    }
      })
    
    observeEvent(input$button4,{
      shinyjs::hide("button4")
      shinyjs::show("pull4")
      shinyjs::show("radio4")
      if(values$nof==4){
      output$pull4 <- renderUI({
        req(input$level3)
        meta_choice4 = all_meta
        meta_choice4 <- meta_choice4[!(meta_choice4 %in% c(input$meta1,input$meta2,input$meta3))]
       
        selectInput(inputId="meta4",
                    label= NULL,
                    width = "70%",
                    choices=meta_choice4,
                    multiple = FALSE
        )
      })
      output$radio4 <- renderUI({
        req(input$meta4,input$level3)
        tep_meta = data_meta[data_meta["library_layout"]==input$layout,]
        for(i in 1:3){
          tep_meta = tep_meta[tep_meta[input[[meta_fields[i]]]]==input[[choice_fields[i]]],]
        }
        values_4 <- unique(tep_meta[,input$meta4])
        radioButtons(inputId = "level4",
                     label = NULL,
                     choices = values_4,
                     selected = values_4[1])
      })
      values$nof <- 5
      }
      shinyjs::show("button5")
      })
    
    observeEvent(input$button5,{
      shinyjs::hide("button5")
      shinyjs::show("pull5")
      shinyjs::show("radio5")
      if(values$nof==5){
      output$pull5 <- renderUI({
        req(input$level4)
        meta_choice5 = all_meta
        meta_choice5 <- meta_choice5[!(meta_choice5 %in% c(input$meta1,input$meta2,input$meta3,
                                                           input$meta4))]
        
        selectInput(inputId="meta5",
                    label= NULL,
                    width = "70%",
                    choices=meta_choice5,
                    multiple = FALSE
        )
      })
      output$radio5 <- renderUI({
        req(input$level4,input$meta5)
        tep_meta = data_meta[data_meta["library_layout"]==input$layout,]
        for(i in 1:4){
          tep_meta = tep_meta[tep_meta[input[[meta_fields[i]]]]==input[[choice_fields[i]]],]
        }
        values_5 <- unique(tep_meta[,input$meta5])
        radioButtons(inputId = "level5",
                     label = NULL,
                     choices = values_5,
                     selected = values_5[1])
      })
      values$nof <- 6
      }
      shinyjs::show("button6")
      })
    
    observeEvent(input$button6,{
      shinyjs::hide("button6")
      shinyjs::show("pull6")
      shinyjs::show("radio6")
      if(values$nof==6){
      output$pull6 <- renderUI({
        req(input$level5)
        meta_choice6 = all_meta
        meta_choice6 <- meta_choice6[!(meta_choice6 %in% c(input$meta1,input$meta2,input$meta3,
                                                           input$meta4,input$meta5))]
        
        selectInput(inputId="meta6",
                    label= NULL,
                    width = "70%",
                    choices=meta_choice6,
                    multiple = FALSE
        )
      })
      
      output$radio6 <- renderUI({
        req(input$level5,input$meta6)
        tep_meta = data_meta[data_meta["library_layout"]==input$layout,]
        for(i in 1:5){
          tep_meta = tep_meta[tep_meta[input[[meta_fields[i]]]]==input[[choice_fields[i]]],]
        }
        values_6 <- unique(tep_meta[,input$meta6])
        radioButtons(inputId = "level6",
                     label = NULL,
                     choices = values_6,
                     selected = values_6[1])
      })
      values$nof <- 7
      }
      shinyjs::show("button7")
      })
    
    observeEvent(input$button7,{
      shinyjs::hide("button7")
      shinyjs::show("pull7")
      shinyjs::show("radio7")
      if(values$nof==7){
      output$pull7 <- renderUI({
        req(input$level6)
        meta_choice7 = all_meta
        meta_choice7 <- meta_choice7[!(meta_choice7 %in% c(input$meta1,input$meta2,input$meta3,
                                                           input$meta4,input$meta5,input$meta6))]
        
        selectInput(inputId="meta7",
                    label= NULL,
                    width = "70%",
                    choices=meta_choice7,
                    multiple = FALSE
        )
      })
      output$radio7 <- renderUI({
        req(input$level6,input$meta7)
        tep_meta = data_meta[data_meta["library_layout"]==input$layout,]
        for(i in 1:6){
          tep_meta = tep_meta[tep_meta[input[[meta_fields[i]]]]==input[[choice_fields[i]]],]
        }
        values_7 <- unique(tep_meta[,input$meta7])
        radioButtons(inputId = "level7",
                     label = NULL,
                     choices = values_7,
                     selected = values_7[1])
      })
      values$nof <- 8
      }
      shinyjs::show("filter")
      })
     
    
    observeEvent(input$filter, {
      #shinyjs::hide("filter")
      #print(values$nof)
       
    })
    
    observeEvent(input$remove, {
      if(values$nof!=1){
        nof_in = values$nof-1
        updateSelectInput(session, meta_fields[nof_in], label = NULL, choices = c(1),selected = character(0))
        updateRadioButtons(session, choice_fields[nof_in], label = NULL, choices = c(1),selected = character(0))
        shinyjs::hide(pull_fields[nof_in])
        shinyjs::hide(radio_fields[nof_in])
        shinyjs::hide(button_fields[values$nof])
        shinyjs::show(button_fields[nof_in])
        values$nof = values$nof-1
        shinyjs::hide("filter")
      }
    })
    
    #----------------------------------------
    
    output$mian_sample <- renderUI({
      tep_meta = data_meta[data_meta["library_layout"]==input$layout,]
      if(!is.null(input$level1)){
        tempmeta <- unlist(metaData(), use.names = FALSE)
        tempchoice <- unlist(choiceData(), use.names = FALSE)
        for(i in 1: length(tempchoice)){
          tep_meta = tep_meta[tep_meta[tempmeta[i]]==tempchoice[i],]
        }
      }
      all_va=unique(tep_meta$db_id)
      
      selectInput(inputId="main",
                  label= strong("Choose a main sample:"),
                  choices=all_va)
      
      
    })
    
    output$choice_sample <- renderUI({
      #all_choice=all_va
      tep_meta = data_meta[data_meta["library_layout"]==input$layout,]
      if(!is.null(input$level1)){
        tempmeta <- unlist(metaData(), use.names = FALSE)
        tempchoice <- unlist(choiceData(), use.names = FALSE)
        for(i in 1: length(tempchoice)){
          tep_meta = tep_meta[tep_meta[tempmeta[i]]==tempchoice[i],]
        }
        #print(tempmeta)
        #print(tempchoice)
        #print(values$nof)
      }
      
      all_choice=unique(tep_meta$db_id)
      all_choice = all_choice[!(all_choice %in% c(input$main))]
      
      checkboxGroupInput(inputId = "db_id",
                         label = strong("Compare with:"),
                         choices = all_choice)
    })
    
    
    #----------------------------------------
    observeEvent(input$bt_clear, {
      updateCheckboxInput(session, "db_id", value = character(0))
    })
    
    observeEvent(input$bt_samples, {
      tep_meta = data_meta[data_meta["library_layout"]==input$layout,]
      if(!is.null(input$level1)){
        tempmeta <- unlist(metaData(), use.names = FALSE)
        tempchoice <- unlist(choiceData(), use.names = FALSE)
        for(i in 1: length(tempchoice)){
          tep_meta = tep_meta[tep_meta[tempmeta[i]]==tempchoice[i],]
        }
      }
      all_choice=unique(tep_meta$db_id)
      
      bowl=all_choice[all_choice %in% c(input$db_id,input$main)== FALSE]
      if (length(bowl)>0){
        if(length(bowl)>=5){
          new_sample=sample(bowl, 5, replace=FALSE)
        }else{
          new_sample=sample(bowl, length(bowl), replace=FALSE)
        }
        updateCheckboxInput(session, "db_id", value = c(input$db_id,new_sample))
      }
    })
    #----------------------------------------
    
    
    
    output$N_1.5 <- renderPlotly({
      req(input$main)
      ly=data_meta[data_meta$db_id==input$main,]$library_layout
      if(ly=="SINGLE"){
        data1=data
      }else{
        data1=data[data$read_type ==1, ]
        main=data1[data1$db_id == input$main, ]
      }
      
      p=ggplot() + aes(x=b, y=n) + 
        geom_point(data = data1[data1$db_id == input$main, ],color="#3399FF")
      if (!is.null(input$db_id)){
        if(length(input$db_id)>3){
          p <- p + geom_boxplot(data = data1[data1$db_id %in% input$db_id, ],aes(group=b))+ 
            theme(legend.position = "none")
        }else{
          p <- p + 
            geom_line(data = data1[data1$db_id %in% input$db_id, ],aes(color=db_id))+
            scale_color_brewer(palette="Dark2")
        }
      }
      if(ly=="SINGLE"){
        p <- p +
          scale_x_continuous(breaks = scales::pretty_breaks(n = 10))+
          labs(x = "Base", y= "Percentage N-count")
      }else{
        p <- p +
          scale_x_continuous(breaks = scales::pretty_breaks(n = 10))+
          labs(x = "Base", y= "Percentage N-count")+
          ggtitle("read type: 1") 
      }
      ggplotly(p)
    })
    output$N_2.5 <- renderPlotly({
      req(input$main)
      ly=data_meta[data_meta$db_id==input$main,]$library_layout
      
      if(ly!="SINGLE"){
        data1=data[data$read_type ==2, ]
        main=data1[data1$db_id == input$main, ]
        
        p=ggplot() + aes(x=b, y=n) + 
          geom_point(data = data1[data1$db_id == input$main, ],color="#3399FF")
        
        if (!is.null(input$db_id)){
          if(length(input$db_id)>3){
            p <- p + geom_boxplot(data = data1[data1$db_id %in% input$db_id, ],aes(group=b))+ 
              theme(legend.position = "none")
          }else{
            p <- p + 
              geom_line(data = data1[data1$db_id %in% input$db_id, ],aes(color=db_id))+
              scale_color_brewer(palette="Dark2")
          }
        }
        
        p <- p +
          scale_x_continuous(breaks = scales::pretty_breaks(n = 10))+ 
          labs(x = "Base", y= "Percentage N-count")+
          ggtitle("read type: 2")
        ggplotly(p)
      }
      
    })
    
    

    
    #----------------------------------------
    output$SC_1 <- renderPlotly({
      req(input$main)
      ly=data_meta[data_meta$db_id==input$main,]$library_layout
      inputs_sc <- c(input$main,input$db_id)
      if(ly=="SINGLE"){
        p <- ggplot(data_s %>% filter(db_id %in% inputs_sc), 
                    aes(x=b))
      }else{
        p <- ggplot(data_s[data_s$read_type==1,] %>% filter(db_id %in% inputs_sc), 
                    aes(x=b))+ggtitle(label = "read type: 1",subtitle = inputs_sc)
      }
      p <- p + geom_line(aes(y = a,color="%A"))+
        geom_line(aes(y = c,color="%C"))+
        geom_line(aes(y = g,color="%G"))+
        geom_line(aes(y = t,color="%T"))+
        scale_color_manual(name="",values = c("%A" = "red", "%C" = "blue", "%G" ="green", "%T"= "black" ))+
        scale_x_continuous(breaks = scales::pretty_breaks(n = 10))+ 
        labs(x = "Base", y= "% Read")
      
      if (length(inputs_sc)>3){
        p <- p+facet_wrap(vars(db_id))
      }else{
        p <- p+facet_wrap(~db_id,dir="v")
      }
      #+theme(strip.text=element_blank())
      ggplotly(p)
      
      
    })
    output$SC_2 <- renderPlotly({
      req(input$main)
      ly=data_meta[data_meta$db_id==input$main,]$library_layout
      inputs_sc <- c(input$main,input$db_id)
      if(ly!="SINGLE"){
        p <- ggplot(data_s[data_s$read_type==2,] %>% filter(db_id %in% inputs_sc), 
                    aes(x=b))
        p <- p + geom_line(aes(y = a,color="%A"))+
          geom_line(aes(y = c,color="%C"))+
          geom_line(aes(y = g,color="%G"))+
          geom_line(aes(y = t,color="%T"))+
          scale_color_manual(name="",values = c("%A" = "red", "%C" = "blue", "%G" ="green", "%T"= "black" ))+
          scale_x_continuous(breaks = scales::pretty_breaks(n = 10))+ 
          labs(x = "Base", y= "% Read")+ggtitle(label = "read type: 2",subtitle = inputs_sc)
        if (length(inputs_sc)>3){
          p <- p+facet_wrap(vars(db_id))
        }else{
          p <- p+facet_wrap(~db_id,dir="v")
        }
        ggplotly(p)
      }
        
      
      
    })
    #----------------------------------------
    
    output$GC_1.5 <- renderPlotly({
      req(input$main)
      ly=data_meta[data_meta$db_id==input$main,]$library_layout
      if(ly=="SINGLE"){
        data_read=data_g
      }else{
        data_read=data_g[data_g$read_type==1,]
      }
      
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
        geom_line(data=data_read[data_read$db_id==input$main,],aes(color="main"))

      if (!is.null(input$db_id)){
        p=ggplot() + aes(x=g,y=c)+
          geom_ribbon(data=df_minmax,aes(ymin = minl, ymax = maxl), fill = "#CCCCCC")+
          geom_line(data=data_read[data_read$db_id==input$main,],aes(color="main"))+
          geom_line(data=df_mean,linetype = "dashed",aes(color="mean"))
      }
      p <- p +
        scale_x_continuous(breaks = scales::pretty_breaks(n = 10))+ 
        labs(x = "%GC", y= "Count")+
        scale_color_manual(name="",values = c("main" = "#FF6666", "mean" = "#000000"))
      if(ly!="SINGLE"){
        p=p+ggtitle("read type: 1") 
      }
      ggplotly(p)
    })
    
    output$GC_2.5 <- renderPlotly({
      req(input$main)
      ly=data_meta[data_meta$db_id==input$main,]$library_layout
      if(ly!="SINGLE"){
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
          geom_line(data=data_read[data_read$db_id==input$main,],aes(color="main"))
        
        if (!is.null(input$db_id)){
          p=ggplot() + aes(x=g,y=c)+
            geom_ribbon(data=df_minmax,aes(ymin = minl, ymax = maxl), fill = "#CCCCCC") +
            geom_line(data=data_read[data_read$db_id==input$main,],aes(color="main"))+
            geom_line(data=df_mean,linetype = "dashed",aes(color="mean"))
          
        }
        
        p <- p +
          scale_x_continuous(breaks = scales::pretty_breaks(n = 10))+ 
          labs(x = "%GC", y= "Count")+
          scale_color_manual(name="",values = c("main" = "#3399FF", "mean" = "#000000"))
        if(ly!="SINGLE"){
          p=p+ggtitle("read type: 2") 
        }
        ggplotly(p)
      }
      
      
      
    })
    
    
    #----------------------------------------

    
    output$QS_1.5 <- renderPlotly({
      req(input$main)
      ly=data_meta[data_meta$db_id==input$main,]$library_layout
      if(ly=="SINGLE"){
        data_read=data_sq
      }else{
        data_read=data_sq[data_sq$read_type==1,]
      }
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
        geom_line(data=data_read[data_read$db_id==input$main,],aes(color="main"))
      
      if (!is.null(input$db_id)){
        p <- p +
          geom_line(data=df_mean,linetype = "dashed",aes(color="mean"))
      }
      p <- p +
        scale_x_continuous(breaks = scales::pretty_breaks(n = 10))+ 
        labs(x = "sequence quality", y= "Count")+
        scale_color_manual(name="",values = c("main" = "#FF6666", "mean" = "#000000"))
        
      if(ly!="SINGLE"){
        p=p+ggtitle("read type: 1")
      }
      ggplotly(p)
    })
    
    output$QS_2.5 <- renderPlotly({
      req(input$main)
      ly=data_meta[data_meta$db_id==input$main,]$library_layout
      if(ly!="SINGLE"){
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
          geom_line(data=data_read[data_read$db_id==input$main,],aes(color="main"))
        
        if (!is.null(input$db_id)){
          p <- p +
            geom_line(data=df_mean,linetype = "dashed",aes(color="mean"))
        }
        
        p <- p +
          scale_x_continuous(breaks = scales::pretty_breaks(n = 10))+ 
          labs(x = "sequence quality", y= "Count")+
          scale_color_manual(name="",values = c("main" = "#3399FF", "mean" = "#000000"))+
          ggtitle("read type: 2")
        ggplotly(p)
      }
      
    })
    

    #----------------------------------------

    
    output$MQS_1.5 <- renderPlotly({
      req(input$main)
      ly=data_meta[data_meta$db_id==input$main,]$library_layout
      if(ly=="SINGLE"){
        data_read=data_q
      }else{
        data_read=data_q[data_q$read_type==1,]
      }
      
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
        geom_line(data=data_read[data_read$db_id==input$main,],aes(color="main"))
      
      if (!is.null(input$db_id)){
        p <- p +
          geom_line(data=df_mean,linetype = "dashed",aes(color="mean"))
      }
      
      p <- p +
        scale_x_continuous(breaks = scales::pretty_breaks(n = 10))+
        scale_color_manual(name="",values = c("main" = "#FF6666", "mean" = "#000000"))+
        labs(x = "base", y= "score")
        
      if(ly!="SINGLE"){
        p = p + ggtitle("read type: 1")
      }
      ggplotly(p)

    })
    
    output$MQS_2.5 <- renderPlotly({
      req(input$main)
      ly=data_meta[data_meta$db_id==input$main,]$library_layout
      if(ly!="SINGLE"){
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
          geom_line(data=data_read[data_read$db_id==input$main,],aes(color="main"))
        
        if (!is.null(input$db_id)){
          p <- p +
            geom_line(data=df_mean,linetype = "dashed",aes(color="mean"))
        }
        
        p <- p +
          scale_x_continuous(breaks = scales::pretty_breaks(n = 10))+
          scale_color_manual(name="",values = c("main" = "#3399FF", "mean" = "#000000"))+
          labs(x = "base", y= "score")+
          ggtitle("read type: 2")
        ggplotly(p)
      }
    })
    
  }
)



