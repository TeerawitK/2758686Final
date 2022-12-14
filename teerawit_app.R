library(shiny)
library(shinythemes)
library(openxlsx)
library(dplyr)
library(tidyr)
library(ggplot2)
library(plotly)
library(shinydashboard)

dat <- read.xlsx("https://github.com/TeerawitK/2758686Final/raw/main/finalexam65.xlsx")
glimpse(dat)

dat <- dat %>% mutate(midterm = midterm1+midterm2,
                      final = final1+final2)

ui <- dashboardPage(
  dashboardHeader(title = "ระบบติดตามการเรียน"),
  dashboardSidebar(sidebarMenu(
    menuItem("นิสิต", tabName = "student", icon = icon("users")),
    menuItem("อาจารย์", tabName = "teacher", icon = icon("chalkboard-user"))
  )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "student",
              fluidRow(h2(HTML('&nbsp;'),"ระบบติดตามการเรียนรู้วิชาสถิติ (สำหรับนิสิต)")),
              fluidRow(
                column(4,textInput("id","โปรดระบุรหัสนิสิต"))
                       ),
              fluidRow(column(4,actionButton(inputId = "submit", 
                                             label = c("ยืนยัน")))
                      ),
              br(),
              fluidRow(column(4, textOutput("error"), style = "color:red")
                      ),
              br(),
              fluidRow(
                valueBoxOutput("s.pro", width = 2),
                valueBoxOutput("s.mid", width = 2),
                valueBoxOutput("s.final", width = 2)
               ),
              fluidRow(
                  box(
                  title = "เวลาการเข้าเรียนแต่ละสัปดาห์",
                  plotlyOutput("plot.time")),
                box(
                  title = "คะแนนการบ้านแต่ละสัปดาห์",
                  plotlyOutput("plot.hw"))
                ),
              
              fluidRow(
                box(title = "คะแนนสอบกลางภาค",
                  plotlyOutput("plot.mid")),
                box(title = "คะแนนสอบปลายภาค",
                    plotlyOutput("plot.final"))
              )
              ),
      tabItem(tabName = "teacher",
              fluidRow(h2(HTML('&nbsp;'),"ระบบติดตามการเรียนรู้วิชาสถิติ (สำหรับอาจารย์)")),
              fluidRow(
                column(4,
                       textInput("pw", "โปรดระบุรหัสผ่าน (123456)"))
              ),
              fluidRow(
                column(4,actionButton(inputId = "submit2", 
                                      label = c("ยืนยัน")))
              ),
              br(),
              fluidRow(column(4, textOutput("error2"), style = "color:red")
              ),
              br(),
              tabsetPanel(
                tabPanel("ภาพรวม",
              fluidRow(
                valueBoxOutput("t.pro", width = 2),
                valueBoxOutput("t.mid", width = 2),
                valueBoxOutput("t.final", width = 2)
              ),
              fluidRow(
                box(
                  title = "เวลาการเข้าเรียนโดยเฉลี่ย",
                  plotlyOutput("plot.time2")),
                box(
                    title = "คะแนนการบ้านโดยเฉลี่ย",
                    plotlyOutput("plot.hw2"))
            ),
            fluidRow(
              box(
                title = "คะแนนสอบกลางภาคโดยเฉลี่ย",
                plotlyOutput("plot.mid2")),
              box(
                title = "คะแนนสอบปลายภาคโดยเฉลี่ย",
                plotlyOutput("plot.final2"))
            )
                ),
            tabPanel("รายบุคคล",
                     fluidRow(
                       box(
                         width = 12,
                         title = "เวลาการเข้าเรียนและคะแนนการบ้านแต่ละสัปดาห์",
                         plotlyOutput("plot.t1")),
                     ),
                     fluidRow(
                       box(width = 12,
                           title = "คะแนนสอบ",
                           plotlyOutput("plot.t2"))
                     )
                     )
              )
       )
      )
    )
  )


server <- function(input, output) {
    s.dat <- reactive(dat %>% filter(id == input$id))
    
    observeEvent(input$submit, {
      observeEvent(input$id, {
        if(input$id %in% dat$id == T) {
          output$error <- NULL
        
      output$plot.time <- renderPlotly({
      time.dat <- s.dat() %>%
      pivot_longer(cols = contains("time"), 
                   names_to = "week", 
                   values_to = "time") %>%
       select(week,time)
      time.dat$week <- as.integer(sub("time","", time.dat$week))
   
      time.dat2 <- dat %>% pivot_longer(cols = contains("time"), 
                                        names_to = "week", 
                                        values_to = "time") %>% 
        group_by(week) %>% select(week,time)
      time.dat2$week <- as.integer(sub("time","", time.dat2$week))
      time.dat2 <- time.dat2 %>% summarise(time = mean(time))
      time.dat2$time <- round(time.dat2$time,1)
      
     plot.time <- time.dat %>% plot_ly(x =~ week,
                                        hoverinfo = "text",
                                        text=~paste("</br> สัปดาห์ที่ :", week,
                                                    "</br> เวลาเรียน :", time)) %>%
                                     add_lines(y =~ time,
                                               name = input$id) %>%
                                    add_lines(data = time.dat2 ,
                                              x =~ week,
                                              y =~ time,
                                              name = "ค่าเฉลี่ยของรายวิชา") %>%
                                    add_markers(data = time.dat,
                                                y =~ time,
                                                showlegend = F,
                                                color = I("steelblue")) %>%
                                    add_markers(data = time.dat2,
                                                y =~ time,
                                                showlegend = F,
                                                color = I("darkorange")) %>%
                                    layout(xaxis = list(title = "สัปดาห์ที่",
                                                        dtick = 2),
                                       yaxis = list(title = "เวลาเรียน (นาที)",
                                                    range = c(0,180))
                                       )
                                     
    })
    
      output$plot.hw <- renderPlotly({
        hw.dat <- s.dat() %>%
          pivot_longer(cols = contains("hw"), 
                       names_to = "week", 
                       values_to = "score") %>%
          select(week,score)
        hw.dat$week <- as.integer(sub("hw","", hw.dat$week))
        
        hw.dat2 <- dat %>% pivot_longer(cols = contains("hw"), 
                                          names_to = "week", 
                                          values_to = "score") %>% 
          group_by(week) %>% select(week,score)
        hw.dat2$week <- as.integer(sub("hw","", hw.dat2$week))
        hw.dat2 <- hw.dat2 %>% summarise(score = mean(score))
        hw.dat2$score <- round(hw.dat2$score,1)
        
        plot.hw <- hw.dat %>% plot_ly(x =~ week,
                                      hoverinfo = "text",
                                      text=~paste("</br> สัปดาห์ที่ :", week,
                                                  "</br> คะแนน :", score)) %>%
          add_lines(y =~ score,
                    name = input$id) %>%
          add_lines(data = hw.dat2 ,
                    x =~ week,
                    y =~ score,
                    name = "ค่าเฉลี่ยของรายวิชา") %>%
          add_markers(data = hw.dat,
                      y =~ score,
                      showlegend = F,
                      color = I("steelblue")) %>%
          add_markers(data = hw.dat2,
                      y =~ score,
                      showlegend = F,
                      color = I("darkorange")) %>%
          layout(xaxis = list(title = "สัปดาห์ที่",
                              dtick = 2),
                 yaxis = list(title = "คะแนน (10)",
                              range = c(0,10)))
      })
      
      output$plot.mid <- renderPlotly({      
      mid1 <- tibble(id = "คะแนนเฉลี่ยของรายวิชา",
                   midterm1 = mean(dat$midterm1),
                   midterm2 = mean(dat$midterm2))
      
      mid2 <- s.dat() %>% select(id,midterm1,midterm2)
      
      mid.dat <- rbind(mid2,mid1)
      
      plot.mid <- mid.dat %>% plot_ly(x =~ id,
                    y =~ midterm1,
                    name = "ปรนัย",
                    type = "bar") %>%
        add_trace(y =~ midterm2,
                  name = "อัตนัย") %>%
        layout(barmode = "stack",
               yaxis = list(title = "คะแนน",
                            range = c(0,60)),
               xaxis = list(title = ""))
    })
      
      output$plot.final <- renderPlotly({      
        f1 <- tibble(id = "คะแนนเฉลี่ยของรายวิชา",
                       final1 = mean(dat$final1),
                       final2 = mean(dat$final2))
        
        f2 <- s.dat() %>% select(id,final1,final2)
        
        f.dat <- rbind(f2,f1)
        
        plot.final <- f.dat %>% plot_ly(x =~ id,
                                        y =~ final1,
                                        name = "ปรนัย",
                                        type = "bar") %>%
          add_trace(y =~ final2,
                    name = "อัตนัย") %>%
          layout(barmode = "stack",
                 yaxis = list(title = "คะแนน",
                              range = c(0,60)),
                 xaxis = list(title = ""))
      }) 
      
      output$s.pro <- renderValueBox(
      valueBox(s.dat()$project,
               "คะแนนโปรเจ็ค (10)",
               color = ifelse(s.dat()$project < 5,"red","blue"))
        )
      output$s.mid <- renderValueBox(
        valueBox(s.dat()$midterm,
                 "คะแนนสอบกลางภาค (60)",
                 color = ifelse(s.dat()$midterm < 30,"red","blue"))
          )
        output$s.final <- renderValueBox(
          valueBox(s.dat()$final,
                   "คะแนนสอบปลายภาค (60)",
                   color = ifelse(s.dat()$final < 30,"red","blue"))
          )
        }

        else {
          output$error <- renderText({paste("รหัสนิสิตไม่ถูกต้อง")})
          output$plot.time <- NULL
          output$plot.hw <- NULL
          output$s.pro <- NULL
          output$s.mid <- NULL
          output$s.final <- NULL

        }
})   

    })
    
    observeEvent(input$submit2, {
      observeEvent(input$pw, {
        if(input$pw == "123456") {
          output$error2 <- NULL
          
          output$t.pro <- renderValueBox(
            valueBox(round(mean(dat$project),2),
                     "คะแนนโปรเจ็คเฉลี่ย (10)",
                     color = ifelse(mean(dat$project) < 5,"red","blue"))
            )
          output$t.mid <- renderValueBox(
            valueBox(round(mean(dat$midterm),2),
                     "คะแนนสอบกลางภาคเฉลี่ย (60)",
                     color = ifelse(mean(dat$midterm) < 30,"red","blue"))
          )
          output$t.final <- renderValueBox(
            valueBox(round(mean(dat$final),2),
                     "คะแนนสอบปลายภาคเฉลี่ย (60)",
                     color = ifelse(mean(dat$final) < 30,"red","blue"))
          )
          
          output$plot.t1 <- renderPlotly({
            dat.t11 <- dat %>% pivot_longer(cols = contains("time"),
                                           names_to = "week",
                                           values_to = "time") %>%
            select(id,section,week,time)
            dat.t11$week <- as.integer(sub("time","", dat.t11$week))
            
            dat.t12 <- dat %>% pivot_longer(cols = contains("hw"),
                           names_to = "week",
                           values_to = "score") %>%
              select(id,section,week,score)
            dat.t12$week <- as.integer(sub("hw","", dat.t12$week))
              glimpse(dat.t12)                                  
            dat.t1 <- inner_join(dat.t11,dat.t12, by = c("id","section","week"))
            
            plot.t1 <- dat.t1 %>% plot_ly(x =~ time,
                                          y =~ score,
                                          split =~ factor(section),
                                          frame =~ week,
                                          hoverinfo = "text",
                                          type = 'scatter',
                                          mode = 'markers',
                                          text = ~paste("</br>", id,
                                                       "</br> เวลาเข้าเรียน :", time,
                                                       "</br> คะแนนการบ้าน :", score)) %>%
                                  layout(xaxis = list(title = "่เวลาเข้าเรียน (นาที)",
                                                      range = c(0,190)),
                                         yaxis = list(title = "คะแนนการบ้าน",
                                                      range = c(0,11)),
                                         legend=list(title=list(text="<b> ตอนเรียน </b>"))) %>%
              animation_opts(1500, easing = "elastic", redraw = FALSE)
                            
            
          })
          dat.t21 <- dat %>% select(id,section,project,midterm1,midterm2) %>%
            mutate(exam = factor("midterm")) %>%
            rename("part1" = "midterm1","part2" = "midterm2")
          
          
          dat.t22 <- dat %>% select(id,section,project,final1,final2) %>%
            mutate(exam = factor("final")) %>%
            rename("part1" = "final1","part2" = "final2")
          
          dat.t2 <- rbind(dat.t21,dat.t22)
          
          output$plot.t2 <- renderPlotly({
            plot.t2 <- dat.t2 %>% plot_ly(x =~ part1,
                                          y =~ part2,
                                          split =~ factor(section),
                                          frame =~ exam,
                                          hoverinfo = "text",
                                          type = 'scatter',
                                          mode = 'markers',
                                          text = ~paste("</br>", id,
                                                        "</br> ปรนัย :", part1,
                                                        "</br> อัตนัย :", part2,
                                                        "</br> โปรเจ็ค :", project)) %>%
              layout(xaxis = list(title = "คะแนนปรนัย",
                                  range = c(0,42)),
                     yaxis = list(title = "คะแนนอัตนัย",
                                  range = c(0,22)),
                     legend=list(title=list(text="<b> ตอนเรียน </b>"))) %>%
              animation_opts(1500, easing = "elastic", redraw = FALSE)
             
          })
          output$plot.time2 <- renderPlotly({
            time.dat24 <- dat %>% pivot_longer(cols = contains("time"), 
                                               names_to = "week", 
                                               values_to = "time") %>%
              group_by(week) %>% select(section,week,time) %>%
              filter(section == 4)
            time.dat24$week <- as.integer(sub("time","", time.dat24$week))
            time.dat24 <- time.dat24 %>% summarise(time = mean(time)) %>%
              mutate(section = 4)
            time.dat24$time <- round(time.dat24$time,1)
            
            time.dat25 <- dat %>% pivot_longer(cols = contains("time"), 
                                               names_to = "week", 
                                               values_to = "time") %>%
              group_by(week) %>% select(section,week,time) %>%
              filter(section == 5)
            time.dat25$week <- as.integer(sub("time","", time.dat25$week))
            time.dat25 <- time.dat25 %>% summarise(time = mean(time)) %>%
              mutate(section = 5)
            time.dat25$time <- round(time.dat25$time,1)
            
            time.dat22 <- rbind(time.dat24,time.dat25)
            
            plot.time2 <- time.dat22 %>% plot_ly(x =~ week,
                                                 y =~ time,
                                                 hoverinfo = "text",
                                                 split =~ section,
                                                 mode = "lines+markers",
                                                 text =~ paste(
                                                   "</br> สัปดาห์ที่ :", week,
                                                   "</br> เวลาเรียน :", time)) %>%
                    layout(xaxis = list(title = "สัปดาห์ที่",
                                  dtick = 2),
                     yaxis = list(title = "เวลาเรียนเฉลี่ย (นาที)",
                                  range = c(0,180)),
                     legend=list(title=list(text="<b> ตอนเรียน </b>"))
              )
            
          })
          
          output$plot.hw2 <- renderPlotly({
            hw.dat24 <- dat %>% pivot_longer(cols = contains("hw"), 
                                               names_to = "week", 
                                               values_to = "score") %>%
              group_by(week) %>% select(section,week,score) %>%
              filter(section == 4)
            hw.dat24$week <- as.integer(sub("hw","", hw.dat24$week))
            hw.dat24 <- hw.dat24 %>% summarise(score = mean(score)) %>%
              mutate(section = 4)
            hw.dat24$score <- round(hw.dat24$score,1)
            
            hw.dat25 <- dat %>% pivot_longer(cols = contains("hw"), 
                                             names_to = "week", 
                                             values_to = "score") %>%
              group_by(week) %>% select(section,week,score) %>%
              filter(section == 5)
            hw.dat25$week <- as.integer(sub("hw","", hw.dat25$week))
            hw.dat25 <- hw.dat25 %>% summarise(score = mean(score)) %>%
              mutate(section = 5)
            hw.dat25$score <- round(hw.dat25$score,1)
            
            hw.dat22 <- rbind(hw.dat24,hw.dat25)
            
            plot.hw2 <- hw.dat22 %>% plot_ly(x =~ week,
                                                 y =~ score,
                                                 hoverinfo = "text",
                                                 split =~ section,
                                                 mode = "lines+markers",
                                                 text =~ paste(
                                                   "</br> สัปดาห์ที่ :", week,
                                                   "</br> คะแนน :", score)) %>%
              layout(xaxis = list(title = "สัปดาห์ที่",
                                  dtick = 2),
                     yaxis = list(title = "คะแนนเฉลี่ย",
                                  range = c(0,10)),
                     legend=list(title=list(text="<b> ตอนเรียน </b>"))
              )
            
          })
          output$plot.mid2 <- renderPlotly({
            mid.dat2 <- dat %>% group_by(section) %>%
                          summarise(midterm1 = mean(midterm1),
                                    midterm2 = mean(midterm2))

            plot.mid2 <- mid.dat2 %>% plot_ly(x =~ factor(section),
                                              y =~ midterm1,
                                              name = "ปรนัย",
                                              type = "bar") %>%
              add_trace(y =~ midterm2,
                        name = "อัตนัย") %>%
              layout(barmode = "stack",
                     yaxis = list(title = "คะแนน",
                                  range = c(0,60)),
                     xaxis = list(title = "ตอนเรียน"))
                  
            
          })
          output$plot.final2 <- renderPlotly({
            final.dat2 <- dat %>% group_by(section) %>%
              summarise(final1 = mean(final1),
                        final2 = mean(final2))
            
            plot.final2 <- final.dat2 %>% plot_ly(x =~ factor(section),
                                              y =~ final1,
                                              name = "ปรนัย",
                                              type = "bar") %>%
              add_trace(y =~ final2,
                        name = "อัตนัย") %>%
              layout(barmode = "stack",
                     yaxis = list(title = "คะแนน",
                                  range = c(0,60)),
                     xaxis = list(title = "ตอนเรียน"))
            
          })
        }
        else {
          output$error2 <- renderText({paste("รหัสผ่านไม่ถูกต้อง")})
          output$t.pro <- NULL
          output$t.mid <- NULL
          output$t.final <- NULL
          output$plot.time2 <- NULL
          output$plot.hw2 <- NULL
          output$plot.mid2 <- NULL
          output$plot.final2 <- NULL
          output$plot.t1 <- NULL
          output$plot.t2 <- NULL
        }
      })
    })
}

shinyApp(ui, server)
