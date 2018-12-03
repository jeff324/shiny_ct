#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(maps)
library(plyr)
library(dplyr)
library(ggplot2)
library(reshape2)
library(lubridate)
library(nnet)



load_file = function(file_name){
     load(file_name)
     obj_names = ls()
     obj_names = obj_names[obj_names != 'file_name']
     if(length(obj_names) > 1){
          #return a list of objects
          dat = sapply(obj_names, function(x)get(x), simplify=FALSE, USE.NAMES=TRUE)
     }else{
          #return a single object
          dat = get(obj_names)     
     }
     return(dat)
}

load_theme_2 = function(){
     theme(axis.line = element_line(color = 'black'),
           panel.border = element_rect(color='black',fill=NA),
           legend.title = element_text(size=18),
           legend.text = element_text(size=15),
           strip.background = element_blank(),
           strip.text.x = element_text(size=14),
           strip.text.y = element_text(size=14),
           axis.text.x = element_text(size=14),
           axis.text.y = element_text(size=14),
           axis.title = element_text(size=14),
           legend.background = element_rect(colour = "black"))      
}


load_theme = function(){
     theme(axis.line = element_line(colour = "black"),
           axis.text = element_text(colour = "black"),
           panel.grid.major = element_blank(),
           panel.grid.minor = element_blank(),
           strip.background = element_blank(),
           panel.border = element_blank(),
           panel.background = element_blank(),
           legend.title = element_text(size=18),
           legend.text = element_text(size=15),
           strip.text.x = element_text(size=14),
           strip.text.y = element_text(size=14),
           axis.text.x = element_text(size=14),
           axis.text.y = element_text(size=14),
           axis.title = element_text(size=14),
           legend.background = element_rect(colour = "black"))      
}

predict_prob = function(dat,test_year,test_age){
     fit_hf = multinom(hf ~ Age + Year + Sex,na.omit(dat))
     test_df = data.frame(Sex = rep(c('Male','Female'),each=length(test_age)*length(test_year)),
                          Age=test_age,
                          Year=test_year)
     pred_df = cbind(test_df, predict(fit_hf, newdata = test_df, type = "probs", se = TRUE))
     pred_df_melt = melt(pred_df,id.vars = c('Sex','Age','Year'),variable.name='Drug Present')    
     return(pred_df_melt)
}

predict_prob_race = function(dat,test_year,test_age){
     fit_hf = multinom(hf ~ Age + Year + Race,na.omit(dat))
     test_df = data.frame(Race = rep(c('White','Black','Hispanic'),each=length(test_age)*length(test_year)), 
                          Age=test_age,
                          Year=test_year)
     pred_df = cbind(test_df, predict(fit_hf, newdata = test_df, type = "probs", se = TRUE))
     pred_df_melt = melt(pred_df,id.vars = c('Race','Age','Year'),variable.name='Drug Present')    
     return(pred_df_melt)
}

predict_prob_race_sex = function(dat,test_year,test_age){
     fit_hf = multinom(hf ~ Age + Year + Race + Sex,na.omit(dat))
     test_df = expand.grid(list(Race = rep(c('White','Black','Hispanic')),
                                Sex = rep(c('Male','Female')),
                                Age = test_age,
                                Year = test_year))
     pred_df = cbind(test_df, predict(fit_hf, newdata = test_df, type = "probs", se = TRUE))
     pred_df_melt = melt(pred_df,id.vars = c('Race','Age','Year','Sex'),variable.name='Drug Present')    
     return(pred_df_melt)
}

dat = load_file('data/dat.RData')

colnames(dat)[which(colnames(dat)=='Morphine..not.heroin.')] = 'Morphine'

dv_idx = c(which(colnames(dat)=='Heroin'):which(colnames(dat)=='Morphine'))
for(i in dv_idx){
     dat[,i] = as.numeric(as.character(mapvalues(dat[,i],c('Y','y',''),c('1','1','0'))))
}

dat_long = melt(dat,id.vars = c('CaseNumber', 'Date','Age','Sex','Race','Death.City','Location','lat','lon'),
                measure.vars = dv_idx,variable.name = 'drug')


#top 5 causes
dat_long %>%
     group_by(drug) %>%
     summarise(num_deaths = sum(value,na.rm=T)) %>%
     arrange(desc(num_deaths)) ->
     drug_death

dat_long %>%
     group_by(Death.City,lat,lon) %>%
     summarise(num_deaths = sum(value,na.rm=T)) %>%
     arrange(desc(num_deaths)) ->
     city_death

dat_long$Date = mdy(dat_long$Date)
dat_long$Year = year(dat_long$Date)

ct = map_data('state') %>% filter(region=='connecticut')

city_death$lat = as.numeric(as.character(city_death$lat))
city_death$lon = as.numeric(as.character(city_death$lon))

dat$Date = mdy(dat$Date)
dat$Year = year(dat$Date)
dat$hf = NA
dat$hf[dat$Heroin == 1 & dat$Fentanyl == 0] = 'Heroin'
dat$hf[dat$Heroin == 0 & dat$Fentanyl == 1] = 'Fentanyl'
dat$hf[dat$Heroin == 1 & dat$Fentanyl == 1] = 'Heroin & Fentanyl'
dat$hf[dat$Heroin == 0 & dat$Fentanyl == 0 & 
            dat$Cocaine == 0 & dat$EtOH == 0 & 
            dat$Amphet == 0 & dat$Benzodiazepine == 0] = 'Prescription Opiate'

dat$hf = factor(dat$hf, levels = sort(unique(dat$hf)))


mytheme = load_theme()



ui <- fluidPage(


   headerPanel(HTML('<h1>Drug Related Deaths in Connecticut from 2012-2017<h1>
                    <h3><a href=https://data.ct.gov/Health-and-Human-Services/Accidental-Drug-Related-Deaths-2012-2017/rybz-nyjw>
                   Source: data.ct.gov</a></h3>'),
               'CT Drug Related Deaths'),
   # Sidebar with a slider input for number of bins 
   
   sidebarLayout(
      sidebarPanel(
           checkboxGroupInput(inputId = 'drug', 
                     label = 'Select drug',
                     choices = as.character(drug_death$drug),selected = 'Heroin'),
      checkboxGroupInput(inputId = 'year', 
                         label = 'Select year',
                         choices = c('All years',paste(2012:2017)),selected = 'All years')
     ),
      
      
      # Show a plot of the generated distribution
      mainPanel(
           tabsetPanel(
               tabPanel('Map',plotOutput("ct_plot")),
               tabPanel('Age',plotOutput('age_plot')),
               tabPanel('Year',plotOutput('year_plot')),
               tabPanel('Year by Age',plotOutput('year_age_plot',height='800px')),
               tabPanel('Model by Sex',plotOutput('model_sex',height='800px')),
               tabPanel('Model by Race',plotOutput('model_race',height='800px'))
           )
      )
   )
   

)

# Define server logic required to draw a histogram
server <- function(input, output) {
   

     
   output$ct_plot <- renderPlot({
        if(any(input$year == 'All years')){
             yrs = 2012:2017
        }else{
             yrs = input$year
        }
        
        dat_long %>%
             filter(Year %in% yrs) %>%
             group_by(drug,lat,lon,Death.City) %>%
             summarise(num_deaths = sum(value,na.rm=T)) %>%
             filter(drug %in% input$drug) %>%
             arrange(desc(num_deaths)) %>%
             filter(num_deaths > 0) ->
             dat_loc
        
        dat_loc$lat = as.numeric(as.character(dat_loc$lat))
        dat_loc$lon = as.numeric(as.character(dat_loc$lon))
         
         p1 = ggplot(data=dat_loc) +
              geom_polygon(data = ct, aes(x=long, y = lat, group = group), fill="grey", alpha=0.3) +
              geom_point(aes(x=lon, y=lat, size = num_deaths, color = drug, alpha = num_deaths)) +
              geom_text(data = city_death[1:10,], aes(x=lon, y=lat,label=Death.City), size=3) +
              scale_size_continuous(name="Number of Deaths",range=c(1,8)) +
              scale_alpha_continuous(name="Number of Deaths") +
              scale_color_discrete(name='Drug')  +
              coord_map() +
              theme_void() +
              theme(legend.text = element_text(size=15),
                    legend.title = element_text(size=20)) +
              guides( colour = guide_legend(override.aes = list(size=5)))
         plot(p1)
   })
   
   output$age_plot <- renderPlot({
        
        if(any(input$year == 'All years')){
             yrs = 2012:2017
        }else{
             yrs = input$year
        }
        dat_long %>%
             filter(Year %in% yrs) %>%
             group_by(Age,drug) %>%
             summarise(p_death = mean(value,na.rm=T)) %>%
             filter(drug %in% input$drug) %>%
             arrange(desc(p_death)) ->
             age_death_prob
        
        p2 = ggplot(data=age_death_prob,aes(x=Age,y=p_death,fill=drug)) +
             geom_bar(stat = 'identity',position = position_dodge(.9)) +
             labs(y='P(Drug Present)',x='Age') +
             scale_fill_discrete(name='Drug') +
             scale_x_continuous(breaks=seq(15,85,by=5)) +
             scale_y_continuous(limits=c(0,1)) +
             mytheme
        plot(p2)
        
        plot(p2)
   })
   
   output$year_plot <- renderPlot({
        if(any(input$year == 'All years')){
             yrs = 2012:2017
        }else{
             yrs = input$year
        }
        dat_long %>%
             filter(Year %in% yrs) %>%
             group_by(drug,Year) %>%
             summarise(p_death = mean(value,na.rm=T)) %>%
             filter(drug %in% input$drug) ->
             year_death_prob
        
        p3 = ggplot(data=year_death_prob,aes(x=Year,y=p_death,group=drug,color=drug)) +
             geom_point(size=3) +
             geom_line(size=1) +
             labs(y='P(Drug Present)') +
             scale_color_discrete(name='Drug') +
             scale_y_continuous(limits=c(0,1)) +
             load_theme()
        plot(p3)
   })
   
   output$year_age_plot <- renderPlot({

        if(any(input$year == 'All years')){
             yrs = 2012:2017
        }else{
             yrs = input$year
        }
        dat_long %>%
             filter(Year %in% yrs) %>%
             group_by(Year,drug,Age) %>%
             summarise(p_death = mean(value,na.rm=T)) %>%
             filter(drug %in% input$drug) %>%
             na.omit() ->
             year_age_death_prob
        
        p5 = ggplot(data=year_age_death_prob,
                    aes(x=Age,y=p_death,fill=drug),color='white') +
             geom_bar(stat = 'identity',position = 'dodge') +
             labs(y='P(Drug Present)',x='Age') +
             scale_x_continuous(breaks=seq(15,85,by=5)) + 
             scale_fill_discrete(name='Drug') +
             scale_y_continuous(limits=c(0,1)) +
             facet_grid(Year~.) + 
             mytheme
        plot(p5)   
   })

 
   output$model_sex <- renderPlot({
        
        if(any(input$year == 'All years')){
             yrs = 2012:2017
        }else{
             yrs = input$year
        }
        
        test_age = 15:90
        test_year = as.numeric(c(yrs,2018,2019))
        dat %>% filter(Year %in% yrs) -> dat
        dat$Race = as.character(dat$Race)
        dat$Race[dat$Race == 'Hispanic, Black' | dat$Race == 'Hispanic, White'] = 'Hispanic'
        dat = dat[dat$Race == 'Hispanic' | dat$Race == 'White' | dat$Race == 'Black',]
        
        preds =  predict_prob_race_sex(dat,test_year,test_age)
        
        preds %>% 
             mutate(Age_int=cut_interval(Age,5)) %>%
             group_by(Year,Sex,Age_int,`Drug Present`) %>%
             summarise(value = mean(value)) ->
             preds_sex
        preds_sex$Age_int = paste('Age:',preds_sex$Age_int,sep='')
        mytheme = load_theme_2()
        p_model_sex = ggplot(data = preds_sex, aes(x=Year,y=value,group=`Drug Present`,color=`Drug Present`)) +
             geom_line() +
             facet_grid(Age_int~Sex) +
             labs(y='Predicted P(Drug Present)') +
             ggtitle('Multinomial logreg: Drug ~ Year + Age + Sex + Race') +
             scale_y_continuous(limits=c(0,1)) +
             mytheme
        plot(p_model_sex)
        
        
   })
   
   output$model_race <- renderPlot({
        if(any(input$year == 'All years')){
             yrs = 2012:2017
        }else{
             yrs = input$year
        }
        
        test_age = 15:90
        test_year = as.numeric(c(yrs,2018,2019))
        dat %>% filter(Year %in% yrs) -> dat
        dat$Race = as.character(dat$Race)
        dat$Race[dat$Race == 'Hispanic, Black' | dat$Race == 'Hispanic, White'] = 'Hispanic'
        dat = dat[dat$Race == 'Hispanic' | dat$Race == 'White' | dat$Race == 'Black',]
        
        preds = predict_prob_race_sex(dat,test_year,test_age)
        preds %>% 
             mutate(Age_int=cut_interval(Age,5)) %>%
             group_by(Year,Race,Age_int,`Drug Present`) %>%
             summarise(value = mean(value)) ->
             preds_race
        preds_race$Age_int = paste('Age:',preds_race$Age_int,sep='')
        mytheme = load_theme_2()
        preds_race$Race = factor(preds_race$Race,levels=c('Black','Hispanic','White'))
        p_model_race = ggplot(data = preds_race, aes(x=Year,y=value,group=`Drug Present`,color=`Drug Present`)) +
             geom_line() +
             facet_grid(Age_int~Race) +
             labs(y='Predicted P(Drug Present)') +
             ggtitle('Multinomial logreg: Drug ~ Year + Age + Sex + Race') +
             scale_y_continuous(limits=c(0,1)) +
             mytheme + 
             theme(axis.text.x = element_text(angle = 65, hjust = 1))
        plot(p_model_race)
        })
   
}

# Run the application 
shinyApp(ui = ui, server = server)

