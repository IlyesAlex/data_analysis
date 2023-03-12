# Load packages
library(shiny)
library(shinythemes)
library(dplyr)
#library(readr)
library(tidyverse)
#library(tidytuesdayR)
library(readxl)
library(tidyr)
library(scales)
library(ggplot2)
library(shinyWidgets)
#library(ggdist)


# CUSTOM THEMES
theme_set(theme_light())

custom_theme = theme(
  plot.title = element_text(color="black", size=15, face="bold",  hjust = 0.5 ), 
  axis.text.x = element_text(vjust = 0.5, hjust=0.5, size = 12, face="bold"),
  axis.text.y = element_text(vjust = 0.5, hjust=0.5, size = 10, face="bold"),
  axis.title.x = element_text(vjust = 0.5, hjust=0.5, size = 14, face="bold"),
  axis.title.y = element_text(vjust = 0.5, hjust=0.5, size = 14, face="bold"),
  legend.text = element_text(vjust = 0.5, hjust=0.5, size = 10, face="bold"),
  legend.title = element_text(vjust = 0.5, hjust=0.5, size = 12, face="bold"),
  strip.text = element_text(vjust = 0.5, hjust=0.5, size = 13, face="bold") 
)

# READ IN DATA
semmst_data = read.csv2(text = readLines("smst_all_longformat.csv", warn = FALSE),header=T, colClasses = c("ID" = "character")) 
covariate_data = read.csv2(text = readLines("papir_ceruza_logs.csv", warn = FALSE),header=T, colClasses = c("ID" = "character")) %>% 
  select(-AgeGroup)

#semmst_data2 = read_excel("analysis/data_viewer/smst_all_longformat.xlsx")
#covariate_data = read_excel("papir_ceruza_logs.xlsx") %>% 
#  rename(ID = `Résztvevő kódja`)

semmst_data = merge(semmst_data, covariate_data, by = "ID") %>% 
  mutate(Sex = ifelse(Sex == "nő", "female", "male"))

semmst_data_rec = semmst_data %>%
  dplyr::filter(task=="REC")

semmst_data_enc = semmst_data %>%
  dplyr::filter(task=="ENC")

## MST
mst_data = read_excel("mst_all_longformat.xlsx")
mst_enc = mst_data %>%
  filter(task == 'ENC')

mst_rec = mst_data %>%
  filter(task == 'REC')


# PALT
palt_data = read_excel("palt_all_longformat.xlsx")

# NBACK
nback_data = read_excel("nback_all_longformat.xlsx")

nback_data_filtered = nback_data %>%
  filter(block != 'nback_1_practice' & block != 'nback_2_practice')


# CONSTRUCT DATASETS FOR PLOTS
s_id = c("004077", "005704", "006150", "030718", "036190", "038953", "076057", "085639", "093503", "104096", "123479", "124080", "158228", "198059", "211099", "220502", "235864", "247786", "272304", "288292", "292991", "337352", "343064", "357990", "384825", "392539", "396251", "401526", "417089", "421568", "421865", "430442", "433288", "449122", "457887", "459283", "483574", "483932", "505193", "548792", "564241", "568568", "573588", "593958", "626942", "631197", "658018", "663878", "664334", "676145", "681251", "690719", "696365", "697600", "704497", "726775", "728692", "729477", "733782", "734689", "756279", "775124", "801231", "806000", "808485", "851046", "854499", "871070", "878924", "898339", "905303", "909857", "924806" ,"924863", "928116", "938383", "954830", "988938", "993115", "997941")
#print(s_id)
#ID_colors = setNames(palette(rainbow(80)), s_id)
ID_colors <- c("red", "#FF1300", "#FF2600", "#FF3900", "#FF4D00", "#FF6000", "#FF7300", "#FF8600", "#FF9900", "#FFAC00", "#FFBF00", "#FFD200", "#FFE600", "#FFF900", "#F2FF00", "#DFFF00", "#CCFF00", "#B9FF00", "#A6FF00", "#93FF00", "#80FF00", "#6CFF00", "#59FF00", "#46FF00", "#33FF00", "#20FF00", "#0DFF00", "#00FF06", "#00FF1A", "#00FF2D", "#00FF40", "#00FF53", "#00FF66", "#00FF79", "#00FF8C", "#00FF9F", "#00FFB3", "#00FFC6", "#00FFD9", "#00FFEC", "cyan",    "#00ECFF", "#00D9FF", "#00C6FF", "#00B2FF", "#009FFF", "#008CFF", "#0079FF", "#0066FF", "#0053FF", "#0040FF", "#002DFF", "#0019FF", "#0006FF", "#0D00FF", "#2000FF", "#3300FF", "#4600FF", "#5900FF", "#6C00FF", "#8000FF", "#9300FF", "#A600FF", "#B900FF", "#CC00FF", "#DF00FF", "#F200FF", "#FF00F9", "#FF00E5", "#FF00D2", "#FF00BF", "#FF00AC", "#FF0099", "#FF0086", "#FF0073", "#FF0060", "#FF004C", "#FF0039", "#FF0026", "#FF0013")
names(ID_colors) <- s_id

### sMST
enc_percentage = semmst_data_enc %>% 
  dplyr::filter(response != "None") %>% 
  select(ID, AgeGroup, stimtype, response) %>% 
  group_by(ID, AgeGroup, stimtype, response) %>% 
  count() %>%
  group_by(ID, stimtype) %>% 
  mutate(percentage = n/sum(n))

enc_rt = semmst_data_enc %>% 
  drop_na(reaction_time) %>% 
  select(ID, AgeGroup, itemno, stimtype, response, reaction_time)
  
rec_percentage = semmst_data_rec %>%
  dplyr::filter(response != "None") %>%
  select(AgeGroup, ID, stimtype, response) %>%
  group_by(AgeGroup, ID, stimtype, response) %>% dplyr::filter(response != "4") %>% 
  count() %>%
  ungroup() %>%
  group_by(AgeGroup, ID, stimtype) %>%
  mutate(percentage_id = n/sum(n)) %>%
  ungroup()
  
rec_rt = semmst_data_rec %>% 
  drop_na(reaction_time) %>%
  dplyr::filter(response != "4") %>% 
  select(ID, AgeGroup, itemno, stimtype, response, reaction_time) 

### MST
mst_enc_perc = mst_enc %>%
  drop_na(response) %>% 
  group_by(AgeGroup, ID, condition, response) %>%
  count() %>%
  ungroup() %>%
  group_by(AgeGroup, ID, condition) %>%
  mutate(percentage_id = n/sum(n))

mst_enc_rt = mst_enc %>%
  group_by(ID, response) %>%
  drop_na(reaction_time)

mst_rec_perc = mst_rec %>%
  drop_na(response) %>%
  select(AgeGroup, ID, condition, response) %>%
  group_by(AgeGroup, ID, condition, response) %>%
  count() %>%
  ungroup() %>%
  group_by(AgeGroup, ID, condition) %>%
  mutate(percentage_id = n/sum(n)) %>%
  ungroup()

mst_rec_rt = mst_rec %>%
  drop_na(response) %>% 
  group_by(ID, condition)

### NBACK
nback_rate = nback_data_filtered %>% 
  group_by(AgeGroup, block, ID, response_type) %>%
  count() %>%
  ungroup() %>%
  group_by(AgeGroup, ID, block) %>%
  mutate(percentage_id = n/sum(n)) %>%
  ungroup()

nback_rt = nback_data_filtered %>%
  filter(response_type == 'hit' | response_type == 'false alarm')


### COVARIATES
paper_pencil_data_long = semmst_data %>%
  select(ID, AgeGroup, Sex, Age, Beck, Vocabulary, DigitSymbol, Non_word_span, Non_word_sum) %>% 
  distinct() %>% 
  gather(questionnaire, result, -ID, -AgeGroup, -Sex, -Age)

palt_plot = palt_data %>% 
  group_by(ID, AgeGroup, which_rep, correctness) %>% 
  summarise(sum_corr = n()) %>% 
  filter(correctness == 1) %>% 
  ungroup() %>% 
  group_by(ID, AgeGroup) %>% 
  top_n(1, sum_corr) %>% 
  gather(score, result, -ID, -AgeGroup, -correctness)
  

semmst_age_OA = semmst_data %>% 
  dplyr::filter(AgeGroup != "YA")
semmst_age_YA = semmst_data %>% 
  dplyr::filter(AgeGroup != "OA")

# Define UI
ui <- shinyUI(fluidPage(
  
  # Application title
  titlePanel("Looking at one persons data"),
  
  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      radioGroupButtons(inputId = "select_ID",
                  label = "Choose an Old Adult ID:",
                  choices = unique(semmst_age_OA$ID),
                  status = "danger"),
      radioGroupButtons(inputId = "select_ID_YA",
                        label = "Choose a Young Adult ID:",
                        choices = unique(semmst_age_YA$ID),
                        status = "primary"),
      htmlOutput(outputId= "smst_text"),
      htmlOutput(outputId= "smst_text_YA")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("SMST Plots", 
                           fluidRow(
                    
                    column(width = 6,
                           plotOutput("smstplot1")
                    ),
                    column(width = 6,
                           plotOutput("smstplot2"))
                  ),
                  fluidRow(
                    column(width = 6,
                           plotOutput("smstplot3")
                    ),
                    column(width = 6,
                           plotOutput("smstplot4"))
                  ),
                  fluidRow(
                    column(width = 7,
                           plotOutput("nback_plot1")
                    ),
                    column(width = 5,
                           plotOutput("nback_plot2"))
                  )),
                  tabPanel("MST Plots", 
                           fluidRow(
                             
                             column(width = 6,
                                    plotOutput("mstplot1")
                             ),
                             column(width = 6,
                                    plotOutput("mstplot2"))
                           ),
                           fluidRow(
                             column(width = 6,
                                    plotOutput("mstplot3")
                             ),
                             column(width = 6,
                                    plotOutput("mstplot4"))
                           )),
                  tabPanel("Covariate Plots", 
                           plotOutput("covariate_plot", 
                                      width = 1200,
                                      height = 700),
                           plotOutput("palt_plot_1",
                                      width = 800,
                                      height = 350)),
      )
      
    )
  )
))

# Define server
server <- shinyServer(function(input, output) {
  
  #OA
  dat <- reactive({
    enc_percentage %>% dplyr::filter(ID == input$select_ID)
  })
  
  dat_enc_rt <- reactive({
    enc_rt %>% dplyr::filter(ID == input$select_ID)
  })
  
  dat_rec_percent <- reactive({
    rec_percentage %>% dplyr::filter(ID == input$select_ID)
  })
  
  dat_rec_rt <- reactive({
    rec_rt %>% dplyr::filter(ID == input$select_ID)
  })
  
  dat_text <- reactive({
    semmst_data %>% dplyr::filter(ID == input$select_ID)
  })
  
  
  
  dat_mst_enc_p <- reactive({
    mst_enc_perc %>% dplyr::filter(ID == input$select_ID)
  })
  
  dat_mst_enc_r <- reactive({
    mst_enc_rt %>% dplyr::filter(ID == input$select_ID)
  })
  
  dat_mst_rec_p <- reactive({
    mst_rec_perc %>% dplyr::filter(ID == input$select_ID)
  })
  
  dat_mst_rec_r <- reactive({
    mst_rec_rt %>% dplyr::filter(ID == input$select_ID)
  })
  
  
  
  paper_pencil_dat <- reactive({
    paper_pencil_data_long %>% dplyr::filter(ID == input$select_ID)
  })
  
  palt_dat <- reactive({
    palt_plot %>% dplyr::filter(ID == input$select_ID)
  })
  
  nback_rate_dat <- reactive({
    nback_rate %>% dplyr::filter(ID == input$select_ID)
  })
  
  nback_rt_dat <- reactive({
    nback_rt %>% dplyr::filter(ID == input$select_ID)
  })
  
  
  #YA
  dat_YA <- reactive({
    enc_percentage %>% dplyr::filter(ID == input$select_ID_YA)
  })
  
  dat_enc_rt_YA <- reactive({
    enc_rt %>% dplyr::filter(ID == input$select_ID_YA)
  })
  
  dat_rec_percent_YA <- reactive({
    rec_percentage %>% dplyr::filter(ID == input$select_ID_YA)
  })
  
  dat_rec_rt_YA <- reactive({
    rec_rt %>% dplyr::filter(ID == input$select_ID_YA)
  })
  
  dat_text_YA <- reactive({
    semmst_data %>% dplyr::filter(ID == input$select_ID_YA)
  })
  
  
  
  dat_mst_enc_p_YA <- reactive({
    mst_enc_perc %>% dplyr::filter(ID == input$select_ID_YA)
  })
  
  dat_mst_enc_r_YA <- reactive({
    mst_enc_rt %>% dplyr::filter(ID == input$select_ID_YA)
  })
  
  dat_mst_rec_p_YA <- reactive({
    mst_rec_perc %>% dplyr::filter(ID == input$select_ID_YA)
  })
  
  dat_mst_rec_r_YA <- reactive({
    mst_rec_rt %>% dplyr::filter(ID == input$select_ID_YA)
  })
  
  
  
  
  paper_pencil_dat_YA <- reactive({
    paper_pencil_data_long %>% dplyr::filter(ID == input$select_ID_YA)
  })
  
  palt_dat_YA <- reactive({
    palt_plot %>% dplyr::filter(ID == input$select_ID_YA)
  })
  
  nback_rate_dat_YA <- reactive({
    nback_rate %>% dplyr::filter(ID == input$select_ID_YA)
  })
  
  nback_rt_dat_YA <- reactive({
    nback_rt %>% dplyr::filter(ID == input$select_ID_YA)
  })
  
    
  output$smstplot1 <- renderPlot({
    
    # draw the histogram with the specified number of bins
    
    enc_percentage %>% 
      ggplot() + 
        geom_bar(aes(x=response, y=percentage), alpha = .6, stat="summary") +
        geom_bar(data = dat(), aes(x=response, y=percentage, fill = ID), alpha = .4, stat = "identity") +
        geom_bar(data = dat_YA(), aes(x=response, y=percentage, fill = ID), alpha = .4, stat = "identity") +
        scale_fill_manual(values = ID_colors, drop = TRUE, breaks = c(input$select_ID, input$select_ID_YA)) +
        custom_theme + 
        labs(x = "Response made", y = "Percentage of responses", title = "ENC: Good - not good") +
        scale_y_continuous(breaks = seq(0,1, by =0.2), labels=percent) +
        facet_grid(AgeGroup~stimtype,
                   labeller = labeller(AgeGroup = c("OA" ="Old Adults", "YA" = "Young Adults"))) +
        coord_cartesian(ylim = c(0, 1))
    
  })
  
  output$smstplot2 <- renderPlot({
    
    # draw the histogram with the specified number of bins
    
    enc_rt %>% 
      ggplot(aes(x = response, y = reaction_time)) +
      custom_theme +
      geom_violin(position = position_dodge()) +
      geom_boxplot(position = position_dodge(width = 1), width = 0.3) +
      geom_point(data = dat_enc_rt(), aes(colour = ID), stat = "summary", size = 3) +
      geom_point(data = dat_enc_rt_YA(), aes(colour = ID), stat = "summary", size = 3) +
      scale_colour_manual(values = ID_colors, drop = TRUE, breaks = c(input$select_ID, input$select_ID_YA)) +
      labs(x = "Response made", 
           y = "Reaction time", 
           title = "ENC: Reaction Time" 
           ) +
      facet_grid(AgeGroup~stimtype,
                 labeller = labeller(AgeGroup = c("OA" ="Old Adults", "YA" = "Young Adults")))

  })
  
  output$smstplot3 <- renderPlot({
    
    # draw the histogram with the specified number of bins
    
    rec_percentage %>% 
      ggplot() +
      geom_bar(aes(x = factor(stimtype, levels=c("TARG", "CLOSE", "DISTANT", "FOIL")), y = percentage_id, fill=factor(response, levels=c("old", "new"))), stat = "summary", position = position_dodge(width=1), alpha = .8) +
      geom_bar(data = dat_rec_percent(), aes(x = factor(stimtype, levels=c("TARG", "CLOSE", "DISTANT", "FOIL")), y = percentage_id, fill=factor(response, levels=c("old", "new")), color = ID), position = position_dodge2(width=1, preserve = 'single'), stat = "identity", alpha = .4, size = 1.2) +
      geom_bar(data = dat_rec_percent_YA(), aes(x = factor(stimtype, levels=c("TARG", "CLOSE", "DISTANT", "FOIL")), y = percentage_id, fill=factor(response, levels=c("old", "new")), color = ID), position = position_dodge2(width=1, preserve = 'single'), stat = "identity", alpha = .4, size = 1.2) +
      labs(x = "Type of stimulus", y = "Percentages of responses", title = "REC: results", fill = "Response") +
      scale_colour_manual(values = ID_colors, drop = TRUE, breaks = c(input$select_ID, input$select_ID_YA)) +
      scale_y_continuous(breaks = seq(0,1, by =0.2), labels=percent) +
      custom_theme +
      scale_fill_brewer(palette = "Dark2") +
      coord_cartesian(ylim = c(0, 1)) +
      facet_wrap(~AgeGroup,
                 labeller = labeller(AgeGroup = c("OA" ="Old Adults", "YA" = "Young Adults")))
    
  })
  
  
  output$smstplot4 <- renderPlot({
    
    # draw the histogram with the specified number of bins
    
    rec_rt %>% 
      ggplot(aes(x = stimtype, y = reaction_time)) +
      custom_theme +
      geom_violin(position = position_dodge()) +
      geom_boxplot(position = position_dodge(width = 1), width = 0.3) +
      geom_point(data = dat_rec_rt(), aes(colour = ID), stat = "summary", size = 3) +
      geom_point(data = dat_rec_rt_YA(), aes(colour = ID), stat = "summary", size = 3) +
      scale_colour_manual(values = ID_colors, drop = TRUE, breaks = c(input$select_ID, input$select_ID_YA)) +
      labs(x = "Type of Stimulus", 
           y = "Reaction time", 
           title = "REC: Reaction Time" 
      ) +
      facet_grid(AgeGroup~response,
                 labeller = labeller(AgeGroup = c("OA" ="Old Adults", "YA" = "Young Adults")))

  })
  
  
  
  output$mstplot1 <- renderPlot({
    
    # draw the histogram with the specified number of bins
    
    mst_enc_perc %>% 
      ggplot() + 
      geom_bar(aes(x=response, y=percentage_id), alpha = .6, stat="summary") +
      geom_bar(data = dat_mst_enc_p(), aes(x=response, y=percentage_id, fill = ID), alpha = .4, stat = "identity") +
      geom_bar(data = dat_mst_enc_p_YA(), aes(x=response, y=percentage_id, fill = ID), alpha = .4, stat = "identity") +
      scale_fill_manual(values = ID_colors, drop = TRUE, breaks = c(input$select_ID, input$select_ID_YA)) +
      custom_theme + 
      labs(x = "Response made", y = "Percentage of responses", title = "ENC: Indoor - outdoor") +
      scale_y_continuous(breaks = seq(0,1, by =0.2), labels=percent) +
      facet_grid(AgeGroup~condition,
                 labeller = labeller(AgeGroup = c("OA" ="Old Adults", "YA" = "Young Adults"),
                                     condition = c("SL" = "Lure Later", "SR" = "Repeat Later"))) +
      coord_cartesian(ylim = c(0, 1))
    
  })
  
  output$mstplot2 <- renderPlot({
    
    # draw the histogram with the specified number of bins
    
    mst_enc_rt %>% 
      ggplot(aes(x = response, y = reaction_time)) +
      custom_theme +
      geom_violin(position = position_dodge()) +
      geom_boxplot(position = position_dodge(width = 1), width = 0.3) +
      geom_point(data = dat_mst_enc_r(), aes(colour = ID), stat = "summary", size = 3) +
      geom_point(data = dat_mst_enc_r_YA(), aes(colour = ID), stat = "summary", size = 3) +
      scale_colour_manual(values = ID_colors, drop = TRUE, breaks = c(input$select_ID, input$select_ID_YA)) +
      labs(x = "Response made", 
           y = "Reaction time", 
           title = "ENC: Reaction Time" 
      ) +
      facet_grid(AgeGroup~condition,
                 labeller = labeller(AgeGroup = c("OA" ="Old Adults", "YA" = "Young Adults"),
                                     condition = c("SL" = "Lure Later","SR" = "Repeat Later")))
    
  })
  
  output$mstplot3 <- renderPlot({
    
    # draw the histogram with the specified number of bins
    
    mst_rec_perc %>% 
      ggplot() +
      geom_bar(aes(x = factor(condition, levels=c("TF", "TL", "TR"), labels=c("Foil", "Lure", "Repeat")), y = percentage_id, fill=factor(response, labels=c("new", "old", "similar"))), stat = "summary", position = position_dodge(width=1), alpha = .8) +
      geom_bar(data = dat_mst_rec_p(), aes(x = factor(condition, levels=c("TF", "TL", "TR"), labels=c("Foil", "Lure", "Repeat")), y = percentage_id, fill=factor(response, labels=c("new", "old", "similar")), color = ID), position = position_dodge2(width=1, preserve = 'single'), stat = "identity", alpha = .4, size = 1.2) +
      geom_bar(data = dat_mst_rec_p_YA(), aes(x = factor(condition, levels=c("TF", "TL", "TR"), labels=c("Foil", "Lure", "Repeat")), y = percentage_id, fill=factor(response, labels=c("new", "old", "similar")), color = ID), position = position_dodge2(width=1, preserve = 'single'), stat = "identity", alpha = .4, size = 1.2) +
      labs(x = "Type of stimulus", y = "Percentages of responses", title = "REC: results", fill = "Response") +
      scale_colour_manual(values = ID_colors, drop = TRUE, breaks = c(input$select_ID, input$select_ID_YA)) +
      scale_y_continuous(breaks = seq(0,1, by =0.2), labels=percent) +
      custom_theme +
      scale_fill_brewer(palette = "Dark2") +
      coord_cartesian(ylim = c(0, 1)) +
      facet_wrap(~AgeGroup,
                 labeller = labeller(AgeGroup = c("OA" ="Old Adults", "YA" = "Young Adults")))
    
  })
  
  
  output$mstplot4 <- renderPlot({
    
    # draw the histogram with the specified number of bins
    
    mst_rec_rt %>% 
      ggplot(aes(x = condition, y = reaction_time)) +
      scale_x_discrete(labels= c("Foil", "Lure", "Repeat")) +
      custom_theme +
      geom_violin(position = position_dodge()) +
      geom_boxplot(position = position_dodge(width = 1), width = 0.3) +
      geom_point(data = dat_mst_rec_r(), aes(colour = ID), stat = "summary", size = 3) +
      geom_point(data = dat_mst_rec_r_YA(), aes(colour = ID), stat = "summary", size = 3) +
      scale_colour_manual(values = ID_colors, drop = TRUE, breaks = c(input$select_ID, input$select_ID_YA)) +
      labs(x = "Type of Stimulus", 
           y = "Reaction time", 
           title = "REC: Reaction Time" 
      ) +
      facet_grid(AgeGroup~response,
                 labeller = labeller(AgeGroup = c("OA" ="Old Adults", "YA" = "Young Adults"),
                                     response = c("n" = "new", "o" = "old", "s" = "similar")))
    
  })
  

  
  
  output$covariate_plot <- renderPlot({
    
    # draw the histogram with the specified number of bins
    
    paper_pencil_data_long %>% 
      ggplot(aes(x = as.factor(AgeGroup), y = as.numeric(result))) +
      geom_violin(aes(fill = AgeGroup)) +
      geom_boxplot(aes(fill = AgeGroup), width=0.2) +
      geom_point(position = position_jitter(),  size = 3) +
      geom_point(data = paper_pencil_dat(), aes(colour = ID),  size = 5) +
      geom_point(data = paper_pencil_dat_YA(), aes(colour = ID),  size = 5) +
      scale_colour_manual(values = ID_colors, drop = TRUE, breaks = c(input$select_ID, input$select_ID_YA)) +
      facet_wrap(~questionnaire, scales = "free") +
      custom_theme +
      #geom_text(aes(label=result, fill = 'black'), hjust = -0.5) +
      labs(title = "Results on paper and pencil tests", x = "AgeGroups", y = "Results of different tests", color = "ID of Participants")
    
  })
  
  
  output$palt_plot_1 <- renderPlot({
    
    # draw the histogram with the specified number of bins
    
    palt_plot %>% 
      ggplot(aes(x = as.factor(AgeGroup), y = as.numeric(result))) +
      geom_violin(aes(fill = AgeGroup)) +
      geom_boxplot(aes(fill = AgeGroup), width=0.1) +
      geom_point(position = position_jitter(),  size = 3) +
      geom_point(data = palt_dat(), aes(colour = ID),  size = 5) +
      geom_point(data = palt_dat_YA(), aes(colour = ID),  size = 5) +
      scale_colour_manual(values = ID_colors, drop = TRUE, breaks = c(input$select_ID, input$select_ID_YA)) +
      facet_wrap(~score, scales = "free") +
      custom_theme +
      #geom_text(aes(label=result, fill = 'black'), hjust = -0.5) +
      labs(title = "Results of PALT", x = "AgeGroups", y = "Scores", color = "ID of Participants")
    
  })
  
  
  output$nback_plot1 <- renderPlot({
    
    # draw the histogram with the specified number of bins
    
    nback_rate %>% 
      ggplot() +
        custom_theme +
        geom_bar(aes(x = response_type, y = percentage_id), stat = 'summary', position = position_dodge()) +
        geom_bar(data = nback_rate_dat(), aes(x = response_type, y = percentage_id, fill = ID), position = position_dodge2(width=1, preserve = 'single'), stat = "identity", alpha = .4, size = 1.2) +
        geom_bar(data = nback_rate_dat_YA(), aes(x = response_type, y = percentage_id, fill = ID), position = position_dodge2(width=1, preserve = 'single'), stat = "identity", alpha = .4, size = 1.2) +
        scale_x_discrete(labels=c("CR", "FA", "hit", "miss")) +
        scale_colour_manual(values = ID_colors, drop = TRUE, breaks = c(input$select_ID, input$select_ID_YA)) +
        facet_grid(AgeGroup~block) +
        scale_y_continuous(breaks = seq(0,1, by =0.2), labels=percent) +
        labs(title = "Percentage of responses in N-back", x = "Response types", y = "Number of responses in type", fill = "ID of Participants")

  })
  
  
  output$nback_plot2 <- renderPlot({
    
    # draw the histogram with the specified number of bins
    
    nback_rt %>% 
      ggplot(aes(x = as.factor(AgeGroup), y = as.numeric(reaction_time))) +
      geom_violin(aes(fill = AgeGroup)) +
      geom_boxplot(aes(fill = AgeGroup), width=0.1) +
      geom_point(data = nback_rt_dat(), aes(colour = ID),  size = 5, stat = "summary") +
      geom_point(data = nback_rt_dat_YA(), aes(colour = ID),  size = 5, stat = "summary") +
      scale_colour_manual(values = ID_colors, drop = TRUE, breaks = c(input$select_ID, input$select_ID_YA)) +
      custom_theme +
      labs(title = "RTs in N-back", x = "AgeGroups", y = "Reaction Time", color = "ID of Participants") +
      facet_wrap(~response_type)
    
  })
  
  
  
  
  output$smst_text <- renderText({
    (paste("<b>Old Adult selected: <br>",
           "Sex: ", dat_text()$Sex[1], "<br>", 
           "Age Group:", dat_text()$AgeGroup[1], "<br>",
           "Age:", dat_text()$Age[1], "<br>",
           "MOCA:", dat_text()$Moca[1], "</b>"))
  })
  

  output$smst_text_YA <- renderText({
    (paste("<b><br>",
           "Young Adult selected: <br>",
           "Sex: ", dat_text_YA()$Sex[1], "<br>", 
           "Age Group:", dat_text_YA()$AgeGroup[1], "<br>",
           "Age:", dat_text_YA()$Age[1], "<br>",
           "MOCA:", dat_text_YA()$Moca[1], "</b>"))

  })
  
})

# Create Shiny object
shinyApp(ui = ui, server = server)


