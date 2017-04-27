library(shiny)
library(ggvis)
library(dplyr)
library(reshape2)
library(GGally)
library(ggplot2)
library(scales)


df <- read.csv('dataset_diabetes/diabetic_data.csv', stringsAsFactors = F)
# Subset to first inpatient visit of patients who were discharged to home at end of inpatient stay, and only keep select columns
df <- df %>% subset(subset = !duplicated(patient_nbr) & discharge_disposition_id==1,
                    select = c(race, gender, age, time_in_hospital, medical_specialty, num_lab_procedures, num_procedures, num_medications,
                               number_outpatient, number_emergency, number_inpatient, number_diagnoses, diag_1, readmitted))
# Prep data for analysis
df$age2 <- sapply(df$age, function(x) as.numeric(gsub('-', '', substr(as.character(x), 2, 3))) + 5)

df$race[df$race == 'AfricanAmerican'] <- 'African American'
df$race[(df$race == '?')] <- 'Missing/Unknown'
df$race <- factor(df$race, levels = c('Caucasian', 'African American', 'Hispanic', 'Asian', 'Other', 'Missing/Unknown'))

df$medical_specialty[df$medical_specialty == '?'] <- 'Missing/Unknown'
df$medical_specialty[grepl('Family', df$medical_specialty)] <- 'Family Practice'
df$medical_specialty[grepl('Internal', df$medical_specialty)] <- 'Internal Medicine'
df$medical_specialty[grepl('Surg', df$medical_specialty)] <- 'Surgery'
df$medical_specialty[grepl('Emerg', df$medical_specialty)] <- 'Emergency Medicine'
df$medical_specialty[grepl('Cardiology', df$medical_specialty)] <- 'Cardiology'
df$medical_specialty[!grepl('Family|Internal|Surg|Emerg|Cardiology|Missing', df$medical_specialty)] <- "Other"
df$medical_specialty <- factor(df$medical_specialty, levels = c('Internal Medicine', 'Family Practice', 'Cardiology', 
                                                                'Surgery', 'Emergency Medicine', 'Other', 'Missing/Unknown'))

df$primary_diagnosis <- 'Other'
df$primary_diagnosis[grepl('^39\\d|^4[0-5]\\d|^785', df$diag_1)] <- 'Circulatory System'
df$primary_diagnosis[grepl('^250', df$diag_1)] <- 'Diabetes'
df$primary_diagnosis[grepl('^4[6-9]\\d|^5[0-1]\\d|^786', df$diag_1)] <- 'Respiratory System'
df$primary_diagnosis[grepl('^5[2-7]\\d|^787', df$diag_1)] <- 'Digestive System'
df$primary_diagnosis[grepl('^[8-9]\\d\\d', df$diag_1)] <- 'Injury and Poisoning'
df$primary_diagnosis[grepl('^7[1-3]\\d', df$diag_1)] <- 'Musculoskeletal System'
df$primary_diagnosis[grepl('^5[8-9]\\d|^6[0-2]\\d|^788', df$diag_1)] <- 'Genitourinary System'
df$primary_diagnosis[grepl('^1[4-9]\\d|^2[0-3]\\d', df$diag_1)] <- 'Neoplasms'
df$primary_diagnosis <- factor(df$primary_diagnosis, levels = c('Circulatory System', 'Diabetes', 'Respiratory System',
                                                                'Digestive System', 'Injury and Poisoning', 'Musculoskeletal System',
                                                                'Genitourinary System', 'Neoplasms', 'Other'))

df[df$readmitted == "NO", 'Readmission'] <- 0
df[df$readmitted == "<30", 'Readmission'] <- .5
df[df$readmitted == ">30", 'Readmission'] <- 1

df$readmitted[df$readmitted != "NO"] <- "YES"


#for plot 1
num_df <- subset(df, select = c('age2', 'number_diagnoses', 'number_inpatient', 'number_emergency', 'number_outpatient',
                                'time_in_hospital', 'num_procedures', 'num_lab_procedures', 'num_medications', 'Readmission'))
num_df <- as.data.frame(apply(num_df, 2, scale))
numeric_cols <- c("Age", "Diagnoses", "Inpatient", "ER", "Outpatient", 
                  "LOS", "Procedures", "Labs", "Medications", "Readmission")
names(num_df) <- numeric_cols


#for plot3
heat_df <- subset(df, select = c('age','race', 'number_diagnoses', 'num_medications'))
names(heat_df) <- c("Age", "Race", "Diagnoses", "Medications")


##################################
#              UI               #
#################################
ui <- fluidPage(
    verticalLayout(
        titlePanel("Phil Rotella, DataViz HW3 -- 
                   Data is on first inpatient visit of patients who were discharged to home at end of inpatient stay.
                   May take 1-2 minutes to load."),
        plotOutput("plot1"),
        selectInput("p1color", label = 'Select Color Variable', selected = 'Readmission', choices = numeric_cols, multiple = F), 
        plotOutput("plot2"),
        selectInput('p2var', label = 'Select Factor', selected = 'Age', 
                    choices = c('Age Group', 'Race', 'Specialty of Admitting Physician', 'Disease Category of Primary Diagnosis'), 
                    multiple = F),
        plotOutput("plot3"),
        radioButtons('p3var', label = 'Select Factor', choices = c('Medications', 'Diagnoses'), selected = 'Medications')
    )
)

##################################
#           Server              #
#################################
server <- function(input, output) {
    theme1 <- theme(axis.text = element_text(size = 12),
                    axis.title = element_text(size = 14, face = 'bold'),
                    legend.text = element_text(size = 12),
                    legend.title = element_text(size = 14, face = 'bold'),
                    plot.title = element_text(size = 20, face = 'bold'),
                    strip.text.x = element_text(size = 12))
    
    output$plot1 <- renderPlot({
        ggparcoord(num_df, scale = 'uniminmax', columns = 1:5, groupColumn = input$p1color,
                   alphaLines = .7, showPoints=T) + theme1 +
            scale_color_gradientn(colours=c('#96ceb4', '#ffeead', '#ffcc5c', '#ff6f69')) + 
            theme(axis.text.y = element_blank()) + 
            labs(x = '\nPatient Characteristics (Age, Number of Diagnoses, Visits in Prior Year)', y = 'Standardized Value\n',
                 title = 'Parallel Coordinates Plot of Patient Characteristics') 
            
    })
    ##Options: select variable for color grouping; filter or select by race, filter or select by discharge disposition; select on brushing 
    p2var <- reactive({
        if(input$p2var == 'Age Group') {
            return(df$age)
        } else if(input$p2var == 'Race') {
            return(df$race)
        } else if(input$p2var == 'Specialty of Admitting Physician') {
            return(df$medical_specialty)
        } else {
            return(df$primary_diagnosis)
        }
        })

    output$plot2 <- renderPlot({
        ggplot(df, aes(x = readmitted)) +
            geom_bar(aes(y = (..count..)/tapply(..count..,..PANEL..,sum)[..PANEL..],
                         fill = cut((..count..)/tapply(..count..,..PANEL..,sum)[..PANEL..], c(0, .387, 1)))) + 
            facet_wrap(~p2var()) +
            theme1 +
            scale_fill_manual(name = "Readmission Rate",values = c('dodgerblue', 'grey'),
                               labels = c("Lower than average", "At or higher than average")) +
            scale_y_continuous(labels=percent) +
            labs(y = paste('Frequency by', input$p2_var,'\n'), x = '\nReadmission Status', 
                 title = 'Small-Multiples Plot of Readmission Rates') +
            scale_x_discrete(labels = c('No Readmission', 'Readmitted'))
    })
    
    output$plot3 <- renderPlot({
        if(input$p3var == 'Medications'){
            heat_df %>%
                group_by(Race, Age) %>%
                summarise(mean = mean(Medications), count = sum(Medications)/mean(Medications)) %>% 
                ggplot(aes(factor(Age), factor(Race))) + 
                geom_tile(aes(fill = mean), colour = "white") + 
                theme1 +
                scale_fill_gradient(low = "white", high = "steelblue", name=paste('Mean Number\nof Medications')) + 
                labs(x = 'Age Group', y = 'Race', title = paste('Heat Plot of Medications by Age and Race'))
        } else {
            heat_df %>%
                group_by(Race, Age) %>%
                summarise(mean = mean(Diagnoses), count = sum(Diagnoses)/mean(Diagnoses)) %>% 
                ggplot(aes(factor(Age), factor(Race))) + 
                geom_tile(aes(fill = mean), colour = "white") + 
                theme1 +
                scale_fill_gradient(low = "white", high = "steelblue", name=paste('Mean Number\nof Diagnoses')) + 
                labs(x = 'Age Group', y = 'Race', title = paste('Heat Plot of Diagnoses by Age and Race'))
        }
        })

}

shinyApp(ui = ui, server = server)


