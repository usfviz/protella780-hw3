library(shiny)
library(ggvis)
library(dplyr)
library(reshape2)
library(GGally)
library(ggplot2)


df <- read.csv('dataset_diabetes/diabetic_data.csv', stringsAsFactors = F)

df$age2 <- sapply(df$age, function(x) as.numeric(gsub('-', '', substr(as.character(x), 2, 3))) + 5)
df$race[df$race == 'AfricanAmerican'] <- 'African American'
df$race[(df$race == '?')] <- 'Missing/Unknown'
df$race[is.null(df$race)] <- 'Missing/Unknown'
df$race <- factor(df$race, levels = c('Caucasian', 'African American', 'Hispanic', 'Asian', 'Other', 'Missing/Unknown'))

#df$medical_specialty[df$medical_specialty == '?' | is.null(df$medical_specialty)] <- 'Missing/Unknown'
#df$medical_specialty[df$medical_specialty != 'Missing/Unknown' & df$medical_specialty != "Family Practice" &
#                         df$medical_specialty != 'Cardiology' & df$medical_specialty != "Surgery"] <- "Other"

df[df$readmitted == "NO", 'Readmission'] <- 0
df[df$readmitted == "<30", 'Readmission'] <- .5
df[df$readmitted == ">30", 'Readmission'] <- 1
first_visits_df <- df[!duplicated(df$patient_nbr), ]


first_visits_df %>%
    group_by(race, age) %>%
    summarise(mean_meds = mean(num_medications), count = sum(num_medications)/mean(num_medications)) -> results




df[df$readmitted == "NO", 'Readmission'] <- 0
df[df$readmitted == "<30", 'Readmission'] <- .5
df[df$readmitted == ">30", 'Readmission'] <- 1
first_visits_df <- df[!duplicated(df$patient_nbr), ]

num_df <- subset(first_visits_df, df$discharge_disposition_id==1, select = c('age2', 'number_diagnoses', 'number_outpatient', 
                                                                             'number_inpatient', 'number_emergency', 'time_in_hospital',
                                                                             'Readmission'))
scaled_num_df <- as.data.frame(apply(num_df, 2, scale))
test_scaled_num_df <- scaled_num_df[sample(nrow(scaled_num_df), 1000), ]
test_df <- first_visits_df[sample(nrow(first_visits_df), 1000), ]

ui <- fluidPage(
    verticalLayout(
        titlePanel("Phil Rotella, DataViz HW3"),
        plotOutput("plot1"),
        plotOutput("plot2"),
        selectInput('p2var', label = 'Select Factor', selected = 'Age', choices = c('Age Group', 'Race', 'Specialty of Admitting Physician'), multiple = F),
        plotOutput("plot3")
    )
)

server <- function(input, output) {
    output$plot1 <- renderPlot({
        ggparcoord(test_scaled_num_df, columns = c(1, 6, 2, 7, 4, 5, 3), scale = 'uniminmax', groupColumn = 7) +
            scale_color_gradientn(colours=c('#96ceb4', '#ffeead', '#ffcc5c', '#ff6f69'))
    })
    ##Options: select variable for color grouping; filter or select by race, filter or select by discharge disposition; select on brushing 
    p2var <- reactive({
        if(input$p2var == 'Age Group') {
            return(test_df$age)
        } else if(input$p2var == 'Race') {
            return(test_df$race)
        } else {
            return(test_df$medical_specialty)
        }
        })
    
    output$plot2 <- renderPlot({
        ggplot(test_df, aes(x = readmitted)) +
            geom_bar(aes(y = (..count..)/tapply(..count..,..PANEL..,sum)[..PANEL..])) + 
            facet_wrap(~p2var()) +
            scale_y_continuous(labels=percent) +
            ylab(paste('Frequency by', input$p2_var)) +
            xlab('Readmission Status') +
            scale_x_discrete(labels = c('Within\n30 days', 'After\n30 days', 'Not\nreadmitted'))
    })
    output$plot3 <- renderPlot({
        ggplot(results, aes(age, race)) + 
            geom_tile(aes(fill = mean_meds), colour = "white") + 
            scale_fill_gradient(low = "white", high = "steelblue", name='Mean Number\nof Medications') + 
            labs(x = 'Age Group', y = 'Race', title = 'Number of Medications by Age and Race')
    })
    

##Heat map: demographics (age, gender, race) vs. Admission Source, % Readmission 
##(can choose any readmission, <30, or >30)
}

shinyApp(ui = ui, server = server)


