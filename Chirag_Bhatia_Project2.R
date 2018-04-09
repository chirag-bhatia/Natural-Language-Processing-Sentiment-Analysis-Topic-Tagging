# Clear workspace 
rm(list = ls())

#Specifying the packages of interest
packages = c( "DT","shiny","shinythemes","rvest","stringr","stringi","tidyverse", "tidytext", "sqldf", "ggplot2", "ggthemes", "class", "gmodels", "dplyr")

#Using function to check if each package is on the local machine
#if a package is installed, it will be loaded
#if any are not, the missing package(s) will be installed and loaded
package.check = lapply(packages, FUN = function(x) {
  if (!require(x, character.only = TRUE)) {
    install.packages(x, dependencies = TRUE)
    library(x, character.only = TRUE)
  }
})



# Define UI ----
ui = fluidPage(
  
   navbarPage("Chirag Bhatia: NLP",
    inverse = TRUE,
    theme = shinytheme("cerulean"),
    tabPanel("Training Data",
                  titlePanel("Toyota Camry 2012 - 2016 Reviews"),
                         
                         mainPanel(
                           submitButton("action", "Download Training Data"), #class = "btn-primary"),
                           DT::dataTableOutput("Table1")
                         )
  
                ),
    tabPanel("Test Data", 
             titlePanel("Toyota Camry 2017 Reviews"),
             mainPanel(
               DT::dataTableOutput("Table2")
             )
             ),
    tabPanel("Normalized Review Text", 
             mainPanel(
               tabsetPanel(
                 type = "tabs",
                 tabPanel("Normalized: Training Data",
                          DT::dataTableOutput("Table3")
                  ),
                 tabPanel("Normalized: Test Data",
                          DT::dataTableOutput("Table4")
                 )
                )
             )
    ),
    tabPanel("Tagging of Reviews", 
             mainPanel(
               tabsetPanel(
                 type = "tabs",
                 tabPanel("Tagged Reviews: Training Data",
                          DT::dataTableOutput("Table5")
                 ),
                 tabPanel("Tagged Reviews: Test Data",
                          DT::dataTableOutput("Table6")
                 )
               )
             )
    ),
    tabPanel("Sentiment Analysis", 
             mainPanel(
               tabsetPanel(
                 type = "tabs",
                 tabPanel("Sentiment Analysis: Training Data",
                          DT::dataTableOutput("Table7")
                 ),
                 tabPanel("Sentiment Analysis: Test Data",
                          DT::dataTableOutput("Table8")
                 )
               )
             )
    ),
    tabPanel("Average Comparison", 
             mainPanel(
             tabsetPanel(
               type = "tabs",
               tabPanel("Average Comparison: Overall Training Set",
                       DT::dataTableOutput("Table9"),
                       h2("Details on the t-test"),
                       p("We are testing the null hypothesis that both means are the same"),
                       p(""),
                       p("The observed test statistic (difference in means divided by pooled standard error:"),
                       p(""),
                       textOutput('tvalue'),
                       p(""),
                       textOutput('pvalue'),
                       p(""),
                       p("A low P value suggests that your sample provides enough evidence that you can reject the null hypothesis. The means are statistically different from each other."),
                       p(""),
                       p("Sample Estimates:"),
                       p(""),
                       textOutput('samplemeans1'),
                       textOutput('samplemeans2'),
                       h2("Details on Correlation Statistic"),
                       DT::dataTableOutput("Table10"),
                       p("The correlation value above between User Star Rating and Text Review Sentiment Rating above is not significant.")
              ),
              tabPanel("Average Comparison: Tagged Training Set",
                       DT::dataTableOutput("Table11"),
                       DT::dataTableOutput("Table12"),
                       DT::dataTableOutput("Table13"),
                       DT::dataTableOutput("Table14")
              )
             )
            )
    ),
    tabPanel("Prediction Model", 
             mainPanel(
               tabsetPanel(
                 type = "tabs",
                 tabPanel("Using Training Data to Build Model",
                          h2("Model Building Process"),
                          p("The predictor variable in this case is User Star Rating, which is a set of discrete values, ranging from 1 to 5"),
                          p("This rules out the possibility of using model like a Linear Regression"),
                          p("The available options were all the classifiers, which includes Multinomial Logit, Support Vector Machine, Artificial Neural Network, Naive Bayes Classifier and K-Nearest Neighbor"),
                          p("I selected K-Nearest Neighbor algorithm for the data in hand, as it is one of the simplest algorithms around to be implemented. While I also tried my hands with SVM, results didn't look good and was forced to switch back to KNN")
                          ),
                 tabPanel("Analyzing Model Performance",
                          h2("Confusion Matrix"),
                          p("While 'x' represents input sentiment rating, 'y' depicts predicted user star rating"),
                          DT::dataTableOutput("Table15"),
                          p(),
                          h2("Accuracy of Predicted Results"),
                          DT::dataTableOutput("Table16"),
                          p()
                 )
               )
             )
    ),
    tabPanel("TF-IDF Score", 
             mainPanel(
               tabsetPanel(
                 type = "tabs",
                 tabPanel("Service Tag Reviews & Visualization",
                          DT::dataTableOutput("Table17"),
                          plotOutput("Graph1")
                 ),
                 tabPanel("Price Tag Reviews & Visualization",
                          DT::dataTableOutput("Table18"),
                          plotOutput("Graph2")
                 ),
                 tabPanel("Handling Tag Reviews & Visualization",
                          DT::dataTableOutput("Table19"),
                          plotOutput("Graph3")
                 ),
                 tabPanel("Interior Tag Reviews & Visualization",
                          DT::dataTableOutput("Table20"),
                          plotOutput("Graph4")
                 )
                          
                 )
               )
        )
    
    )
  )


server <- function(input, output) {
 
  #----------------------Task 1: Download the training data (reviews) from the online link---------------------
  
  #**************************************DF1**************************************************
  
  #Specifying the url for desired website to be scrapped
  url_1 = 'https://www.cars.com/research/toyota-camry-2012/consumer-reviews/?nr=250&pg=1'
  
  #Reading the HTML code from the website
  webpage = read_html(url_1)
  
  #Using CSS selectors to scrape the Rating Section of Review
  rating_data_html_1 = html_nodes(webpage,'.cr-star-rating')
  rating_data_1 = html_attr(rating_data_html_1,"rating")
  rating_data_df_1 = as.data.frame(rating_data_1)
  
  #Using CSS selectors to scrape the Text Section of Review
  text_data_html_1 = html_nodes(webpage,"div.mmy-reviews__blurb")
  text_data_1 = html_text(text_data_html_1, trim = TRUE)
  text_data_df_1 = as.data.frame(text_data_1)
  
  #Using CSS selectors to scrape the Date and eventially Year of Review
  year_data_html_1 = html_nodes(webpage,"p meta")
  year_data_1 = html_attr(year_data_html_1,"content")
  year_df_1 = as.data.frame(year_data_1)
  year_1 = format(as.Date(year_df_1$year_data_1, format="%B%d,%Y"),"%Y")
  year_final_df_1 = data.frame(year_1)
  
  #Creating a combined Data Frame for Rating, Text and Year
  data_frame_1 = cbind(rating_data_df_1, text_data_df_1, year_final_df_1)
  names(data_frame_1) = c("rating", "text", "year")
  
  #***********************************************DF2*******************************************
  
  #Specifying the url for desired website to be scrapped
  url_2 = 'https://www.cars.com/research/toyota-camry-2013/consumer-reviews/?nr=250&pg=1'
  
  #Reading the HTML code from the website
  webpage = read_html(url_2)
  
  #Using CSS selectors to scrape the Rating Section of Review
  rating_data_html_2 = html_nodes(webpage,'.cr-star-rating')
  rating_data_2 = html_attr(rating_data_html_2,"rating")
  rating_data_df_2 = as.data.frame(rating_data_2)
  
  #Using CSS selectors to scrape the Text Section of Review
  text_data_html_2 = html_nodes(webpage,"div.mmy-reviews__blurb")
  text_data_2 = html_text(text_data_html_2, trim = TRUE)
  text_data_df_2 = as.data.frame(text_data_2)
  
  #Using CSS selectors to scrape the Date and eventially Year of Review
  year_data_html_2 = html_nodes(webpage,"p meta")
  year_data_2 = html_attr(year_data_html_2,"content")
  year_df_2 = as.data.frame(year_data_2)
  year_2 = format(as.Date(year_df_2$year_data_2, format="%B%d,%Y"),"%Y")
  year_final_df_2 = data.frame(year_2)
  
  #Creating a combined Data Frame for Rating, Text and Year
  data_frame_2 = cbind(rating_data_df_2, text_data_df_2, year_final_df_2)
  names(data_frame_2) = c("rating", "text", "year")
  
  #***********************************************DF3*******************************************
  
  #Specifying the url for desired website to be scrapped
  url_3 = 'https://www.cars.com/research/toyota-camry-2014/consumer-reviews/?nr=250&pg=1'
  
  #Reading the HTML code from the website
  webpage = read_html(url_3)
  
  #Using CSS selectors to scrape the Rating Section of Review
  rating_data_html_3 = html_nodes(webpage,'.cr-star-rating')
  rating_data_3 = html_attr(rating_data_html_3,"rating")
  rating_data_df_3 = as.data.frame(rating_data_3)
  
  #Using CSS selectors to scrape the Text Section of Review
  text_data_html_3 = html_nodes(webpage,"div.mmy-reviews__blurb")
  text_data_3 = html_text(text_data_html_3, trim = TRUE)
  text_data_df_3 = as.data.frame(text_data_3)
  
  #Using CSS selectors to scrape the Date and eventially Year of Review
  year_data_html_3 = html_nodes(webpage,"p meta")
  year_data_3 = html_attr(year_data_html_3,"content")
  year_df_3 = as.data.frame(year_data_3)
  year_3 = format(as.Date(year_df_3$year_data_3, format="%B%d,%Y"),"%Y")
  year_final_df_3 = data.frame(year_3)
  
  #Creating a combined Data Frame for Rating, Text and Year
  data_frame_3 = cbind(rating_data_df_3, text_data_df_3, year_final_df_3)
  names(data_frame_3) = c("rating", "text", "year")
  
  #***********************************************DF4*******************************************
  
  #Specifying the url for desired website to be scrapped
  url_4 = 'https://www.cars.com/research/toyota-camry-2015/consumer-reviews/?nr=250&pg=1'
  
  #Reading the HTML code from the website
  webpage = read_html(url_4)
  
  #Using CSS selectors to scrape the Rating Section of Review
  rating_data_html_4 = html_nodes(webpage,'.cr-star-rating')
  rating_data_4 = html_attr(rating_data_html_4,"rating")
  rating_data_df_4 = as.data.frame(rating_data_4)
  
  #Using CSS selectors to scrape the Text Section of Review
  text_data_html_4 = html_nodes(webpage,"div.mmy-reviews__blurb")
  text_data_4 = html_text(text_data_html_4, trim = TRUE)
  text_data_df_4 = as.data.frame(text_data_4)
  
  #Using CSS selectors to scrape the Date and eventially Year of Review
  year_data_html_4 = html_nodes(webpage,"p meta")
  year_data_4 = html_attr(year_data_html_4,"content")
  year_df_4 = as.data.frame(year_data_4)
  year_4 = format(as.Date(year_df_4$year_data_4, format="%B%d,%Y"),"%Y")
  year_final_df_4 = data.frame(year_4)
  
  #Creating a combined Data Frame for Rating, Text and Year
  data_frame_4 = cbind(rating_data_df_4, text_data_df_4, year_final_df_4)
  names(data_frame_4) = c("rating", "text", "year")
  
  
  #***********************************************DF5*******************************************
  
  #Specifying the url for desired website to be scrapped
  url_5 = 'https://www.cars.com/research/toyota-camry-2016/consumer-reviews/?nr=250&pg=1'
  
  #Reading the HTML code from the website
  webpage = read_html(url_5)
  
  #Using CSS selectors to scrape the Rating Section of Review
  rating_data_html_5 = html_nodes(webpage,'.cr-star-rating')
  rating_data_5 = html_attr(rating_data_html_5,"rating")
  rating_data_df_5 = as.data.frame(rating_data_5)
  
  #Using CSS selectors to scrape the Text Section of Review
  text_data_html_5 = html_nodes(webpage,"div.mmy-reviews__blurb")
  text_data_5 = html_text(text_data_html_5, trim = TRUE)
  text_data_df_5 = as.data.frame(text_data_5)
  
  #Using CSS selectors to scrape the Date and eventially Year of Review
  year_data_html_5 = html_nodes(webpage,"p meta")
  year_data_5 = html_attr(year_data_html_5,"content")
  year_df_5 = as.data.frame(year_data_5)
  year_5 = format(as.Date(year_df_5$year_data_5, format="%B%d,%Y"),"%Y")
  year_final_df_5 = data.frame(year_5)
  
  #Creating a combined Data Frame for Rating, Text and Year
  data_frame_5 = cbind(rating_data_df_5, text_data_df_5, year_final_df_5)
  names(data_frame_5) = c("rating", "text", "year")
  
  
  #Merging DF1 (2012) to DF5 (2016) to create training_df
  training_df = rbind(data_frame_1, data_frame_2, data_frame_3, data_frame_4, data_frame_5) 
  
  #output$Table1=renderTable({
   # Table1 = training_df
    
#   Table1 = eventReactive(input$action, {
    output$Table1 = DT::renderDataTable({
      DT::datatable(training_df, options = list(orderClasses = TRUE))
    })
    
#  })
  

  
  #----------------------Task 2: Download the test data (reviews) from the online link---------------------
  
  #**************************Creating Test DataSet***************************************
  #Specifying the url for desired website to be scrapped
  url_6 = 'https://www.cars.com/research/toyota-camry-2017/consumer-reviews/?nr=250&pg=1'
  url_7 = 'https://www.cars.com/research/toyota-camry-2017/consumer-reviews/?nr=250&pg=2'
  
  #Reading the HTML code from the website
  webpage_1 = read_html(url_6)
  webpage_2 = read_html(url_7)
  
  #Using CSS selectors to scrap the Rating Section of Review
  rating_data_html_6 = html_nodes(webpage_1,'.cr-star-rating')
  rating_data_html_7 = html_nodes(webpage_2,'.cr-star-rating')
  rating_data_6 = html_attr(rating_data_html_6,"rating")
  rating_data_7 = html_attr(rating_data_html_7,"rating")
  rating_data_df_6 = as.data.frame(rating_data_6)
  rating_data_df_7 = as.data.frame(rating_data_7)
  
  #Using CSS selectors to scrap the Text Section of Review
  text_data_html_6 = html_nodes(webpage_1,"div.mmy-reviews__blurb")
  text_data_html_7 = html_nodes(webpage_2,"div.mmy-reviews__blurb")
  text_data_6 = html_text(text_data_html_6, trim = TRUE)
  text_data_7 = html_text(text_data_html_7, trim = TRUE)
  text_data_df_6 = as.data.frame(text_data_6)
  text_data_df_7 = as.data.frame(text_data_7)
  
  #Using CSS selectors to scrap the Date and eventially Year of Review
  year_data_html_6 = html_nodes(webpage_1,"p meta")
  year_data_html_7 = html_nodes(webpage_2,"p meta")
  year_data_6 = html_attr(year_data_html_6,"content")
  year_data_7 = html_attr(year_data_html_7,"content")
  year_df_6 = as.data.frame(year_data_6)
  year_df_7 = as.data.frame(year_data_7)
  year_6 = format(as.Date(year_df_6$year_data_6, format="%B%d,%Y"),"%Y")
  year_7 = format(as.Date(year_df_7$year_data_7, format="%B%d,%Y"),"%Y")
  year_final_df_6 = data.frame(year_6)
  year_final_df_7 = data.frame(year_7)
  
  #Creating a combined Data Frame for Rating, Text and Year
  data_frame_6 = cbind(rating_data_df_6, text_data_df_6, year_final_df_6)
  names(data_frame_6) = c("rating", "text", "year")
  data_frame_7 = cbind(rating_data_df_7, text_data_df_7, year_final_df_7)
  names(data_frame_7) = c("rating", "text", "year")
  test_df = rbind(data_frame_6, data_frame_7)
  
  output$Table2 = DT::renderDataTable({
    DT::datatable(test_df, options = list(orderClasses = TRUE))
  })
  
  
  #**********************Task 3: Remove all punctuation, Convert to lowercase*********************************************
  
  #Removing all punctuation from training text data
  normalized_1 = gsub('[[:punct:] ]+',' ',training_df$text)
  
  #Converting to lower case
  normalized_2 = as.data.frame(tolower(normalized_1))
  names(normalized_2) = c("normalized_text")
  
  #Adding normalized text as new column to training data
  training_df_1 = cbind(training_df, normalized_2)
  
  output$Table3 = DT::renderDataTable({
    DT::datatable(training_df_1, options = list(orderClasses = TRUE))
  })
  
  #Repeating the process for test data
  normalized_3 = gsub('[[:punct:] ]+',' ',test_df$text)
  normalized_4 = as.data.frame(tolower(normalized_3))
  names(normalized_4) = c("normalized_text")
  test_df_1 = cbind(test_df, normalized_4)
  
  output$Table4 = DT::renderDataTable({
    DT::datatable(test_df_1, options = list(orderClasses = TRUE))
  })
 
  
  #**********************Task 4: Tag each review into SERVICE, PRICE, HANDLING, INTERIOR*********************************************
  
  #Creating a pattern to search for tags from
  pattern = list("service", "price", "handling", "interior")
  
  #------------------------------------------Tagging Training Data Set-----------------------------------------
  training_df_1$normalized_text= as.character(training_df_1$normalized_text)
  
  #Using stringi library to search for tags within text
  l = lapply(training_df_1$normalized_text, function(T){
    stri_match_last_regex(T, pattern)
  })
  lu2 = data.frame(t(sapply(l,c)))
  
  #Changing column names
  names(lu2) = c("service", "price", "handling", "interior")
  
  #Changing data type to character
  lu2$service = as.character(lu2$service)
  lu2$price = as.character(lu2$price)
  lu2$handling = as.character(lu2$handling)
  lu2$interior = as.character(lu2$interior)
  #lu2 = sapply(lu2, as.character)
  
  #Replacing NA with white space
  lu2[is.na(lu2)] = " "
  
  #Combining tags in one column
  lu2$tag = paste(lu2$service, lu2$price, lu2$handling, lu2$interior, sep = " ")
  
  #Inserting new tag column to original training data
  training_df_2 = cbind(training_df_1, lu2$tag)
  
  #Changing column name
  names(training_df_2)[names(training_df_2) == "lu2$tag"] = "Tag" 
  
  output$Table5 = DT::renderDataTable({
    DT::datatable(training_df_2, options = list(orderClasses = TRUE))
  })
  
  #-----------------------Tagging Test Data Set-------------------------------------
  test_df_1$normalized_text= as.character(test_df_1$normalized_text)
  
  #Using stringi library to search for tags within text
  l1 = lapply(test_df_1$normalized_text, function(T){
    stri_match_last_regex(T, pattern)
  })
  lu3 = data.frame(t(sapply(l1,c)))
  
  #Changing column names
  names(lu3) = c("service", "price", "handling", "interior")
  
  #Changing data type to character
  lu3$service = as.character(lu3$service)
  lu3$price = as.character(lu3$price)
  lu3$handling = as.character(lu3$handling)
  lu3$interior = as.character(lu3$interior)
  #lu2 = sapply(lu2, as.character)
  
  #Replacing NA with white space
  lu3[is.na(lu3)] = " "
  
  #Combining tags in one column
  lu3$tag = paste(lu3$service, lu3$price, lu3$handling, lu3$interior, sep = " ")
  
  #Inserting new tag column to original training data
  test_df_2 = cbind(test_df_1, lu3$tag)
  
  #Changing column name
  names(test_df_2)[names(test_df_2) == "lu3$tag"] = "Tag" 
  
  output$Table6 = DT::renderDataTable({
    DT::datatable(test_df_2, options = list(orderClasses = TRUE))
  })
  
  
  #**********************Task 5: Sentiment Analysis of Text Reviews*********************************************
  
  #Calculating AFINN scores for training data
  #Assigning individual row number to each review
  training_df_2 = training_df_2 %>% mutate(serial_number = row_number())
  
  #To split, or unnest, the document text into the appropriate level
  word_training = training_df_2 %>% unnest_tokens(word, normalized_text) 
  
  #Calculating word frequency to check prevalence of stop words
  word_training %>% count(word, sort = TRUE)
  
  #Removing stop-words: tidytext package comes with a predefined lexicon of stop words
  word_training = word_training %>% anti_join(stop_words)
  
  #After the stop word removal the most common words make more sense
  word_training %>% count(word, sort = TRUE)
  
  #Getting the AFINN sentiment lexicon
  set.seed(0) #for replicability
  get_sentiments("afinn")
  
  #To assign the sentiments using AFINN, we use dplyr's inner_join() function
  sentiment_afinn = word_training %>% 
    inner_join(get_sentiments("afinn")) %>% 
    group_by(serial_number) %>% 
    summarise(sentiment_rating = sum(score)) %>%
    ungroup()
  
  #Linking the sentiment scores back to the original dataframe with missing values filled as zero values
  training_df_3 = full_join(training_df_2, sentiment_afinn)  %>% 
    mutate_each(funs(replace(., which(is.na(.)), 0)), starts_with("score"))
  
  training_df_3[["sentiment_rating"]][is.na(training_df_3[["sentiment_rating"]])] = 0
  
  output$Table7 = DT::renderDataTable({
    DT::datatable(training_df_3, options = list(orderClasses = TRUE))
  })
  
  #----------------------------------------------------------------------------------------------------
  
  #Calculating AFINN scores for test data
  #Assigning individual row number to each review
  test_df_2 = test_df_2 %>% mutate(serial_number = row_number())
  test_df_2$normalized_text= as.character(test_df_2$normalized_text)
  
  #To split, or unnest, the document text into the appropriate level
  word_test = test_df_2 %>% unnest_tokens(word, normalized_text) 
  
  #Calculating word frequency to check prevalence of stop words
  word_test %>% count(word, sort = TRUE)
  
  #Removing stop-words: tidytext package comes with a predefined lexicon of stop words
  word_test = word_test %>% anti_join(stop_words)
  
  #After the stop word removal the most common words make more sense
  word_test %>% count(word, sort = TRUE)
  
  #Getting the AFINN sentiment lexicon
  set.seed(0) #for replicability
  get_sentiments("afinn")
  
  #To assign the sentiments using AFINN, we use dplyr's inner_join() function
  sentiment_afinn = word_test %>% 
    inner_join(get_sentiments("afinn")) %>% 
    group_by(serial_number) %>% 
    summarise(sentiment_rating = sum(score)) %>%
    ungroup()
  
  #Linking the sentiment scores back to the original dataframe with missing values filled as zero values
  test_df_3 = full_join(test_df_2, sentiment_afinn)  %>% 
    mutate_each(funs(replace(., which(is.na(.)), 0)), starts_with("score"))
  
  test_df_3[["sentiment_rating"]][is.na(test_df_3[["sentiment_rating"]])] = 0
  
  output$Table8 = DT::renderDataTable({
    DT::datatable(test_df_3, options = list(orderClasses = TRUE))
  })
  
  #************************Task 6A: Average Sentiment Rating of the Reviews*********************************************
  
  training_df_3$rating = as.numeric(training_df_3$rating)
  training_df_3$sentiment_rating = as.numeric(training_df_3$sentiment_rating)
  mean_training = sqldf("SELECT AVG(rating) AS AVERAGE_USER_STAR_RATING, 
               AVG(sentiment_rating) AS AVERAGE_SENTIMENT_RATING
               FROM training_df_3")
  
  output$Table9 = DT::renderDataTable({
    DT::datatable(mean_training, options = list(orderClasses = TRUE))
  })
  
  ttestout = reactive({
    t.test(training_df_3$rating, training_df_3$sentiment_rating, var.equal = TRUE)
    })
  
  output$tvalue = renderPrint({
    vals = ttestout()
    if (is.null(vals)){return(invisible())}
    vals$statistic
  })
  
   output$samplemeans1 <- renderPrint({
    vals <- ttestout()
    if (is.null(vals)){return(invisible())}
    vals$estimate[1]
    
  })
  
  output$samplemeans2 <- renderPrint({
    vals <- ttestout()
    if (is.null(vals)){return(invisible())}
    vals$estimate[2]
    
  })
  
  output$pvalue <- renderPrint({
    vals <- ttestout()
    if (is.null(vals)){return(invisible())}
    vals$p.value
    
  })
  
  correlation = as.data.frame(cor(training_df_3$rating, training_df_3$sentiment_rating))
  names(correlation) = c("Correlation")
  output$Table10 = DT::renderDataTable({
    DT::datatable(correlation, options = list(orderClasses = TRUE))
  })
  
  
  #************************Task 6B: Average Sentiment Rating SPLIT BY TAGS*********************************************
  
  training_df_4 = cbind(training_df_3, lu2)
  training_df_4 = training_df_4[, -12]
  
  service_text = sqldf("
                       SELECT AVG(sentiment_rating) AS SERVICE_AVERAGE_SENTIMENT_RATING, 
                       AVG(rating) AS SERVICE_AVERAGE_USER_RATING FROM training_df_4
                       WHERE service = 'service'")
  
  service_text_comp = cbind(mean_training,service_text)
  output$Table11 = DT::renderDataTable({
    DT::datatable(service_text_comp, options = list(orderClasses = TRUE))
  })

  
  price_text = sqldf("
                      SELECT AVG(sentiment_rating) AS PRICE_AVERAGE_SENTIMENT_RATING, 
                      AVG(rating) AS PRICE_AVERAGE_USER_RATING FROM training_df_4
                      WHERE price = 'price'")

  price_text_comp = cbind(mean_training,price_text)
  output$Table12 = DT::renderDataTable({
    DT::datatable(price_text_comp, options = list(orderClasses = TRUE))
  })
  

  handling_text = sqldf("
                        SELECT AVG(sentiment_rating) AS HANDLING_AVERAGE_SENTIMENT_RATING, 
                        AVG(rating) AS HANDLING_AVERAGE_USER_RATING FROM training_df_4
                        WHERE handling = 'handling'")

  handling_text_comp = cbind(mean_training,handling_text)
  output$Table13 = DT::renderDataTable({
    DT::datatable(handling_text_comp, options = list(orderClasses = TRUE))
  })

  interior_text = sqldf("
                        SELECT AVG(sentiment_rating) AS INTERIOR_AVERAGE_SENTIMENT_RATING, 
                        AVG(rating) AS INTERIOR_AVERAGE_USER_RATING FROM training_df_4
                        WHERE interior = 'interior'")

  interior_text_comp = cbind(mean_training,interior_text)
  output$Table14 = DT::renderDataTable({
    DT::datatable(interior_text_comp, options = list(orderClasses = TRUE))
  })
  
  #*********************TASK 7&8: BUILDING & ANALYZING PREDICTION MODEL ON TRAINING DATA*********************************************
  
  #Removing reviews with 0 sentiment rating for training purpose
  #The NAs were converted to 0 as well, so it makes sense to leave them out for training the model
  training_temp = sqldf("SELECT rating, sentiment_rating FROM training_df_3 WHERE sentiment_rating IS NOT '0'")
  training_df_5 = sqldf("SELECT sentiment_rating FROM training_temp")
  test_df_5 = sqldf("SELECT sentiment_rating FROM test_df_3")
  
  
  #**************************KNN***********************************************
  
  
  normalize <- function(x) {
    return ((x - min(x)) / (max(x) - min(x))) }
  
  training_dataset = as.data.frame(lapply(training_df_5, normalize))
  test_dataset = as.data.frame(lapply(test_df_5, normalize))
  
  #train_labels = sqldf("SELECT rating FROM training_temp")
  train_labels = training_temp[, 1]
  
  #test_labels = sqldf("SELECT rating from test_df_3")
  test_labels = test_df_3[,1]
  
  test_pred = knn(train = training_dataset, test = test_dataset , cl = train_labels, k=25)
  
  a = CrossTable(x = test_labels, y = test_pred, prop.chisq = FALSE)
  confusion_matrix = as.data.frame(a$t)
  
  accuracy = as.data.frame(sum(test_pred == test_labels)/length(test_pred)*100)
  names(accuracy) = c("accuracy")
  
  output$Table15 = DT::renderDataTable({
    DT::datatable(confusion_matrix, options = list(orderClasses = TRUE))
  })
  
  output$Table16 = DT::renderDataTable({
    DT::datatable(accuracy, options = list(orderClasses = TRUE))
  })
  
  
  #*********************TASK 9: COMPUTING TF-IDF FOR EVERY WORD*********************************************
  
  #Calculating tf-idf for Service Tag Text reviews
  service_training_words = sqldf("SELECT normalized_text, serial_number, Tag FROM training_df_4
                                 WHERE service = 'service'")
  
  #Split, or unnest, the document text into the appropriate level
  #Removing stop-words: tidytext package comes with a predefined lexicon of stop words
  
  service_training_words = service_training_words %>% 
    unnest_tokens(word, normalized_text) %>% 
    anti_join(stop_words) %>% 
    count(serial_number, word, sort = TRUE) %>% 
    ungroup()
  
  #Calculate tf-idf and sorting in descending order
  service_training_words <- service_training_words %>% bind_tf_idf(word, serial_number, n) %>%  arrange(desc(tf_idf)) %>%
    mutate(word = factor(word, levels = rev(unique(word))))
  
  #Calculate top 10 words with most tf-idf score in Service Tag
  top_ten_service = head(service_training_words,10)
  
  output$Table17 = DT::renderDataTable({
    DT::datatable(service_training_words, options = list(orderClasses = TRUE))
  })
  
  output$Graph1 = renderPlot({
    #Plotting top 10 words with highest tf-idf score
    ggplot(top_ten_service, aes(x=reorder(word, -tf_idf), y = tf_idf)) + geom_bar(stat = "identity", fill='orange',color='black')+ theme(axis.text.x = element_text(angle = 45, hjust = 1))+theme_calc()+scale_color_gdocs()
    
  })
  
  #--------------------------------------------------------------------------------------------------------
  
  #Calculating tf-idf for Price Tag Text reviews
  price_training_words = sqldf("SELECT normalized_text, serial_number, Tag FROM training_df_4
                                 WHERE price = 'price'")
  
  #Split, or unnest, the document text into the appropriate level
  #Removing stop-words: tidytext package comes with a predefined lexicon of stop words
  
  price_training_words = price_training_words %>% 
    unnest_tokens(word, normalized_text) %>% 
    anti_join(stop_words) %>% 
    count(serial_number, word, sort = TRUE) %>% 
    ungroup()
  
  #Calculate tf-idf and sorting in descending order
  price_training_words <- price_training_words %>% bind_tf_idf(word, serial_number, n) %>%  arrange(desc(tf_idf)) %>%
    mutate(word = factor(word, levels = rev(unique(word))))
  
  #Calculate top 10 words with most tf-idf score in price Tag
  top_ten_price = head(price_training_words,10)
  
  output$Table18 = DT::renderDataTable({
    DT::datatable(price_training_words, options = list(orderClasses = TRUE))
  })
  
  output$Graph2 = renderPlot({
    #Plotting top 10 words with highest tf-idf score
    ggplot(top_ten_price, aes(x=reorder(word, -tf_idf), y = tf_idf)) + geom_bar(stat = "identity", fill='orange',color='black')+ theme(axis.text.x = element_text(angle = 45, hjust = 1))+theme_calc()+scale_color_gdocs()
    
  })
  
  #--------------------------------------------------------------------------------------------------------
  
  #Calculating tf-idf for Handling Tag Text reviews
  handling_training_words = sqldf("SELECT normalized_text, serial_number, Tag FROM training_df_4
                               WHERE handling = 'handling'")
  
  #Split, or unnest, the document text into the appropriate level
  #Removing stop-words: tidytext package comes with a predefined lexicon of stop words
  
  handling_training_words = handling_training_words %>% 
    unnest_tokens(word, normalized_text) %>% 
    anti_join(stop_words) %>% 
    count(serial_number, word, sort = TRUE) %>% 
    ungroup()
  
  #Calculate tf-idf and sorting in descending order
  handling_training_words <- handling_training_words %>% bind_tf_idf(word, serial_number, n) %>%  arrange(desc(tf_idf)) %>%
    mutate(word = factor(word, levels = rev(unique(word))))
  
  #Calculate top 10 words with most tf-idf score in handling Tag
  top_ten_handling = head(handling_training_words,10)
  
  output$Table19 = DT::renderDataTable({
    DT::datatable(handling_training_words, options = list(orderClasses = TRUE))
  })
  
  output$Graph3 = renderPlot({
    #Plotting top 10 words with highest tf-idf score
    ggplot(top_ten_handling, aes(x=reorder(word, -tf_idf), y = tf_idf)) + geom_bar(stat = "identity", fill='orange',color='black')+ theme(axis.text.x = element_text(angle = 45, hjust = 1))+theme_calc()+scale_color_gdocs()
    
  }) 
  
  #--------------------------------------------------------------------------------------------------------
  
  #Calculating tf-idf for Interior Tag Text reviews
  interior_training_words = sqldf("SELECT normalized_text, serial_number, Tag FROM training_df_4
                                  WHERE interior = 'interior'")
  
  #Split, or unnest, the document text into the appropriate level
  #Removing stop-words: tidytext package comes with a predefined lexicon of stop words
  
  interior_training_words = interior_training_words %>% 
    unnest_tokens(word, normalized_text) %>% 
    anti_join(stop_words) %>% 
    count(serial_number, word, sort = TRUE) %>% 
    ungroup()
  
  #Calculate tf-idf and sorting in descending order
  interior_training_words <- interior_training_words %>% bind_tf_idf(word, serial_number, n) %>%  arrange(desc(tf_idf)) %>%
    mutate(word = factor(word, levels = rev(unique(word))))
  
  #Calculate top 10 words with most tf-idf score in interior Tag
  top_ten_interior = head(interior_training_words,10)
  
  output$Table20 = DT::renderDataTable({
    DT::datatable(interior_training_words, options = list(orderClasses = TRUE))
  })
  
  output$Graph4 = renderPlot({
    #Plotting top 10 words with highest tf-idf score
    ggplot(top_ten_interior, aes(x=reorder(word, -tf_idf), y = tf_idf)) + geom_bar(stat = "identity", fill='orange',color='black')+ theme(axis.text.x = element_text(angle = 45, hjust = 1))+theme_calc()+scale_color_gdocs()
    
  }) 
  
}

# Create Shiny app ----
shinyApp(ui, server)

