#Библиотеки
library(shiny)
library(readr)
library(dplyr)
library(tidyr)
library(recommenderlab)
library(shinyWidgets)
devtools::install_github("stefanwilhelm/ShinyRatingInput")
library(ShinyRatingInput)
library(readr)
library(shinythemes)
devtools::install_github('lyzander/tableHTML')
library(tableHTML)
last <- function(x) { return( x[length(x)] ) }
#paste0("<a href='",reccomend$bookURL,"' target='_blank'>",reccomend$bookURL,"</a>")
#info <- list(id = "john")
#images <- paste0("student-info/",info$id,".png")
#knitr::asis_output(paste0("<img src='",images,"' title='",info$pref_name," ", info$last_name,"' style='height:60px'>"))

#Подгружаем данные
fullcharacteristics <- read_csv("/students/vakraynikova/project books/fullcharacteristics.csv")
communities<-read.csv("/students/vakraynikova/project books/communities10k.csv")
communities[,1]<-NULL
fullratings <-read_csv("/students/vakraynikova/project books/fullratings.csv")
fullratings[,1]<-NULL

#fulldata<- read_csv("Books/fulldata.csv")
#fulldata[,1]<-NULL
f=NULL
#Для модели по юзерам
q<-fullratings
#для модели по комьюнитис
comdata<-left_join(communities,q,by="user_id")

#для интерфейса оценки
w1=dplyr::select(fullcharacteristics,goodreads_book_id,title,genre,count)
w1=w1 %>% group_by(genre) %>% top_n(20,count)
w1=dplyr::select(w1,title)


ui <- fluidPage(theme = shinytheme("slate"),
                tags$style(type="text/css",
                           ".shiny-output-error { visibility: hidden; }",
                           ".shiny-output-error:before { visibility: hidden; }",
                           "#Genrelover{color: lightblue; font-size: 20px;font-style: bold; }"
                ),
              
                # Application title
                titlePanel("BRS: нет, это не Steam"),
                
                # Sidebar with a slider input for number of bins 
                sidebarLayout(
                  sidebarPanel(
                    
                    pickerInput(inputId = "Id062", 
                                label = "Поиск", choices = w1$title, multiple = FALSE, options = list(`live-search` = TRUE,`none-selected-text` = "Выберите")),
                    
                    ratingInput("movieRating", label="Оцените эту книгу:", dataStop=5, dataFractions=1),
                    htmlOutput("movieRating"),
                    
                    actionButton("id_primary","Оценить",styleclass="primary",size="mini"),
                    
                    actionButton("submit","Хватит, давайте рекомендации!",styleclass="primary",size="mini")
                  ),
                  
                  
                  mainPanel(verticalLayout(  
                    tabsetPanel(type = "tabs",
                                tabPanel("Выбранные книги",  dataTableOutput("value5")),
                                tabPanel("Рекомендации по жанрам", textOutput("Genrelover"),dataTableOutput("Recommendation_Genre")),
                                tabPanel("Рекомендации по юзерам",  dataTableOutput("Recommendation_Users")),
                                tabPanel("Рекомендации по комьюнити",  dataTableOutput("Recommendation_Community"))
                    )
                    
                  )
                  )
                )
)

# Define server logic required to draw a histogram
server <- function(input, output,session ) {
  
  
  output$movieRating <- renderText({
    paste("Текущая оценка книге:",input$movieRating)})
  
  
  output$value1 <- renderDataTable({ 
    
    transformation()})
  
  output$value3 <- renderPrint({ 
    input$action })
  
  output$list<-renderPrint({
    myValues$dList
    
  })
  output$list1<-renderPrint({
    myValues$dList1
  })
  
  myValues <- reactiveValues()
  observe({
    if(input$id_primary > 0){
      myValues$dList <- c(isolate(myValues$dList), isolate(input$Id062))
      myValues$dList1 <- c(isolate(myValues$dList1), isolate(input$movieRating))
      
    }
  })
    output$value5 <- renderDataTable({ 
      t1 = as.data.frame(myValues$dList1)
      t = as.data.frame(myValues$dList)           
      names(t1)[1]="rates"
      names(t)[1]="book"
    
      # tableHTML(bind_cols(t,t1), headers = c("Книга","Оценка"), border = 1,
      #         collapse = "separate_shiny", spacing = "10px",
      #        theme = "rshiny-blue") %>%
      # add_css_column(list('background', '#D8E6F3'), c(0, 1,2)) %>% 
      #add_css_column(list('text-align', 'center'), c(0, 1,2))
     
      bind_cols(t,t1)
      
      
    })
    
   
    
    observe({
      if(input$submit > 0){
        t1 = as.data.frame(myValues$dList1)
        t = as.data.frame(myValues$dList)           
        names(t1)[1]="rates"
        names(t)[1]="book"
        updateTextInput(session, "Id062", value = " ")   
        updateTextInput(session, "movieRating", value = " ")   
        value5=bind_cols(t,t1)
      }  
    output$Genrelover<-renderText({
      user.test<-as.data.frame(value5)
      colnames(user.test)[1]<-"title"
      colnames(user.test)[2]<-"rating"
      user.test=left_join(user.test,fullcharacteristics,by="title")
      user.genre = dplyr::select(user.test,rating,genre)
      user.genre$genre<-as.factor(user.genre$genre)
      user.genre<-user.genre %>% group_by(genre) %>% summarise(count=n(),rate=mean(rating)) %>% arrange(desc(count))
      a<- top_n(user.genre,1,count)
      a<-a$genre
      paste("Хм, сдаётся нам у нас тут",a[1],"-lover:")
    })
    output$Recommendation_Genre <- renderDataTable({
      user.test<-as.data.frame(value5)
      colnames(user.test)[1]<-"title"
      colnames(user.test)[2]<-"rating"
      user.test=left_join(user.test,fullcharacteristics,by="title")
      user.genre = dplyr::select(user.test,rating,genre)
      user.genre$genre<-as.factor(user.genre$genre)
      user.genre<-user.genre %>% group_by(genre) %>% summarise(count=n(),rate=mean(rating)) %>% arrange(desc(count))
      a<- top_n(user.genre,1,count)
      a<-a$genre
      booksforuser<-anti_join(fullcharacteristics,user.test,by="goodreads_book_id")
      
      #модель
      if (length(a)>2) {finalrec=filter(booksforuser,booksforuser$genre==a[1])} else {if (length(a)>1) {finalrec=filter(booksforuser,booksforuser$genre==a[1] | booksforuser$genre==a[2])} else {finalrec=filter(booksforuser,booksforuser$genre==a[1])}}
      finalrec=finalrec[!duplicated(finalrec[,2]),]
      reccomend=head(arrange(finalrec,desc(averagerate)),10)
      reccomend$title=paste0("<a href='",reccomend$bookURL,"' target='_blank'>",reccomend$title,"</a>")
      reccomend=head(select(reccomend,title:averagerate,cover),10)
      reccomend$cover=knitr::asis_output(paste0("<img src='",reccomend$cover,"' style='height:60px'>"))
      return(reccomend)
    },escape = FALSE)
    
    output$Recommendation_Users <- renderDataTable({
      user.test<-as.data.frame(value5)
      colnames(user.test)[1]<-"title"
      colnames(user.test)[2]<-"rating"
      user.test$user_id=0
      user=left_join(user.test,fullcharacteristics,by="title")
      user=user[!duplicated(user[,1]),]
      user=select(user,user_id,goodreads_book_id,rating,title,author,genre:bookURL)
      user=select(user,user_id, goodreads_book_id, rating)
      user$rating=as.numeric(user$rating)
      q$hasbook=q$goodreads_book_id%in%user$goodreads_book_id
      qa=q %>% group_by(user_id,hasbook) %>% summarise(count=n())
      qa=filter(qa,hasbook==TRUE)
      pairs=head(arrange(qa,desc(count)),50)
      q$leave=q$user_id%in%pairs$user_id
      q=filter(q,leave==TRUE)
      q=select(q,user_id, goodreads_book_id, rating)
      q=bind_rows(q,user)
      q<-q %>% 
        spread(key=goodreads_book_id,value=rating)
      q=as.data.frame(q)
      rownames(q)<-q$user_id
      q=dplyr::select(q,-user_id)
      qr<-as.matrix(q)
      qr<-as(qr,"realRatingMatrix")
      recc_model <- Recommender(data = qr, method = "UBCF")
      recc_predicted <- predict(object = recc_model, newdata = qr, n = 5)
      prediction=as.data.frame(t(as.data.frame(recc_predicted@items[1])))
      prediction$user_id=0
      p=t(select(prediction,-user_id))
      books_user_1 <- recc_predicted@itemLabels[p]
      books=as.data.frame(books_user_1)
      colnames(books)[1]<-"goodreads_book_id"
      books$goodreads_book_id=as.character(books$goodreads_book_id)
      books$goodreads_book_id=as.numeric(books$goodreads_book_id)
      books=left_join(books,fullcharacteristics,by="goodreads_book_id")
      books=books[!duplicated(books[,2]),]
      reccomend=head(select(books,title:averagerate,cover,bookURL),10)
      reccomend$title=paste0("<a href='",reccomend$bookURL,"' target='_blank'>",reccomend$title,"</a>")
      reccomend=head(select(reccomend,title:averagerate,cover),10)
      reccomend$cover=knitr::asis_output(paste0("<img src='",reccomend$cover,"' style='height:60px'>"))
      return(reccomend)
    },escape=FALSE)
    
    
    output$Recommendation_Community <- renderDataTable({
      user.test<-as.data.frame(value5)
      colnames(user.test)[1]<-"title"
      colnames(user.test)[2]<-"rating"
      user.test$rating=as.numeric(user.test$rating)
      user.test$user_id=0
      user=left_join(user.test,fullcharacteristics,by="title")
      united=left_join(select(user,user_id,goodreads_book_id,rating),comdata,by="goodreads_book_id")
      colnames(united)[1] <- "user_id"
      colnames(united)[3] <- "user_rating"
      colnames(united)[4] <- "user_id_pair"
      colnames(united)[7] <- "pair_rating"
      united=na.omit(united)
      united$user_rating=as.character(united$user_rating)
      united$user_rating=as.numeric(united$user_rating)
      united$pair_rating=as.character(united$pair_rating)
      united$pair_rating=as.numeric(united$pair_rating)
      united$dev=abs(united$user_rating-united$pair_rating)
      ufg<-united %>% group_by(communities) %>% summarise(avdev=mean(dev),sumdev=sum(dev),count=n(),rss=sum(dev^2))
      reccomend=filter(communities,communities==ufg$communities[which.min(ufg$rss)])
      reccomend=left_join(reccomend,fullratings,by="user_id")
      reccomend=anti_join(reccomend,user,by="goodreads_book_id")
      reccomend=left_join(reccomend,fullcharacteristics,by="goodreads_book_id")
      reccomend<-reccomend %>% group_by(goodreads_book_id,title,genre,averagerate,cover,bookURL) %>% summarise(count=n(),avmark=mean(rating))
      reccomend=reccomend[!duplicated(reccomend[,2]),]
      reccomend=head(arrange(reccomend,desc(count),desc(avmark)),10)
      reccomend=ungroup(reccomend)
      reccomend$title=paste0("<a href='",reccomend$bookURL,"' target='_blank'>",reccomend$title,"</a>")
      reccomend=head(select(reccomend,title:averagerate,cover),10)
      reccomend$cover=knitr::asis_output(paste0("<img src='",reccomend$cover,"' style='height:60px'>"))
      return(reccomend)
       },escape=FALSE)
    
    
    })
}


# Run the application 

shinyApp(ui = ui, server = server)
