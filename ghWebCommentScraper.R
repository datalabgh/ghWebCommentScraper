# http://www.ghanaweb.com/GhanaHomePage/NewsArchive/artikel.php?
# ID=372752&comment=11965149#com
#' Title
#'
#' @param ghWebURL 
#'
#' @return
#' @export
#'
#' @examples
ghWebCommentScraper <- function(ghWebURL)
{
  require(RCurl)
  require(XML)
  require(lubridate)
  require(stringr)
  require(tm)
  
  # check that it is a NewsArticle
  newsURL <- str_split(ghWebURL, "/")
  
  if (!(c("NewsArchive") %in% unlist(newsURL)))
    return(print("URL not a valid news item"))
  
  # split url on'=' for baseURL, articleID and commentID parts
  urlSplit <- str_split(ghWebURL, "=")
    
  # split on '&' to get article and comment halves  
  urlSplitArticleHalf <- str_split(urlSplit[[1]][2], "&")
    
  # get article id
  urlArticleID <- urlSplitArticleHalf[[1]][1]
    
  # split on '#' to get comment id
  urlSplitCommentHalf <- str_split(urlSplit[[1]][3], "#")
    
  # get comment id
  urlCommentID <- urlSplitCommentHalf[[1]][1]
    
  # get URL base from first split
  ghWebBaseURL <- urlSplit[[1]][1]
    
  ghWebLink <- paste(ghWebBaseURL, "=", 
                       urlArticleID, "&comment=", 
                       urlCommentID, "#com", 
                       sep="")
    
  ghWebRaw <- getURL(ghWebLink, encoding="UTF-8")
    
  # parse html doc
  ghWebPARSED <- htmlParse(ghWebRaw, asText = TRUE)
    
  # apply xpath on div element
  # <div class='subject_txt'><a href=""/></div>
  commentLinks <- xpathSApply(ghWebPARSED, 
                              "//div[@class='subject_txt']//a/@href")
  
  allCommentIDs <- NULL
  # start from the second comment. 
  # 1st has comment id 0
  for (i in 2:length(commentLinks)){
    # break up <a href="artikel.php?ID=articleID&comment=commentID#com">
    
    # split on &
    split_one <- str_split(commentLinks[i],"&")
    # split on =
    split_two <- str_split(split_one[[1]][2],"=")
    # split on #
    split_three <- str_split(split_two[[1]][2], "#")
    # actual comment id
    commentID <- as.integer(split_three[[1]][1])
    
    # build comments id vector
    allCommentIDs <- c(allCommentIDs, commentID)
  }
  
  # add initial comment ID
  allCommentIDs <- c(urlCommentID, allCommentIDs)
  
  # build URLS
  target_URLs <- paste(ghWebBaseURL, 
                       "=", urlArticleID,
                       "&comment=", allCommentIDs, 
                       "#com", sep="")
  
  commentMatrix <- NULL
  for (i in 1:length(target_URLs)){
    
    nextCommentURL <- getURL(target_URLs[i], encoding="UTF-8")
    
    #print(nextCommentURL)
    
    urlPARSED <- htmlParse(nextCommentURL, asText = TRUE, encoding = "UTF-8")
    
    # convert output to text
    commentInfo <- xpathSApply(urlPARSED,"//div[@id='comment_area']/
                               div[@class='comment']/text()", 
                               xmlValue)
    
    # retrieve individual components
    commentAuthor <- str_trim(commentInfo[2])
    
    commentTime <- str_trim(commentInfo[3])
    
    commentTo <- str_trim(commentInfo[4])
    
    # some comments are broken up hence this (stick them together)
    # comments start from index 6
    comment <- str_trim(paste(commentInfo[6:length(commentInfo)], collapse = " "))
    
    commentSubjectInfo <- xpathSApply(urlPARSED, 
                                      "//div[@id='comment_area']/
                                      div[@class='threads']/
                                      div[@class='line_active']//
                                      div[@class='subject_txt']/text()", 
                                      xmlValue)
    commentSubject <- str_trim(commentSubjectInfo)
    
    # add to vector
    commentMatrix <- rbind(commentMatrix, 
                           c(commentAuthor, commentTime, 
                             commentTo, comment, commentSubject))
  }
  
  colnames(commentMatrix) <- c("Author", "Time", "Comment To", "Comment", 
                               "Comment Subject")
  
  # return comments as data frame
  comment.df <- data.frame(commentMatrix,
                       stringsAsFactors = FALSE)

  myCorpus <- Corpus(VectorSource(comment.df$Comment))
  
  myCorpus <- tm_map(myCorpus, content_transformer(tolower))
  
  removeURL <- function(x) gsub("http[^[:space:]]*", "", x)
  
  myCorpus <- tm_map(myCorpus, content_transformer(removeURL))
  
  removeNumPunct <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)
  
  myCorpus <- tm_map(myCorpus, content_transformer(removeNumPunct))
  
  myCorpus <- tm_map(myCorpus, stripWhitespace)
  
  myCorpus <- tm_map(myCorpus, removeWords, stopwords("english"))
  
  tdm <- TermDocumentMatrix(myCorpus,
                            control = list(wordLengths = c(1, Inf)))
  
  m2 <- as.matrix(tdm)
  
  # cluster terms
  distMatrix <- dist(scale(m2))
  fit <- hclust(distMatrix, method = "ward.D")
  
  plot(fit)
  rect.hclust(fit, k = 6)  # cut tree into 6 clusters
  
}