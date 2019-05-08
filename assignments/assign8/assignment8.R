#Name: Dhaval Salwala
#Student ID: 18230845

#loading libraries
library(readr)
library(ggplot2)
library(dplyr)
library(stringr)

stopwords <- c("a", "about", "above", "above", "across", "after", "afterwards", "again", 
               "against", "all", "almost", "alone", "along", "already", "also","although",
               "always","am","among", "amongst", "amoungst", "amount",  "an", "and", "another", 
               "any","anyhow","anyone","anything","anyway", "anywhere", "are", "around", "as",  
               "at", "back","be","became", "because","become","becomes", "becoming", "been", 
               "before", "beforehand", "behind", "being", "below", "beside", "besides", 
               "between", "beyond", "bill", "both", "bottom","but", "by", "call", "can", 
               "cannot", "cant", "co", "con", "could", "couldnt", "cry", "de", "describe", 
               "detail", "do", "done", "down", "due", "during", "each", "eg", "eight", "either", 
               "eleven","else", "elsewhere", "empty", "enough", "etc", "even", "ever", "every", 
               "everyone", "everything", "everywhere", "except", "few", "fifteen", "fify", 
               "fill", "find", "fire", "first", "five", "for", "former", "formerly", "forty", 
               "found", "four", "from", "front", "full", "further", "get", "give", "go", "had", 
               "has", "hasnt", "have", "he", "hence", "her", "here", "hereafter", "hereby", 
               "herein", "hereupon", "hers", "herself", "him", "himself", "his", "how", "however", 
               "hundred", "ie", "if", "in", "inc", "indeed", "interest", "into", "is", "it", "its", 
               "itself", "keep", "last", "latter", "latterly", "least", "less", "ltd", "made", 
               "many", "may", "me", "meanwhile", "might", "mill", "mine", "more", "moreover", 
               "most", "mostly", "move", "much", "must", "my", "myself", "name", "namely", 
               "neither", "never", "nevertheless", "next", "nine", "no", "nobody", "none", "noone", 
               "nor", "not", "nothing", "now", "nowhere", "of", "off", "often", "on", "once", "one", 
               "only", "onto", "or", "other", "others", "otherwise", "our", "ours", "ourselves", 
               "out", "over", "own","part", "per", "perhaps", "please", "put", "rather", "re", 
               "same", "see", "seem", "seemed", "seeming", "seems", "serious", "several", "she", 
               "should", "show", "side", "since", "sincere", "six", "sixty", "so", "some", "somehow", 
               "someone", "something", "sometime", "sometimes", "somewhere", "still", "such", "system",
               "take", "ten", "than", "that", "the", "their", "them", "themselves", "then", 
               "thence", "there", "thereafter", "thereby", "therefore", "therein", "thereupon", 
               "these", "they", "thickv", "thin", "third", "this", "those", "though", "three", 
               "through", "throughout", "thru", "thus", "to", "together", "too", "top", "toward",
               "towards", "twelve", "twenty", "two", "un", "under", "until", "up", "upon", "us", 
               "very", "via", "was", "we", "well", "were", "what", "whatever", "when", "whence", 
               "whenever", "where", "whereafter", "whereas", "whereby", "wherein", "whereupon", 
               "wherever", "whether", "which", "while", "whither", "who", "whoever", "whole", 
               "whom", "whose", "why", "will", "with", "within", "without", "would", "yet", "you", 
               "your", "yours", "yourself", "yourselves", "the")

invalid_characters <- c("--","\\?","\\!","\\.",",","\\.","'",":")

#set working directory
setwd("/home/dsalwala")

f_pre <- readLines("Chapter 02.txt")
str(f_pre)
f_pre[1:10]

#1 function to split lines into words
convert_to_words_vector <- function(f_pre)
{
  unlist(str_split(str_c(f_pre, collapse=" "),regex('\\s+')))
}

f_pre_vec <- convert_to_words_vector(f_pre)
str(f_pre_vec)
f_pre_vec[1:10]

#2 function to apply pre processing on words vector
pre_processing<-function(x)
{
  #removing invalid characters
  x<-str_remove_all(x,str_c(invalid_characters,collapse = "|"))
  #removing empty words
  x<-x[x != ""]
  #converting words to lower case
  x<-str_to_lower(x)
  #removing stop words
  x<-x[!x %in% stopwords]
  return (x)
}
f_post<-pre_processing(f_pre_vec)
str(f_post)

#4
ans<-tibble(Words=unique(f_post),Pattern=str_c("^",Words,"$"),WLength=str_length(Words))
ans

#5
freq<-ans %>% group_by(WLength) %>% summarise(WFrequency=n())
freq

#6
ggplot(data=freq, aes(x=WLength, y=WFrequency))+geom_point(color="blue")+geom_line()+
  xlab("Word Length")+ylab("Word Frequency")