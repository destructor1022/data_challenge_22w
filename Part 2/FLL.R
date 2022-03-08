library(readtext)
library(tidyr)
library(dendextend)

working_dir = "C:/Users/sauna/Documents/Documents/Dartmouth/Clubs/DALI - Data Vis/data_challenge_22w/Part 2/"

# make it easier to find data
path_to = function(target) {
  return(paste(working_dir, target, sep = ""))
}

#get the list of books and path names
get_list_of_books = function(language) {
  # get list of all book names in folder
  books = list.files(path=path_to(paste("Books/", language, sep="")), pattern="*.txt", full.names=FALSE, recursive=FALSE)
  # get full path of all books in folder
  books_paths = list.files(path=path_to(paste("Books/", language, sep="")), pattern="*.txt", full.names=TRUE, recursive=FALSE)
  # convert to data frame
  as_df = as.data.frame(cbind(books, books_paths))
  # remove .txt from book names
  as_df$books = substr(as_df$books,1,nchar(as_df$books)-4)
  return(as_df)
}

# given a book, return the frequency count of first letters
get_frequency_count = function(full_path) {
  # import the text to a string variable
  text = readtext(full_path)
  # get first letter of all text in books & capitalize it & remove line breaks
  first_letters = gsub('\\b(\\pL)\\pL{0,}|.','\\U\\1', text$text, perl = TRUE)
  first_letters = gsub("[\r\n]", "", first_letters)
  # convert to array
  first_letters_array = unlist(strsplit(first_letters, ""))
  # return the frequency array
  return(as.factor(first_letters_array))
}

good_letters_vector = function() {
  return(c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M", "N", "O", "P", "Q", "R", "S", "T", "U", "V", "W", "X", "Y", "Z"))
}

get_language_color = function(language) {
  if(language == "English") {
    return("Red")
  } 
  if (language == "German") {
    return("Black")
  }
  if (language == "Spanish") {
    return("Gold")
  }
  if (language == "Portuguese") {
    return("Forest Green")
  }
  return("Gray")
}

languages = list.dirs(path = path_to("Books"), full.names = FALSE, recursive = FALSE)

dend_data = vector()
dend_color = vector()
kmeans_data = vector()

for(language in languages) {
  # get all the books in the language
  books = get_list_of_books(language)
  MSE_vector = vector()
  for(row in 1:nrow(books)) {
    # set our important variables
    book_name = books[row, "books"]
    book_path = books[row, "books_paths"]
    # get the frequency count of a book
    freq_count = summary(get_frequency_count(book_path))
    # filter out only letters that belong in the language
    freq_count = freq_count[good_letters_vector()]
    freq_count = replace_na(freq_count, 0)
    # convert to percentages
    percentages_nosort = freq_count / sum(freq_count)
    # sort descending
    percentages = sort(percentages_nosort, decreasing = TRUE)
    # find out how many unique first letters there are in the book
    nl = length(percentages)
    x = 1:nl
    # define the first letter law for the book
    fll = (nl - (nl - 1)*log(nl - 1, nl) - x*log(x, nl) + (x - 1)*log(x - 1, nl)) / (nl * (nl - 1) * log(nl / (nl - 1), nl))
    fll[1] = (nl - (nl - 1)*log(nl - 1, nl) - 1*log(1, nl)) / (nl * (nl - 1) * log(nl / (nl - 1), nl))
    # get MSE and store to variable
    MSE = mean((percentages - fll)^2)
    MSE_vector = rbind(MSE_vector, MSE)
    # make a pretty graph & save it
    png_file = path_to(paste("Graphs/", language, "/", book_name, ".png", sep=""))
    dir.create(dirname(png_file), showWarnings = FALSE)
    png(file=png_file, width=1200, height=600)
    barplot(rbind(percentages, as.vector(fll)), main = paste(book_name, " (", language, ")", sep = ""), col = c(get_language_color(language), "Gray"), beside = T)
    legend(20, 0.13, legend = c(paste("MSE =", MSE), language, "Expected"), fill = c("White", get_language_color(language), "Gray"), cex = 2)
    dev.off()
    # add the data to the dendogram vector
    dend_data = rbind(dend_data, percentages_nosort)
    rownames(dend_data)[nrow(dend_data)] = book_name
    dend_color = rbind(dend_color, get_language_color(language))
    # add data to kmeans vector
    kmeans_data = rbind(kmeans_data, percentages_nosort)
    rownames(kmeans_data)[nrow(kmeans_data)] = paste(book_name, " (", language, ")", sep = "")
  }
  print(paste(language, mean(MSE_vector)))
}

# create dendogram by percentages
png_file = path_to(paste("Graphs/", "Dendrogram", ".png", sep=""))
dir.create(dirname(png_file), showWarnings = FALSE)
png(file=png_file, width=1200, height=600)
dend = as.dendrogram(hclust(dist(dend_data)))
dend_color = dend_color[order.dendrogram(dend)]
labels_colors(dend) = dend_color
par(cex=0.3)
plot(dend)
colors_list = c("English", "German", "Portuguese", "Spanish")
legend(32, 0.25, legend=colors_list,
       col=unlist(lapply(colors_list, get_language_color)), 
       lty=c(1, 1, 1, 1), cex=2.5)
dev.off()

# create kmeans clusters
kclusters = kmeans(kmeans_data, centers = 4)
print(kclusters$cluster)
