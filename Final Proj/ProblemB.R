library(XML)
library(tm)

get_obj <- function(source) {
	object <- Corpus(VectorSource(source))
	object <- tm_map(object, tolower)
   	object <- tm_map(object, removePunctuation)
   	object <- tm_map(object, stripWhitespace)
   	object <- tm_map(object, removeNumbers)
   	object <- tm_map(object, removeWords,stopwords('en'))
   	return(object)
}

get_data <- function(link) {
	file_list <- as.character(unlist(data.frame(readHTMLTable(link),stringsAsFactors <- FALSE)['NULL.Name']))
	data <- data.frame()
	for (i in 3:length(file_list)) {
  		file_name <- file_list[i]

	  	if (length(grep('.tex', file_name)) != 0) {
    		file_content <- readLines(paste(link, file_name, sep = '/'))
    		mat <- as.matrix(TermDocumentMatrix(get_obj(file_content)))
    		
    		count <- c()
    		words <- rownames(mat)

    		# get sum of word count
    		for (i in 1:length(words))
     	 		count <- c(count, sum(mat[words[i], 1:length(mat[words[i],])]))

     	 	# combine words and count
    		mat <- as.data.frame(cbind(words[1:length(words)], count[1:length(count)]))
    		
    		# add to data matrix
    		data <- rbind(data, mat)
		}
	}

	data <- merge_dup_rows(data)
	
	# decresing order
	data <- data[order(as.numeric(data[,'V2']), decreasing = TRUE),]
	
	# add column names
	data <- data.frame(Words = data[,'V1'], Frequency = data[,'V2'])
	
	# reset row names
	rownames(data) <- data[,'Words']

	return(data)
}

merge_dup_rows <- function(raw_data) {
	raw_data <- as.matrix(raw_data)
	dup_words <- raw_data[duplicated(raw_data[,'V1']),]
	data <- raw_data[!duplicated(raw_data[,'V1']),]
	
	for (i in 1:length(dup_words[,'V1'])) {
		index <- which(data[,'V1'] == dup_words[i,'V1'])
		data[index,'V2'] <- as.character(as.numeric(data[index,'V2']) + as.numeric(dup_words[i,'V2']))
	}

	return(data)
}

link_50 <- 'http://heather.cs.ucdavis.edu/~matloff/50/OldExams'
link_132 <- 'http://heather.cs.ucdavis.edu/~matloff/132/OldExams'
link_145 <- 'http://heather.cs.ucdavis.edu/~matloff/145/Exams'
link_152A <- 'http://heather.cs.ucdavis.edu/~matloff/152A/Exams'
link_154A <- 'http://heather.cs.ucdavis.edu/~matloff/154A/OldExams'
link_154B <- 'http://heather.cs.ucdavis.edu/~matloff/154B/Exams'
link_156 <- 'http://heather.cs.ucdavis.edu/~matloff/156/Exams'
link_158 <- 'http://heather.cs.ucdavis.edu/~matloff/158/OldExams'
#link_188 <- 'http://heather.cs.ucdavis.edu/~matloff/188/OldExams'
link_256 <- 'http://heather.cs.ucdavis.edu/~matloff/256/Exams'

#data_50 <- get_data(link_50)
#data_132 <- get_data(link_132)
#data_145 <- get_data(link_145)
#data_152A <- get_data(link_152A)
#data_154A <- get_data(link_154A)
#data_154B <- get_data(link_154B)
#data_156 <- get_data(link_156)
data_158 <- get_data(link_158)
data_256 <- get_data(link_256)

words_all <- c(data_158['Words'], data_256['Words'])
words_all <- words_all[!duplicated(words_all)]

#write.table(get_data(link_50), '50_result.txt')
#write.table(get_data(link_132), '132_result.txt')
#write.table(get_data(link_145), '145_result.txt')
#write.table(get_data(link_154A), '154A_result.txt')
#write.table(get_data(link_156), '156_result.txt')
#write.table(get_data(link_158), '158_result.txt')
#write.table(get_data(link_256), '256_result.txt')

print(readHTMLTable('http://heather.cs.ucdavis.edu/~matloff/152A/OldExams.html'))
