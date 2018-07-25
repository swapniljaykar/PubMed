#
install.packages(easyPubMed)
library(easyPubMed)

first_query <- "Stable isotope"                   # text of the query
first_records <- get_pubmed_ids(first_query)      # submit the query to PubMed
fetch_pubmed_data(first_records, retmax = 1)      # retrieve the first output record

#Install BusinessPubMed for scraping the data from PubMed
library(devtools)
install_github("dami82/businessPubMed", force = TRUE)

#Query to Search in PubMed Citation Library without country fillter
query <- "\"Stable isotope\")" 

#Query to Search in PubMed Citation Library with Title , Abstract and country fillter
query <- "\"Stable Isotope \"[TIAB] AND (IND[Affiliation] OR \"India\")" 

#Query to Search in PubMed Citation Library with Title country fillter
query <- "\"Stable Isotope \"[TI] AND (IND[Affiliation] OR \"India\")" 

#Citation Publication Year wise Filter
query <- paste(my.query, "AND (\"2015\"[EDAT] : \"2018\"[EDAT])")
idlist <- get_pubmed_ids(query)
idlist$Count # returns 5379
#
batch.size <- 1000
my.seq <- seq(1, as.numeric(idlist$Count), by = batch.size)

# Go ahead with the retrieval

pubmed.data <- lapply(my.seq, (function(ret.start)
batch.xml <- fetch_pubmed_data(idlist, retstart = ret.start, retmax = batch.size)


# Retrieve a sample "batch.xml"...
batch.xml <- fetch_pubmed_data(idlist, retstart = 1, retmax = batch.size)
#
record.list <- easyPubMed::articles_to_list(batch.xml)
tmp.record <- article_to_df(pubmedArticle = record.list[[1]], 
                            autofill = TRUE,                     # impute NA affiliations
                            max_chars = 0)                       # do not retrieve abstract
class(tmp.record)                                                # data.frame
colnames(tmp.record)
# [1] "pmid" "doi" "title" "abstract" "year" "month" "day" "jabbrv"   
# [9] "journal" "lastname" "firstname" "address" "email"

pubmed.data <- lapply(1:length(record.list), (function(i){
  #
  # monitor progress
  if (length(record.list) > 60) {
    custom.seq <- as.integer(seq(1, length(record.list), length.out = 50)) 
    if (i %in% custom.seq) { message(".", appendLF = FALSE)}
  } else {
    message(".", appendLF = FALSE)
  }
  #
  # extract info
  tmp.record <- tryCatch(easyPubMed::article_to_df(pubmedArticle = record.list[[i]], 
                                                   autofill = TRUE, 
                                                   max_chars = 10),
                         error = function(e) { NULL } )
  #
  # return data that matter
  if (!is.null(tmp.record)) {
    required.cols <- c("pmid", "doi", "title", "abstract", "year", "month", "day", "jabbrv", "journal", "lastname", "firstname", "address", "email")
    out.record <- data.frame(matrix(NA, nrow = nrow(tmp.record), ncol = length(required.cols)))
    colnames(out.record) <- required.cols
    match.cols <-  colnames(tmp.record)[colnames(tmp.record) %in% required.cols]
    out.record[,match.cols] <- tmp.record[,match.cols]
  } else {
    out.record <- NULL
  }
  out.record
}))

#Bind the data
pubmed.data <-do.call(rbind, pubmed.data)

pubmed.data[100:110,c(4,5,7)]                      # final data frame, excerpt

#Save the extracted data in csv file
write.csv(pubmed.data,"pubmed_india_isolated_2018.csv",row.names = F)
