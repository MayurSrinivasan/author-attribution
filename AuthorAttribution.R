library(tm)

readerPlain = function(fname){
  readPlain(elem=list(content=readLines(fname)), 
            id=fname, language='en') }

# Remember to source in the "reader" wrapper function

## Rolling two directories together into a single corpus
author_dirs1 = Sys.glob('../data/ReutersC50/C50train/*')
author_dirs2 = Sys.glob('../data/ReutersC50/C50test/*')
#author_dirs3 = rbind(author_dirs1, author_dirs2)
file_list = NULL
file_list1 = NULL
labels = NULL
labels1 = NULL
labels2 = NULL

for(author in author_dirs1) {
  files_to_add = Sys.glob(paste0(author, '/*.txt'))
  file_list = append(file_list, files_to_add)
}

for(author in author_dirs2) {
  files_to_add = Sys.glob(paste0(author, '/*.txt'))
  file_list1 = append(file_list1, files_to_add)
}

file_list2 = append(file_list,file_list1)


for(author in author_dirs1) {
  author_name = substring(author, first=29)
  files_to_add = Sys.glob(paste0(author, '/*.txt'))
  labels1 = append(labels1, rep(author_name, length(files_to_add)))
}

for(author in author_dirs2) {
  author_name = substring(author, first=28)
  files_to_add = Sys.glob(paste0(author, '/*.txt'))
  labels2 = append(labels2, rep(author_name, length(files_to_add)))
}

labels <- unique(append(labels1, labels2))

all_docs = lapply(file_list2, readerPlain) 
names(all_docs) = file_list2
names(all_docs) = sub('.txt', '', names(all_docs))

my_corpus = Corpus(VectorSource(all_docs))
names(my_corpus) = names(all_docs)

# Preprocessing
my_corpus = tm_map(my_corpus, content_transformer(tolower)) # make everything lowercase
my_corpus = tm_map(my_corpus, content_transformer(removeNumbers)) # remove numbers
my_corpus = tm_map(my_corpus, content_transformer(removePunctuation)) # remove punctuation
my_corpus = tm_map(my_corpus, content_transformer(stripWhitespace)) ## remove excess white-space
my_corpus = tm_map(my_corpus, content_transformer(removeWords), stopwords("SMART"))

DTM = DocumentTermMatrix(my_corpus)
DTM # some basic summary statistics

class(DTM)  # a special kind of sparse matrix format

inspect(DTM[46:55,1:20])
DTM = removeSparseTerms(DTM, 0.975)
DTM

X = as.matrix(DTM)

X_train <- X[1:2500,]

labels <- unique(labels)

smooth_count = 1/nrow(X_train)

for(i in 1:50) 
{ 
  nam1 <- paste("w",labels[i], sep = "_")
  temp <- colSums(X_train[(50*i-49):(50*i),] + smooth_count)
  assign(nam1, temp/sum(temp))
}

X_test <- X[2501:5000,]

result = matrix(, nrow = 2500, ncol = 51)
for(i in 1:2500) 
{ for(j in 1:50)
  {
  nam1 <- paste("w",labels[j], sep = "_")
  #check <- log(get(nam1))
  result[i,j] = sum(X_test[i,]*log(get(nam1)))
  }
}

result[1:5,1:10]

#result = cbind(labels1,result)

for (i in 1:2500)
{
  result[i,51] = which.max(result[i,])
}

result1 = NULL
result1 = cbind((rep(1:50, each=50)),result[,51])
result1$auth <- rep(1:50, each=50)
result1$pred_auth <- result[,51]


result[, "max"] <- apply(result[, 1:50], 1, which.max)



library()





