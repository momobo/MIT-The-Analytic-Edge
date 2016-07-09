# get list of questions i.e. all columns starting with a 'Q'
questions = grep("^Q",names(train))

# loop over all the questions
for (i in questions) {
  # convert the ith question, note double brackets [[ notation ...
  train[[i]] = as.numeric(train[[i]]) - 1
}
