
citations = read.csv("citation_data.csv")

citations$First.Name = as.character(citations$First.Name)
citations$Last.Name = as.character(citations$Last.Name)
citations$Citation.Count = as.integer(citations$Citation.Count)

male.auth = subset(citations, Gender.of.Article.Author == 1)
female.auth = subset(citations, Gender.of.Article.Author == 0)
