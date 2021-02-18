library(ggplot2)
citations = read.csv("citation_data.csv")

citations$First.Name = as.character(citations$First.Name)
citations$Last.Name = as.character(citations$Last.Name)
citations$Citation.Count = as.integer(citations$Citation.Count)

male.auth = subset(citations, Gender.of.Article.Author == 1)
female.auth = subset(citations, Gender.of.Article.Author == 0)

# Is the average number of female citations = to the known presence in the field?
t.test(citations$Gender.of.Citation==0, alternative = "less", mu=.42)
# Among male authors?
t.test(male.auth$Gender.of.Citation==0, alternative = "less", mu=.42)
# Among female authors?
t.test(female.auth$Gender.of.Citation==0, alternative = "less", mu=.42)
# Is there a significant difference in % female citations between male and female authors?
t.test(male.auth$Gender.of.Citation==0, female.auth$Gender.of.Citation==0)
# All p-values are extremely low.  There is almost 0 chance that this is by chance.
# Female authors are cited less than 42% of the time regardless of gender of the author
# and are cited significantly less often by male authors.

ggplot(citations, aes(x=Gender.of.Citation)) +
  geom_bar()
