library(ggplot2)
library(dplyr)
citations = read.csv("citation_data.csv", colClasses = c("character", "character", 
                                                         "integer", rep("factor",7)))

levels(citations$Gender.of.Article.Author) = list("Female" = 0, "Male" = 1)
levels(citations$Gender.of.Citation) = list("Female" = 0, "Male" = 1)
levels(citations$field) = list("History" = 1, "Philosophy/MJT" = 2, 
                               "Literature" = 3, "Ancient Text" = 4, 
                               "Sociology/Ethnography/Contemporary" = 5)
levels(citations$time.period) = list("Ancient" = 1, "Medieval" = 2, "Modern" =3)

male.auth = subset(citations, Gender.of.Article.Author == "Male")
female.auth = subset(citations, Gender.of.Article.Author == "Female")

# Is the average number of female citations = to the known presence in the field?
t.test(citations$Gender.of.Citation=="Female", alternative = "less", mu=.42)
# Among male authors?
t.test(male.auth$Gender.of.Citation=="Female", alternative = "less", mu=.42)
# Among female authors?
t.test(female.auth$Gender.of.Citation=="Female", alternative = "less", mu=.42)
# Is there a significant difference in % female citations between male and female authors?
t.test(male.auth$Gender.of.Citation=="Female", female.auth$Gender.of.Citation=="Female")
# All p-values are extremely low.  There is almost 0 chance that this is by chance.
# Female authors are cited less than 42% of the time regardless of gender of the author
# and are cited significantly less often by male authors.

# Basic plot to start with
ggplot(citations, aes(x=Gender.of.Citation)) +
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  scale_y_continuous(labels = scales::percent) +
  ylab("Percent") +
  xlab("Gender of Cited Author") +
  ggtitle("Gender of Cited Authors")

ggplot(citations, aes(x=Gender.of.Citation)) +
  geom_bar(aes(y = (..count..)/sum(..count..), fill=Gender.of.Article.Author)) +
  scale_y_continuous(labels = scales::percent) +
  ylab("Percent") +
  xlab("Gender of Cited Author") +
  ggtitle("Gender of Cited Authors") +
  labs(fill = "Gender of Article Author")



