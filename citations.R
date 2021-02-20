library(ggplot2)
library(dplyr)
library(ggmosaic)
library(tidyr)

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
  geom_bar(aes(y = (..count..)/sum(..count..), fill=Gender.of.Article.Author)) +
  scale_y_continuous(labels = scales::percent) +
  ylab("Percent") +
  xlab("Gender of Cited Author") +
  ggtitle("Gender of Cited Authors") +
  labs(fill = "Gender of Article Author") +
  scale_fill_manual(values=c("indianred1","royalblue2"))

# I wouldn't use this one.  I think the next is better.
ggplot(citations, aes(x=Gender.of.Article.Author)) +
  geom_bar(aes(fill=Gender.of.Citation), 
           position = "dodge") +
  xlab("Gender of Article Author") +
  ggtitle("Gender of Cited Authors") +
  labs(fill = "Gender of Cited Author") +
  scale_fill_manual(values=c("indianred1","royalblue2"))

# I think this is the best visualization.
ggplot(citations, aes(x=Gender.of.Article.Author, fill = forcats::fct_rev(Gender.of.Citation))) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  ylab("Percent") +
  xlab("Article Author") +
  ggtitle("Gender of Cited Authors by Gender of Article Author") +
  labs(fill = "Cited Author",
    subtitle = "dotted line at 42%") +
  geom_hline(yintercept = .42, linetype = "dotted") +
  scale_fill_manual(values=c("royalblue2", "indianred1"))

# There is clearly a difference in different subfields
by_field = citations %>%
  group_by(field)
by_field = by_field %>% summarise(
  mean = mean(Gender.of.Citation == "Female"))

# P-value is low enough to indicate the difference is significant 
# (Chen, is this the test we should run?)
field.table = with(citations, table(Gender.of.Citation, field))
chisq.test(field.table)




# best by subfield visualization
ggplot(citations, aes(x=field, fill = forcats::fct_rev(Gender.of.Citation))) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  ylab("Percent") +
  xlab("Subfield") +
  coord_flip() +
  ggtitle("Gender of Cited Authors by Subfield") +
  labs(fill = "Cited Author",
       subtitle = "dotted line at 42%") +
  geom_hline(yintercept = .42, linetype = "dotted") +
  scale_fill_manual(values=c("royalblue2", "indianred1"))













#4. Are women cited at different rates for scholarly work on different time periods?
citations[!complete.cases(citations),]
citations <- na.omit(citations)

ggplot(citations, aes(x=time.period, fill = forcats::fct_rev(Gender.of.Citation))) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  ylab("Percent") +
  xlab("Time periods") +
  coord_flip() +
  ggtitle("Gender of Cited Authors by different time periods") +
  labs(fill = "Cited Author",
       subtitle = "dotted line at 42%") +
  geom_hline(yintercept = .42, linetype = "dotted") +
  scale_fill_manual(values=c("royalblue2", "indianred1"))



#5. How do Jewish Studies journals compare to the Religious Studies journal, 
# with respect to the proportion of women scholars cited? 
# Does that change if we control for differences between male authors and female authors?
ggplot(citations, aes(x=journal, fill = forcats::fct_rev(Gender.of.Citation))) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  ylab("Percent") +
  xlab("Journal") +
  coord_flip() +
  ggtitle("Gender of Cited Authors by different time periods") +
  labs(fill = "Cited Author",
       subtitle = "dotted line at 42%") +
  geom_hline(yintercept = .42, linetype = "dotted") +
  scale_fill_manual(values=c("royalblue2", "indianred1"))
  