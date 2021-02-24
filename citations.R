library(ggplot2)
library(dplyr)
library(ggmosaic)
library(tidyr)

citations = read.csv("citation_data.csv", colClasses = c("character", "character", 
                                                         "integer", rep("factor",7)))

levels(citations$Gender.of.Article.Author) = list("Female" = 0, "Male" = 1)
levels(citations$Gender.of.Citation) = list("Female" = 0, "Male" = 1)
levels(citations$field) = list("History" = 1, "Philosophy/ \n MJT" = 2, 
                               "Literature" = 3, "Ancient Text" = 4, 
                               "Sociology/ \n Ethnography/ \n Contemporary" = 5)
levels(citations$time.period) = list("Ancient" = 1, "Medieval" = 2, "Modern" =3)

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
  geom_bar(aes(fill=Gender.of.Citation), position = "dodge") +
  xlab("Gender of Article Author") +
  ggtitle("Gender of Cited Authors") +
  labs(fill = "Gender of Cited Author") +
  scale_fill_manual(values=c("indianred1","blue")) 


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

# best by subfield visualization
ggplot(citations) +
  geom_mosaic(aes(x=product(field), fill = Gender.of.Citation)) +
  scale_y_continuous(labels = scales::percent) +
  ylab("Percent") +
  xlab("Field") +
  ggtitle("Gender of Cited Authors by Field") +
  labs(fill = "Gender of Cited Author",
    subtitle = "dotted line at 42%") +
  geom_hline(yintercept = .42, linetype = "dotted") +
  coord_flip()


ggplot(citations) +
  geom_mosaic(aes(x=product(time.period), fill = Gender.of.Citation)) +
  scale_y_continuous(labels = scales::percent) +
  ylab("Percent") +
  xlab("Time Period") +
  ggtitle("Gender of Cited Authors by Time Period") +
  labs(fill = "Gender of Cited Author",
       subtitle = "dotted line at 42%") +
  geom_hline(yintercept = .42, linetype = "dotted") +
  coord_flip()










total.citation = sum(citations$Citation.Count)
female.cited = subset(citations, Gender.of.Citation == "Female")
male.cited = subset(citations, Gender.of.Citation == "Male")
female.cited$perc.citation = female.cited$Citation.Count/total.citation
male.cited$perc.citation = male.cited$Citation.Count/total.citation
kruskal.test(female.cited$perc.citation ~ female.cited$time.period)#regarding different time periods
kruskal.test(female.cited$perc.citation ~ female.cited$JS.or.RS)#regarding different journals of women 
kruskal.test(male.cited$perc.citation ~ male.cited$JS.or.RS)#regarding different journals of men

citations$perc.citation = citations$Citation.Count/total.citation
kruskal.test(citations$perc.citation ~ citations$JS.or.RS)

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

ggplot(citations, aes(x=JS.or.RS, fill = forcats::fct_rev(Gender.of.Citation))) +
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


ggplot(citations, aes(x=Citation.Count, fill=JS.or.RS)) +
  geom_histogram(position = "dodge2") +
  ggtitle("Citation count by different journals")
  