library(ggplot2)
library(dplyr)
library(ggmosaic)
library(tidyr)
library(extrafont)


citations = read.csv("citation_data.csv", colClasses = c("character", "character", 
                                                         "integer", rep("factor",7)))

levels(citations$Gender.of.Article.Author) = list("Female" = 0, "Male" = 1)
levels(citations$Gender.of.Citation) = list("Female" = 0, "Male" = 1)
levels(citations$field) = list("History" = 1, "Philosophy/ \n MJT" = 2, 
                               "Literature" = 3, "Ancient Text" = 4, 
                               "Sociology/ \n Ethnography/ \n Contemporary" = 5)
levels(citations$time.period) = list("Ancient" = 1, "Medieval" = 2, "Modern" =3)
levels(citations$JS.or.RS) = list("Jewish Studies" = "JS", "Religious Studies" = "RS")

#1 b/w
ggplot(citations, aes(x=Gender.of.Citation)) +
  geom_bar(aes(y = (..count..)/sum(..count..), fill=Gender.of.Article.Author)) +
  scale_y_continuous(labels = scales::percent) +
  ylab("Percent") +
  xlab("Gender of Cited Author") +
  ggtitle("Gender of Cited Authors") +
  geom_hline(yintercept = .42, linetype = "dotted") +
  labs(fill = "Gender of Article Author",
       subtitle = "dotted line at 42%") +
  scale_fill_grey(start = .5, end = 0)

#1 color
ggplot(citations, aes(x=Gender.of.Citation)) +
  geom_bar(aes(y = (..count..)/sum(..count..), fill=Gender.of.Article.Author)) +
  scale_y_continuous(labels = scales::percent) +
  ylab("Percent") +
  xlab("Gender of Cited Author") +
  ggtitle("Gender of Cited Authors") +
  geom_hline(yintercept = .42, linetype = "dotted") +
  labs(fill = "Gender of Article Author",
       subtitle = "dotted line at 42%") +
  scale_fill_manual(values=c("indianred1","blue")) 


#2 b/w
ggplot(citations, aes(x=Gender.of.Article.Author, fill = forcats::fct_rev(Gender.of.Citation))) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  ylab("Percent") +
  xlab("Article Author") +
  ggtitle("Gender of Cited Authors by Gender of Article Author") +
  labs(fill = "Cited Author",
       subtitle = "dotted line at 42%") +
  geom_hline(yintercept = .42, linetype = "dotted") +
  scale_fill_grey(start = .5, end = 0)

#2 color
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

#3 b/w
ggplot(citations) +
  geom_mosaic(aes(x=product(field), fill = Gender.of.Citation)) +
  scale_y_continuous(labels = scales::percent) +
  ylab("Percent") +
  xlab("Field") +
  ggtitle("Gender of Cited Authors by Field") +
  labs(fill = "Gender of Cited Author",
       subtitle = "dotted line at 42%") +
  geom_hline(yintercept = .42, linetype = "dotted") +
  scale_fill_grey(start = 0, end = .5) +
  coord_flip()

#3 color
ggplot(citations) +
  geom_mosaic(aes(x=product(field), fill = Gender.of.Citation)) +
  scale_y_continuous(labels = scales::percent) +
  ylab("Percent") +
  xlab("Field") +
  ggtitle("Gender of Cited Authors by Field") +
  labs(fill = "Gender of Cited Author",
       subtitle = "dotted line at 42%") +
  geom_hline(yintercept = .42, linetype = "dotted") +
  scale_fill_manual(values=c("indianred1", "royalblue2")) +
  coord_flip()

#4 b/w
ggplot(na.omit(citations)) +
  geom_mosaic(aes(x=product(time.period), fill = Gender.of.Citation)) +
  scale_y_continuous(labels = scales::percent) +
  ylab("Percent") +
  xlab("Time Period") +
  ggtitle("Gender of Cited Authors by Time Period") +
  labs(fill = "Gender of Cited Author",
       subtitle = "dotted line at 42%") +
  geom_hline(yintercept = .42, linetype = "dotted") +
  scale_fill_grey(start = 0, end = .5) +
  coord_flip()
  
#4 color
ggplot(na.omit(citations)) +
  geom_mosaic(aes(x=product(time.period), fill = Gender.of.Citation)) +
  scale_y_continuous(labels = scales::percent) +
  ylab("Percent") +
  xlab("Time Period") +
  ggtitle("Gender of Cited Authors by Time Period") +
  labs(fill = "Gender of Cited Author",
       subtitle = "dotted line at 42%") +
  geom_hline(yintercept = .42, linetype = "dotted") +
  scale_fill_manual(values=c("indianred1", "royalblue2")) +
  coord_flip()

#5a b/w
ggplot(citations, aes(x=JS.or.RS, fill = forcats::fct_rev(Gender.of.Citation))) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  ylab("Percent") +
  xlab("Journal") +
  ggtitle("Gender of Cited Authors by Journal") +
  labs(fill = "Cited Author",
       subtitle = "dotted line at 42%") +
  geom_hline(yintercept = .42, linetype = "dotted") +
  scale_fill_grey(start = .5, end = 0)

#5a color
ggplot(citations, aes(x=JS.or.RS, fill = forcats::fct_rev(Gender.of.Citation))) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  ylab("Percent") +
  xlab("Journal") +
  ggtitle("Gender of Cited Authors by Journal") +
  labs(fill = "Cited Author",
       subtitle = "dotted line at 42%") +
  geom_hline(yintercept = .42, linetype = "dotted") +
  scale_fill_manual(values=c("royalblue2", "indianred1"))

#5b b/w
ggplot(citations) +
  geom_mosaic(aes(x=product(Gender.of.Citation, JS.or.RS, Gender.of.Article.Author), 
                  fill = Gender.of.Citation),
              divider = ddecker()) +
  scale_y_continuous(labels = scales::percent) +
  ylab("Percent") +
  xlab("Journal and Gender of Author") +
  ggtitle("Gender of Cited Authors by Journal and Gender of Author") +
  labs(fill = "Gender of Cited Author",
       subtitle = "dotted line at 42%") +
  geom_hline(yintercept = .42, linetype = "dotted") +
  scale_fill_grey(start = 0, end = .5) +
  coord_flip()

#5b color
ggplot(citations) +
  geom_mosaic(aes(x=product(Gender.of.Citation, JS.or.RS, Gender.of.Article.Author), 
                  fill = Gender.of.Citation),
                  divider = ddecker()) +
  scale_y_continuous(labels = scales::percent) +
  ylab("Percent") +
  xlab("Journal and Gender of Author") +
  ggtitle("Gender of Cited Authors by Journal and Gender of Author") +
  labs(fill = "Gender of Cited Author",
       subtitle = "dotted line at 42%") +
  geom_hline(yintercept = .42, linetype = "dotted") +
  scale_fill_manual(values=c("indianred1", "royalblue2")) +
  coord_flip()




#1 b/w pie
pie = ggplot(citations, aes(x=1, fill = forcats::fct_rev(Gender.of.Citation))) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  ylab("") +
  xlab("") +
  ggtitle("Gender of Cited Authors") +
  labs(fill = "Cited Author",
       subtitle = "dotted line at 42%") +
  geom_hline(yintercept = .42, linetype = "dotted") +
  theme(axis.text = element_blank(), panel.background = element_rect(fill = "white"),
        axis.ticks = element_line(color = "white")) +
  scale_fill_grey(start = .5, end = 0)
pie + coord_polar(theta = "y")

#1 color pie
pie = ggplot(citations, aes(x=1, fill = forcats::fct_rev(Gender.of.Citation))) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  ylab("") +
  xlab("") +
  ggtitle("Gender of Cited Authors") +
  labs(fill = "Cited Author",
       subtitle = "dotted line at 42%") +
  geom_hline(yintercept = .42, linetype = "dotted") +
  theme(axis.text = element_blank(), panel.background = element_rect(fill = "white"),
        axis.ticks = element_line(color = "white")) +
  scale_fill_manual(values=c("royalblue2", "indianred1"))
pie + coord_polar(theta = "y")


citations$Gender.of.Article.Author = factor(citations$Gender.of.Article.Author)
citations$JS.or.RS = factor(citations$JS.or.RS)
citations$field = factor(citations$field)
citations$time.period = factor(citations$time.period)

citations$Gender.of.Citation = citations$Gender.of.Citation - 1
citations$Gender.of.Citation = factor(citations$Gender.of.Citation)
test = glm(Gender.of.Citation ~ Gender.of.Article.Author + JS.or.RS + field + time.period, 
          data = na.omit(citations), family=binomial(link=logit))
summary(test)
