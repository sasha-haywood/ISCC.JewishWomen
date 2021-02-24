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





# I don't think we want any of this pie stuff I did next.
gender = c(rep("Female", 27), rep("Male", 73), rep("Female", 42), rep("Male", 58))
source = c(rep("Citation", 100), rep("Research", 100))
donut1 = data.frame(gender, source)

basic = ggplot(donut1, aes(x=source, fill=forcats::fct_rev(gender))) +
  geom_bar() +
  scale_fill_manual(values=c("royalblue2", "indianred1")) +
  xlab("") +
  ylab("") +
  labs(fill = "Gender of Citation Author") +
  theme(axis.text = element_blank(), panel.background = element_rect(fill = "white"),
        axis.ticks = element_line(color = "white")) +
  
basic + coord_polar(theta = "y")

a + coord_polar(theta = "y")

# Basic plot to start with
ggplot(citations, aes(x=Gender.of.Citation)) +
  geom_bar(aes(y = (..count..)/sum(..count..), fill=Gender.of.Article.Author)) +
  scale_y_continuous(labels = scales::percent) +
  ylab("Percent") +
  xlab("Gender of Cited Author") +
  ggtitle("Gender of Cited Authors") +
  labs(fill = "Gender of Article Author") +
  geom_hline(yintercept = .42, linetype = "dotted") +
  scale_fill_grey(start = .5, end = 0) +
  theme(text=element_text(size=16, 
        family="Comic Sans MS"))

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
  