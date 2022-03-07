library(dplyr)
library(ggplot2)

# set this directory to wherever the csv file is located
df = read.csv("C:/Users/sauna/Documents/Documents/Dartmouth/Clubs/DALI - Data Vis/data_challenge_22w/Part 1/wiid.csv")

# graph 1, inequality of Caucuses region 
AM.data = df %>% filter(c2 == "AM")
GE.data = df %>% filter(c2 == "GE")
AZ.data = df %>% filter(c2 == "AZ")
caucuses.data = rbind(AM.data, GE.data, AZ.data)

caucuses.colors <- c(
  "AM"="green", 
  "GE"="black", 
  "AZ"="red"
)

plot(caucuses.data$year, (100 - caucuses.data$gini_reported) / 100,
     col = caucuses.colors[caucuses.data$c2],
     ylab="Income Equality (1 - Gini)", xlab="Year")
legend(1972, 0.6, legend=c("Armenia", "Georgia", "Azerbaijan"),
       col=c("green", "black", "red"), lty=c(1, 1, 1), cex=0.8)
abline(v = 1992)
text(1982, 0.82, "Dissolution of USSR 1992")

everysecond <- function(x){
  x <- sort(unique(x))
  x[seq(20, length(x), 20)] <- ""
  x
}

# graph 2
gini.by.cont = aggregate(gini_reported~year+country+region_un, df, mean)
gini.by.cont = aggregate(gini_reported~year+region_un, gini.by.cont, mean)
ggplot(data = gini.by.cont, aes(x = factor(year), y = gini_reported, color = region_un)) +       
  geom_line(aes(group = region_un)) + 
  geom_point() + 
  scale_x_discrete(guide = guide_axis(check.overlap = TRUE)) +
  labs(title="Average Gini by Continent Over Time", color = "Continent") +
  xlab("Year") +
  ylab("Gini")

# graph 3
earnings.by.cont = df %>% filter(resource == "Earnings") %>% filter(!is.na(gdp_ppp_pc_usd2011))
earnings.by.cont = aggregate(gdp_ppp_pc_usd2011~year+country+region_un, earnings.by.cont, mean)
earnings.by.cont = aggregate(gdp_ppp_pc_usd2011~year+region_un, earnings.by.cont, mean)
ggplot(data = earnings.by.cont, aes(x = factor(year), y = gdp_ppp_pc_usd2011, color = region_un)) +       
  geom_line(aes(group = region_un)) + 
  geom_point() + 
  scale_x_discrete(guide = guide_axis(check.overlap = TRUE)) +
  labs(title="Average Earnings by Continent Over Time", color = "Continent") +
  xlab("Year") +
  ylab("Earnings (USD PPP 2011)")

# graph 4
pop.by.cont = df %>% filter(!is.na(population))
pop.by.cont = aggregate(population~year+country+region_un, pop.by.cont, mean)
pop.by.cont.1900s = pop.by.cont %>% filter(year >= 1900) %>% filter(year < 2000) 
pop.by.cont.1900s = aggregate(population~country+region_un, pop.by.cont.1900s, mean)
pop.by.cont.1900s = aggregate(population~region_un, pop.by.cont.1900s, sum)
pop.by.cont.1900s$century = "1900"
pop.by.cont.2000s = pop.by.cont %>% filter(year >= 2000)
pop.by.cont.2000s = aggregate(population~country+region_un, pop.by.cont.2000s, mean)
pop.by.cont.2000s = aggregate(population~region_un, pop.by.cont.2000s, sum)
pop.by.cont.2000s$century = "2000"
pop.by.cont.both = bind_rows(pop.by.cont.1900s, pop.by.cont.2000s)
pop.by.cont.both = pop.by.cont.both %>% group_by(century) %>% mutate(percentage=paste0(round(population/sum(population)*100,2),"%"))
ggplot(pop.by.cont.both, aes(region_un, population, fill=century)) + 
  geom_bar(stat = "identity", position = 'dodge') +
  geom_text(aes(label=percentage), position=position_dodge(width=0.9), vjust=-0.25,  size=2) +
  labs(title="Continent Average Population 1900s vs 2000s") +
  xlab("Continent (percentage of world pop. at time)") +
  ylab("Population")
