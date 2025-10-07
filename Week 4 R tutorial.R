install.packages("tidyverse")
library(tidyverse)

exped_url <- "https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-01-21/exped_tidy.csv"
exped <- read_csv(exped_url)

peak_url <- "https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-01-21/peaks_tidy.csv"
peaks <- read.csv(peak_url)

head(exped)
View(exped)
glimpse(exped)

cat(nrow(exped), "records of expeditions\n")
cat(nrow(peaks), "records of peaks\n")

range(exped$YEAR, na.rm = TRUE)

exped %>% 
  count(YEAR)

expeditions_per_year<- table(exped$YEAR)
View(expeditions_per_year)

plot(names(expeditions_per_year),
     expeditions_per_year,
     type = "l",
     main = "Number of Expeditions by Year",
     xlab = "Year",
     ylab = "Number of Expeditions")

seasons_dist <- table(exped$SEASON_FACTOR)
View(seasons_dist)

barplot(seasons_dist,
        xlab = "Season",
        ylab = "Number of Expeditions",
        main = "Number of Expeditions by Season")



year_season_table <- table(exped$SEASON_FACTOR, exped$YEAR)
par(mar = c(5,4,4,8))
barplot(year_season_table,
        beside = TRUE,
        col = c("blue", "green", "yellow", "orange"),
        main = "Expeditions per Year and Season",
        xlab = "Year",
        ylab = "Number of Expeditions",
        legend.text = TRUE,
        args.legend = list(title = "Season", x = "topleft"))


par(mar=c(5,4,4,8))
barplot(year_season_table,
        beside=TRUE,
        col = c("blue", "green", "yellow", "orange"),
        main = "Expeditions per Year and Season",
        xlab = "Year",
        ylab = "Number of Expeditions",
        las = 2)

legend(x = par("usr")[2]+0.5,
       y = max(year_season_table),
       legend = row.names(year_season_table),
       fill = c("blue", "green", "yellow", "orange"),
       title = "Season",
       xpd = TRUE,
       bty = "n")

exped %>% 
  count(PEAKID)

peak_counts<- table(exped$PEAKID)
peak_counts_df <- as.data.frame(peak_counts)
names(peak_counts_df)<- c("PEAKID", "expedtion_count")

peak_counts_df <- peak_counts_df[order(
  peak_counts_df$expedtion_count), ]

peak_joined<-merge(peak_counts_df, peaks, ny="PEAKID", all.x = TRUE)

peak_counts_df <- peak_counts_df[order(-peak_counts_df$expedition_count), ]





peak_counts <- table(exped$PEAKID)
peak_counts_df <- as.data.frame(peak_counts)
names(peak_counts_df) <- c("PEAKID", "expedition_count")

# Sort by expedition count
peak_counts_df <- peak_counts_df[order(-peak_counts_df$expedition_count), ]

# Merge with peaks dataset to get peak names
peak_joined <- merge(peak_counts_df, peaks, by = "PEAKID", all.x = TRUE)

# Top 20 peaks
top20peaks <- head(peak_joined[order(-peak_joined$expedition_count), ], 20)

barplot(top20peaks$expedition_count,
        names.arg = top20peaks$PKNAME,
        las = 2,
        col = "lightblue",
        main = "Top 20 Most Climbed Peaks",
        ylab = "Number of Expeditions")



exped$SUCCESS <- grepl("Success", exped$TERMREASON_FACTOR, ignore.case = TRUE)




success_years <- sort(unique(exped$YEAR))
success_rate<-numeric(length(success_years))
for(i in 1:length(success_years)) {
  data_year<-exped[exped$YEAR== success_years[i],]
  success_rate[i] <- mean(data_year$SUCCESS, na.rm=TRUE)
}

plot(success_years,
     success_rate,
     type = "l",
     col = "darkgreen",
     main = "Expedition Success Rate Over Years",
     xlab = "Year",
     ylab = "Success Rate")

total_by_peak<-table(exped$PEAKID)

success_by_peak<- table(exped$PEAKID[exped$SUCCESS==TRUE])
total_peak_df<-as.data.frame(total_by_peak)
names(total_peak_df)<-c("PEAKID", "total_expeditions")
success_peak_df<-as.data.frame(success_by_peak)
names(success_peak_df)<-c("PEAKID", "successful_expeditions")

peak_summary<-merge(total_peak_df, success_peak_df, by="PEAKID",
                    all.x=TRUE)

peak_summary$successful_expeditions[is.na(peak_summary$successful_expeditions)] <- 0

peak_summary$success_rate<-peak_summary$successful_expeditions/peak_summary$total_expeditions

peak_summary<-merge(peak_summary,peaks[,c("PEAKID", "PKNAME")], by = "PEAKID", all.x = TRUE)

peak_summary<-peak_summary[order(-peak_summary$success_rate), ]


par(mar = c(10, 5, 4, 2))
barplot(peak_summary$success_rate,
        names.arg = peak_summary$PKNAME,
        las = 2,          # rotate x-axis labels vertically
        cex.names = 0.4,  # make the names smaller
        col = "lightgreen",
        main = "Top 10 Peaks by Expedition Success Rate",
        ylab = "Success Rate")

View(peak_summary)
View(peaks)  


# Count expeditions per country
country_counts <- exped %>%
  count(COUNTRIES) %>%
  arrange(desc(n))

# View top 10 countries
head(country_counts, 10)

avg_members_country<- exped %>% 
  group_by(COUNTRIES) %>% 
  summarise(
    avg_total_members = mean(TOTMEMBERS, na.rm = TRUE),
    expeditions = n()
  ) %>% 
  arrange(desc(avg_total_members))

head(avg_members_country, 10)

top_countries<-head(country_counts, 10)

barplot(top_countries$n,
        names.arg = top_countries$COUNTRIES,
        las = 2,
        cex.names = 0.7,
        col = "skyblue",
        main = "Top 10 Countries Involved in Expeditions",
        ylab = "Number of Expeditions",
        xlab = "Country")
        
top_avg_members <- head(avg_members_country, 10)

barplot(top_avg_members$avg_total_members,
        names.arg = top_avg_members$COUNTRIES,
        las=2,
        cex.names = 0.7,
        col = "lightgreen",
        main = "Average Expedition Size by Country",
        ylab = "Average Number of Members",
        xlab = "Country")






