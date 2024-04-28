tempStr <- "8 Albania
12 Algeria
16 AmericanSamoa
20 Andorra
24 Angola
28 AntiguaBarbuda 
32 Argentina
51 Armenia
36 Australia
40 Austria
31 Azerbaijan
50 Bangladesh
52 Barbados
56 Belgium
60 Bermuda
64 Bhutan
68 Bolivia
70 Bosnia
72 Botswana
76 Brazil
84 Belize 
100 Bulgaria
854 Burkina Faso 
108 Burundi
112 Belarus
116 Cambodia 
120 Cameroon 
124 Canada
148 Chad
152 Chile
156 China
170 Colombia 
384 Côted́Ivoire 
184 CookIslands 
188 CostaRica 
191 Croatia
192 Cuba 
196 Cyprus
203 CzechRepublic
180 DemRepofCongo 
208 Denmark
214 DominicanRepublic 
818 Egypt
218 Ecuador
222 ElSalvador 
226 EquatorialGuinea 
231 Ethiopia
232 Eritrea
233 Estonia 
246 Finland
250 France
268 Georgia
270 Gambia
624 GuineaBissau 
276 Germany 
288 Ghana
292 Gibraltar 
300 Greece
320 Guatemala 
324 Guinea
328 Guyana
826 GreatBritain 
332 Haiti
340 Honduras 
344 HongKong 
348 Hungary 
352 Iceland
356 India
360 Indonesia 
364 Iran
368 Iraq
372 Ireland
376 Israel
380 Italy
400 Jordan
388 Jamaica
392 Japan
398 Kazakhstan 
404 Kenya
414 Kuwait
417 Kyrgyzstan 
418 Laos
422 Lebanon
426 Lesotho
428 Latvia
430 Liberia
434 Libya
450 Madagascar 
454 Malawi
458 Malaysia 
466 Mali
470 Malta
474 Martinique
478 Mauritania
480 Mauritius
484 Mexico
492 Monaco
496 Mongolia
498 Moldova
504 Morocco
508 Mozambique
104 Myanmar
912 Montenegro
807 Macedonia
516 Namibia
524 Nepal
528 Netherlands
554 NewZealand
558 Nicaragua
562 Niger
566 Nigeria
578 Norway
408 NorthKorea
512 Oman
586 Pakistan
591 Panama
598 PapuaNewGuinea
600 Paraguay
604 Peru
608 Philippines
616 Poland
620 Portugal
275 Palestine
630 PuertoRico
634 Qatar
642 Romania
643 Russia
646 Rwanda
682 SaudiArabia
144 SriLanka
686 Senegal
690 Seychelles
694 SierraLeone
702 Singapore
703 Slovakia 
705 Slovenia
706 Somalia 
710 SouthAfrica 
724 Spain 
736 Sudan
740 Suriname
752 Sweden
756 Switzerland
760 Syria
410 SouthKorea
891 Montenegro 
911 Serbia
626 Timor-Leste
762 Tajikistan
764 Thailand
768 Togo
780 Trinidad
788 Tunisia
158 Taiwan
792 Turkey
795 Turkmenistan
834 Tanzania
784 UnitedArabEmirates 
800 Uganda
804 Ukraine
840 UnitedStates
850 USVirginIslands 
858 Uruguay
860 Uzbekistan
704 Vietnam
862 Venezuela
887 Yemen
894 Zambia
716 Zimbabwe"

setwd("/Users/adi/Downloads")
worldData <- read.csv("WVS_Cross-National_Wave_7_csv_v5_0.csv")
netflixData <- read.csv("netflix_titles.csv")
test <- as.data.frame(table(worldData$B_COUNTRY))

worldDataCOntrol <- read.csv("WVS_Cross-National_Wave_7_csv_v5_0.csv")

library(ggplot2)

# plot number of responses from each country surveyed
ggplot(test) + geom_col(aes(x = Var1, y = Freq))

# clean ghost commas from country produced
netflixData$country <- strsplit(netflixData$country, ",")
for (show in 1:length(netflixData$title)) {
  for (country in 1:length(netflixData$country[[show]])) {
    netflixData$country[[show]][country] <-
      trimws(netflixData$country[[show]][country])
  }
}

# Create factor for all country names
tempStr <- unlist(strsplit(tempStr, c("\n", " ")))
countryFactor <- NULL
countryNames <- NULL
for(j in tempStr) {
  countryFactor <- append(countryFactor,  unlist(strsplit(j, " "))[1])
  countryNames <- append(countryNames, unlist(strsplit(j, " "))[2])
}

worldData$B_COUNTRY <- factor(worldData$B_COUNTRY, 
                              levels = countryFactor, 
                              labels = countryNames)

# Cleaning Netflix data
netflixData$country <- (lapply(netflixData$country, gsub, pattern = " ", replacement = ""))
netflixData$country <- (lapply(netflixData$country, gsub, 
                               pattern = "EastGermany", replacement = "Germany"))
netflixData$country <- (lapply(netflixData$country, gsub, 
                               pattern = "WestGermany", replacement = "Germany"))

# Counting instances of each country for movies released after 2017
recentNetflixData <- netflixData[which(netflixData$release_year >= 2017), ]
rCounts <- rep(0, length(levels(worldData$B_COUNTRY)))
names(rCounts) <- (levels(worldData$B_COUNTRY))

for(i in recentNetflixData$country) {
  for (j in i) {
    rCounts[which(names(rCounts) == j)] <- rCounts[which(names(rCounts) == j)] + 1 
  }
}

rCounts <- cbind(as.data.frame(rCounts), b = names(rCounts))
ggplot(rCounts[which(rCounts$rCounts >= 50), ]) + geom_col(aes(x = b, y = rCounts))

# Identifying unique genres
netflixData$listed_in <- strsplit(netflixData$listed_in, ",")
genres <- NULL

for (j in 1:length(netflixData$listed_in)) {
  netflixData$listed_in[j] <- lapply(netflixData$listed_in[j], gsub, pattern = " ", replacement = "")
  genres <- append(genres, netflixData$listed_in[j])
}
uniqueGenres <- unique(unlist(genres))

tables <- NULL