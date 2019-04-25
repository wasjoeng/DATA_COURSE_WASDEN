#I.

library(ggplot2)
df = read.table("DNA_Conc_by_Extraction_Date copy.csv")

Katydf = df[c(2:200),c(1,4,6)]
Bendf = df[c(2:200),c(1,5,6)]

Katydf$V4 = as.numeric(as.character(Katydf$V4))
ggplot(Katydf, aes(x=V4)) +
  geom_histogram(color = "white", fill = "darkblue") +
  xlab("DNA Concentration") +
  ylab("Count") +
  labs(title="Katy's DNA Concentrations") +
  theme_bw()

Bendf$V5 = as.numeric(as.character(Bendf$V5))
ggplot(Bendf, aes(x=V5)) +
  geom_histogram(color = "white", fill = "darkred") +
  xlab("DNA Concentration") +
  ylab("Count") +
  labs(title="Ben's DNA Concentrations") +
  theme_bw()

#II. and III.
Your second task is to look at DNA concentrations from the different extraction years. 
One way to do this in a separate figure for each student is demonstrated in those two files:	ZAHN_Plot1.jpeg and ZAHN_Plot2.jpeg 
Open those files in some image viewing program and take a look. I'd like you to re-create these exactly, including the labels.
This is tricky, so I'll give a hint: the plot() function behaves differently depending on the classes of vectors that are given to it.

jpeg('WASDEN_Plot1.jpeg')
boxplot(V4 ~ V1, data = Katydf, main = "Katy's Extractions", xlab = "YEAR", ylab = "DNA Concentration")
dev.off()

jpeg('WASDEN_Plot1_2.jpeg')
ggplot(Katydf, aes(x=V1, y=V4)) +
  geom_boxplot() +
  coord_flip() +
  xlab("DNA Concentration") +
  ylab("Year") +
  labs(title = "Katy's DNA Concentrations by Year")
dev.off()

jpeg('WASDEN_Plot2.jpeg')
boxplot(V5 ~ V1, data = Bendf, main = "Ben's Extractions", xlab = "YEAR", ylab = "DNA Concentration")
dev.off()

jpeg('WASDEN_Plot2_1.jpeg')
ggplot(Bendf, aes(x=V1, y=V5)) +
  geom_boxplot() +
  coord_flip() +
  xlab("DNA Concentration") +
  ylab("Year") +
  labs(title = "Ben's DNA Concentrations by Year")
dev.off


#IV. and V.
unique(Bendf$V1)
?aggregate  

BendfAverages <- aggregate(Bendf[,2], list(Bendf$V1), mean)
KatydfAverages <- aggregate(Katydf[,2], list(Katydf$V1), mean)

names(BendfAverages) <- c("Years", "DNAExtractionAverage")
names(KatydfAverages) <- c("Years", "DNAExtractionAverage")

Dif = BendfAverages$DNAExtractionAverage-KatydfAverages$DNAExtractionAverage
min(Dif)
Dif == min(Dif)
BendfAverages[Dif == min(Dif),]
# Row 1, or 2000 has the lowest average for Ben's DNA extractions.

write.csv(BendfAverages, file = "Ben_Average_Conc.csv")

Push the following to your github web page in your new Exam_1 directory:
1. Boxplot of DNA concentration values by year for Katy
2. Boxplot of DNA concentration values by year for Ben
3. Your .csv file of a data frame with YEAR as column 1 and Ben_Average_Conc as column 2 (named "Ben_average_by_year.csv")
4. Your complete R script file, saved as LASTNAME_Skills_Test_1.R

10 points will be deducted from your grade for each missing item from the first 3 items.
75 points will be deducted from your grade if your R script isn't in there.

