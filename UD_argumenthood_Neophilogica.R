# This is the script used for the argumenthood annotation of UD data (applied to data extracted from Russian treebanks).

# Part I. Data analysis

# 1. Install packages

install.packages("readxl")
library(readxl)

install.packages("writexl")
library(writexl)

# 2. Read and preprocess the files with raw data

# Read the file with the original data for the current language (CL) extracted from UD 
# and semi-manually annotated for encoding devices
# "Russian_data_with_encoding.xlsx" in this case

CL_data <- read_excel(file.choose())

# Make a backup copy in order to be able to restore the original data

backup_CL_data <- CL_data

# Convert tibble into a dataframe
CL_data <- as.data.frame(CL_data)

# Create an additional column with clause ids
CL_data$clause_id <- paste(CL_data$id, CL_data$`verb position in the sentence`, sep = "_")

# Weed out examples containing NAs
# Filter out clause ids where at least one the object rows contains "<NA>" as the label for the encoding device
# bad_clause_ids <- unique(CL_data$clause_id[CL_data$`encoding device` == "<NA>"])
# CL_data <- CL_data[!CL_data$clause_id %in% bad_clause_ids, ]

# Filter out all row where the dependent is tagged as "EXPL", "SUBJ", "OBJLVC", "<NA>"
CL_data <- subset(CL_data, !(`encoding device` %in% c('EXPL', 'SUBJ', 'OBJLVC', '<NA>')))

# 3. Create a dataframe with clauses and encoding devices

# Create a table with clause_ids, infinitives and valency encoding devices encountered in each clause
clause_ids <- unique(CL_data$clause_id)
no_clauses <- length(clause_ids)
encoding_devices <- unique(CL_data$`encoding device`)
no_encoding_devices <- length(encoding_devices)

valency_frames <- unique(CL_data[, c("clause_id", "verb lemma")])
for (ed in 1:no_encoding_devices) {
  current_ed <- encoding_devices[ed]
  valency_frames[[current_ed]] <- 0
  valency_frames[[current_ed]] <- ifelse(valency_frames$clause_id %in% CL_data$clause_id[CL_data$`encoding device` == current_ed], 1, 0)
}
head(valency_frames)

write.table(valency_frames, file = 'valency_frames.txt', row.names = T, col.names = T, sep = '\t')
write_xlsx(valency_frames, path = "valency_frames.xlsx")

# 4. Create table with frequencies of verb lemmas 
# and their cooccurrence with encodin devices

# Create a table with frequencies of verb lemmas

verb_lemmas <- unique(valency_frames$`verb lemma`)
no_verb_lemmas <- length(verb_lemmas)

verb_frequencies <- as.data.frame(table(valency_frames$`verb lemma`))
names(verb_frequencies) <- c('verb lemma', 'frequency')
verb_frequencies <- verb_frequencies[order(match(verb_frequencies$"verb lemma", verb_lemmas)), ]
rownames(verb_frequencies) <- verb_frequencies$`verb lemma`
verb_frequencies$`verb lemma` <- NULL
write.table(verb_frequencies, file = 'verb_frequencies.txt', row.names = T, col.names = T, sep = '\t')

# Create a table with verb lemmas and frequencies of their co-occurrence with each encoding device
verbs_and_encoding_devices <- matrix(0, nrow = no_verb_lemmas, ncol = no_encoding_devices)
colnames(verbs_and_encoding_devices) <- encoding_devices
rownames(verbs_and_encoding_devices) <- verb_lemmas

for (v in 1:no_verb_lemmas) {
  current_verb <- verb_lemmas[v]
  temp_valency_frames <- valency_frames[valency_frames$`verb lemma` == current_verb, ]
  verbs_and_encoding_devices[v, ] <- colSums(temp_valency_frames[encoding_devices])
}

verbs_and_encoding_devices <- as.data.frame(verbs_and_encoding_devices)

write.table(verbs_and_encoding_devices, file = 'verbs_and_encoding_devices.txt', row.names = T, col.names = T, sep = '\t')
write_xlsx(verbs_and_encoding_devices, path = "verbs_and_encoding_devices.xlsx")

# 5. Annotation of argumenthood in the raw data 

# Create a table where each verb + encoding device combinations is annotated as 
# "arg" or "nonarg" based on whether this combination occurs in the dataset with a frequency 
# that is statistically significantly higher than for the other verbs in the dataset

## calculate frequencies of occurrence of each encoding device

encoding_devices_frequencies <- colSums(verbs_and_encoding_devices)

## create an empty matrix for writing the argumenthood status
argumenthood_df <- matrix(0, nrow = no_verb_lemmas, ncol = no_encoding_devices)
rownames(argumenthood_df) <- rownames(verbs_and_encoding_devices)
colnames(argumenthood_df) <- colnames(verbs_and_encoding_devices)

## fill this matrix for each verb and device combination
for (i in 1:no_verb_lemmas) {
  for (j in 1:no_encoding_devices) {
    observed <- verbs_and_encoding_devices[i, j]
    expected <- (verb_frequencies$frequency[i] * encoding_devices_frequencies[j])/no_clauses
    if (expected >= 5) {
      chi_square <- (observed - expected)^2 / expected
      p_value <- pchisq(chi_square, df = 1, lower.tail = FALSE)
      if (p_value < 0.05 && observed > expected) {
        argumenthood_df[i, j] <- 1
      }
    } else {
      verb_total <- verb_frequencies$frequency[i] - observed
      pattern_total <- encoding_devices_frequencies[j] - observed
      others <- no_clauses - (observed + verb_total + pattern_total)
      contingency_table <- matrix(c(observed, pattern_total, verb_total, others), 
                                  nrow = 2, byrow = TRUE)
      fisher_result <- fisher.test(contingency_table, alternative = "greater")
      
      if (fisher_result$p.value < 0.05 && observed > expected && observed > 2) {
        argumenthood_df[i, j] <- 1
      }
    }
  }  
}

write.table(argumenthood_df, file = 'argumenthood_df.txt', row.names = T, col.names = T, sep = '\t')
write_xlsx(argumenthood_df, path = "argumenthood_df.xlsx")

# Add to the original data "argumenthood" binary annotation operationalized in terms of frequencies
# For dependents with encoding devices such as "ACC" or "DO", apply the argumenthood annotation based on the syntactic tags in UD treebanks 

no_dependents <- nrow(CL_data)
for (i in 1:no_dependents) {
  current_verb <- CL_data$`verb lemma`[i]
  current_encoding_device <- CL_data$`encoding device`[i]
  current_dr <- CL_data$`object dependency relation`[i]
  if (current_encoding_device %in% c("ACC", "DO") &&  current_dr == "obj") {
    CL_data$argumenthood[i] <- 1
  } else if (current_encoding_device %in% c("ACC", "DO") &&  current_dr != "obj") {
    CL_data$argumenthood[i] <- 0
  } else  {
    CL_data$argumenthood[i] <- argumenthood_df[current_verb, current_encoding_device]
  }
}

write_xlsx(CL_data, path = "data.xlsx")