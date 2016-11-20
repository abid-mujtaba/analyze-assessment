# An R script for reading in score data and performing analysis on it

scores = read.csv('scores.csv')

# Plot the data in a histogram
scores$q3 = scores$q3_descriptive + scores$q3_binary + scores$q3_computational       # Calculate the total for Q.3
# hist(scores$q3, breaks=16, main="Histogram of the Total Score", xlab="Score (out of 8)")


# Create custom functions to evaluate whether a score means a student has understanding of a question
# First off we use q3_descriptive (max score of 2) to guage in a binary (boolean) fashion whether a student has conceptual knowledge about Series and Parallel combinations of resistors
# This score was given on the basis of the written justification provided.
# A score of >= 1.5 out of 2 is judged as having coneptual understanding
func = function(x) { return (x >= 1.5) }

# Similary for knowledge about the limits of R_AB. This can come from concept or from brute-force. It is an oblique way of testing coneptual knowledge.
# Same threshold of 1.5 so we simply copy the function
funl = func

# Test for knowledge on the applied technical side (algorithmic)
funa = function(x) { return (x >= 3) }


# We use this function to create new columns in the 'scores' data frame representing the truth of falsehood of three claims:
# 1. Student successfully answered the binary question = B
# 2. Ditto for descriptive question (conceptual) = D
# 3. Ditto for computational question = C

# We use sapply to apply the function to every element of a vector (1st arg) to create a new vector which we save in the data frame
scores$D = sapply(scores$q3_descriptive, func)
scores$B = sapply(scores$q3_binary, funl)
scores$C = sapply(scores$q3_computational, funa)


# print your results
# print(scores[,c("q3_descriptive", "D", "q3_binary", "B", "q3_computational", "C")])
print(scores[,c("D", "B", "C")])

cat("\n\n")

# Calculate probabilities from relative frequency
library(plyr)           # Load the 'plyr' library to use the 'count' and 'aggregate' functions

tot = length(scores$D)          # Total number of results, will be needed to calculate the probabilities from relative frequencies

# We calculate the frequency of entries based on 'unique' entries in the C, L, and A columns
# This gives a table giving all of the frequencies we need for a correlation analysis
c = count(scores, c("D", "B", "C"))

cat("\nFrequencies of D-B-C outcomes:\n\n")
print(c)

# We use the 'aggregate' function from 'plyr' to collect data
# For cC we aggreate over the 'freq' column based on unique values in just the 'C' column in the data 'c' and we use the 'sum' feature that is we will be adding the frequencies over unique values in C
# This gives us a simple data frame relating TRUE and FALSE in C to their frequencies
cD = aggregate(freq ~ D, c, sum)
cB = aggregate(freq ~ B, c, sum)
cC = aggregate(freq ~ C, c, sum)

# Now calculate the probabilities from relative frequency
pD = cD[cD$D, "freq"] / tot           # Access the "freq" value of the row whose cD$D value is TRUE (Note how we are using the fact that cD$D contains boolean values)
pB = cB[cB$B, "freq"] / tot
pC = cC[cC$C, "freq"] / tot

# Calculate intersection probabilities
cDnC = aggregate(freq ~ D + C, c, sum)
cDnB = aggregate(freq ~ D + B, c, sum)
cBnC = aggregate(freq ~ B + C, c, sum)
cBnDnC = aggregate(freq ~ B + C + D, c, sum)

pDnC = cDnC[cDnC$D & cDnC$C, "freq"] / tot
pDnB = cDnB[cDnB$D & cDnB$B, "freq"] / tot
pBnC = cBnC[cBnC$B & cBnC$C, "freq"] / tot
pBnDnC = cBnDnC[cBnDnC$B & cBnDnC$D & cBnDnC$C, "freq"] / tot

# Calculate the conditional probabilities
pCgD = pDnC / pD
pBgD = pDnB / pD

pDgC = pDnC / pC
pDgB = pDnB / pB

pBgC = pBnC / pC
pCgB = pBnC / pB

cat("\n\nProbabilities:\n\n")

cat(paste("P(D) = ", pD, "\n"))
cat(paste("P(B) = ", pB, "\n"))
cat(paste("P(C) = ", pC, "\n"))

cat(paste("\nP(D + C) = ", pDnC, "\n"))
cat(paste("P(D + B) = ", pDnB, "\n"))
cat(paste("P(B + C) = ", pBnC, "\n"))
cat(paste("P(B + D + C) = ", pBnDnC, "\n"))

cat(paste("\nP(C|D) = ", pCgD, "\n"))
cat(paste("P(B|D) = ", pBgD, "\n"))

cat(paste("\nP(D|C) = ", pDgC, "\n"))
cat(paste("P(D|B) = ", pDgB, "\n"))

cat(paste("\nP(B|C) = ", pBgC, "\n"))
cat(paste("P(C|B) = ", pCgB, "\n"))

cat("\n\n")
