# An R script for generating histograms from the scores awarded to the three parts

scores = read.csv('scores.csv')

scores$q3 = scores$q3_descriptive + scores$q3_binary + scores$q3_computational       # Calculate the total for Q.3


# Write a custom function to take a vector of student obtained scores (28) and a vector of levels (attainable scores)
# The function will return a data frame containing the attainable scores versus frequency
ccount = function(data, levels) {

    tcount = table(factor(data, levels=levels))     # Use factor to add levels to the data and then tabulate it. 'factor' is needed otherwise scores with zero frequency will be neglected
    dfcount = as.data.frame(tcount, stringsAsFactors=F)         # Convert table to data frame. Setting stringsAsFactors false means the level values are stored as a character vector making conversion to numeric easier
    colnames(dfcount)[1] = "score"          # Change name of first column (Val1 by default)
    dfcount$score = as.numeric(dfcount$score)     # Convert type of first column to numeric.

    return(dfcount)
}

# Plot the data in a histogram
library(ggplot2)

# Source (get) plot utility functions invert_ticks (variable containing the function) and mirror_ticks() from separate file
source("plot_utilities.R")


# Use the more advanced ggplot functions
# To get spacing between the bars we use geom_bar() instead of geom_histogram()
# Note how the labels and titles are provided

# Use of 'factor' in 'aes' inside 'ggplot' turns q3_binary in to discrete data
plot_binary = ggplot(scores, aes(x=factor(q3_binary), y=..count..)) + geom_bar() + labs(x="Score (out of 2)", y="Number of students") + ggtitle("Binary Question") + theme_bw() + invert_ticks

# Since we want zero frequency data we use 'ccount' and set 'stat' to 'identity' because appearance of values of x don't need to be counted
plot_descriptive = ggplot(ccount(scores$q3_descriptive, (0:8) * 0.25), aes(x=score, y=Freq)) + geom_bar(stat="identity") + labs(x="Score (out of 2)", y="Number of students") + ggtitle("Descriptive Question") + theme_bw() + invert_ticks

plot_computational = ggplot(ccount(scores$q3_computation, (0:16) * 0.25), aes(x=score, y=Freq)) + geom_bar(stat="identity") + labs(x="Score (out of 4)", y="Number of students") + ggtitle("Computational Question") + theme_bw() + invert_ticks

# For the total score we first bin the data in to integer intervals by rounding each score
rtot = round(scores$q3, digits=0)

# Plot an actual histogram of the total score (too many unique inividual totals for a bar plot)
plot_total = ggplot(data.frame(score=rtot), aes(x=factor(score), y=..count..)) + geom_bar() + labs(x="Score (out of 8 - rounded to nearest integer)", y="Number of students") + ggtitle("Total Score") + theme_bw() + invert_ticks


# Invert and mirror ticks on all plots. This converts the objects to grobs
grob_binary = mirror_ticks(plot_binary)
grob_descriptive = mirror_ticks(plot_descriptive)
grob_computational = mirror_ticks(plot_computational)
grob_total = mirror_ticks(plot_total)

# Save the plots as pdfs
# ggsave('bar_binary.pdf', grob_binary)
# ggsave('bar_descriptive.pdf', grob_descriptive)
# ggsave('bar_computational.pdf', grob_computational)
# ggsave('bar_total.pdf', grob_total)

# Note: ggplot2 objects need to be explicitly printed for them to be show up when the script is sourced
# grid.draw(grob_binary)
# grid.draw(grob_descriptive)
# grid.draw(grob_computational)
# grid.draw(grob_total)
