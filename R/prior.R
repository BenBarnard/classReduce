# Calculate the Within and Between Class Scatter Matrices. These are necessary
# for the Loog and Duin, SIR, and SAVE methods.
# First, calculate the a priori probabilities:
prior <- lapply(1:length(N), function(population) {
  N[[population]] / Reduce(`+`,N)
})
