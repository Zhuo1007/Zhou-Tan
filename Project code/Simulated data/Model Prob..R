var_names <- colnames(X)

model_ids <- apply(ind_samples_GVS, 1, function(x) paste(x, collapse = ","))

# model frequency
model_counts <- table(model_ids)

# model prob.
model_probs <- model_counts / nrow(ind_samples_GVS)

model_probs <- sort(model_probs, decreasing = TRUE)

head(model_probs, 5)

model_labels <- sapply(names(model_probs), function(id) {
  indices <- which(strsplit(id, ",")[[1]] == "1")
  if (length(indices) == 0) {
    "Intercept Only"
  } else {
    paste(var_names[indices], collapse = ", ")
  }
})

result <- data.frame(
  Model = model_labels,
  Probability = as.numeric(model_probs)
)

result
head(result, 5)