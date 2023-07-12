# We will need reticulate to call the Python functions
library(reticulate)

# Make sure you're using the right version of Python
use_python("/Users/erichare/.pyenv/shims/python")

# Source in the word separation code from this directory!
source_python("word_separation.py")

# Configure the input image
input_image="images/w0001_s03_pPHR_r01.png"

# Split the lines
split_images = detect_lines(input_image)
split_images

# Display each split image
for (split_image in split_images) {
  show_image(separate_word(file_name=split_image))
}

# Get every word extracted as a contour
all_words <- list()
for (split_image in split_images) {
  im1_contours = separate_word(file_name=split_image, ret="contours")
  all_words[[length(all_words) + 1]] <- annotate_image(split_image, im1_contours)
}

# Let's take a look!
all_words

###
# Batch Processing
###
batch_process("images")
