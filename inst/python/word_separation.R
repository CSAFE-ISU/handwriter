library(reticulate)

# Make sure you're using the right version of Python
use_python("/Users/erichare/.pyenv/shims/python")

# Source in the word separation code from this directory!
source_python("word_separation.py")

# Call the functions
detect_lines(file_name="images/early_bird.png")
show_image(separate_word(file_name="images/early_bird.png"))
im1_contours <- separate_word(file_name="images/early_bird.png", ret="contours")
annotate_image("images/early_bird.png", im1_contours)