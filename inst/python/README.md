 ```diff 
 - NOTE: This is our beta version of handwriter
 - For the most stable version of handwriter please use:
 - install.packages("handwriter")
 - to get handwriter from CRAN: https://CRAN.R-project.org/package=handwriter 
 ```
 
# README

This readme covers the functionality of the Python-based `handwriter` routines.

## Python Word Separation

Here are the step by step instructions for Python Word Separation:

1. Ensure that Python is installed. There are several ways: https://www.python.org/downloads/
2. Clone the `handwriter` repository to your local machine
3. Install the following python packages: `pip install opencv-python matplotlib ipython jupyter`
4. Open up a terminal
5. Change your working directory to the `inst/python` directory of the `handwriter` repository. For example, on MacOS, you would run `cd /path/to/cloned/handwriter/inst/python`, replacing the path with the actual path on your local machine.

The next set of steps depend on whether you (a) prefer to run the code natively in Python, (b) prefer to run the code from a Jupyter Notebook, or (c) prefer to run the code from R.

### Option A: Run Natively

6. Launch the Python interpreter by calling: 

```bash
python
```

7. Import the module: 

```python
import word_separation as ws
```

8. Split the lines: 

```python
input_image="images/w0001_s03_pPHR_r01.png"

# Split the lines
split_images = ws.detect_lines(input_image)
split_images
```

9. Separate the word:

```python
# Display each split image
for split_image in split_images:
    ws.show_image(ws.separate_word(file_name=split_image))
```

10. Display the contours!

```python
# Get every word extracted as a contour
all_words = []
for split_image in split_images:
    im1_contours = ws.separate_word(file_name=split_image, ret="contours")
    all_words.append(ws.annotate_image(split_image, im1_contours))
```

11. Display the Bounding Boxes!

```python
# Let's take a look!
all_words
```

12. Batch Processing

```python
batched = ws.batch_process("images")
```

### Option B: Run Natively through Jupyter

6. Launch the Jupyter Notebook client by calling:

```bash
jupyter notebook
```

7. Open up `word_separation.ipynb`

8. Execute the cells of the notebook


### Option C: Run in R with `reticulate`

Execute the `word_separation.R` script in the `inst/python` directory:

```r
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
```
