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
from word_separation import *
```

8. Split the lines: 

```python
detect_lines(file_name="images/early_bird.png")
```

8. Separate the word:

```python
show_image(separate_word(file_name="images/early_bird.png"))
```

10. Display the contours!

```python
im1_contours = separate_word(file_name="images/early_bird.png", ret="contours")
annotate_image("images/early_bird.png", im1_contours)
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
library(reticulate)

# Make sure you're using the right version of Python
use_python("/Users/erichare/.pyenv/shims/python")

# Source in the word separation code from this directory!
source_python("word_separation.py")

# Call the functions
detect_lines(file_name="images/early_bird.png")
show_image(separate_word(file_name="images/early_bird.png"))
im1_contours <- separate_word(file_name="images/early_bird.png", ret="contours")
ws.annotate_image("inst/python/images/early_bird.png", im1_contours)
```
