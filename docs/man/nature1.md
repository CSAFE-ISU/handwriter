

# Full page image of the 4th sample (nature) of handwriting from the first writer.

## Description

Full page image of the 4th sample (nature) of handwriting from the first
writer.

## Usage

<pre><code class='language-R'>nature1
</code></pre>

## Format

Binary image matrix. 811 rows and 1590 columns.

## Examples

``` r
library(handwriter)

nature1_document <- list()
nature1_document$image <- nature1
plotImage(nature1_document)

nature1_document <- list()
nature1_document$image <- nature1
plotImage(nature1_document)
nature1_document$thin <- thinImage(nature1_document$image)
plotImageThinned(nature1_document)
nature1_processList <- processHandwriting(nature1_document$thin, dim(nature1_document$image))
```
