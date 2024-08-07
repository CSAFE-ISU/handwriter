

# Full page image of the handwritten London letter.

## Description

Full page image of the handwritten London letter.

## Usage

<pre><code class='language-R'>message
</code></pre>

## Format

Binary image matrix. 1262 rows and 1162 columns.

## Examples

``` r
library(handwriter)

message_document <- list()
message_document$image <- message
plotImage(message_document)

message_document <- list()
message_document$image <- message
plotImage(message_document)
message_document$thin <- thinImage(message_document$image)
plotImageThinned(message_document)
message_processList <- processHandwriting(message_document$thin, dim(message_document$image))
```
