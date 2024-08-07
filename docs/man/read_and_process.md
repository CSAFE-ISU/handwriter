

# Read and Process

[**Source code**](https://github.com/CSAFE-ISU/handwriter/tree/176-automatic-documentation/R/#L)

## Description

<a href="https://lifecycle.r-lib.org/articles/stages.html#superseded"><img src="../help/figures/lifecycle-superseded.svg" alt='[Superseded]' /></a>

Development on <code>read_and_process()</code> is complete. We recommend
using <code>processDocument()</code>. <code>read_and_process(image_name,
“document”)</code> is equivalent to
<code>processDocument(image_name)</code>.

## Usage

<pre><code class='language-R'>read_and_process(image_name, transform_output)
</code></pre>

## Arguments

<table>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="image_name">image_name</code>
</td>
<td>
The file path to an image
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="transform_output">transform_output</code>
</td>
<td>
The type of transformation to perform on the output
</td>
</tr>
</table>

## Value

A list of the processed image components

## Examples

``` r
library(handwriter)

# use handwriting example from handwriter package
image_path <- system.file("extdata", "phrase_example.png", package = "handwriter")
doc <- read_and_process(image_path, "document")
```
