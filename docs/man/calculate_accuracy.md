

# Calculate Accuracy

[**Source code**](https://github.com/CSAFE-ISU/handwriter/tree/176-automatic-documentation/R/#L)

## Description

Fit a model with <code>fit_model()</code> and calculate posterior
probabilities of writership with
<code>analyze_questioned_documents()</code> of a set of test documents
where the ground truth is known. Then use
<code>calculate_accuracy()</code> to measure the accuracy of the fitted
model on the test documents. Accuracy is calculated as the average
posterior probability assigned to the true writer.

## Usage

<pre><code class='language-R'>calculate_accuracy(analysis)
</code></pre>

## Arguments

<table>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="analysis">analysis</code>
</td>
<td>
Writership analysis output by <code>analyze_questioned_documents</code>
</td>
</tr>
</table>

## Value

The model’s accuracy on the test set as a number

## Examples

``` r
library(handwriter)

# calculate the accuracy for example analysis performed on test documents and a model with 1 chain
calculate_accuracy(example_analysis)

main_dir <- "/path/to/main_dir"
test_images_dir <- "/path/to/test_images"
analysis <- analyze_questioned_documents(
  main_dir = main_dir,
  questioned_docs = test_images_dir,
  model = model,
  num_cores = 2,
  writer_indices = c(2, 5),
  doc_indices = c(7, 18)
)
calculate_accuracy(analysis)
```
