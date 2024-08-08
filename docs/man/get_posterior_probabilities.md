

# Get Posterior Probabilities

[**Source code**](https://github.com/CSAFE-ISU/handwriter/tree/176-automatic-documentation/R/#L)

## Description

Get the posterior probabilities for questioned document analyzed with
<code>analyze_questioned_documents()</code>.

## Usage

<pre><code class='language-R'>get_posterior_probabilities(analysis, questioned_doc)
</code></pre>

## Arguments

<table>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="analysis">analysis</code>
</td>
<td>
The output of <code>analyze_questioned_documents()</code>. If more than
one questioned document was analyzed with this function, then the data
frame analysis$posterior_probabilities lists the posterior probabilities
for all questioned documents. <code>get_posterior_probabilities()</code>
creates a data frame of the posterior probabilities for a single
questioned document and sorts the known writers from the most likely to
least likely to have written the questioned document.
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="questioned_doc">questioned_doc</code>
</td>
<td>
The filename of the questioned document
</td>
</tr>
</table>

## Value

A data frame of posterior probabilities for the questioned document

## Examples

``` r
library(handwriter)

get_posterior_probabilities(
  analysis = example_analysis,
  questioned_doc = "w0030_s03_pWOZ_r01"
)
```

            known_writer w0030_s03_pWOZ_r01
    2 known_writer_w0030                  1
    1 known_writer_w0009                  0
    3 known_writer_w0238                  0
