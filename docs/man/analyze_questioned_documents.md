

# Analyze Questioned Documents

[**Source code**](https://github.com/CSAFE-ISU/handwriter/tree/176-automatic-documentation/R/#L)

## Description

<code>analyze_questioned_documents()</code> estimates the posterior
probability of writership for the questioned documents using Markov
Chain Monte Carlo (MCMC) draws from a hierarchical model created with
<code>fit_model()</code>.

## Usage

<pre><code class='language-R'>analyze_questioned_documents(
  main_dir,
  questioned_docs,
  model,
  num_cores,
  writer_indices,
  doc_indices
)
</code></pre>

## Arguments

<table>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="main_dir">main_dir</code>
</td>
<td>
A directory that contains a cluster template created by
<code>make_clustering_template()</code>
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="questioned_docs">questioned_docs</code>
</td>
<td>
A directory containing questioned documents
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="model">model</code>
</td>
<td>
A fitted model created by <code>fit_model()</code>
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="num_cores">num_cores</code>
</td>
<td>
An integer number of cores to use for parallel processing with the
<code>doParallel</code> package.
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="writer_indices">writer_indices</code>
</td>
<td>
A vector of start and stop characters for writer IDs in file names
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="doc_indices">doc_indices</code>
</td>
<td>
A vector of start and stop characters for document names in file names
</td>
</tr>
</table>

## Value

A list of likelihoods, votes, and posterior probabilities of writership
for each questioned document.

## Examples

``` r
library(handwriter)

main_dir <- "/path/to/main_dir"
questioned_docs <- "/path/to/questioned_images"
analysis <- analyze_questioned_documents(
  main_dir = main_dir,
  questioned_docs = questioned_docs,
  model = model,
  num_cores = 2,
  writer_indices = c(2, 5),
  doc_indices = c(7, 18)
)
analysis$posterior_probabilities
```
