

# Fit Model

[**Source code**](https://github.com/CSAFE-ISU/handwriter/tree/176-automatic-documentation/R/#L)

## Description

<code>fit_model()</code> fits a Bayesian hierarchical model to the model
training data in <code>model_docs</code> and draws samples from the
model as Markov Chain Monte Carlo (MCMC) estimates.

## Usage

<pre><code class='language-R'>fit_model(
  main_dir,
  model_docs,
  num_iters,
  num_chains = 1,
  num_cores,
  writer_indices,
  doc_indices,
  a = 2,
  b = 0.25,
  c = 2,
  d = 2,
  e = 0.5
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
<code id="model_docs">model_docs</code>
</td>
<td>
A directory containing model training documents
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="num_iters">num_iters</code>
</td>
<td>
An integer number of iterations of MCMC.
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="num_chains">num_chains</code>
</td>
<td>
An integer number of chains to use.
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="num_cores">num_cores</code>
</td>
<td>
An integer number of cores to use for parallel processing clustering
assignments. The model fitting is not done in parallel.
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="writer_indices">writer_indices</code>
</td>
<td>
A vector of the start and stop character of the writer ID in the model
training file names. E.g., if the file names are writer0195_doc1,
writer0210_doc1, writer0033_doc1 then writer_indices is ‘c(7,10)’.
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="doc_indices">doc_indices</code>
</td>
<td>
A vector of the start and stop character of the "document name" in the
model training file names. This is used to distinguish between two
documents written by the same writer. E.g., if the file names are
writer0195_doc1, writer0195_doc2, writer0033_doc1, writer0033_doc2 then
doc_indices are ‘c(12,15)’.
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="a">a</code>
</td>
<td>
The shape parameter for the Gamma distribution in the hierarchical model
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="b">b</code>
</td>
<td>
The rate parameter for the Gamma distribution in the hierarchical model
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="c">c</code>
</td>
<td>
The first shape parameter for the Beta distribution in the hierarchical
model
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="d">d</code>
</td>
<td>
The second shape parameter for the Beta distribution in the hierarchical
model
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="e">e</code>
</td>
<td>
The scale parameter for the hyper prior for mu in the hierarchical model
</td>
</tr>
</table>

## Value

A list of training data used to fit the model and the fitted model

## Examples

``` r
library(handwriter)

main_dir <- "/path/to/main_dir"
model_docs <- "path/to/model_training_docs"
questioned_docs <- "path/to/questioned_docs"

model <- fit_model(
  main_dir = main_dir,
  model_docs = model_docs,
  num_iters = 100,
  num_chains = 1,
  num_cores = 2,
  writer_indices = c(2, 5),
  doc_indices = c(7, 18)
)

model <- drop_burnin(model = model, burn_in = 25)

analysis <- analyze_questioned_documents(
  main_dir = main_dir,
  questioned_docs = questioned_docs,
  model = model,
  num_cores = 2
)
analysis$posterior_probabilities
```
