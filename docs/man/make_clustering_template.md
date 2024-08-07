

# Make Clustering Template

[**Source code**](https://github.com/CSAFE-ISU/handwriter/tree/176-automatic-documentation/R/#L)

## Description

<code>make_clustering_template()</code> applies a K-means clustering
algorithm to the input handwriting samples pre-processed with
<code>process_batch_dir()</code> and saved in the input folder
<code style="white-space: pre;">main_dir \> data \>
template_graphs</code>. The K-means algorithm sorts the graphs in the
input handwriting samples into groups, or <em>clusters</em>, of similar
graphs.

## Usage

<pre><code class='language-R'>make_clustering_template(
  main_dir,
  template_docs,
  writer_indices,
  centers_seed,
  K = 40,
  num_dist_cores = 1,
  max_iters = 25
)
</code></pre>

## Arguments

<table>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="main_dir">main_dir</code>
</td>
<td>
Main directory that will store template files
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="template_docs">template_docs</code>
</td>
<td>
A directory containing template training images
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="writer_indices">writer_indices</code>
</td>
<td>
A vector of the starting and ending location of the writer ID in the
file name.
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="centers_seed">centers_seed</code>
</td>
<td>
Integer seed for the random number generator when selecting starting
cluster centers.
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="K">K</code>
</td>
<td>
Integer number of clusters
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="num_dist_cores">num_dist_cores</code>
</td>
<td>
Integer number of cores to use for the distance calculations in the
K-means algorithm. Each iteration of the K-means algorithm calculates
the distance between each input graph and each cluster center.
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="max_iters">max_iters</code>
</td>
<td>
Maximum number of iterations to allow the K-means algorithm to run
</td>
</tr>
</table>

## Value

List containing the cluster template

## Examples

``` r
library(handwriter)

main_dir <- "path/to/folder"
template_docs <- "path/to/template_training_docs"
template_list <- make_clustering_template(
  main_dir = main_dir,
  template_docs = template_docs,
  writer_indices = c(2, 5),
  K = 10,
  num_dist_cores = 2,
  max_iters = 25,
  centers_seed = 100,
)
```
