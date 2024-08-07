

# get_cluster_fill_counts

[**Source code**](https://github.com/CSAFE-ISU/handwriter/tree/176-automatic-documentation/R/#L)

## Description

<code>get_cluster_fill_counts()</code> creates a data frame that shows
the number of graphs in each cluster for each input document.

## Usage

<pre><code class='language-R'>get_cluster_fill_counts(df)
</code></pre>

## Arguments

<table>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="df">df</code>
</td>
<td>
A data frame with columns <code>writer</code>, <code>doc</code>, and
<code>cluster</code>. Each row corresponding to a graph and lists the
writer of that graph, the document from which the graph was obtained,
and the cluster to which that graph is assigned.
</td>
</tr>
</table>

## Value

A dataframe of cluster fill counts for each document in the input data
frame.

## Examples

``` r
library(handwriter)

writer <- c(rep(1, 20), rep(2, 20), rep(3, 20))
docname <- c(rep('doc1',20), rep('doc2', 20), rep('doc3', 20))
doc <- c(rep(1, 20), rep(2, 20), rep(3, 20))
cluster <- sample(3, 60, replace=TRUE)
df <- data.frame(docname, writer, doc, cluster)
get_cluster_fill_counts(df)
```

    # A tibble: 3 × 6
    # Groups:   docname, writer, doc [3]
      docname writer   doc   `1`   `2`   `3`
      <chr>    <dbl> <dbl> <int> <int> <int>
    1 doc1         1     1     6     6     8
    2 doc2         2     2     4     8     8
    3 doc3         3     3     6     4    10
