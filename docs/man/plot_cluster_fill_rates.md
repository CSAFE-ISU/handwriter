

# Plot Cluster Fill Rates

[**Source code**](https://github.com/CSAFE-ISU/handwriter/tree/176-automatic-documentation/R/#L)

## Description

Plot the cluster fill rates for each document in
<code>formatted_data</code>.

## Usage

<pre><code class='language-R'>plot_cluster_fill_rates(formatted_data, facet = FALSE)
</code></pre>

## Arguments

<table>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="formatted_data">formatted_data</code>
</td>
<td>
Data created by <code>format_template_data()</code>,
<code>fit_model()</code>, or <code>analyze_questioned_documents()</code>
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="facet">facet</code>
</td>
<td>
<code>TRUE</code> uses <code>facet_wrap</code> to create a subplot for
each writer. <code>FALSE</code> plots the data on a single plot.
</td>
</tr>
</table>

## Value

ggplot plot of cluster fill rates

## Examples

``` r
library(handwriter)

# Plot cluster fill rates for template training documents
template_data <- format_template_data(example_cluster_template)
plot_cluster_fill_rates(formatted_data = template_data, facet = TRUE)
```

![](plot_cluster_fill_rates.markdown_strict_files/figure-markdown_strict/unnamed-chunk-1-1.png)

``` r
# Plot cluster fill rates for model training documents
plot_cluster_fill_rates(formatted_data = example_model, facet = TRUE)
```

![](plot_cluster_fill_rates.markdown_strict_files/figure-markdown_strict/unnamed-chunk-1-2.png)

``` r
# Plot cluster fill rates for questioned documents
plot_cluster_fill_rates(formatted_data = example_analysis, facet = FALSE)
```

![](plot_cluster_fill_rates.markdown_strict_files/figure-markdown_strict/unnamed-chunk-1-3.png)
