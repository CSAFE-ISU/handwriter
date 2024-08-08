

# Get Credible Intervals

[**Source code**](https://github.com/CSAFE-ISU/handwriter/tree/176-automatic-documentation/R/#L)

## Description

In a model created with <code>fit_model()</code> the pi parameters are
the estimate of the true cluster fill count for a particular writer and
cluster. The function <code>get_credible_intervals()</code> calculates
the credible intervals of the pi parameters for each writer in the
model.

## Usage

<pre><code class='language-R'>get_credible_intervals(model, interval_min = 0.05, interval_max = 0.95)
</code></pre>

## Arguments

<table>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="model">model</code>
</td>
<td>
A model output by <code>fit_model()</code>
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="interval_min">interval_min</code>
</td>
<td>
The lower bound for the credible interval. The number must be between 0
and 1.
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="interval_max">interval_max</code>
</td>
<td>
The upper bound for the credible interval. The number must be greater
than <code>interval_min</code> and must be less than 1.
</td>
</tr>
</table>

## Value

A list of data frames. Each data frame lists the credible intervals for
a single writer.

## Examples

``` r
library(handwriter)

get_credible_intervals(model=example_model)
```

    [[1]]
      quantile  cluster_1  cluster_2 cluster_3 cluster_4 cluster_5 writer
    1       5% 0.06042425 0.02493705 0.1418073 0.5529695 0.1306786  w0009
    2      50% 0.07366519 0.03478669 0.1647020 0.5782510 0.1489102  w0009
    3      95% 0.08875836 0.04671823 0.1840218 0.6088443 0.1701284  w0009

    [[2]]
      quantile   cluster_1    cluster_2 cluster_3 cluster_4 cluster_5 writer
    1       5% 0.008342953 5.982238e-05 0.2480043 0.4153643 0.2096460  w0030
    2      50% 0.017026968 1.492831e-03 0.2840930 0.4549356 0.2402698  w0030
    3      95% 0.029416108 8.076710e-03 0.3161589 0.4931652 0.2830293  w0030

    [[3]]
      quantile  cluster_1  cluster_2  cluster_3 cluster_4  cluster_5 writer
    1       5% 0.02039637 0.01621031 0.08938908 0.7044609 0.08201707  w0238
    2      50% 0.02877164 0.02494265 0.10993121 0.7323968 0.10357189  w0238
    3      95% 0.04086654 0.03510818 0.12997545 0.7596250 0.12387242  w0238

``` r
get_credible_intervals(model=example_model, interval_min=0.05, interval_max=0.95)
```

    [[1]]
      quantile  cluster_1  cluster_2 cluster_3 cluster_4 cluster_5 writer
    1       5% 0.06042425 0.02493705 0.1418073 0.5529695 0.1306786  w0009
    2      50% 0.07366519 0.03478669 0.1647020 0.5782510 0.1489102  w0009
    3      95% 0.08875836 0.04671823 0.1840218 0.6088443 0.1701284  w0009

    [[2]]
      quantile   cluster_1    cluster_2 cluster_3 cluster_4 cluster_5 writer
    1       5% 0.008342953 5.982238e-05 0.2480043 0.4153643 0.2096460  w0030
    2      50% 0.017026968 1.492831e-03 0.2840930 0.4549356 0.2402698  w0030
    3      95% 0.029416108 8.076710e-03 0.3161589 0.4931652 0.2830293  w0030

    [[3]]
      quantile  cluster_1  cluster_2  cluster_3 cluster_4  cluster_5 writer
    1       5% 0.02039637 0.01621031 0.08938908 0.7044609 0.08201707  w0238
    2      50% 0.02877164 0.02494265 0.10993121 0.7323968 0.10357189  w0238
    3      95% 0.04086654 0.03510818 0.12997545 0.7596250 0.12387242  w0238
