

# About Varialbe

[**Source code**](https://github.com/CSAFE-ISU/handwriter/tree/176-automatic-documentation/R/#L)

## Description

<code>about_variable()</code> returns information about the model
variable.

## Usage

<pre><code class='language-R'>about_variable(variable, model)
</code></pre>

## Arguments

<table>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="variable">variable</code>
</td>
<td>
A variable in the fitted model output by <code>fit_model()</code>
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
</table>

## Value

Text that explains the variable

## Examples

``` r
library(handwriter)

about_variable(
  variable = "mu[1,2]",
  model = example_model
)
```

    [1] "Mu is the location parameter of a wrapped-Cauchy distribution for writer ID w0009 and cluster 2"
