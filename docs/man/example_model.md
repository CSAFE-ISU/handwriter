

# Example of a hierarchical model

## Description

Example of a hierarchical model

## Usage

<pre><code class='language-R'>example_model
</code></pre>

## Format

A hierarchical model created by <code>fit_model</code> with a single
chain of 100 MCMC iterations. It is a named list of 4 objects:

<dl>
<dt>
graph_measurements
</dt>
<dd>
A data frame of model training data that shows the writer, document
name, cluster assignment, slope, principle component rotation angle, and
wrapped principle component rotation angle for each training graph.
</dd>
<dt>
cluster_fill_counts
</dt>
<dd>
A data frame of the cluster fill counts for each model training
document.
</dd>
<dt>
rjags_data
</dt>
<dd>
The model training information from <code>graph_measurements</code> and
<code>cluster_fill_counts</code> formatted for RJAGS.
</dd>
<dt>
fitted_model
</dt>
<dd>
A model fit using the <code>rjags_data</code> and the RJAGS and coda
packages. It is an MCMC list that contains a single MCMC object.
</dd>
</dl>

## Examples

``` r
library(handwriter)

# convert to a data frame and view all variable names
df <- as.data.frame(coda::as.mcmc(example_model$fitted_model))
colnames(df)

# view a trace plot
plot_trace(variable = "mu[1,1]", model = example_model)

# drop the first 25 MCMC iterations for burn-in
model <- drop_burnin(model = example_model, burn_in = 25)

# analyze questioned documents
main_dir <- /path/to/main_dir
questioned_docs <- /path/to/questioned_documents_directory
analysis <- analyze_questioned_documents(
   main_dir = main_dir,
   questioned_docs = questioned_docs
   model = example_model
   num_cores = 2
)
analysis$posterior_probabilities
```
