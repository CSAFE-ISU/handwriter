

# addToFeatures

[**Source code**](https://github.com/CSAFE-ISU/handwriter/tree/176-automatic-documentation/R/#L)

## Description

addToFeatures

## Usage

<pre><code class='language-R'>addToFeatures(FeatureSet, LetterList, vectorDims)
</code></pre>

## Arguments

<table>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="FeatureSet">FeatureSet</code>
</td>
<td>
The current list of features that have been calculated
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="LetterList">LetterList</code>
</td>
<td>
List of all letters and their information
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="vectorDims">vectorDims</code>
</td>
<td>
Vectors with image Dims
</td>
</tr>
</table>

## Value

A list consisting of current features calculated in FeatureSet as well
as measures of compactness, loop count, and loop dimensions
