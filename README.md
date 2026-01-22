# llmflow

<!-- badges: start -->
[![R-CMD-check](https://github.com/Zaoqu-Liu/llmflow/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/Zaoqu-Liu/llmflow/actions/workflows/R-CMD-check.yaml)
[![CRAN status](https://www.r-pkg.org/badges/version/llmflow)](https://CRAN.R-project.org/package=llmflow)
[![r-universe](https://zaoqu-liu.r-universe.dev/badges/llmflow)](https://zaoqu-liu.r-universe.dev/llmflow)
<!-- badges: end -->

## Overview

**llmflow** is an R package that provides a framework for integrating Large Language Models (LLMs) with R programming through automated workflow management. The package implements the ReAct (Reasoning and Acting) architecture, enabling iterative problem-solving through alternating reasoning and code execution steps.

## Features

- **ReAct Architecture**: Automated reasoning and acting loops for complex analytical tasks
- **Error Handling**: Automatic retry mechanisms with error escalation strategies
- **Session Management**: Persistent R session state across multiple LLM interactions
- **JSON Validation**: Structured output with schema validation
- **RAG Integration**: Retrieval-augmented generation with function documentation

## Installation

From R-universe:

```r
install.packages("llmflow", repos = c("https://zaoqu-liu.r-universe.dev", "https://cloud.r-project.org"))
```

From GitHub:

```r
# install.packages("pak")
pak::pak("Zaoqu-Liu/llmflow")
```

## Dependencies

The package requires [ellmer](https://github.com/hadley/ellmer) for LLM communication:

```r
install.packages("ellmer")
```

## Usage

### Basic Setup

```r
library(llmflow)
library(ellmer)

# Initialize LLM client
llm <- chat_openai(model = "gpt-4o")
```

### AutoFlow

The `AutoFlow()` function provides a complete workflow combining documentation retrieval with ReAct execution:

```r
result <- AutoFlow(
  react_llm = llm,
  task_prompt = "Perform linear regression of mpg on hp and wt using mtcars"
)
```

### ReAct Workflow

For more control over the reasoning process:
```r
result <- react_r(
  chat_obj = llm,
  task = "Calculate correlation matrix for iris numeric columns",
  max_turns = 10,
  verbose = TRUE
)

# Access results
result$final_answer
result$code_summary$complete_script
```

### JSON Response with Schema Validation

```r
schema <- list(
  type = "object",
  properties = list(
    analysis_type = list(type = "string"),
    findings = list(type = "array", items = list(type = "string"))
  ),
  required = c("analysis_type", "findings")
)

response <- response_as_json(
  chat_obj = llm,
  prompt = "Summarize the iris dataset",
  schema = schema,
  schema_strict = TRUE
)
```

### Code Generation and Execution

```r
result <- response_to_r(
  chat_obj = llm,
  prompt = "Create a scatter plot of mpg vs hp from mtcars",
  pkgs_to_use = c("ggplot2"),
  return_mode = "full"
)
```

## Main Functions

| Function | Description |
|----------|-------------|
| `AutoFlow()` | Complete workflow with RAG and ReAct |
| `react_r()` | ReAct loop for iterative problem solving |
| `response_to_r()` | LLM response to R code execution |
| `response_as_json()` | Structured JSON output with validation |
| `retrieve_docs()` | Retrieve function documentation |
| `extract_r_code()` | Extract R code from text |
| `extract_json()` | Extract JSON from text |

## License

GPL (>= 3)

## Citation

If you use this package in your research, please cite:

```
Liu Z (2024). llmflow: LLM-R Integration with ReAct Workflow Automation.
R package version 3.0.1, https://github.com/Zaoqu-Liu/llmflow
```

## Author

Zaoqu Liu (liuzaoqu@163.com)

ORCID: [0000-0002-0452-742X](https://orcid.org/0000-0002-0452-742X)
