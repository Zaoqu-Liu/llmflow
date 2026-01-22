# llmflow

<!-- badges: start -->
[![R-CMD-check](https://github.com/Zaoqu-Liu/llmflow/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/Zaoqu-Liu/llmflow/actions/workflows/R-CMD-check.yaml)
[![CRAN status](https://www.r-pkg.org/badges/version/llmflow)](https://CRAN.R-project.org/package=llmflow)
[![r-universe](https://zaoqu-liu.r-universe.dev/badges/llmflow)](https://zaoqu-liu.r-universe.dev/llmflow)
<!-- badges: end -->

## Overview

**llmflow** provides a framework for automated data analysis through the integration of Large Language Models (LLMs) with R. Built on the ReAct (Reasoning and Acting) architecture, the package enables iterative problem-solving by alternating between reasoning steps and code execution, allowing LLMs to autonomously analyze data, handle errors, and refine solutions.

## Key Features

- **ReAct Architecture**: Iterative reasoning and acting loops for complex analytical tasks
- **Automated Code Execution**: LLM-generated R code runs in isolated sessions with error handling
- **Intelligent Error Recovery**: Automatic retry mechanisms with progressive error escalation
- **Session Persistence**: Maintain R session state across multiple LLM interactions
- **Structured Output**: JSON schema validation for reliable, parseable responses
- **RAG Integration**: Retrieval-augmented generation with R function documentation

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

## Requirements

The package requires [ellmer](https://github.com/tidyverse/ellmer) for LLM communication:

```r
install.packages("ellmer")
```

## Quick Start

```r
library(llmflow)
library(ellmer)

# Initialize LLM client
llm <- chat_openai(model = "gpt-4o")

# Automated analysis with ReAct workflow
result <- AutoFlow(
 react_llm = llm,
 task_prompt = "Perform linear regression of mpg on hp and wt using mtcars"
)
```

## Core Functions

| Function | Description |
|----------|-------------|
| `AutoFlow()` | Complete workflow combining RAG and ReAct |
| `react_r()` | ReAct loop for iterative problem solving |
| `response_to_r()` | Execute LLM-generated R code |
| `response_as_json()` | Structured JSON output with schema validation |
| `retrieve_docs()` | Retrieve R function documentation for RAG |

## Usage Examples

### ReAct Workflow

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

## License

GPL (>= 3)

## Citation

```
Liu Z (2026). llmflow: Reasoning and Acting Workflow for Automated Data Analysis.
R package version 3.0.1, https://github.com/Zaoqu-Liu/llmflow
```

## Author

Zaoqu Liu

- Email: liuzaoqu@163.com
- ORCID: [0000-0002-0452-742X](https://orcid.org/0000-0002-0452-742X)
