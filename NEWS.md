# llmflow 3.0.2

* Fixed CRAN NOTE: replaced `.GlobalEnv` with `globalenv()` function calls

# llmflow 3.0.1

Initial CRAN submission.

## Features

* `AutoFlow()`: Complete workflow combining retrieval-augmented generation (RAG) with ReAct execution
* `react_r()`: ReAct loop implementation with smart history management and context summarization
* `response_to_r()`: LLM-to-R code generation and execution in isolated sessions
* `response_as_json()`: Structured JSON output with schema validation
* `retrieve_docs()`: R function documentation retrieval for RAG workflows

## Architecture

* Dual-LLM support for optimized cost and performance
* Error escalation and task degradation strategies
* Intermediate result protection near turn limits
* Session continuity via `existing_session` parameter

## Notes

* Functions `extract_package_docs()`, `extract_function_docs()`, `extract_docs_from_tarball()`, and `generate_qa_from_docs()` have been moved to the separate `llmtools` package.
