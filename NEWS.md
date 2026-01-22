# llmflow 3.0.1

## New Features

* Added `AutoFlow()` function combining RAG documentation retrieval with ReAct workflow execution
* Support for dual-LLM architecture to optimize cost and performance
* Enhanced `react_r()` with smart history management and context summarization
* Error escalation and task degradation strategies
* Intermediate result protection near turn limits
* Comprehensive execution metrics and error analysis

## Improvements

* Session continuity via `existing_session` parameter
* Improved error messages and recovery suggestions
* Performance metrics in ReAct results
* Support for custom objects via `objects_to_use`

## Breaking Changes

* Removed `extract_package_docs()`, `extract_function_docs()`, `extract_docs_from_tarball()`, and `generate_qa_from_docs()`. These functions are now available in the `llmtools` package.

# llmflow 2.0.0

## New Features

* Added `response_to_r()` for LLM-to-R code generation and execution
* Added `extract_chat_history()` for conversation management
* Added `prompt_from_history()` for context building

## Improvements

* Improved JSON extraction with nested object support
* Enhanced code parsing and validation

# llmflow 1.0.0

* Initial release
