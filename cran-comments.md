## R CMD check results

0 errors | 0 warnings | 1 note

## Test environments

* local macOS (aarch64-apple-darwin20), R 4.4.0
* win-builder (R-devel)
* GitHub Actions (ubuntu-latest): R release, R devel
* GitHub Actions (windows-latest): R release
* GitHub Actions (macos-latest): R release

## Notes

### NOTE: New submission
This is the first CRAN submission of llmflow.

## Dependencies

* The package suggests 'ellmer' for LLM communication, which provides 
  a unified interface to various LLM providers (OpenAI, Anthropic, etc.).

* The package uses 'callr' to create isolated R sessions for safe code execution.

* All examples are wrapped in \dontrun{} as they require API credentials.

## Downstream dependencies

This is a new submission. There are currently no downstream dependencies.
