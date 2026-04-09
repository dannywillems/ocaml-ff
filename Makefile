.PHONY: help
help: ## Ask for help!
	@grep -E '^[a-zA-Z0-9_-]+:.*?## .*$$' $(MAKEFILE_LIST) | \
		sort | awk 'BEGIN {FS = ":.*?## "}; \
		{printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'

.PHONY: build
build: ## Build the project
	dune build

.PHONY: check
check: ## Check code for compilation errors
	dune build @check

.PHONY: check-format
check-format: ## Check code formatting
	dune fmt -- --check

.PHONY: format
format: ## Format code
	dune fmt

.PHONY: test
test: ## Run tests (native + JS)
	dune runtest

.PHONY: doc
doc: ## Generate documentation
	dune build @doc

.PHONY: doc-open
doc-open: doc ## Generate and open documentation
	xdg-open _build/default/_doc/_html/index.html

.PHONY: clean
clean: ## Clean build artifacts
	dune clean

.PHONY: setup
setup: ## Setup development environment
	opam install . --deps-only --with-test --with-doc

.PHONY: release
release: ## Release to opam (tag, tarball, PR)
	dune-release tag
	dune-release distrib
	dune-release publish
	dune-release opam pkg
	dune-release opam submit
