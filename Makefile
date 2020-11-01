help: ## Print documentation
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'

build: ## Build the app
	stack build

test: ## Test the app
	ghcid --poll --command "stack ghci tryhard:lib tryhard:test:tryhard-test --ghci-options=-fobject-code" --test ":main"

update-deps: ## Update the deps from the package.yaml file
	stack build

repl: ## Repl
	stack ghci

run: ## Run de program in dev mode. Pass arguments in ARGS
	stack run -- $$ARGS

.PHONY: serve build test update-deps repl run watch