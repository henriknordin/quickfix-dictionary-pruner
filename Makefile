help: ## Print documentation
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'

ghcid: ## Run ghcid with the adventofcode2016 project
	ghcid \
	    --command "stack ghci quickfix-dictionary-pruner --ghci-options='-Wall'"

.PHONY: ghcid help
