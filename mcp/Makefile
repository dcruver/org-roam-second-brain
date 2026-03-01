.PHONY: help install install-dev test lint format clean build publish-gitea publish-pypi release deploy deploy-agent deploy-n8n

help:
	@echo "Available commands:"
	@echo "  make install       - Install package in current environment"
	@echo "  make install-dev   - Install package with dev dependencies"
	@echo "  make test          - Run tests"
	@echo "  make lint          - Check code quality"
	@echo "  make format        - Format code with black"
	@echo "  make clean         - Clean build artifacts"
	@echo "  make build         - Build distribution packages"
	@echo "  make publish-gitea - Publish to Gitea package registry"
	@echo "  make publish-pypi  - Publish to PyPI"
	@echo "  make release       - Create a new release (build + tag + publish)"
	@echo "  make deploy        - Deploy to all production servers"
	@echo "  make deploy-agent  - Deploy to org-roam-agent-backend only"
	@echo "  make deploy-n8n    - Deploy to n8n-backend only"

install:
	pip install -e .

install-dev:
	pip install -e ".[dev,build]"

test:
	pytest -v

test-cov:
	pytest --cov=src/ --cov-report=term-missing

lint:
	black --check src/ tests/
	ruff check src/ tests/
	mypy src/

format:
	black src/ tests/
	ruff check --fix src/ tests/

clean:
	rm -rf build/ dist/ *.egg-info
	find . -type d -name __pycache__ -exec rm -rf {} + 2>/dev/null || true
	find . -type f -name "*.pyc" -delete

build: clean
	pip install build twine
	python -m build
	python -m twine check dist/*
	@echo "\nBuild complete! Distribution files:"
	@ls -lh dist/

publish-gitea: build
	@echo "Publishing to Gitea package registry..."
	python -m twine upload --repository gitea dist/*

publish-pypi: build
	@echo "Publishing to PyPI..."
	python -m twine upload dist/*

# Interactive release process
release:
	@echo "Current version: $$(grep '^version =' pyproject.toml | cut -d'"' -f2)"
	@read -p "Enter new version: " version; \
	sed -i "s/^version = .*/version = \"$$version\"/" pyproject.toml; \
	git add pyproject.toml; \
	git commit -m "Bump version to $$version"; \
	git tag -a "v$$version" -m "Release version $$version"; \
	echo "Tagged version $$version"; \
	echo "Run 'git push && git push --tags' to push the release"

run:
	@echo "Starting org-roam-mcp server..."
	org-roam-mcp

check-deps:
	@echo "Checking if Emacs server is accessible..."
	@emacsclient --server-file=~/emacs-server/server --eval "(+ 1 1)" || echo "❌ Emacs server not accessible"
	@echo "Checking Python dependencies..."
	@pip check

# Deployment targets
deploy: publish-gitea
	@echo "Deploying to all production servers..."
	./deploy.sh all

deploy-agent: publish-gitea
	@echo "Deploying to org-roam-agent-backend..."
	./deploy.sh agent

deploy-n8n: publish-gitea
	@echo "Deploying to n8n-backend..."
	./deploy.sh n8n
