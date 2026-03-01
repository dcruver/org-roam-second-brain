# Distribution Guide

This document explains how to distribute the `org-roam-mcp` package.

## 🚀 Quick Reference (Verified Working Setup)

**Gitea Package Registry** (gitea-backend.cruver.network):
- URL: `http://gitea-backend.cruver.network:3080/api/packages/dcruver/pypi`
- Protocol: HTTP (not HTTPS)
- Port: 3080

**Current published version**: 0.1.1

### Publish New Version

```bash
cd mcp
make publish-gitea  # Builds and uploads automatically
```

### Install on Production Servers

```bash
# org-roam-agent-backend (192.168.20.136)
ssh root@192.168.20.136 "systemctl stop org-roam-mcp && \
  /opt/org-roam-mcp-venv/bin/pip install \
  --index-url http://gitea-backend.cruver.network:3080/api/packages/dcruver/pypi/simple \
  --trusted-host gitea-backend.cruver.network \
  --upgrade org-roam-mcp && \
  systemctl start org-roam-mcp"

# n8n-backend.cruver.network
ssh root@n8n-backend.cruver.network "systemctl stop org-roam-mcp && \
  /opt/org-roam-mcp-venv/bin/pip install \
  --index-url http://gitea-backend.cruver.network:3080/api/packages/dcruver/pypi/simple \
  --trusted-host gitea-backend.cruver.network \
  --upgrade org-roam-mcp && \
  systemctl start org-roam-mcp"
```

### Create Release

```bash
# 1. Update version in pyproject.toml
# 2. Build and publish
cd mcp && make publish-gitea

# 3. Tag release
git tag -a v0.1.x -m "Release version 0.1.x"
git push origin v0.1.x
```

---

## Quick Install (For Users)

### Option 1: Install from PyPI (Recommended)

Users can install directly from PyPI:

```bash
# Install latest version
pip install org-roam-mcp

# Install specific version
pip install org-roam-mcp==0.1.14
```

### Option 2: Install from Wheel

Users can install from a distributed `.whl` file:

```bash
pip install org_roam_mcp-0.1.0-py3-none-any.whl
```

## For Maintainers: Building and Publishing

### 1. Building Distribution Packages

Install build tools:

```bash
pip install -e ".[build]"
```

Build the package:

```bash
# Build both wheel and source distribution
python -m build

# Output will be in dist/:
# - org_roam_mcp-0.1.0-py3-none-any.whl (wheel)
# - org_roam_mcp-0.1.0.tar.gz (source distribution)
```

### 2. Publishing to Gitea Package Registry

Gitea supports Python package hosting. To publish:

#### Configure Gitea repository:

1. Add to `.pypirc` (create in `~/.pypirc`):

```ini
[distutils]
index-servers =
    gitea

[gitea]
repository = http://gitea-backend.cruver.network:3080/api/packages/dcruver/pypi
username = dcruver
password = <your-gitea-token>
```

**IMPORTANT**: Use HTTP (not HTTPS) and port 3080 for gitea-backend.cruver.network

#### Upload to Gitea:

```bash
# Upload using twine
python -m twine upload --repository gitea dist/*

# Or upload via curl
curl --user dcruver:<token> \
  --upload-file dist/org_roam_mcp-0.1.0-py3-none-any.whl \
  http://gitea-backend.cruver.network:3080/api/packages/dcruver/pypi
```

#### Install from Gitea registry:

```bash
pip install --index-url http://gitea-backend.cruver.network:3080/api/packages/dcruver/pypi/simple \
  --trusted-host gitea-backend.cruver.network \
  org-roam-mcp
```

**Note**: `--trusted-host` is required because we're using HTTP instead of HTTPS

### 3. Publishing to PyPI (Public)

If you want to make this publicly available on PyPI:

#### Configure PyPI credentials:

```bash
# Get API token from https://pypi.org/manage/account/token/
# Add to ~/.pypirc:
[pypi]
username = __token__
password = pypi-<your-token>
```

#### Upload to PyPI:

```bash
python -m twine upload dist/*
```

#### Install from PyPI:

```bash
pip install org-roam-mcp
```

### 4. GitHub Package Registry

To also publish on GitHub (if mirrored there):

```bash
# Add GitHub remote
git remote add github git@github.com:username/org-roam-mcp.git

# Configure .pypirc
[github]
repository = https://upload.pypi.org/legacy/
username = <github-username>
password = <github-token>

# Upload
python -m twine upload --repository github dist/*
```

## Version Management

### Updating Version

Edit version in `pyproject.toml`:

```toml
version = "0.2.0"
```

### Creating Releases

#### On Gitea:

```bash
# Tag the release
git tag -a v0.2.0 -m "Release version 0.2.0"
git push origin v0.2.0

# Build and upload
python -m build
python -m twine upload --repository gitea dist/*
```

#### Create release on Gitea web UI:
1. Go to repository → Releases
2. Create new release from tag
3. Attach wheel/tarball files

## Distribution Best Practices

### 1. Before Publishing

```bash
# Run tests
pytest

# Check code quality
black --check src/ tests/
ruff check src/ tests/
mypy src/

# Build package
python -m build

# Check package metadata
python -m twine check dist/*
```

### 2. Version Numbering

Follow [Semantic Versioning](https://semver.org/):
- **MAJOR**: Incompatible API changes
- **MINOR**: Add functionality (backward compatible)
- **PATCH**: Bug fixes (backward compatible)

Examples:
- `0.1.0` - Initial development
- `0.2.0` - Add new MCP tool
- `0.2.1` - Fix bug in existing tool
- `1.0.0` - First stable release

### 3. Changelog

Maintain a `CHANGELOG.md`:

```markdown
# Changelog

## [0.2.0] - 2025-10-20
### Added
- New semantic search tool
### Fixed
- Async entry point bug

## [0.1.0] - 2025-10-19
### Added
- Initial release
```

## CI/CD Automation

### Gitea Actions Example

Create `.gitea/workflows/publish.yml`:

```yaml
name: Publish Package

on:
  push:
    tags:
      - 'v*'

jobs:
  publish:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3

      - name: Set up Python
        uses: actions/setup-python@v4
        with:
          python-version: '3.11'

      - name: Install dependencies
        run: |
          pip install build twine

      - name: Build package
        run: python -m build

      - name: Publish to Gitea
        env:
          TWINE_USERNAME: ${{ secrets.GITEA_USERNAME }}
          TWINE_PASSWORD: ${{ secrets.GITEA_TOKEN }}
        run: |
          python -m twine upload --repository-url https://gitea-backend.cruver.network/api/packages/dcruver/pypi dist/*
```

## For End Users

### Installing from Different Sources

```bash
# From PyPI (recommended for users)
pip install org-roam-mcp

# From PyPI with specific version
pip install org-roam-mcp==0.1.14

# From GitHub repository (for development)
pip install git+https://github.com/dcruver/org-roam-ai.git#subdirectory=mcp

# From local wheel file
pip install /path/to/org_roam_mcp-0.1.0-py3-none-any.whl

# Development install from source
git clone https://github.com/dcruver/org-roam-ai.git
cd org-roam-ai/mcp
pip install -e ".[dev]"
```

### Updating

```bash
# Upgrade to latest version from PyPI
pip install --upgrade org-roam-mcp
```

## Troubleshooting

### "Could not find a version that satisfies the requirement"

Make sure you're using the correct index URL for Gitea:
```bash
pip install --index-url http://gitea-backend.cruver.network:3080/api/packages/dcruver/pypi/simple \
  --trusted-host gitea-backend.cruver.network \
  org-roam-mcp
```

**Note**: Must use HTTP (not HTTPS) and port 3080. The `--trusted-host` flag is required for HTTP repositories.

### Authentication Issues

For Gitea SSH access, ensure your SSH key is added:
```bash
ssh-add ~/.ssh/id_rsa
ssh -T gitea@gitea-backend.cruver.network
```

### Build Errors

Clean and rebuild:
```bash
rm -rf dist/ build/ *.egg-info
python -m build
```
