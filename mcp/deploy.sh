#!/bin/bash
# Deploy org-roam-mcp to production servers
# Usage: ./deploy.sh [server]
#   server: 'agent', 'n8n', or 'all' (default: all)

set -e  # Exit on error

GITEA_INDEX="http://gitea-backend.cruver.network:3080/api/packages/dcruver/pypi/simple"
PACKAGE_NAME="org-roam-mcp"

# Server configurations
AGENT_SERVER="root@192.168.20.136"
N8N_SERVER="root@n8n-backend.cruver.network"
VENV_PATH="/opt/org-roam-mcp-venv"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

deploy_to_server() {
    local server=$1
    local server_name=$2

    echo -e "${YELLOW}Deploying to ${server_name}...${NC}"

    ssh "$server" "systemctl stop org-roam-mcp && \
        ${VENV_PATH}/bin/pip install \
            --index-url ${GITEA_INDEX} \
            --trusted-host gitea-backend.cruver.network \
            --upgrade ${PACKAGE_NAME} && \
        systemctl start org-roam-mcp && \
        systemctl status org-roam-mcp --no-pager | head -10"

    if [ $? -eq 0 ]; then
        echo -e "${GREEN}✓ Successfully deployed to ${server_name}${NC}"

        # Show installed version
        VERSION=$(ssh "$server" "${VENV_PATH}/bin/pip show ${PACKAGE_NAME} | grep Version")
        echo -e "${GREEN}  ${VERSION}${NC}"
    else
        echo -e "${RED}✗ Failed to deploy to ${server_name}${NC}"
        return 1
    fi
}

# Parse arguments
TARGET="${1:-all}"

case "$TARGET" in
    agent)
        deploy_to_server "$AGENT_SERVER" "org-roam-agent-backend (192.168.20.136)"
        ;;
    n8n)
        deploy_to_server "$N8N_SERVER" "n8n-backend.cruver.network"
        ;;
    all)
        echo -e "${YELLOW}Deploying to all production servers...${NC}\n"
        deploy_to_server "$AGENT_SERVER" "org-roam-agent-backend (192.168.20.136)"
        echo ""
        deploy_to_server "$N8N_SERVER" "n8n-backend.cruver.network"
        echo -e "\n${GREEN}✓ All deployments complete!${NC}"
        ;;
    *)
        echo -e "${RED}Error: Unknown target '${TARGET}'${NC}"
        echo "Usage: $0 [server]"
        echo "  server: 'agent', 'n8n', or 'all' (default: all)"
        exit 1
        ;;
esac
