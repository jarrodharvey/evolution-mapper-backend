#!/bin/bash

# Quick update script for Evolution Mapper Backend
# Pushes codebase to remote server and restarts the application

set -e  # Exit on any error

# Load environment variables
if [ -f .Renviron ]; then
    export $(grep -v '^#' .Renviron | xargs)
else
    echo "Error: .Renviron file not found"
    exit 1
fi

# Check if DO_DROPLET_IP is set
if [ -z "$DO_DROPLET_IP" ]; then
    echo "Error: DO_DROPLET_IP not found in .Renviron"
    exit 1
fi

echo "🚀 Starting quick update deployment to $DO_DROPLET_IP..."

# Define remote paths
REMOTE_USER="root"
REMOTE_PATH="/var/plumber/evolution-mapper"

# Files and directories to sync (excluding .gitignore patterns)
echo "📦 Syncing codebase to remote server..."
rsync -avz --progress \
    --exclude-from='.gitignore' \
    --exclude='.git' \
    ./ $REMOTE_USER@$DO_DROPLET_IP:$REMOTE_PATH/

echo "🔐 Setting group permissions on remote files..."
ssh $REMOTE_USER@$DO_DROPLET_IP "chgrp -R plumber $REMOTE_PATH && chmod -R g+rw $REMOTE_PATH"

echo "🔄 Restarting plumber service on remote server..."
ssh $REMOTE_USER@$DO_DROPLET_IP "sudo systemctl restart plumber-evolution-mapper"

echo "⏳ Waiting for service to start..."
RETRY_COUNT=0
MAX_RETRIES=30
SERVICE_READY=false

while [ $RETRY_COUNT -lt $MAX_RETRIES ]; do
    if ssh $REMOTE_USER@$DO_DROPLET_IP "sudo systemctl is-active --quiet plumber-evolution-mapper"; then
        echo "✅ Service is active"
        SERVICE_READY=true
        break
    else
        echo "⏳ Service not ready yet (attempt $((RETRY_COUNT + 1))/$MAX_RETRIES)..."
        sleep 2
        RETRY_COUNT=$((RETRY_COUNT + 1))
    fi
done

if [ "$SERVICE_READY" = false ]; then
    echo "❌ Service failed to start after $MAX_RETRIES attempts"
    echo "📋 Service status:"
    ssh $REMOTE_USER@$DO_DROPLET_IP "sudo systemctl status plumber-evolution-mapper --no-pager -l"
    exit 1
fi

echo "🌐 Waiting for health endpoint to respond..."
HEALTH_RETRY_COUNT=0
MAX_HEALTH_RETRIES=30
HEALTH_URL="https://$DO_DROPLET_DOMAIN/api/health"

while [ $HEALTH_RETRY_COUNT -lt $MAX_HEALTH_RETRIES ]; do
    if curl -s --max-time 10 "$HEALTH_URL" | grep -q '"status":\s*\["ok"\]'; then
        echo "✅ Health endpoint responding successfully!"
        break
    else
        echo "⏳ Health endpoint not ready yet (attempt $((HEALTH_RETRY_COUNT + 1))/$MAX_HEALTH_RETRIES)..."
        sleep 3
        HEALTH_RETRY_COUNT=$((HEALTH_RETRY_COUNT + 1))
    fi
done

if [ $HEALTH_RETRY_COUNT -eq $MAX_HEALTH_RETRIES ]; then
    echo "❌ Health endpoint failed to respond after $MAX_HEALTH_RETRIES attempts"
    echo "📋 Service status:"
    ssh $REMOTE_USER@$DO_DROPLET_IP "sudo systemctl status plumber-evolution-mapper --no-pager -l"
    exit 1
fi

echo "🎉 Quick update deployment completed successfully!"
echo "🌐 API is ready at: $HEALTH_URL"