#!/bin/bash

# Evolution Mapper - Restart Backend Server (Plumber)
# This script restarts only the R Plumber backend server (port 8000)

set -e  # Exit on any error

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
BLUE='\033[0;34m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Log function
log() {
    echo -e "${BLUE}[$(date +'%H:%M:%S')]${NC} $1"
}

success() {
    echo -e "${GREEN}✅ $1${NC}"
}

error() {
    echo -e "${RED}❌ $1${NC}"
}

warning() {
    echo -e "${YELLOW}⚠️  $1${NC}"
}

# Function to check if a port is in use
check_port() {
    local port=$1
    if lsof -ti:$port >/dev/null 2>&1; then
        return 0  # Port is in use
    else
        return 1  # Port is free
    fi
}

# Function to wait for server to be ready
wait_for_server() {
    local url=$1
    local name=$2
    local max_attempts=30
    local attempt=1

    log "Waiting for $name to be ready..."

    while [ $attempt -le $max_attempts ]; do
        if curl -s "$url" >/dev/null 2>&1; then
            success "$name is ready!"
            return 0
        fi

        printf "   Attempt $attempt/$max_attempts...\r"
        sleep 2
        attempt=$((attempt + 1))
    done

    error "$name failed to start within $((max_attempts * 2)) seconds"
    return 1
}

# Main script
main() {
    echo -e "${BLUE}🔄 Evolution Mapper Backend Server Restart${NC}"
    echo "============================================"

    # Check if we're in the right directory (should work from project root or backend dir)
    if [[ -d "evolution-mapper-backend" ]]; then
        # We're in the project root
        BACKEND_DIR="evolution-mapper-backend"
        LOG_DIR="."
    elif [[ -f "plumber.R" ]]; then
        # We're in the backend directory
        BACKEND_DIR="."
        LOG_DIR=".."
    else
        error "Please run this script from the evolution-mapper root directory or evolution-mapper-backend directory"
        exit 1
    fi

    log "Cleaning up existing backend processes..."

    if check_port 8000; then
        warning "Port 8000 is in use, killing existing processes..."
        lsof -ti:8000 | xargs kill -9 2>/dev/null || true
        sleep 2
    fi

    log "Starting R Plumber backend server on port 8000..."
    cd "$BACKEND_DIR"

    if ! command -v R &> /dev/null; then
        error "R is not installed or not in PATH"
        exit 1
    fi

    nohup R -e "library(plumber); pr('plumber.R') %>% pr_run(host = '0.0.0.0', port = 8000)" > "$LOG_DIR/backend.log" 2>&1 &
    BACKEND_PID=$!
    echo $BACKEND_PID > "$LOG_DIR/backend.pid"

    if ! wait_for_server "http://localhost:8000/api/health" "Backend"; then
        error "Backend server failed to start"
        exit 1
    fi

    echo ""
    success "🎉 Backend server restarted successfully!"
    echo ""
    echo "📍 Server URL:"
    echo "   Backend API (local):  http://localhost:8000"
    echo "   Backend API (LAN):    http://<your_local_ip>:8000"
    echo "   API Documentation:    http://localhost:8000/__docs__/"
    echo ""
    echo "📝 Log File:"
    echo "   Backend logs: $LOG_DIR/backend.log"
    echo ""
}

# Run main function
main
