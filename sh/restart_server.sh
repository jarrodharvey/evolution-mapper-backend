#!/bin/bash

# Restart the Evolution Mapper R Plumber API server
# Usage: ./restart_server.sh

echo "🔄 Restarting Evolution Mapper API server..."

# Kill any existing processes on port 8000
echo "   Stopping existing server processes..."
if lsof -ti:8000 >/dev/null 2>&1; then
    lsof -ti:8000 | xargs kill -9
    echo "   ✅ Killed existing processes on port 8000"
else
    echo "   ℹ️  No existing processes found on port 8000"
fi

# Wait a moment for cleanup
sleep 2

# Start the new server
echo "   Starting new server..."
R -e "library(plumber); pr('plumber.R') %>% pr_run(port = 8000)" &
SERVER_PID=$!

echo "   🚀 Server started with PID: $SERVER_PID"

# Wait for server to initialize
echo "   Waiting for server to initialize..."
sleep 4

# Check if server is responding
echo "   Testing server health..."
if curl -s -H "X-API-Key: demo-key-12345" "http://localhost:8000/api/health" >/dev/null 2>&1; then
    echo "   ✅ Server is healthy and responding"
    echo ""
    echo "🎉 Evolution Mapper API server successfully restarted!"
    echo "   Server URL: http://localhost:8000"
    echo "   Swagger docs: http://localhost:8000/__docs__/"
    echo "   Health check: http://localhost:8000/api/health"
    echo ""
    echo "   Available endpoints:"
    echo "   • GET  /api/health           - Health check"
    echo "   • GET  /api/species          - Search species" 
    echo "   • POST /api/tree             - Generate tree from species list"
    echo "   • GET  /api/random-tree      - Generate random tree"
    echo "   • GET  /api/debug-tree       - Debug tree generation"
else
    echo "   ❌ Server health check failed"
    echo "   Check the server logs for errors"
    exit 1
fi