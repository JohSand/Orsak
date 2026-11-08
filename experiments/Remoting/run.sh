#!/usr/bin/env bash
# Runs the spike end to end: starts the .NET server, compiles the client with Fable and runs it on Node.js against the
# server, then stops the server. The client prints one line per check.
set -euo pipefail
cd "$(dirname "$0")"

dotnet build Server -c Release > /dev/null
(cd Client && npm install --no-audit --no-fund --silent)
dotnet fable Client -o Client/out --noCache > /dev/null

dotnet run --project Server -c Release --no-build > server.log 2>&1 &
server=$!
trap 'kill $server 2> /dev/null || true' EXIT

# the server is up once it answers, even with an error for a route it doesn't have
for _ in $(seq 1 60); do
    curl -s -o /dev/null http://localhost:5179/ && break
    sleep 0.5
done

node Client/run.js
