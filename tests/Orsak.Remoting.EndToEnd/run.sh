#!/usr/bin/env bash
# Orsak.Remoting end to end: starts the .NET server, then runs the client against it, compiled with Fable on Node.js,
# and on .NET, and stops the server. Fails unless both clients print what's expected.
set -euo pipefail
cd "$(dirname "$0")"

dotnet build Server -c Release > /dev/null
dotnet build Client -c Release > /dev/null
dotnet fable Client -o Client/out --noCache > /dev/null

dotnet run --project Server -c Release --no-build > server.log 2>&1 &
server=$!
trap 'kill $server 2> /dev/null || true' EXIT

# the server is up once it answers, even with an error for a route it doesn't have
for _ in $(seq 1 60); do
    curl -s -o /dev/null http://localhost:5179/ && break
    sleep 0.5
done

expected='placed: Ok N, notes Some ring twice
without a note: Ok N
empty: Error EmptyOrder
unknown: Error UnknownProduct plum
unreachable: raised RemoteCallException'

check() {
    local name=$1 actual=$2
    # order ids grow, with each run against the same server
    actual=$(printf '%s\n' "$actual" | sed -E 's/Ok [0-9]+/Ok N/')
    echo "== $name"
    printf '%s\n' "$actual"
    [ "$actual" = "$expected" ] || { echo "$name: not as expected" >&2; exit 1; }
}

check "Fable, on Node.js" "$(node Client/out/Program.js)"
check ".NET" "$(dotnet run --project Client -c Release --no-build)"
