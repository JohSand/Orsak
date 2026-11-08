#!/usr/bin/env bash
# Packs Orsak and Orsak.Remoting into feed/, under a version of its own, so NuGet's cache never serves an older one, then compiles App with
# Fable against those packages, as a user's project would, and runs it on Node.js. Fails if any step does, or if Fable
# compiled Orsak's .NET EffectBuilder.fs instead of its Fable one.
set -euo pipefail
cd "$(dirname "$0")"

version="0.0.0-local.$(date +%s)"
rm -rf feed out
dotnet pack ../../src/Orsak -c Release -o feed -p:Version="$version" > /dev/null
dotnet pack ../../src/Orsak.Remoting -c Release -o feed -p:Version="$version" > /dev/null

# MSBuild reads environment variables as properties: App.fsproj references Orsak $(OrsakVersion)
OrsakVersion="$version" dotnet fable . -o out --noCache

package="out/fable_modules/Orsak.$version"
echo "compiled from the package: $(cd "$package" && find . -name "*.js" | sort | tr '\n' ' ')"

if [ -e "$package/EffectBuilder.fs.js" ]; then
    echo "Fable compiled the .NET EffectBuilder.fs" >&2
    exit 1
fi

expected='["Hello Fable"; "Hello Node"]
["Hello Fable","Hello Node"]'
actual=$(node out/App.js)
echo "$actual"
[ "$actual" = "$expected" ] || { echo "expected $expected" >&2; exit 1; }
