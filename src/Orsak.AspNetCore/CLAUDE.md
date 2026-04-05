# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## What This Project Is

Orsak.AspNetCore aims to help integrating Orsak and effects with ASP.NET Core Minimal Api. It assumes that an api route correspondes to a particular effect, and aims to help mapping incomming request data as parameters to the effect, constructs an environment type per call, and maps the outcome of the effect to some http response.

## Key Conventions

Creates `RequestDelegate` from effect with low overhead.
Uses `PrintfFormat` to extract route parameters with more type safety.

## C# / F# / .NET Conventions

When implementing C# /F# / .NET code, always verify API signatures and member visibility (internal vs public) before writing code. Use `Read` to check actual type definitions rather than assuming APIs.

## Workflow

After each implementation phase, run `dotnet build` before proceeding to the next phase to catch compile errors early.

## Git Workflow
- Run tests before committing.
- Commit after tests pass, before making additional changes.
- Commits are expected to be squashed manually before any PR is made.
