#!/bin/bash
# =============================================================================
# push_to_github.sh — Initialize Git & Push to GitHub
# Run this script from the project root directory
# =============================================================================

# ── CONFIGURATION (edit these) ───────────────────────────────────────────────
GITHUB_USERNAME="YOUR_GITHUB_USERNAME"
REPO_NAME="pkpd-analysis-r"
COMMIT_MSG="Initial commit: One-compartment PK analysis with Theophylline dataset"

# ── STEP 1: Initialize git repo ───────────────────────────────────────────────
echo "📁 Initializing Git repository..."
git init
git branch -M main

# ── STEP 2: Stage all files ───────────────────────────────────────────────────
echo "📦 Staging files..."
git add .
git status

# ── STEP 3: First commit ──────────────────────────────────────────────────────
echo "✅ Creating initial commit..."
git commit -m "$COMMIT_MSG"

# ── STEP 4: Add remote origin ─────────────────────────────────────────────────
echo "🔗 Adding remote origin..."
git remote add origin "https://github.com/$GITHUB_USERNAME/$REPO_NAME.git"

# ── STEP 5: Push ──────────────────────────────────────────────────────────────
echo "🚀 Pushing to GitHub..."
git push -u origin main

echo ""
echo "══════════════════════════════════════════════════════════"
echo "  ✅ Successfully pushed to:"
echo "  https://github.com/$GITHUB_USERNAME/$REPO_NAME"
echo "══════════════════════════════════════════════════════════"
