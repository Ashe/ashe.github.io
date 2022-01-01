#!/usr/bin/env bash
set -eo pipefail

# Clean rebuild
echo '' && echo '[BUILD]'
echo 'Stashing local changes..'
git stash
git_hash=$(git rev-parse --short HEAD)
echo "Generating site for commit: $git_hash.."
nix run ./#site rebuild

# Create deploy environment inside of .deploy
echo '' && echo '[DEPLOY]'
echo 'Creating build/site folder..'
mkdir -p ./build/site/
cd ./build/site/
echo 'Creating git repository..'
git init
echo 'Adding remote branch tracking..'
git remote add origin https://github.com/Ashe/ashe.github.io.git

# Add built site files
echo '' && echo '[COMMIT]'
echo 'Committing files..'
git add .
git commit -m "Publish site from $git_hash"
echo 'Setting branch..'
git checkout -b site

# Push to the master branch
echo '' && echo '[PUSH]'
echo 'Pushing files to remote..'
git push -f origin site

# Cleanup
echo '' && echo '[CLEAN]'
echo 'Cleaning build files..'
cd ../../ && nix run ./#site clean

# Restore pending changes
echo 'Restoring changes..'
git stash pop

# Completion message
echo 'Done!'
