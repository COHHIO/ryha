#!/bin/bash
set -e

echo ">>> Installing system dependencies..."
apt-get update -qq
apt-get install -y -qq curl xz-utils

echo ">>> Installing arf..."
curl --proto '=https' --tlsv1.2 -LsSf \
  https://github.com/eitsupi/arf/releases/latest/download/arf-console-installer.sh | sh

echo ">>> Done!"
