#!/bin/bash
set -euo pipefail

FILE="${1:-}"
if [[ -z "$FILE" ]]; then
  echo "Usage: send-to-kindle.sh <file>"
  exit 1
fi

if [[ ! -f "$FILE" ]]; then
  echo "File not found: $FILE"
  exit 1
fi

# Find Kindle MTP root
KINDLE_ROOT=$(gio mount -l 2>/dev/null | grep -oP 'mtp://Amazon_Kindle_[^/]+/' | head -n1)

if [[ -z "$KINDLE_ROOT" ]]; then
  echo "No Kindle found via MTP."
  echo "Check:"
  echo "  1. Kindle is plugged in via USB"
  echo "  2. Screen is unlocked"
  echo "  3. 'Allow' was tapped on the device"
  exit 1
fi

TARGET="${KINDLE_ROOT}Internal%20Storage/documents/"
gio copy "$FILE" "$TARGET"
echo "Sent $(basename "$FILE") to Kindle documents/."
