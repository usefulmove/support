---
name: send-to-kindle
description: Detect a USB-connected Kindle via MTP and copy an ebook (.azw3, .mobi, .epub, .pdf) to its documents folder. Use when the user wants to transfer a generated or existing ebook to their Kindle device.
license: MIT
compatibility: opencode
---

## When to Use

- User asks to "send to Kindle", "copy to Kindle", "put on my Kindle", or similar.
- A `.azw3`, `.mobi`, `.epub`, or `.pdf` file has just been generated and needs to be transferred.
- User mentions they want to read something on their Kindle.

## Workflow

### Step 1: Detect the Kindle

Check if a Kindle is connected via USB using `mtp-detect` or `gio mount -l`:

```bash
# Method A: gio (preferred on Ubuntu/GNOME)
gio mount -l | grep -i kindle

# Method B: lsusb fallback
lsusb | grep -i kindle
```

If nothing appears, the Kindle is either:
- Not plugged in → Ask user to connect it.
- Not unlocked / not in USB mode → Ask user to unlock and tap "Allow" on the device screen.

### Step 2: Find the MTP mount path

`gio mount -l` returns a path like:
```
mtp://Amazon_Kindle_<serial>/
```

That is the **device root**. The internal storage lives under:
```
mtp://Amazon_Kindle_<serial>/Internal%20Storage/
```

Confirm access by listing the contents:
```bash
gio list "mtp://Amazon_Kindle_<serial>/Internal%20Storage/"
```

Expected output includes `documents`, `audible`, `screenshots`, `system`.

### Step 3: Copy the file

The target folder is always `Internal Storage/documents/`.

```bash
gio copy <source-file> "mtp://Amazon_Kindle_<serial>/Internal%20Storage/documents/"
```

### Step 4: Verify

List the documents folder to confirm the file arrived:
```bash
gio list "mtp://Amazon_Kindle_<serial>/Internal%20Storage/documents/" | grep -i <filename>
```

### Step 5: Report to user

Tell the user the file is on the device and will appear in the library after disconnecting.

## Helper Script (Optional)

`scripts/send-to-kindle.sh` — Automated detection and copy:

```bash
#!/bin/bash
set -euo pipefail

FILE="${1:-}"
if [[ -z "$FILE" ]]; then
  echo "Usage: send-to-kindle.sh <file>"
  exit 1
fi

# Find Kindle MTP root
KINDLE_ROOT=$(gio mount -l 2>/dev/null | grep -oP 'mtp://Amazon_Kindle_[^/]+/' | head -n1)
if [[ -z "$KINDLE_ROOT" ]]; then
  echo "No Kindle found. Is it connected and unlocked?"
  exit 1
fi

TARGET="${KINDLE_ROOT}Internal%20Storage/documents/"
gio copy "$FILE" "$TARGET"
echo "Sent $(basename "$FILE") to Kindle."
```

## Common Issues

| Issue | Fix |
|-------|-----|
| Kindle shows up in `lsusb` but not `gio mount -l` | Kindle is locked or MTP is off. Unlock screen and look for "USB Drive Mode" or "Allow" prompt. |
| `gio copy` fails with permission error | Ensure the screen is unlocked and the "Allow" dialog was accepted. Re-plug if necessary. |
| File copied but not visible on Kindle | It will appear after you safely disconnect the USB cable. |
| `mtp-detect` missing | `gio` is usually pre-installed on Ubuntu. `mtp-detect` comes from `mtp-tools` (`apt install mtp-tools`). |

## Checklist

- [ ] Kindle detected via `gio mount -l` or `lsusb`
- [ ] Internal Storage listed successfully
- [ ] File copied to `Internal Storage/documents/`
- [ ] File verified in documents folder listing
- [ ] User informed to disconnect and check library
