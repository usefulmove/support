---
description: Copy the last model response to clipboard as markdown
---

Copy the last assistant message to the clipboard.

If multiple sessions exist for the current directory, refuse to guess and require an explicit session id.

!`
DB="$HOME/.local/share/opencode/opencode.db"
DIR="$(pwd)"
ARG_SESSION_ID="$1"

sql_escape() {
  printf '%s' "$@" | sed "s/'/''/g"
}

if [ -n "$ARG_SESSION_ID" ]; then
  SESSION_ID="$ARG_SESSION_ID"
  SESSION_ID_SQL=$(sql_escape "$SESSION_ID")
  SESSION_EXISTS=$(sqlite3 "$DB" "SELECT COUNT(*) FROM session WHERE id = '$SESSION_ID_SQL'")
  if [ "$SESSION_EXISTS" = "0" ]; then
    echo "Error: session not found: $SESSION_ID" >&2
    exit 1
  fi
else
  DIR_SQL=$(sql_escape "$DIR")
  MATCH_COUNT=$(sqlite3 "$DB" "SELECT COUNT(*) FROM session WHERE directory = '$DIR_SQL'")

  if [ -z "$MATCH_COUNT" ] || [ "$MATCH_COUNT" = "0" ]; then
    echo "Error: no session found for $DIR" >&2
    exit 1
  fi

  if [ "$MATCH_COUNT" != "1" ]; then
    echo "Error: multiple sessions found for $DIR. Refusing to guess." >&2
    echo "" >&2
    echo "Recent matching sessions:" >&2
    sqlite3 -separator ' | ' "$DB" "
      SELECT
        id,
        COALESCE(title, '(no title)'),
        COALESCE(slug, '(no slug)'),
        datetime(time_updated / 1000, 'unixepoch', 'localtime')
      FROM session
      WHERE directory = '$DIR_SQL'
      ORDER BY time_updated DESC
      LIMIT 10
    " >&2
    echo "" >&2
    echo "Re-run with an explicit session id: /copy-last <session-id>" >&2
    exit 1
  fi

  SESSION_ID=$(sqlite3 "$DB" "SELECT id FROM session WHERE directory = '$DIR_SQL' LIMIT 1")
  SESSION_ID_SQL=$(sql_escape "$SESSION_ID")
fi

if [ -z "$SESSION_ID" ]; then
  echo "Error: could not resolve session" >&2
  exit 1
fi
TEXT=$(sqlite3 "$DB" "
  SELECT group_concat(json_extract(p.data, '\$.text'), '')
  FROM part p
  JOIN message m ON p.message_id = m.id
  WHERE m.session_id = '$SESSION_ID_SQL'
    AND json_extract(m.data, '\$.role') = 'assistant'
    AND json_extract(p.data, '\$.type') = 'text'
    AND m.id = (
      SELECT m2.id FROM message m2
      WHERE m2.session_id = '$SESSION_ID_SQL'
        AND json_extract(m2.data, '\$.role') = 'assistant'
        AND EXISTS (
          SELECT 1 FROM part p2
          WHERE p2.message_id = m2.id
            AND json_extract(p2.data, '\$.type') = 'text'
        )
      ORDER BY m2.time_created DESC LIMIT 1
    )
  ORDER BY p.time_created
")
if [ -z "$TEXT" ]; then
  echo "Error: no assistant message found in session $SESSION_ID" >&2
  exit 1
fi
printf '%s' "$TEXT" | wl-copy && echo "Copied to clipboard (${#TEXT} chars)"
`

If the shell command succeeded, say: "Done." Do not repeat or summarize the copied content. If it reported an error, show the error instead.
