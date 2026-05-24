---
description: Copy the last model response to clipboard as markdown
---

Copy the last assistant message to the clipboard.

If multiple sessions exist for the current directory in the last 3 hours, disambiguate with title, slug, and preview.

!`
DB="$HOME/.local/share/opencode/opencode.db"
DIR="$(pwd)"
ARG="$1"
THREE_HOURS_MS=10800000

sql_escape() {
  printf '%s' "$1" | sed "s/'/''/g"
}

# --- Resolve session ID ---

if [ -n "$ARG" ]; then
  ARG_SQL=$(sql_escape "$ARG")

  # 1. Exact session id
  SESSION_EXISTS=$(sqlite3 "$DB" "SELECT COUNT(*) FROM session WHERE id = '$ARG_SQL'")
  if [ "$SESSION_EXISTS" = "1" ]; then
    SESSION_ID=$(sqlite3 "$DB" "SELECT id FROM session WHERE id = '$ARG_SQL'")
  else
    # 2. Exact slug
    SESSION_EXISTS=$(sqlite3 "$DB" "SELECT COUNT(*) FROM session WHERE slug = '$ARG_SQL'")
    if [ "$SESSION_EXISTS" = "1" ]; then
      SESSION_ID=$(sqlite3 "$DB" "SELECT id FROM session WHERE slug = '$ARG_SQL'")
    elif [ "$SESSION_EXISTS" -gt 1 ]; then
      echo "Error: multiple sessions with slug '$ARG'" >&2
      exit 1
    else
      # 3. Exact title
      SESSION_EXISTS=$(sqlite3 "$DB" "SELECT COUNT(*) FROM session WHERE title = '$ARG_SQL'")
      if [ "$SESSION_EXISTS" = "1" ]; then
        SESSION_ID=$(sqlite3 "$DB" "SELECT id FROM session WHERE title = '$ARG_SQL'")
      elif [ "$SESSION_EXISTS" -gt 1 ]; then
        echo "Error: multiple sessions with title '$ARG'" >&2
        exit 1
      else
        # 4. Substring title match (warn if ambiguous)
        SESSION_EXISTS=$(sqlite3 "$DB" "SELECT COUNT(*) FROM session WHERE title LIKE '%' || '$ARG_SQL' || '%'")
        if [ "$SESSION_EXISTS" = "1" ]; then
          SESSION_ID=$(sqlite3 "$DB" "SELECT id FROM session WHERE title LIKE '%' || '$ARG_SQL' || '%' LIMIT 1")
        elif [ "$SESSION_EXISTS" -gt 1 ]; then
          echo "Warning: multiple title matches for '$ARG'. Using most recently updated." >&2
          SESSION_ID=$(sqlite3 "$DB" "SELECT id FROM session WHERE title LIKE '%' || '$ARG_SQL' || '%' ORDER BY time_updated DESC LIMIT 1")
        else
          echo "Error: no session found matching '$ARG'" >&2
          exit 1
        fi
      fi
    fi
  fi
else
  DIR_SQL=$(sql_escape "$DIR")
  THRESHOLD_SQL="(strftime('%s', 'now') * 1000 - $THREE_HOURS_MS)"

  MATCH_COUNT=$(sqlite3 "$DB" "SELECT COUNT(*) FROM session WHERE directory = '$DIR_SQL' AND time_updated > $THRESHOLD_SQL")

  if [ "$MATCH_COUNT" = "0" ]; then
    echo "Error: no session found for $DIR in the last 3 hours." >&2
    exit 1
  fi

  if [ "$MATCH_COUNT" = "1" ]; then
    SESSION_ID=$(sqlite3 "$DB" "SELECT id FROM session WHERE directory = '$DIR_SQL' AND time_updated > $THRESHOLD_SQL ORDER BY time_updated DESC LIMIT 1")
  else
    echo "Multiple active sessions found for $DIR (last 3h):" >&2
    echo "" >&2

    sqlite3 "$DB" "
      WITH recent_sessions AS (
        SELECT id, title, slug, time_updated
        FROM session
        WHERE directory = '$DIR_SQL'
          AND time_updated > $THRESHOLD_SQL
        ORDER BY time_updated DESC
        LIMIT 10
      )
      SELECT
        rs.id,
        rs.title,
        rs.slug,
        datetime(rs.time_updated / 1000, 'unixepoch', 'localtime'),
        COALESCE(
          (SELECT substr(json_extract(p.data, '\$.text'), 1, 80)
           FROM part p JOIN message m ON p.message_id = m.id
           WHERE m.session_id = rs.id
             AND json_extract(m.data, '\$.role') = 'assistant'
             AND json_extract(p.data, '\$.type') = 'text'
           ORDER BY m.time_created DESC, p.time_created DESC
           LIMIT 1),
          '(no preview available)'
        )
      FROM recent_sessions rs
      ORDER BY rs.time_updated DESC;
    " | while IFS='|' read -r id title slug updated preview; do
      echo "$title	[$slug]	$updated" >&2
      echo "  Preview: $preview" >&2
      echo "" >&2
    done

    echo "Run /copy-last <slug> or <session-id> to target a specific session." >&2
    exit 1
  fi
fi

if [ -z "$SESSION_ID" ]; then
  echo "Error: could not resolve session" >&2
  exit 1
fi

SESSION_ID_SQL=$(sql_escape "$SESSION_ID")

# --- Retrieve last assistant message text ---

TEXT=$(sqlite3 "$DB" "
  SELECT group_concat(json_extract(p.data, '\$.text'), '')
  FROM part p
  JOIN message m ON p.message_id = m.id
  WHERE m.session_id = '$SESSION_ID_SQL'
    AND json_extract(m.data, '$.role') = 'assistant'
    AND json_extract(p.data, '$.type') = 'text'
    AND m.id = (
      SELECT m2.id FROM message m2
      WHERE m2.session_id = '$SESSION_ID_SQL'
        AND json_extract(m2.data, '$.role') = 'assistant'
        AND EXISTS (
          SELECT 1 FROM part p2
          WHERE p2.message_id = m2.id
            AND json_extract(p2.data, '$.type') = 'text'
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
