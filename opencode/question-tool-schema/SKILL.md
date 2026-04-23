---
name: question-tool-schema
description: Reference for the opencode `question` tool JSON schema. Load before any interactive TUI dialog to avoid malformed invocations and repeated validation errors.
license: MIT
compatibility: opencode
---

## When to Use

- Before rendering an interactive choice dialog via the `question` tool
- After receiving a `question` tool schema validation error (checklist at bottom)
- When writing a story or script that includes a multi-choice user interaction

---

## Schema Reference

The `question` tool accepts a single top-level property `questions`, which is an array of question objects.

### Question Object Fields

| Field     | Type   | Required | Description                  |
|-----------|--------|----------|------------------------------|
| `header`  | string | yes      | Dialog title / header        |
| `question`| string | yes      | Prompt text                  |
| `options` | array  | yes      | Array of option objects      |

### Option Object Fields

| Field         | Type   | Required | Default | Description                        |
|---------------|--------|----------|---------|------------------------------------|
| `label`       | string | **yes**  | —       | Display text in the TUI dialog     |
| `value`       | string | **yes**  | —       | Value returned on selection        |
| `description` | string | no       | ""      | Optional subtitle/explanation      |

---

## Copy-Paste Template

```json
{
  "questions": [
    {
      "header": "Dialog Title",
      "question": "What do you want to do?",
      "options": [
        {
          "label": "Option A (Recommended)",
          "value": "a",
          "description": "This is the best choice"
        },
        {
          "label": "Option B",
          "value": "b",
          "description": "Alternative approach"
        }
      ]
    }
  ]
}
```

---

## Common Mistakes (The "Why It Failed" Checklist)

- [ ] **Missing `header` on question object** — `header` lives *inside* each question, not at the top level.
- [ ] **Options as strings** — `options` must be an array of **objects** (`{ label, value }`), not strings.
- [ ] **Missing `label`** — Every option object **must** include a `label` field. `description` and `value` alone are insufficient.
- [ ] **Missing `value`** — Every option object **must** include a `value` field.
- [ ] **Wrong nesting** — `questions` is the top-level array; each question contains `header`, `question`, and `options`.

---

## One-Liner Verification

Before calling the tool, mentally check: *"Does every option have both `label` and `value`? Is `header` inside the question object?"*
