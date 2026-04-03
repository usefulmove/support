---
name: explain-code
description: Explain unfamiliar code to an accountable engineer. Builds mental models through purpose-driven, top-down explanation with data flow visualization. Use this when the user needs to understand what code does, why it exists, and how it fits into the larger system — not just what changed.
license: MIT
compatibility: opencode
---

## When to Use

Use this skill when the user needs to:
- Understand code they're accountable for
- Learn a new codebase or subsystem
- Build a mental model before making changes

## The Explanation Structure

Present code explanations in this order:

### 1. Purpose (Why)
- What problem does this solve?
- Who uses it and when?
- What would break if it didn't exist?

### 2. High-Level Function (What)
- One-sentence summary
- Key inputs and outputs (concepts, not types)
- Where it sits in the system

### 3. Data Flow (Visual)
- ASCII or bullet diagram
- Start from source (hardware, user, external system)
- End with destination
- Label boundary crossings (HTTP, WebSocket, function call)

### 4. Step-by-Step Walkthrough (How)
- Main method/function flow
- Key state transitions
- What each step produces for the next

### 5. Key Design Decisions
- Why this approach? What are the tradeoffs?
- Optimizations and their conditions
- Connect back to Purpose

---

## Example

### Input Code
```cpp
// WebSocket handler that echoes messages back
class EchoServer : public websocketpp::server<websocketpp::config::asio> {
public:
    void on_message(connection_hdl hdl, message_ptr msg) {
        std::string payload = msg->get_payload();
        
        // Log incoming
        std::cout << "Received: " << payload << std::endl;
        
        // Echo back with prefix
        std::string response = "Echo: " + payload;
        send(hdl, response, websocketpp::frame::opcode::text);
    }
};
```

### Explanation

**1. Purpose**
EchoServer lets clients verify their WebSocket connection works by sending any message and receiving it back. Used during development and for connection health checks.

**2. High-Level Function**
When a message arrives, log it, prepend "Echo: ", and send it back to the same client.

**3. Data Flow**
```
Client (browser/terminal)
    │
    │ WebSocket text frame
    ▼
EchoServer::on_message()
    │
    ├──► Console log (side effect)
    │
    └──► send() with modified payload
              │
              ▼
         Client receives response
```

**4. Step-by-Step**
1. `connection_hdl hdl` — identifies which client sent the message
2. `msg->get_payload()` — extracts the text content
3. Log to stdout for debugging visibility
4. Prepend "Echo: " to create response
5. `send(hdl, ...)` — routes response back to that specific client

**5. Design Decisions**
- **Synchronous send**: No queue or async handling because echo is fast and order matters
- **Per-client routing**: `hdl` ensures response goes to sender, not broadcast
- **Text opcode preserved**: Echo maintains the same message type (text vs binary)

---

## Checklist

Before explaining:
- [ ] Read the class/function signature
- [ ] Trace the main execution path
- [ ] Identify who calls this
- [ ] Map data boundaries
- [ ] Find optimization points
