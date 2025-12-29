# Session Start - Bootstrap with Context

## Step 1: Learn Basic Buffer Access (MCP Docs Essentials)
**First, read just enough to access Dashboard & Daily Focus:**

Call: `skewed-emacs:skewed-emacs__get_docs(id="claude-md")`

**Focus on these sections only (skip the rest for now):**
- "Basic MCP Usage Examples" → Buffer Operations
- "How to access Emacs state"
- Look for examples like: `(switch-to-buffer "*dashboard*")` and `(buffer-string)`

You need ~5 minutes of elisp confidence to read buffers. Don't get overwhelmed by paredit-mode/editing yet.

## Step 2: Review Dashboard & Daily Focus (D&D)
**Now use your basic elisp skills to check current context:**

**Dashboard** (shows environment status, services, recent activity):
```elisp
(with-current-buffer "*dashboard*" (buffer-string))
```

**Daily Focus** (shows priorities, deadlines, active work):
```elisp
(progn
  (org-agenda nil "d")
  (with-current-buffer "*Org Agenda*" (buffer-string)))
```

The Daily Focus shows Must/Should/Could priorities. This tells you what's in flight.

## Step 3: Complete MCP Training (Informed by D&D)
**Now read the full skewed-emacs MCP docs:**

Re-read: `skewed-emacs:skewed-emacs__get_docs(id="claude-md")` - this time completely

**Focus on sections relevant to D&D priorities:**
- File editing (paredit-mode for Lisp files!)
- Detecting unbalanced buffers
- Safe editing patterns
- Shared buffer state warnings

**If D&D mentions GDL/Gendl work, also read:**
```
genworks-gdl-enterprise-smp:genworks-gdl-enterprise-smp__get_docs(id="claude-md")
```

## Step 4: Present Options to Dave
Based on D&D priorities and available services (shown in Dashboard), present Dave with:

1. **What to clock into** - list Must Do items with brief context
2. **Available services** - which MCP/SWANK services are ready
3. **Suggested next steps** - informed by Daily Focus priorities
4. **Questions** - anything unclear about the current task

Format like:
```
Current Daily Focus:
  Must Do:
    - [Task 1] - Ready to start (services available: X, Y)
    - [Task 2] - Needs clarification on [Z]
  
Available Services:
  - genworks-gdl-enterprise-smp ✓
  - skewed-emacs ✓
  
Recommended: Clock into [Task 1] because [reason based on D&D]
Questions: [any blockers or clarifications needed]
```

## Quick Reference
- **projects.org location**: `/home/emacs-user/projects/org/projects.org` (NOT `/projects/projects.org`)
- **Navigate via agenda**: Use `org-agenda-goto` from *Org Agenda* buffer to jump directly to task headings
- **D&D = Dashboard & Daily Focus** (your context source)
- **MCP docs first** (basic skills before attempting work)
- **Paredit-mode mandatory** for Lisp file editing
- **Check balance** before editing any s-expression files

**Start by reading just the buffer access sections of the MCP docs, then review D&D.**