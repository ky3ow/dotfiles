# Interaction preferences

1. To ask questions inbetween actions use `questionnaire` tool. When i mention "ask me" in prompts, it is also about using the tool.

2. **Don't assume the tech stack.** If unsure what's in the current directory, just list files (`ls -la`) or check relevant config files before making assumptions about language, tooling, or project structure.

3. **No unexpected modifications.** Don't edit files or invoke modifying commands unless I asked for it or I'm clearly anticipating it (e.g., you proposed a change, I said go ahead — I anticipate those changes). Reading, searching, and reviewing are always fine. This does not apply to creating throwaway scripts in `/tmp` — those are fine and encouraged (e.g., for verifying logic, testing edge cases).

4. **Don't mindlessly debug.** If the first 1-2 most logical fixes don't work, stop trying on your own. Talk to me — propose vectors to try, but let me have my say and ask clarifications.

5. **Background / persistent processes.** Don't start servers, watchers, or anything meant to keep running without proposing it first. If I approve, use `tmux` — spawn everything into named windows under a single session called `pi`:
   ```
   tmux new-session -d -s pi -n dev-server <process>   # first process
   tmux new-window -t pi -n file-watcher <process>     # second, etc.
   ```
   To see what's running: `tmux list-windows -t pi`. To kill everything: `tmux kill-session -t pi`.

6. **Reason through concepts, verify logic in code.** Don't simulate execution step-by-step in text — that's unreliable for any non-trivial logic (loop bounds, tokenization, parsing, state transitions, etc.). Instead, reason about *what* the expected behavior should be, then write a quick script to actually verify it.
