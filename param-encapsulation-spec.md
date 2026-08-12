# Spec: parameter encapsulation and in-session query shaping

Branch: `param-encapsulation`. Temporary document, committed for the git trail and removed before merge.

## 1. Problem

`consult-hn` treats the consult input line as a CLI: `query -- tags=story author_pg numericFilters=points>100`. That was the original idea; the transient came later and now formats the same string and hands it to `consult-hn` as `:initial`. Two surfaces describe one set of parameters, bridged by a string that one layer prints and the other parses back.

Five consequences, each verified rather than assumed:

P1. Client-side narrowing is impossible. The input is already spoken for by the query and the parameter suffix, so there is nowhere to put a filter expression. Every keystroke re-queries the API and there is no way to filter what was already fetched.

Corrected in Phase 2, and it changes what section 9 has left to decide. The machinery is already there and always has been: `consult--read` wraps any async table with `consult--async-wrap`, which puts `consult--async-split` in front of the pipeline, so the session opens with `#` inserted and everything after a second `#` filters the fetched candidates rather than reaching the endpoint. Measured with the stub counting requests: `#emacs lisp#yet` narrowed fifteen candidates to the one whose comment carries the word, and issued no request at all. So what P1 really describes is that the parameter suffix made the free half of the input unusable, not that consult could not filter; with parameters out of the input, narrowing works today and Phase 3 has documentation and seeding to settle rather than a mechanism to build.

P2. Typing parameters fires garbage requests. `consult-hn--input->params` keeps any allowed key, so typing ` -- tags=story` sends `tags=s`, then `tags=st`, then `tags=sto`, each a syntactically valid request returning junk. Combined with unbounded pagination (below) each intermediate can spawn dozens of page fetches before cancellation.

P3. Unbounded pagination. The fetch chain recurses until `page+1 = nbPages` with no cap and no delay. Measured against the live API: `query=clojure` returns `nbPages 50` at the default `hitsPerPage 20`, so one settled query costs 50 chained requests for 1000 candidates. Requesting `hitsPerPage=100` returns the same coverage in 10 requests (also measured).

P4. The staleness guard is a tautology. `consult-hn--fetch-page-async` compares `(equal input expected-search)`, but the only caller passes `current-search`, which was just assigned the same `input`. Always true. Protection against stale pages rests entirely on killing request buffers, which loses the race against a response already being processed.

P5. The parameter surface is undiscoverable at the point of use, which is why the transient exists at all. Nothing validates values, and the query itself cannot contain ` -- `.

Separately, and the trigger for this work: the annotation is the entire comment filled at 120 columns, unbounded. Measured: 800 characters gives 7 lines, 1500 gives 13, 3000 gives 25. Vertico sizes its display by candidate count and never sees those lines, so fifteen candidates can demand 200+ screen lines; under `vertico-posframe` (auto-fit) the frame is swallowed.

## 2. Goal

The consult session is for the query and for narrowing. Parameters live in a state object, are shown compactly in the prompt, and are changed in-session by keys without leaving the session and without losing the results. The transient stays as an optional front door, rewritten to read and write the same state object rather than to print a string.

Non-goals: rewriting the lookup mechanism (section 11), changing the eww rendering, adding new HN API capabilities.

## 3. Constraints discovered

Verified against the installed transient 0.13.7 and consult 3.7. These are load-bearing; a design that ignores any of them will not work.

T1. A transient cannot be interactive while a minibuffer is active. Entering a minibuffer runs `transient--suspend-override`, which pops `transient--transient-map` and `transient--redisplay-map` off the keymap stack and removes its pre/post-command hooks. This is inherent, not incidental: a consult session needs plain self-inserting keys, and transient wants single letters for suffixes. The two cannot both own the keyboard.

T2. A transient can remain visible during a minibuffer read. `transient-show-during-minibuffer-read` accepts `t` or `fixed`, and applies only to suffixes that do not exit the menu. Visible, not interactive. This makes a transient-shaped legend possible later, but it can never be the live control surface.

C1. Input reaches the async pipeline only through an `after-change-functions` hook installed by `consult--with-async-f`, which calls `(funcall async (minibuffer-contents-no-properties))`. There is no public entry point for pushing input.

C2. `consult--async-throttle` ignores input equal to the previous input (`(unless (equal action input) ...)`). Therefore a parameter change with unchanged text cannot be propagated by re-sending the current input. The source must expose its own restart handle. This single fact determines the re-query protocol in section 8.

C3. Consult narrowing is single-axis: one `consult--narrow` value at a time. It cannot express type plus range plus author simultaneously. Additionally `consult-narrow-key` defaults to nil, so for most users of a published package no narrow keys exist at all. Narrowing may be offered as a bonus affordance but cannot carry the parameter set.

C4. The prompt is decorated with an overlay carrying `before-string` at `(1- (minibuffer-prompt-end))`. This is exactly how `consult-narrow` renders its `[Label]`, so the technique is proven and safe for filter chips. Note it is the same position narrowing uses; if narrowing is ever enabled here the two stack.

C5. Reading a value from inside the session needs `enable-recursive-minibuffers` bound non-nil around the read. Consult does this itself in two places via `consult--local-let`; our commands must do the same locally rather than setting it globally.

C6. Minibuffer buffers are reused, not killed (` *Minibuf-1*`). Any overlay we create must be deleted on session teardown or it leaks into the next unrelated minibuffer. Consult's own indicator both errors on double initialization and deletes on `destroy`. This class of bug cost hours in the sibling package and is not to be repeated.

Measured in Phase 2 on Emacs 31.0.91, and the second half of that is milder than it reads. Taking the teardown out does leave a chip overlay sitting in ` *Minibuf-1*` after the session, which the leak assertion catches. Taking out the install-time cleanup as well does not make it show up in the next prompt, and the count stays at one however many sessions run: entering a minibuffer clears the overlays left in it. So the hazard here is stale state after exit rather than a visible decoration in somebody else's prompt, and the assertion is written against what is actually observable.

C8. A menu that binds `RET` is not reachable from every buffer. Graphical Emacs sends `<return>`, which becomes `RET` only by translation through `local-function-key-map`, and only while nothing else claims it. Measured: a `keymap` text property binding `<return>` beats even the map transient pushes onto `overriding-terminal-local-map`, so in a buffer that decorates its prompt that way, which is what chat and REPL style buffers do, the menu reports `Unbound suffix: <return>` and its only action is unreachable. The same text property loses to a map that binds `<return>` itself. Hence the hidden group binding both spellings; `:hide` keeps a group out of the display without unbinding it, which is the mechanism transient uses for its own common commands.

C7. The pipeline handed to `consult--read` is not the whole pipeline. `consult--async-wrap` puts `consult--async-split` in front of it and `consult--async-indicator` plus `consult--async-refresh` behind it. Two consequences, both load-bearing: a stage that swallows an action instead of passing it on breaks the stages it hides, which is how `destroy` was leaving consult's own indicator overlay behind until Phase 2; and the split stage is what makes section 9 a question about defaults rather than about mechanism.

## 4. Design decisions

D1. Parameters leave the input line entirely. The input carries the query and, optionally, a narrowing expression. Rationale: it is the precondition for P1, and it removes P2 by construction, since a parameter can only change through a command that validates it.

D2. One state object is the single source of truth. A plist held in `consult-hn--params`, snapshotted into the session at open and mutated in place by session commands. The transient reads its initial values from it and writes back into it. No CLI string is produced or parsed anywhere in the normal path. Rejected: keeping the string as an internal representation, because that is precisely the lossy bridge this work removes.

D3. The session owns the keyboard; the parameter surface is a prompt indicator plus session bindings. Forced by T1. Rejected: keeping the transient live during the session, which is impossible; and driving parameters through consult narrowing, which C3 rules out.

D4. Parameter commands live in a package-owned keymap passed as `:keymap` to `consult--read`, under a prefix so `which-key` documents them for free. Not narrowing keys, since those are unbound for most users (C3). Each command mutates the state and calls the session restart handle.

The prefix is `C-c`, and the keys are `t` type, `a` author, `p` points, `c` comments, `r` range, `f` front page, `u` url matching, `s` sort. `C-c` is unbound in `minibuffer-local-map`, and the composed keymap `consult--read` builds merges prefixes rather than replacing them, so a user who already has minibuffer bindings under `C-c` keeps them; checked against a real configuration where `C-c '` and `C-c C-s` are taken, and both still resolved inside a session. Three of the commands cycle, two toggle, three read a value.

D5. The source exposes a restart handle stored in a session variable while the session is alive. Forced by C1 and C2. The handle performs the full restart sequence in section 8. Rejected: re-sending input to the pipeline head, which the throttle discards.

D6. Active parameters render as a compact chip string in the prompt via a `before-string` overlay (C4), created at session setup, updated on every parameter change, and deleted on teardown (C6). Example: `HN [story · pg · >100p · 7d] Search: `. Chips are the only mid-session status surface in this phase; a transient-shaped legend is deferred (T2 keeps the door open).

D7. Pagination becomes bounded and cheaper. `hitsPerPage` defaults to 100 and a `consult-hn-max-pages` defcustom caps the chain, measured five-fold request reduction for identical coverage (P3). A page delay is deliberately not added: the HN Algolia endpoint is generous and, unlike the sibling package's Slack endpoint, has no punitive tier.

D8. Staleness is guarded by a generation counter, not by string comparison. The source increments a counter on every restart; a page callback drops its results unless its captured generation still matches. Buffer killing stays as the cancellation mechanism but is no longer the only defence (P4).

D9. Results are deduplicated by `objectID` per chain. `search_by_date` paginates over a moving window, so an item can appear on two pages when new items arrive mid-chain. Cheap insurance, one hash per chain.

D10. The annotation is capped and cached. A `consult-hn-max-comment-lines` defcustom (default 2 or 3, to be settled by eye) with an ellipsis at the cut, plus session-local `vertico-count` scaling by the per-candidate line footprint, exactly as landed in the sibling package. The inline `:annotate` lambda is extracted into a named function, which also makes it testable; it currently is not. Filled text is cached per candidate, since today it is recomputed for every visible candidate on every keystroke.

D11. Input semantics are settled in section 9 and are the one genuinely user-visible behavioural choice. Recorded there rather than here because it needs a decision, not a rationale.

D12. The `--` syntax keeps being parsed when present, is removed from the documentation, and gains no deprecation message (no echo-area chatter). Programmatic callers get keyword arguments. Rationale: the package is on MELPA and someone may have it in a keybinding; silent compatibility costs one branch in one function.

## 5. Parameter model

| Parameter | Type | API mapping | Default | Chip |
|---|---|---|---|---|
| query | string | `query=` | none | not a chip, it is the input |
| type | `story`, `comment`, `all` | `tags=story` / `tags=comment` | `all` | `story`, `comment` |
| author | string or nil | `tags=author_NAME` | nil | `NAME` |
| points | integer or nil | `numericFilters=points>N` | nil | `>Np` |
| comments | integer or nil | `numericFilters=num_comments>N` | nil | `>Nc` |
| range | `24h`, `week`, `month`, `year`, `all` | `numericFilters=created_at_i>T` | `all` | `24h`, `7d`, `30d`, `1y` |
| front-page | boolean | `tags=front_page` | nil | `front` |
| url-match | boolean | `restrictSearchableAttributes=url` | nil | `url` |
| sort | nil, `date`, `relevance` | endpoint `search_by_date` or `search` | nil, meaning infer | `rel` when relevance, `date` when dated |

Two notes on fidelity to the current behaviour. Sort is currently inferred from the presence of `front_page` in tags, which conflates two independent things; it becomes an explicit parameter whose default reproduces today's behaviour. Multiple `numericFilters` are joined with a comma, as the transient already does.

Settled while building Phase 1, since the two above are in tension. `:sort` nil means infer, and the inference reads the tags actually going out rather than the state, so the legacy `tags=front_page` reaches the relevance endpoint exactly as it does today. `date` and `relevance` are explicit and override the inference. Rendering order is the order of this table: tags are joined `type,author,front_page` and numeric conditions `points,num_comments,created_at_i`. The endpoint treats both as unordered sets, so the order is for legibility and for specs that can compare whole strings.

`hitsPerPage` and `page` are internal and never user-facing parameters, though `hitsPerPage` gets a defcustom for people on slow links.

Chip vocabulary settled while building Phase 2, which is Q2 answered. The table's column is the vocabulary, `date` joins `rel` for an explicitly dated sort, the separator is a middle dot with spaces, and the whole thing is wrapped as ` [a · b · c]` and given the `consult-narrow-indicator` face, since it is the same class of prompt decoration in the same position. A state that constrains nothing renders the empty string, so nothing about the prompt changes for someone who never touches a parameter. `consult-hn--params-chips` is the only place that knows any of this.

Parameters are layered, and the ladder is: the legacy ` -- key=value` suffix beats the state object, which beats `consult-hn-default-search-params`, which beats the page size this package would otherwise ask for. The first rung is what keeps a hand-written keybinding authoritative; the last is what keeps `hitsPerPage` in someone's `consult-hn-default-search-params` working.

## 6. State object and ownership

`consult-hn--params` holds the plist. It persists between invocations, so a session inherits what the last one used, which is what makes in-session shaping feel continuous rather than modal.

The transient becomes a renderer and editor of this plist: infix initial values are read from it, and each infix writes back on set. Consequence worth stating plainly: after this change the transient no longer produces a query string, and `consult-hn-transient-action` simply opens the session.

Built in Phase 2 with one deliberate departure, which is that the menu writes back when it searches rather than as each infix is set. Write-on-set costs a class per infix class and a `transient-infix-set` method to reach into the plist, against R1's instruction to keep the menu thin, and it buys only that abandoning the menu with `C-g` still changes the state. Writing on search is the plainer rule, nothing happens until you search, and it leaves the two pure functions the gate asks for as the whole of the bridge. Q4 is answered as optional front door: `consult-hn` is the command, the menu is a way to compose a search before opening one, and everything it does can be done from inside the session.

Programmatic entry: `(consult-hn &optional query &rest params)` where params are keywords matching section 5, merged over the persisted plist for that call only. The interactive path passes none.

## 7. Session lifecycle

1. `consult-hn` snapshots `consult-hn--params`, seeds the input per section 9, and opens `consult--read` with the package keymap, the chips overlay installed from `minibuffer-with-setup-hook`, and the async pipeline.
2. The pipeline is `split → min-input → throttle → source → indicator → refresh`, matching the sibling package. The split stage is only present if section 9 resolves to a split-based design.
3. On setup the source registers its restart handle in a session variable and initialises generation, dedup set and page counter.
4. On teardown (`destroy`) the source cancels timers, kills in-flight request buffers, clears the restart handle, and the chips overlay is deleted (C6).

## 8. Re-query protocol

Triggered by any parameter command, and by a query change if section 9 makes the query a server-side concept.

1. Increment the generation counter.
2. Cancel any pending page timer; kill in-flight request buffers.
3. Clear the dedup set.
4. Flush the sink, so the previous result set disappears at once rather than mixing.
5. Fetch page 1 with parameters rendered from the current state, learning `nbPages` from the response and continuing up to the cap (D7).
6. Every callback compares its captured generation against the current one and drops silently on mismatch (D8).
7. Update the chips overlay.
8. Results flow through the existing `indicator` and `refresh` stages downstream of the source, so the completion UI updates without additional work.

## 9. Input semantics, decision required

With parameters gone, the input line is free. Two coherent options, and this is a genuine behaviour change either way.

Option A, consistent with the sibling package: the session opens with `#` pre-inserted; text after `#` is the server query; a closing `#` switches to client-side narrowing over everything fetched. Plain input with no leading `#` narrows. Only `#` acts as a separator, never arbitrary punctuation, because HN queries legitimately contain `@`, `:` and URLs. Argument for: a broad query pulls up to a thousand candidates and filtering them locally is free and instant; consistency with the sibling package the same person maintains.

Option B, status quo preserved: plain typing remains the server query, and a separator introduces the narrowing expression. Argument for: no relearning for existing users, and it keeps the mental model that what you type is what HN searches, which matters because narrowing over a fetched subset can imply completeness that is not there.

Recommendation: Option A, for symmetry and because P1 is the complaint that started this. Not implemented until confirmed.

What Phase 2 measured changes the shape of the decision, though not the recommendation. Option A is very nearly what already happens: `consult--async-wrap` installs the split stage for every async table, so the session opens with `#` sitting in the input, everything up to a second `#` is the server query, and everything after it filters the fetched candidates without touching the network. That was verified with the stub counting requests, narrowing fifteen candidates to one on a word that only appears in a comment, at a cost of zero requests. So Phase 3 is not building a split; it is deciding whether to keep consult's default seeding, saying so in the readme, and pinning the behaviour with the e2e scenario that freezes the request counter across typing. Option B, by contrast, now means deliberately turning off machinery that is already on.

## 10. Backwards compatibility

Retained: the ` -- key=value` input syntax is still parsed when present, undocumented and unwarned (D12). `consult-hn-default-search-params` continues to work, merged under the state object.

Changed: `consult-hn` gains keyword arguments; `consult-hn-transient--format-query` disappears along with the string bridge; `consult-hn-initial-input-string` becomes redundant if Option A is chosen and should be deprecated rather than removed.

## 11. Explicitly out of scope

The lookup mechanism, which rebuilds candidates by regex-parsing the displayed row on runs of three or more spaces. It survives today only because titles are space-normalised. The sibling package's invisible-tail plus `consult--lookup-member` is strictly better, but it is a self-contained refactor with its own risk and should not ride along with a UX redesign.

## 12. Function inventory

New: `consult-hn--params-render` (state to API alist), `consult-hn--params-chips` (state to prompt string), `consult-hn--chips-install` / `--chips-update` / `--chips-remove`, `consult-hn--restart`, `consult-hn--annotate`, `consult-hn--scale-vertico-count`, one command per parameter, `consult-hn-session-map`.

Changed: `consult-hn` (keyword args, keymap, setup hook, seeding), `consult-hn--async-source` (restart handle, generation, dedup, page cap), `consult-hn--fetch-page-async` (generation guard, params from state), `consult-hn--input->params` (thin compatibility adapter), `consult-hn--process-results` (carry `objectID` for dedup), the whole of `consult-hn-transient.el` (reads and writes the state object).

Removed: `consult-hn-transient--format-query`.

As built in Phase 1, where the inventory above turned out to want more seams. New: `consult-hn--params-render`, `consult-hn--params-tags`, `consult-hn--params-numeric-filters`, `consult-hn--params-endpoint`, `consult-hn--api-url`, `consult-hn--request-url` (input plus state to URL), `consult-hn--input-split`, `consult-hn--legacy-pairs`, `consult-hn--dedup`, `consult-hn--nonblank`. Removed: `consult-hn--nb-pages`, a defvar every caller shadowed with a `let`, so the global was written and never read.

`consult-hn--input->params` came out of Phase 1 with its behaviour pinned by its specs and no caller left inside the package: the fetch path takes the legacy suffix through `consult-hn--legacy-pairs` instead, because the whole-input version merges `consult-hn-default-search-params` in a way that would let a default outrank the state. It is the compatibility surface and nothing else. Phase 2 removes the last producer of the string, which is the moment to decide whether the parser goes too.

As built in Phase 2, steps 2.1 to 2.5. New: `consult-hn--restart`, a variable holding the live session's way back into the pipeline, set on the source's `setup` and cleared on its `destroy`; `consult-hn--params-chips`; `consult-hn--chips-install`, `--chips-update`, `--chips-remove` and `consult-hn--chips-overlay`; `consult-hn--session-setup`, which is the one minibuffer setup hook and does the height scaling and the chips; `consult-hn--param-set`, `consult-hn--cycle`, `consult-hn--read`, `consult-hn--read-threshold`; `consult-hn-session-map` and the eight commands `consult-hn-session-type`, `-author`, `-points`, `-comments`, `-range`, `-front-page`, `-url-match`, `-sort`; `consult-hn--params-merge` and `consult-hn--param-keys` for the keyword arguments; and `consult-hn--params-searchable-p`, which is what lets an author or a front page carry a search with no query at all, since without it a parameter set in an empty session would silently fetch nothing. Changed: `consult-hn--async-source` remembers its input, exposes the restart, and passes `setup` and `destroy` on to the stages downstream instead of swallowing `destroy`; `consult-hn` takes `(&optional query &rest params)` and holds the merged state for the call only. Step 2.6 and the fate of `consult-hn--input->params` are untouched, both waiting on Q4.

As built in step 2.6. New in `consult-hn-transient.el`: `consult-hn-transient--arguments`, the one alist saying how each parameter is spelled; `consult-hn-transient--value` and `consult-hn-transient--args` for state to menu; `consult-hn-transient--arg-value` and `consult-hn-transient--params` for menu to state; `consult-hn-transient--init`, which is the `:init-value` every infix is seeded by; and a named infix per parameter, since a top-level `transient-define-argument` evaluates its slots where an inline suffix specification leaves that to transient's parser. Removed: `consult-hn-transient--format-query` and `consult-hn-transient--url-matching-enabled`, the string bridge and the global that fed it. The one behaviour that global carried is kept as a rule in `consult-hn-transient--params`: a query that is a URL means matching on URLs, which is what makes pasting a link find the discussion of it.

`consult-hn--input->params` is gone, and its specs with it. Nothing in the package called it after Phase 1 and nothing produces the string it parsed after 2.6; the legacy ` -- key=value` input syntax is untouched, since the fetch path reads it through `consult-hn--legacy-pairs`, which now has specs of its own carrying the cases the removed ones pinned.

Not in the inventory and worth saying plainly: no reset command. Cycling reaches the default, an empty answer clears an author or a threshold, and the chips say what is set, so a separate reset would be a fourth way to do what three already do.

## 13. Testing

Unit, extending the existing 33 specs: parameter rendering for every row of section 5 including combinations; chips string for empty, single and full states; the compatibility adapter for the legacy string; generation guard drops stale pages; dedup drops repeats; page cap honoured; annotation cap and ellipsis; `vertico-count` scaling including the already-buffer-local case; restart handle sequence.

End to end, following the harness that proved itself in the sibling package: a real interactive `emacs -nw -Q` under a PTY with real consult, vertico and embark from a sandbox, the HN API stubbed at the `url-retrieve` seam, scenarios driven through the actual command loop with queued key events, the suite run twice in-process to prove no state leaks, and a watchdog against hangs. Scenarios: streaming with dedup and cap; a parameter command re-queries and replaces results; chips update and are gone after exit; narrowing sends no requests; recursive read for author does not corrupt the session; abort mid-stream stops the chain; the legacy string still works; ` *Minibuf-1*` intact throughout.

One thing the harness had to grow for D8, worth knowing before writing the Phase 2 scenarios. Killing the request buffer is a complete defence inside the stub, because delivery is a timer that checks `buffer-live-p` and Emacs runs it to completion between commands: a mid-stream re-query alone therefore proves nothing about the generation guard. What killing cannot reach is a response arriving in a buffer the caller never received, which is what a redirect does, and the stub now models exactly that under `consult-hn-e2e--detached-page`. The scenario carries its own control, asserting the retired response was in fact delivered, and it fails with the guard removed.

The existing `make test` target is broken in a way worth fixing while here: it calls plain `package-initialize` against the user's package directory, so it fails locally with `void-function buttercup-run-discover` and only works on CI.

As built through Phase 2: eleven scenarios, sixty-six checks a round. Streaming with dedup and cap, the annotation cap, the height budget, teardown, the page cap, the stale chain, the restart handle, the chips lifecycle, a parameter command through the real command loop, the recursive read for an author, and a session opened with keyword arguments from Lisp. Still owed, and owed to Phase 3: narrowing sending no requests, which the throwaway probe measured but the suite does not yet hold.

## 14. Risks

R1. The transient rewrite is the largest single piece and has no test harness today. Mitigation: pure functions for state to arguments and back, tested directly; the transient itself stays thin.

R2. Chips overlay leaking into unrelated minibuffers (C6). Mitigation: explicit teardown plus an e2e assertion that ` *Minibuf-1*` carries no overlay of our category after the session.

R3. Behaviour change for published users (section 9, D12). Mitigation: silent compatibility for the legacy syntax, and a changelog that leads with the change rather than burying it.

R4. Scope creep into the lookup rewrite (section 11).

## 15. Phasing

Four phases, each ending green and committable. Phase 0 is independent and can be merged on its own; phases 1 to 3 are sequential.

Phase 0, defects and height. Obsolete symbols that break `make check-compile` on Emacs 31 (`any` in rx twice, `if-let`, `dom-texts`; the last needs `with-suppressed-warnings` rather than a swap, since `dom-inner-text` arrived in 31.1 and drops the separator). The error handler that itself errors, `(concat "HN parse error: " err)` on an error object. Annotation extraction, cap, cache and `vertico-count` scaling (D10). Acceptance: `make check-compile` clean on 31, unit suite green with new specs, minibuffer height bounded in a live session.

Phase 1, engine. Parameter state object and rendering, page cap and `hitsPerPage` (D7), generation guard (D8), dedup (D9), compatibility adapter (D12). No visible UI change beyond fewer requests and no stale results. Acceptance: unit specs for every mapping and guard, live session verified to issue the expected request count for a known broad query. Done: 91 unit specs, 30 e2e checks over six scenarios run twice, and a live run of `query=clojure` costing 10 requests for 1000 distinct items where the same coverage used to cost 50. `consult-hn-max-pages` defaults to 10, which is what the endpoint offers at 100 hits a page, so the default cap changes no result set and only the pathological cases feel it.

Phase 2, session UI. Keymap and parameter commands, chips overlay, restart handle wiring, transient rewritten onto the state object. Acceptance: e2e scenarios for parameter re-query, chips lifecycle, recursive read, plus the leak assertion. Steps 2.1 to 2.5 done: 120 unit specs and 66 e2e checks over eleven scenarios, run twice, plus a live run against the real endpoint in a real interactive Emacs where `C-c a` read an author, the results came back authored by them alone, the chips said so, and the session closed leaving nothing behind. Three defences were shown to discriminate by removing them and watching the suite go red: the chips teardown, the local binding of `enable-recursive-minibuffers`, and, before it was fixed, the swallowed `destroy`. Step 2.6 followed once Q4 was answered, and Phase 2 is complete: 124 unit specs and 73 e2e checks over twelve scenarios, the last of which drives the real menu on the keys a user presses, sets the type, searches, and asserts the session came up under it. That scenario discriminates too: breaking the menu's reading of its own arguments turns three of its checks red. The menu was also run against the transient Emacs 29.4 and 30.1 bundle, 0.4.3 and 0.7.2.2, since the suite now loads that file for the first time on CI.

Phase 3, input semantics and documentation. The section 9 decision, split style if Option A, readme and changelog, e2e for narrowing sending no requests.

Recommendation on session splitting: not one session. Phase 0 is comfortably one. Phase 1 is one. Phase 2 is the largest and may itself want two, one for the session UI and one for the transient. Phase 3 is one, and can absorb the e2e harness construction if it is not built in Phase 2. Rate limits and context exhaustion are the practical argument as much as the size is; each phase is designed to end at a commit so an interrupted session resumes cleanly.

## 16. Open questions

Q1. Section 9, Option A or Option B.
Q2. Chip vocabulary and separator, `[story · pg · >100p · 7d]` as proposed or something terser.
Q3. Whether Phase 0 merges to main on its own ahead of the redesign, which I would recommend since it fixes a broken local build.
Q4. Whether the transient should remain the primary entry point, or become optional with the session as the main command. Answered: optional. The session shapes parameters itself now, so the menu is a way to compose a search before opening one rather than the way in.
