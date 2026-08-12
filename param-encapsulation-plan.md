# Execution plan: parameter encapsulation

Companion to `param-encapsulation-spec.md`. The spec says what and why; this says how, with the environment facts and acceptance gates. Branch `param-encapsulation`. Both documents are temporary and get removed before merge.

Phases 0, 1 and 2 are done. Phase 3 remains, and is smaller than section 4 makes it sound.

State of play: work happens at the tip of `param-encapsulation`, which is ahead of `main` and has never been pushed; Phase 1 landed in `d8028fc` and Phase 0 in `23f5a27` and `c9558fc`, so `git log 23f5a27~1..` is the whole story. Confirm all three gates below on arrival before building anything on top of them, and if one is red, say so instead of working around it.

## 0. Environment, read this before running anything

The repo's own tooling is partly broken and the sandboxes are not obvious. These invocations are verified working as of Phase 0.

Unit tests. `make test` does NOT work locally: it calls plain `package-initialize` against the user's package directory and dies with `void-function buttercup-run-discover`. It only works on CI. Use:

```sh
emacs -Q --batch \
  -L /Users/ryl/GitHub/agzam/slacko.el/.elpa/buttercup-20260512.2141 \
  -L /Users/ryl/.emacs.d/.local/elpaca/builds/consult \
  -L /Users/ryl/.emacs.d/.local/elpaca/builds/ts \
  -L /Users/ryl/.emacs.d/.local/elpaca/builds/dash \
  -L /Users/ryl/.emacs.d/.local/elpaca/builds/s \
  -L . -L test --eval "(require 'buttercup)" --funcall buttercup-run-discover 2>&1 | rg "Ran|failed"
```

Buttercup is borrowed from the sibling repo's sandbox because consult-hn has no `.elpa`. Fixing `make test` is a Phase 3 item; do not fix it incidentally in Phase 1 or the diff stops being reviewable.

Byte-compile as CI does, which is the gate that was red before Phase 0:

```sh
emacs -Q --batch -L <consult> -L <ts> -L <dash> -L <s> -L . \
  --eval "(setq byte-compile-error-on-warn t)" \
  --eval '(unless (and (byte-compile-file "consult-hn.el") (byte-compile-file "consult-hn-transient.el")) (error "COMPILE FAILED"))'
rm -f *.elc
```

Do NOT put the elpaca build of transient on the load path when compiling: that is transient 0.13.7, which needs `cond-let` and `llama`. Emacs ships a transient that compiles the file fine, and that is what CI uses.

End to end: `make e2e` (sandbox built once by `make e2e-deps` into `.elpa-e2e`, which holds consult, vertico, orderless, ts). It runs a real `emacs -nw -Q` under a PTY, twice in one process, and prints PASS/FAIL lines. Run it twice in a row before believing it.

Live verification in the user's running Emacs, through the `elisp-eval` MCP, is a required level, not a bonus. Phase 0's only serious bug was invisible to both the unit suite and the e2e stub and only appeared against the real API. After editing, always `load-file` the source and recompile the elpaca build at `~/.emacs.d/.local/elpaca/builds/consult-hn/consult-hn.el`, which is a symlink to the repo, or the user's next restart silently reverts to a stale `.elc`.

## 1. The e2e harness contract

`test/consult-hn-e2e.el` exists and Phase 1 extends it rather than reinventing it.

- Fixtures: `consult-hn-e2e--pages`, three pages of five hits, page 0 carrying `consult-hn-e2e--long-comment`, which ends in `ENDMARKER`. The marker must never reach the display; that is how the annotation cap is proven.
- The only stub is `url-retrieve`, overridden by `consult-hn-e2e--url-retrieve`. It records the URL, builds a buffer with real HTTP headers and a `url-http-end-of-headers` marker, and delivers via a 0.03s timer that checks `buffer-live-p` first, which is exactly how cancellation behaves in production.
- Any request that is not to `hn.algolia.com` fails the suite. The browse and preview seams are stubbed through `consult-hn-preview-fn` and `consult-hn-browse-fn`, because previewing otherwise calls `eww` and hits the network.
- `consult-hn-e2e--safe-call` wraps every step: a signalling step reports at the point of failure instead of silently killing the timer chain and surfacing 90 seconds later as a watchdog timeout.
- Assertions must observe what redisplay produced. The height probe reads the `before-string` of `vertico--candidates-ov`, which is the material `vertico--display-candidates` hands the display engine while sizing the window from `(length lines)`, the candidate count. Never call into the code under test to produce the state being asserted.

Adding a scenario: write `consult-hn-e2e--scenario-NAME (k)`, ending in `(funcall k)`, and add it to `consult-hn-e2e--scenarios`. Scenarios run in order and the middle ones observe the session the first one opened. A scenario added after the teardown one opens and closes its own session, as the cap and stale-chain scenarios do.

Phase 2 grew it again, in three places worth knowing before adding a twelfth scenario. `consult-hn-e2e--tagged-pages` serves a page whose titles are the tags the request carried, so a parameterised search is visible in the display and the display says which parameter it came back for; anything asserting that a parameter reached the endpoint and came back should use it rather than counting requests. `consult-hn-e2e--reset` now also restores `consult-hn--params` from `consult-hn-e2e--default-params`, because the state outlives a session by design and without that the scenarios, and then the second round, would inherit each other's parameters. And the probes `consult-hn-e2e--chips` and `--stale-chips` read the chip overlay out of the buffer by its category rather than out of the package's variable, so they observe what redisplay is handed, and the second one reads ` *Minibuf-1*` after the session is over.

There is also a live harness that is not in the repo, left at `/tmp/consult-hn-live/`. It boots the same sandbox and reuses the e2e plumbing but installs no stub, so every request goes to hn.algolia.com, and it drives the session with real keys. That is what verified Phase 2 against reality, and it is cheaper to rebuild than to explain: boot file, a scenario, `consult-hn-e2e--run-scenarios` with `consult-hn-e2e--finish`, and `consult-hn-e2e-results-file` pointed somewhere outside the repo. The same directory holds the throwaway probe that measured client-side narrowing for Phase 3.

Phase 1 grew the harness in three places. The fixtures now serve sixteen hits for fifteen items, since hit 3 sits on two pages, which is what makes every candidate count in the suite a dedup assertion as well. The stub serves a second fixture set, titled `Other story`, for `query=rust`, so a re-query is visible in the display rather than merely counted. And `consult-hn-e2e--detached-page` makes a page answer from a buffer the caller never received, modelling a redirect; it is the only shape of response that cancelling cannot reach, and therefore the only way the generation guard can be observed from the outside. Anything asserting cancellation should use it, and should carry a control proving the response was delivered at all, or the assertion passes for the wrong reason.

## 2. Phase 1, the parameter engine

No user-visible UI change. Fewer requests, no stale results, and multi-word behaviour preserved.

Step 1.1, the state object. `consult-hn--params`, a plist with the keys in spec section 5 (`:query :type :author :points :comments :range :front-page :url-match :sort`). Add `consult-hn--params-render (params &optional page)` returning the API alist that `url-build-query-string` consumes. Rules that must hold, and each wants a spec:

- Values go in raw. `url-build-query-string` hexifies. Phase 0 removed a double encoding here; do not reintroduce it.
- `:type` maps to `tags=story` or `tags=comment`, `:author` to `tags=author_NAME`, joined by comma when combined with front-page.
- `:points`, `:comments` and `:range` join into one `numericFilters` value separated by commas, exactly as the transient does today.
- `:sort` chooses the endpoint, `search_by_date` or `search`. Today the endpoint is inferred from `front_page` appearing in tags; the default must reproduce that behaviour.

Gate: unit specs for every row of the table plus at least three combinations, and one asserting the built URL for a multi-word query contains no `%25`.

Step 1.2, the compatibility adapter. `consult-hn--input->params` stays as the parser for the legacy ` -- key=value` syntax and becomes a thin adapter producing the same alist. Silent, no deprecation message. Gate: the existing specs for it keep passing untouched.

Step 1.3, bounded pagination. `consult-hn-max-pages` defcustom, and `hitsPerPage` defaulting to 100. Measured: `query=clojure` returns `nbPages 50` at the default 20 per page and `nbPages 10` at 100, same coverage. Deliberately no page delay; the endpoint is generous. Gate: an e2e scenario with fixtures declaring more pages than the cap, asserting the chain stops at the cap.

Step 1.4, the generation guard. Replace `expected-search`, which is compared against itself and is therefore always true, with a counter incremented by the source on every restart. Each page callback captures the value and drops silently on mismatch. Keep the buffer killing as cancellation. Gate: an e2e scenario issuing a second query mid-stream and asserting no candidate from the first survives; a unit spec asserting a stale callback pushes nothing.

Step 1.5, dedup. Track `objectID` per chain and drop repeats. `search_by_date` paginates over a moving window, so an item can appear on two pages. Requires carrying `objectID` through `consult-hn--process-results`. Gate: fixtures repeating one hit across pages, e2e asserting the total is the unique count.

Phase 1 acceptance, all of it: compile gate clean, unit suite green, `make e2e` green twice, and a live session against the real API showing the request count bounded by the cap for a known broad query.

Done. Compile gate clean, 91 specs, 30 e2e checks per round green twice. Live, against the real API: `query=clojure` cost 10 requests for 1000 items, all distinct, where the endpoint reports 50 pages at the old 20 hits a page and 10 at 100; the same query under a cap of 3 stopped at 3 requests and 300 items. Every parameter in section 5 was rendered and fetched live, and the returned hits were checked against what the parameter claimed: points above the floor, all within the range, comment hits only, the author's items only, the front page on the relevance endpoint, and a URL-restricted search whose every hit carried the term in its URL. Each of the three defences was also shown to discriminate, by removing it and watching the suite go red: the cap, the generation guard and the dedup.

## 3. Phase 2, session UI and the transient

Only step 2.6 depends on Q4; 2.1 to 2.5 can proceed without it. Natural split if this wants two sessions: 2.1 to 2.4 is the session UI, 2.5 and 2.6 are the programmatic surface and the transient.

Non-obvious constraints, all verified, all in spec section 3: the throttle discards input equal to the previous input, so a parameter change cannot be propagated by faking input; consult narrowing is single-axis and unbound by default, so it cannot carry the parameter set; a transient cannot stay interactive over a live minibuffer; minibuffer buffers are reused, so a leaked overlay shows up in the next unrelated minibuffer.

Step 2.1, the restart handle. The source registers a handle in a session variable while the session lives and clears it on teardown. Forced by C1 and C2: input reaches the pipeline only through an after-change hook, and the throttle drops input equal to the previous input, so a parameter change with unchanged text has no way in. The handle performs the whole sequence in spec section 8, of which the source already does most: Phase 1's `cancel` bumps the generation, clears the dedup set and kills the request buffers, so what is new is exposing it and re-fetching page 0 from the current state.

Gate: a unit spec driving the handle and asserting the sequence, generation bumped, dedup cleared, flush sent downstream, page 0 requested carrying the new parameters; an e2e scenario calling the handle mid-session and asserting the result set is replaced rather than mixed.

Step 2.2, the chips string. `consult-hn--params-chips`, state to string, pure and no overlay. Vocabulary and separator are Q2; pick one, put it in spec section 5's chip column, and keep the function the only place that knows it.

Gate: unit specs for the empty state, a single parameter and a full house, plus one asserting the default state renders nothing at all, since an empty chip string is what keeps the prompt unchanged for someone who never touches a parameter.

Step 2.3, the chips overlay. `before-string` at `(1- (minibuffer-prompt-end))`, installed from `minibuffer-with-setup-hook`, updated on every parameter change, deleted on teardown. C4 says the technique is proven, it is how `consult-narrow` renders its label, and C6 says the deletion is not optional: minibuffer buffers are reused, so a leaked overlay turns up in the next unrelated minibuffer.

Gate: an e2e scenario asserting the chips appear with the expected text, change after a parameter command, and are gone after exit; plus the leak assertion that ` *Minibuf-1*` carries no overlay of our category once the session is over. Show the leak assertion discriminates by skipping the teardown and watching the following scenario's minibuffer carry the chips.

Step 2.4, the keymap and the parameter commands. `consult-hn-session-map` passed as `:keymap` to `consult--read`, one command per parameter under a prefix so which-key documents them for free. Not narrowing keys, which C3 rules out. Each command validates, mutates `consult-hn--params`, updates the chips and calls the restart handle. The author command reads a value from inside the session, which needs `enable-recursive-minibuffers` non-nil around the read, bound locally as consult does it, never globally (C5).

Gate: e2e scenarios for a parameter command replacing the results, and for the recursive read, asserting the depth returns to one, the session survives it, and both the candidates and the chips reflect the author that was read.

Step 2.5, the programmatic surface. `(consult-hn &optional query &rest params)` with keywords matching spec section 5, merged over the persisted plist for that call only. This is what replaces the string for programmatic callers once the transient stops producing one.

Gate: unit specs that a keyword argument reaches the request URL, that it does not persist into `consult-hn--params` past the call, and that the interactive path still passes none.

Done, 2.1 to 2.5. Compile gate clean, 120 unit specs, 66 e2e checks over eleven scenarios green twice in a row. What each step actually cost, in case it is useful for estimating 2.6: the restart handle was mostly exposing what `cancel` already did, plus remembering the input, plus one real defect it uncovered, which is that the source swallowed `destroy` and so the stages downstream of it never tore down, leaving consult's own indicator overlay in the reused minibuffer; the chips were a `before-string` overlay at the position `consult-narrow` uses, installed from the setup hook and removed from a buffer-local `minibuffer-exit-hook`; the commands are three cyclers, two togglers and three readers over one `consult-hn--param-set`; and the keyword arguments are one merge function and a `let`.

One thing 2.4 needed that the plan did not anticipate. With parameters out of the input, an author or a front page is a search with no query at all, and the old two-character minimum meant setting a parameter in an empty session fetched nothing at all and looked broken. `consult-hn--params-searchable-p` is the answer: tags or numeric filters carry a search on their own, while url matching and sort only say how a query should be treated. A session inheriting a parameter from the last one therefore fetches on open, which is what makes the front page a one-key affair.

Three defences were shown to discriminate rather than merely pass. Removing the chips teardown turns the leak assertion red. Removing the local binding of `enable-recursive-minibuffers` makes the author command never open its read at all, and the whole recursive-read scenario times out. And the swallowed `destroy` was caught by probing the reused minibuffer for overlays before the fix went in. The C6 story turned out to be milder than the spec claimed on Emacs 31.0.91, and the spec now says so: a leaked overlay is stale state after exit, not a decoration in somebody else's prompt, because entering a minibuffer clears the overlays left in it.

Live, in a real interactive Emacs against the real endpoint: a session opened on `clojure`, `C-c a` opened a read at minibuffer depth two, `pg` came back at depth one, every candidate on offer was authored by pg, the chips read ` [pg]`, the request carried `author_pg`, the result set was replaced rather than added to, and the session closed leaving no overlay and no handle. Separately, in the user's own running Emacs with their own configuration, the composed keymap resolved our `C-c t` and `C-c a` while their existing `C-c '` and `C-c C-s` still worked, and a scripted session showed a hundred candidates, empty chips, then ` [story]` after the type command, then a clean teardown.

Step 2.6, the transient onto the state object. Blocked on Q4. Infixes read their initial values from the plist and write back on set, `consult-hn-transient-action` only opens the session, and `consult-hn-transient--format-query` is deleted. R1 is that this is the largest single piece with no harness today, so the state-to-arguments and arguments-to-state functions are pure and tested directly while the transient itself stays thin.

Gate: unit specs both directions, state to infix values and infix values back to state, including the round trip. This is also the moment `consult-hn--input->params` loses its last reason to exist, since nothing produces the string any more; decide there whether the parser and its specs go with it.

Done. Q4 answered as optional front door. The bridge is one alist saying how each parameter is spelled and two pure functions over it, so the menu itself is eight named infixes, a `:init-value` shared by all of them, and a two-line action. Written back when the search runs rather than as each infix is set: write-on-set means a class per infix class and a `transient-infix-set` method reaching into the plist, which is exactly what R1 says not to do, and it buys only that abandoning the menu still changes the state. `consult-hn--input->params` went, since nothing had called it since Phase 1; the cases its specs pinned moved onto `consult-hn--legacy-pairs`, which is where the legacy syntax actually lives now.

Two things worth keeping for next time. The menu is now covered by an e2e scenario that opens it, presses the key, searches and checks the session came up parameterised, which is the harness R1 said did not exist; it discriminates, since breaking the argument reading turns three of its checks red. And because the unit suite now loads `consult-hn-transient.el`, and CI runs Emacs 29.4 and 30.1 with the transient they bundle rather than the 0.9 the package declares, the suite was run against those two versions as well by putting `lisp/transient.el` from the matching Emacs release tag on the load path. It passes on 0.4.3, 0.7.2.2 and the 0.13.3 that Emacs 31 bundles. Note that adding transient to `make deps` would achieve nothing: it is built in, so `package-install` declines.

## 4. Phase 3, input semantics and docs

Q1 answered as Option A. Keep the semantics consult and this package already have, obsolete `consult-hn-initial-input-string`, fix `make test`, update the readme, and add an e2e scenario proving narrowing sends no requests by freezing the request counter across typing.

Smaller than it looks, and Q1 is now a question about defaults rather than about mechanism. Measured in Phase 2 with a throwaway probe under the stub: `consult--read` wraps every async table with `consult--async-split`, so the session already opens with `#` in the input, the query is what sits between the first and second `#`, and what follows the second one narrowed fifteen candidates to one without issuing a single request. Option A is therefore a documentation change plus the scenario that pins it. The probe is at `/tmp/consult-hn-live/split-check.el` and wants twenty lines to become a scenario, with the caveat that cost the first attempt a false negative: pick a filter term that genuinely appears in one fixture, since the timestamps make digits match everything.

The work, in the order it wants doing.

Step 3.1, Option A, which asks for no semantics code at all. One claim this plan made about the alternative was wrong and is worth correcting rather than deleting: Option B does not mean leaving the split stage out, since that would remove narrowing altogether and is a third option nobody proposed. Option B as spec section 9 words it is `consult--split-separator`, shipped already as the `comma` and `semicolon` styles, so it is one style entry and a `let` binding `consult-async-split-style` around `consult--read`. It was refused because that `let` overrides a setting the user made for the whole of consult, not because it was expensive.

What is left of the step is `consult-hn-initial-input-string`, obsoleted by `make-obsolete-variable` pointing at the keyword arguments and still read, with our own read wrapped in `with-suppressed-warnings` or the compile gate fails on our own deprecation.

Step 3.2, the e2e scenario the phase is really for: type into an open session past the second separator, assert the displayed count falls, and assert the request counter did not move. That is the only assertion in the suite that would notice if a future consult changed the default split style out from under this package.

Step 3.3, `make test`. It calls plain `package-initialize` against the user's package directory, so it works only on CI; section 0 has the invocation every session has used instead, which is the shape the target should take. Note that CI runs Emacs 29.4 and 30.1 with the transient they bundle, 0.4.3 and 0.7.2.2, rather than the 0.9 this package declares, and that the suite now loads `consult-hn-transient.el`, so any fix should keep working there; the way to check locally is to put `lisp/transient.el` from the matching Emacs release tag on the load path.

Step 3.4, the readme, which is further behind than the code. It still teaches `#SEARCH-TERM -- ADDITIONAL-PARAMETERS` as the way to pass parameters, still presents the transient as the convenient way in when Q4 made it the optional one, and says nothing about the session keys under `C-c`, the chips, or the keyword arguments `consult-hn` now takes. Its `consult-hn-initial-input-string` examples are written in the syntax D12 removes from the documentation. A changelog entry leads with the behaviour change rather than burying it (R3).

Step 3.5, the merge. Delete both of these documents, and decide whether this reaches `main` as a pull request for the record or as a fast forward. The branch has never been pushed, so nothing is public yet, and Q3 is moot now that everything sits on one branch.

Done. 126 unit specs, 92 e2e checks over fifteen scenarios green twice in a row, the compile gate clean, and a live run against hn.algolia.com where `clojure` cost ten requests for a thousand items and typing a filter past the second separator narrowed those to two without moving the request counter.

Three scenarios were added where the phase asked for one. Narrowing sending no requests is the one it was for, and it discriminates: with the split style forced to `none`, five of its eight checks go red and typing the filter fires three requests. The other two were owed from spec section 13 and had never been written. Abort mid-stream stopping the chain discriminates by taking the `cancel` call out of the source's `destroy` branch, whereupon an abandoned session pages on from `(0 1)` to `(0 1 2)` and receives the response it should never have seen. The legacy suffix pins that a hand-written ` -- tags=story` still reaches the endpoint and still outranks the state object, which is the layering rule in spec section 5 and the only reason D12 keeps the parser. The harness grew one knob for the first of those, a page delay the abort scenario raises so the interruption lands while a page is genuinely in flight; at the usual thirty milliseconds the chain is over before a poll can see it.

`make test` was not broken the way section 0 describes. It works whenever `.elpa` is populated and only a clean checkout dies on it, so the fix is an order-only prerequisite that builds the sandbox once. `check-compile` now carries the same prerequisite, so it no longer refreshes the archives over the network on every run, and it now reads the return value of `byte-compile-file` instead of discarding it: verified by planting a call to a function that does not exist and watching `make` exit 2.

The compile gate is now in CI, which never ran it. The risk that could not be closed from this machine is that CI runs Emacs 29.4 and 30.1 while only 31 is installed here. The gate was proven against the transient those two bundle, 0.4.3 and 0.7.2.2, with no warning attributed to either of our files, and the package uses nothing newer than the 29.4 it declares. But the first push is the first time any commit on this branch meets those Emacsen at all.

## 5. Ground rules

One phase per session; each ends at a commit with the tree green. Do not start a phase whose blocking question is unanswered. Update the spec when a decision changes, rather than letting the code drift from it. Report anything the harness cannot prove, and say so plainly instead of implying coverage that does not exist.
