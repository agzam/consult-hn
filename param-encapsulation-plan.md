# Execution plan: parameter encapsulation

Companion to `param-encapsulation-spec.md`. The spec says what and why; this says how, with the environment facts and acceptance gates. Branch `param-encapsulation`. Both documents are temporary and get removed before merge.

Phases 0 and 1 are done and committed. Phases 2 and 3 remain.

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

Depends on Q4. Steps: the session keymap with one command per parameter under a prefix; the chips overlay via `before-string` at `(1- (minibuffer-prompt-end))`, installed from `minibuffer-with-setup-hook`, updated on change, deleted on teardown; the restart handle registered by the source while the session lives; the transient rewritten to read and write the state object, with `consult-hn-transient--format-query` deleted.

Non-obvious constraints, all verified, all in spec section 3: the throttle discards input equal to the previous input, so a parameter change cannot be propagated by faking input; consult narrowing is single-axis and unbound by default, so it cannot carry the parameter set; a transient cannot stay interactive over a live minibuffer; minibuffer buffers are reused, so a leaked overlay shows up in the next unrelated minibuffer.

Gates: e2e scenarios for a parameter command replacing results, chips appearing and being gone after exit, a recursive read for author not corrupting the session, and no overlay of our category left on ` *Minibuf-1*`.

## 4. Phase 3, input semantics and docs

Depends on Q1. Implement the chosen semantics, fix `make test`, update the readme, and add an e2e scenario proving narrowing sends no requests by freezing the request counter across typing.

## 5. Ground rules

One phase per session; each ends at a commit with the tree green. Do not start a phase whose blocking question is unanswered. Update the spec when a decision changes, rather than letting the code drift from it. Report anything the harness cannot prove, and say so plainly instead of implying coverage that does not exist.
