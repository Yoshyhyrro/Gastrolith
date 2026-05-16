# Analysis: Static checks and recommendations for z/OS PL/I and JCL

This directory contains analysis notes and proposals related to improving static
analysis and developer tooling for PL/I, JCL and z/OS-specific runtime issues.

The following English summary documents three main viewpoints considered for
improving reliability and developer experience when maintaining PL/I code
targeting z/OS environments.

1) Memory safety and resource management (inspired by Rust ownership)

- Problem: PL/I pointer/BASED usage and z/OS fixed-size HEAP + ANYWHERE pointers
  cause runtime ABENDs (S0C4, S806). Current compilers/ZOE do not track
  allocation/free pairs across control-flow paths.
- Proposed static checks (examples):
  - "Warning: Pointer PTR allocated but not freed before scope end. Potential
    HEAP exhaustion in z/OS environment." — detect ALLOCATE without a matching
    FREE on all exit paths within the same scope.
  - "Warning: Based variable BASED_PTR used without BASE address set. Risk of
    S0C4 ABEND on dereference." — detect dereference of BASED variables when
    no prior ADDR or assignment to the base pointer exists in the same flow.
  - "Warning: Multiple owners of pointer PTR without clear ownership transfer.
    Possible double-free or dangling reference." — build a simplified ownership
    graph to detect multiple free paths.
  - "Warning: Anywhere pointer used in fixed storage context. Potential storage
    overlay violation." — check ANYWHERE usage against storage class and
    declaration context.

  Implementation note: start with scope-limited, conservative data-flow
  analysis ignoring some dynamic features (for example, fully-dynamic BASED
  rebinding or inter-task heap sharing) to keep analysis low-cost and useful.

2) Type safety, immutability and control-flow coverage (inspired by Haskell/Swift)

- Problem: PL/I's permissive type conversions and implicit casts can cause
  precision loss, silent truncation and logic bugs. SELECT without OTHERWISE
  can miss cases. Variables used like constants can be reassigned unexpectedly.
- Proposed static checks (examples):
  - "Warning: Implicit assignment from FIXED DEC(10,2) to FIXED DEC(5,0)
    may cause precision loss." — flag implicit narrowing assignments.
  - "Warning: SELECT statement lacks full coverage without OTHERWISE. Missing
    case for value '3'." — detect uncovered scalar cases when exhaustive
    domain can be inferred.
  - "Warning: Variable CONST_VAL reassigned after const-like usage. Consider
    DECLARE CONSTANT for immutability." — infer constant-like usage and warn
    on later reassignments.

  Implementation note: adopt opt-in stricter checks (annotations or LSP
  settings) so teams can retain PL/I flexibility where needed while gaining
  safety benefits in critical modules.

3) JCL static and history-based checks (CI/CD pre-submit ideas)

- Problem: JCL failures often come from missing DD producers, unused DDs, or
  overprovisioned SPACE parameters. ZOE's JCL support is currently syntax-level
  only.
- Proposed checks (examples):
  - "Warning: DD DSN=INPUT.DSN referenced with DISP=OLD, but no prior step
    generates it." — intra-JOB DD dependency graph analysis.
  - "Warning: Unused DD statement //TEMP DD in STEP1; not referenced in PGM
    entry or CALL chain." — compare program entry/CALL symbols to DD names
    (static LOADLIB symbol analysis only).
  - "Warning: SPACE=(TRK,(100,50)) allocation unusually high based on SMF
    history; potential overprovisioning." — optional SMF-based historical
    heuristics for recommended SPACE sizing.

  Implementation note: conservative, LPAR-local analyses are recommended to
  limit false positives and respect z/OS catalog/SMF semantics.

Recommended adoption strategy

- B. ZOE LSP extension (priority): medium implementation cost, high payoff.
  Real-time, editor-integrated warnings improve developer feedback during
  coding. This approach can provide both PL/I source and JCL checks via the
  LSP and allows gradual rollout as rules mature.
- C. Independent CLI/CI tool (complement): low cost and easy to plug into
  existing CI/CD pipelines (SonarQube plugin, Git hooks). Good for JCL
  history-based checks and batch analysis of large codebases.

Notes about this repository

- This project contains PL/I parser and related tooling sources. The
  `src/analysis` directory is a natural home for prototypes, rule design
  documents, and small PoC analyzers that implement the checks listed above.
- Practical first steps: create a small CLI prototype (Rust or OCaml) that
  performs scope-limited ALLOC/FREE pair tracking and a simple SELECT
  coverage checker. Wire it into an LSP-prototype or a pre-commit hook for
  quick team feedback.

Next steps

- Experiment: add a small analyzer that scans PL/I ASTs produced by the
  repository's parser and emits the example warnings above. Track false
  positives and refine heuristics.
- Integration: prototype a ZOE LSP extension that consumes the analyzer or
  run the analyzer as a standalone CLI in CI.

Clarification about language examples and practical limits

- The mentions of Rust, Haskell, Swift or similar languages above are
  illustrative: they provide conceptual inspiration (ownership, immutability,
  exhaustive pattern checks) rather than a requirement to implement the tool
  in those languages. In practice, when the actual machine code or module
  load-time behavior is unknown, static source-level analysis can only
  conservatively infer and emit JCL/PL/I-level warnings. This means:
  - Focus first on warning-centric, JCL-aware checks that help avoid ABENDs
    and operational failures (missing producers, space overprovisioning, etc.).
  - Prefer conservative, flow-sensitive analyses that produce actionable
    warnings rather than speculative claims about runtime behavior that
    require precise machine code or load-time context.

