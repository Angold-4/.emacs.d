// Pure data types for tradeoffs-trace's core. No enums, no namespaces, no
// constructor parameter properties, no decorators — Node 25 strips types at
// runtime, so only erasable TS syntax is used anywhere in src/core.
//
// Design references (docs/tradeoffs-trace.md) are noted per type.

// ---------------------------------------------------------------------------
// §7.1 Versions and binding
// ---------------------------------------------------------------------------

export interface ContractVersion {
  snapshot: number; // the plan snapshot number
  sectionSha256: string; // sha256 of the phase's section in that snapshot
}

/** run id · phase id · candidate sha · contract version · record id · record version */
export interface BindingTuple {
  runId: string;
  phaseId: string;
  candidateSha: string;
  contractVersion: ContractVersion;
  recordId: string;
  recordVersion: number;
}

/** The (candidate, contract version, record version) part of a binding tuple
 * — what every ballot, review, finding disposition and owner command must
 * carry per design §7.1, without the run/phase ids (reduce.ts already knows
 * which phase it is reducing). */
export interface RecordBinding {
  boundCandidateSha: string;
  boundContractVersion: ContractVersion;
  boundRecordVersion: number;
}

// ---------------------------------------------------------------------------
// §1.1 Plan and phase contract
// ---------------------------------------------------------------------------

import type { ArchitectureItem, ConstraintItem, RequirementItem } from "./items.ts";

export interface PlanPhase {
  id: string;
  goal: string;
  acceptance: string[];
  checks: string[];
  boundaries: string[];
  reserved: string[];
  provisional: boolean;
  /** Plan 06c: the phase's final check (`#+TT_FINAL_CHECKS`, overridden by a
   * phase's `:FINAL_CHECKS:`). Only the candidate about to be accepted runs
   * it, once. Absent on a plan that declares none, which behaves as before. */
  finalChecks?: string[];
  /** Plan 06g: `#+TT_WORKERS:` — how many lanes one round runs. Absent (or 1)
   * is today's single-candidate loop; 2 is the two-lane round. */
  workers?: number;
  /** Plan 06g: `#+TT_ROUNDS:` — how many rounds one phase may spend before it
   * parks on the owner. Absent means the default of 3. */
  rounds?: number;
  /** Plan 06h (A1): `#+TT_REVIEWERS:` — the odd list of reviewer seats.
   * Absent means `M A B`. */
  seats?: string[];
  /** Plan 06h (A1): `#+TT_LEADER:` — the seat whose approval and tie-break
   * vote decide. Absent means the first seat. */
  leader?: string;
  /** Plan 06b: the structured items of a phase subtree (ref
   * refs/06_ref_plan_format.md). Absent on an old-format plan, where
   * `acceptance`/`reserved` are the only items. */
  architecture?: ArchitectureItem[];
  requirements?: RequirementItem[];
  constraints?: ConstraintItem[];
}

export interface Plan {
  title: string;
  checks: string[]; // TT_CHECKS
  phases: PlanPhase[];
}

/** Plan 06h (A1): the run's seat configuration, recorded in the init event so
 * a run replays with its own seats even after the plan changes. An old log
 * that records none means `M`, `A` and `B`, led by `M`. */
export interface Seats {
  seats: string[];
  leader: string;
  workers: number;
}

/** The frozen contract a phase is evaluated against: one phase of one plan snapshot. */
export interface PhaseContract {
  phaseId: string;
  contractVersion: ContractVersion;
  goal: string;
  acceptance: string[];
  checks: string[];
  boundaries: string[];
  reserved: string[];
  /** Plan 06c: the phase's final check, frozen into the contract. Only a
   * candidate that has passed review with no open blocker runs it, once
   * (core/checks.ts's `checkTier`). */
  finalChecks?: string[];
  /** Plan 06g: `#+TT_WORKERS:` frozen into the contract — how many lanes one
   * round runs. Absent (or 1) is today's single-candidate loop. */
  workers?: number;
  /** Plan 06g: `#+TT_ROUNDS:` frozen into the contract, or undefined for the
   * default (3). `roundBudget()` reads this; nothing else decides the budget. */
  roundsAllowed?: number;
  /** Plan 06h (A1/A2): the frozen reviewer seats. `seatsOf(contract)` is the
   * only source of the seat list; every place that used to hard-code M, A and
   * B reads it. Absent on an old contract means `M A B`. */
  seats?: string[];
  /** Plan 06h (A2): the frozen leader. Absent means the first seat. */
  leader?: string;
  /** Plan 06b: the structured items of the frozen contract. The FSM,
   * prompts, checks, verdicts and acceptance all read these; absent on an
   * old-format phase, where `itemsFromPhase` synthesizes R1..Rn from
   * `acceptance` and C1 from `reserved`. */
  architecture?: ArchitectureItem[];
  requirements?: RequirementItem[];
  constraints?: ConstraintItem[];
  /** Plan 06b: true when the conductor synthesized these items from an
   * old-format `acceptance`/`:RESERVED:` list. The items are carried through
   * the matrix and acceptance either way; the coverage and per-item verdict
   * REQUIREMENTS apply once the worker has submitted coverage (the extension
   * makes every real worker do so), so a hand-built in-process plan keeps its
   * old behaviour. */
  itemsSynthesized?: boolean;
  /** Plan 01f: the phase's own expensive, live command (the plan's `:GATE:`
   * property). Declaring one inserts a GATING stage between RESOLVING and
   * ACCEPTED: the conductor runs this command itself, once per candidate the
   * reviewers already accepted, and only a passing gate lets the ACCEPTED
   * event through (core/gate.ts, conductor.ts's `#runGate`). */
  gate?: string;
  /** Plan 01f: the plan's `:GATE_CLEANUP:` command, run after the gate
   * whatever its outcome (release the resources the gate took). */
  gateCleanup?: string;
}

// ---------------------------------------------------------------------------
// §3 Decisions
// ---------------------------------------------------------------------------

export type DecisionSource = "worker" | "reviewer-discovered" | "trigger";
export type DecisionClass = "detail" | "delegated" | "reserved";

export interface DecisionAlternative {
  option: string;
  consequence: string;
}

export interface DecisionRecommendation {
  choice: string;
  reason: string;
}

/** Phase 1b addition (pure, additive): the plain-language fields a worker
 * discloses with `submit_phase` (design §3.2), before the conductor assigns
 * id/version/phaseId/source/boundCandidateSha/boundContractVersion once a
 * candidate exists. Matches schemas/submission.schema.json's
 * `$defs.decisionDisclosure` and extension/param-shapes.ts's
 * `DECISION_DISCLOSURE_PARAMS` exactly. See SUBMIT_PHASE/FREEZE_COMPLETED
 * below for why the split exists: SUBMIT_PHASE records the raw disclosure;
 * FREEZE_COMPLETED is what turns it into real, bound Decision records. */
export interface DecisionDisclosure {
  choice: string;
  whyItMatters: string;
  alternatives: DecisionAlternative[];
  recommendation: DecisionRecommendation;
  classProposal: DecisionClass;
}

/** Plan 01g: a worker or reviewer may say a criterion cannot be met *as
 * written* rather than merely unmet. `criterion` must name one acceptance
 * item of the current phase contract verbatim; `proposedWording` replaces it
 * if the reviewers' normal tally passes (D1). It is a different claim from a
 * blocking finding: an unmet-but-clear criterion is repaired, an unmeetable
 * one is reworded, and the run never waits for the owner for either. */
export interface CriterionDispute {
  criterion: string;
  why: string;
  proposedWording: string;
}

export type AmendmentStatus = "proposed" | "applied" | "reverted";

/** Plan 01g: the amendment record a `criterionDispute` becomes — a
 * `reserved` decision the reviewers vote on like any other. While
 * `status` is `proposed` a passing tally applies it: the wording is replaced
 * for this phase only, the contract version bumps, and contract findings
 * citing the old wording are superseded. `appliedContractVersion` records
 * the version the amendment produced; the owner can revert it through the
 * input box (a correction naming `id`). */
export interface CriterionAmendment {
  id: string;
  criterion: string; // the replaced acceptance item, verbatim
  proposedWording: string; // the replacement
  /** Plan 06b (OD-1 R6): on a structured phase, the requirement item id the
   * amendment names. The rewrite targets this id, never a list position. */
  itemId?: string;
  why: string;
  raisedBy: "worker" | Reviewer;
  status: AmendmentStatus;
  previousContractVersion?: ContractVersion;
  appliedContractVersion?: ContractVersion;
  revertedAt?: string;
}

export interface Decision {
  id: string;
  version: number;
  phaseId: string;
  source: DecisionSource;
  class: DecisionClass;
  choice: string; // one plain sentence
  whyItMatters: string; // in terms of the plan's goal
  alternatives: DecisionAlternative[]; // >= 1, each with a consequence
  recommendation: DecisionRecommendation;
  linkedFindingId?: string; // set while a contract objection is open (§4.1)
  boundCandidateSha: string;
  boundContractVersion: ContractVersion;
  supersededByCorrection?: string; // correction id, once superseded (§7.5)
  /** Plan 01g: set on an amendment record — a `reserved` decision the
   * reviewers vote on, whose passing tally rewrites one acceptance item for
   * this phase. An amendment decision never blocks acceptance by itself: a
   * failed amendment simply leaves the criterion unchanged. */
  amendment?: CriterionAmendment;
  /** Plan 2c: the record no longer describes the current candidate and is
   * never votable again. Set when a new candidate is frozen and the worker
   * did not carry the record forward ("candidate <sha>: not carried
   * forward"), when the worker withdrew it, or when a reviewer matched its
   * own discovery to another record ("same as <id>"). */
  supersededBy?: string;
  /** Plan 2c: reviewers who independently discovered this same choice and
   * matched their discovery to this record (design §3.3, §10). */
  alsoSeenBy?: Reviewer[];
  /** Plan 06b: the plan item this finding is anchored to (an R, C or A id),
   * when a per-item majority produced it. The repair prompt and the item
   * matrix use it to point at the exact point. */
  itemId?: string;
}

/** Plan 2c: in a repair attempt the worker states, for each of its prior
 * decisions (shown by id in the repair prompt), whether the new candidate
 * keeps it, changes it (with the new plain-language text) or withdraws it.
 * FREEZE_COMPLETED rebinds kept and changed records to the new candidate;
 * every other record from a superseded candidate is marked superseded, so a
 * record that no longer describes the code can never block acceptance. */
export interface PriorDecisionStatement {
  id: string;
  status: "kept" | "changed" | "withdrawn";
  choice?: string;
  whyItMatters?: string;
  alternatives?: DecisionAlternative[];
  recommendation?: DecisionRecommendation;
}

// ---------------------------------------------------------------------------
// §4 Findings
// ---------------------------------------------------------------------------

export type FindingKind = "defect" | "contract" | "integration";
export type FindingSeverity = "blocking" | "advisory";
export type FindingStatus = "open" | "repaired" | "disproved" | "accepted" | "superseded";
/** Plan 06h: a seat is any name `#+TT_REVIEWERS` declares. The default is
 * still `M`, `A` and `B`, but nothing in the code may assume that set. */
export type Reviewer = string;

export interface Finding {
  id: string;
  version: number;
  phaseId: string;
  kind: FindingKind;
  severity: FindingSeverity;
  evidence: string; // file:line, scenario, check result or plan clause — required, non-empty
  /** Reviewers and the conductor raise findings; the owner may raise one by
   * refusing a published message during review (contract §1.3). */
  raisedBy: Reviewer | "conductor" | "owner"; // conductor raises `integration` findings itself
  linkedDecisionId?: string;
  status: FindingStatus;
  boundCandidateSha: string; // the candidate the finding was raised against
  reproduction?: { command: string; result: "reproduced" | "not_reproduced" | "inconclusive" };
  repairedByCandidateSha?: string;
  disprovedEvidence?: string;
  acceptedScope?: string; // required scope note (§4.2, §10.4 `x`)
  /** Plan 01g: the acceptance item this `contract` finding disputes verbatim
   * (set when it was raised with a `criterionDispute`). A later amendment of
   * that item supersedes the finding. */
  criterionDisputed?: string;
  /** Plan 01g: why this finding was closed without repair — set when an
   * amendment replaced the wording it cited. */
  supersededBy?: string;
  /** Plan 2c: other reviewers who raised the same finding ("same as F-…")
   * instead of filing a duplicate. */
  alsoRaisedBy?: Reviewer[];
  /** Plan 05e: what validated this finding — `record` (the check record
   * confirmed the claim), `run <cmd> exit N` (the validator ran it in the
   * candidate's checkout), a `file:line …` citation the evaluator checked,
   * or `panel 2/3 keep` (the round panel). Absent until something validated
   * it. */
  verified?: string;
  /** Plan 05e: why the finding's severity changed from the raised one (the
   * evaluator's plan check, or the round panel's vote). */
  severityReason?: string;
}

// ---------------------------------------------------------------------------
// Contract v1: messages, their lifecycle and the settled ledger
// ---------------------------------------------------------------------------

/** The three structured message kinds (plan 04 §2). */
export type MessageType = "tradeoff" | "finding" | "blocker";

/**
 * A message's lifecycle (contract §1). The allowed edges are the rows of
 * `MESSAGE_TRANSITIONS` (core/messages.ts):
 *
 *   raw → published | merged | dropped
 *   published → accepted | refused | superseded | resolved
 *   refused → resolved | superseded
 */
export type MessageState =
  | "raw"
  | "published"
  | "merged"
  | "dropped"
  | "accepted"
  | "refused"
  | "superseded"
  | "resolved";

/** A terminal message state carries a settlement (contract §2's ledger): who
 * settled it, the reason, and the bindings it was settled under. */
export interface MessageSettlement {
  state: "accepted" | "refused" | "merged" | "dropped" | "resolved";
  settledBy: "owner" | "evaluator" | "panel" | "vote";
  reason?: string;
  at?: string;
  candidateSha: string;
  contractVersion: ContractVersion;
  messageVersion: number;
  contentHash: string;
}

/**
 * Contract v1's first-class message. A trade-off, a finding or a blocker is
 * raised as `raw`, is published (`published`) once it is reviewable, and is
 * settled by an owner verdict (`accepted`/`refused`), an evaluator (`merged`
 * or `dropped`) or a later resolution. Every version carries the candidate,
 * contract version and a `contentHash` of the reviewable content, so a
 * settlement is bound to the CONTENT, not the version number. Ids are
 * `T-n`, `F-n`, `B-n`, unique within a run. */
export interface Message {
  id: string; // T-n | F-n | B-n
  phaseId: string;
  type: MessageType;
  title: string;
  summary: string;
  context: string;
  evidence: string[];
  planRef?: string;
  state: MessageState;
  messageVersion: number;
  /** Plan 03b: who raised the message (`worker', a reviewer, `owner', the
   * `conductor'), and how much it matters (`high'/`normal'/`low'), so the
   * rendered review can group and colour it. Both are review metadata, not
   * part of the reviewable content, so `contentHash` ignores them. Optional:
   * a message raised before this field existed, or a fixture, derives them
   * from the record it came from. */
  raisedBy?: string;
  importance?: "high" | "normal" | "low";
  boundCandidateSha: string;
  boundContractVersion: ContractVersion;
  contentHash: string;
  /** The settlement once this message reaches a terminal state. */
  settlement?: MessageSettlement;
  /** Set when a carry to a new candidate invalidated an existing settlement
   * (content changed, or the contract was amended): the message needs a new
   * verdict. */
  invalidated?: { reason: "content changed" | "contract amended"; atCandidate: string };
  /** The decision or finding id this message was raised from, so a re-freeze
   * or re-review does not raise a duplicate. */
  sourceRecordId?: string;
  /** A refusal after the phase reached DONE is a follow-up, not a blocker. */
  followUp?: boolean;
  /** Plan 04a: the code location a `raise_tradeoff` call anchored the
   * trade-off to (`{path, lines}`), carried through to the published
   * message so the owner sees where the choice lives. */
  anchor?: { path: string; lines: [number, number] };
  /** Plan 04a: an evaluator timed out (or did not evaluate this message), so
   * the raw message was published unchanged, marked unevaluated. */
  unevaluated?: boolean;
  /** Plan 04a: how important the evaluator judged this message
   * (high|medium|low). Metadata, not part of the reviewable contentHash. */
  importance?: "high" | "medium" | "low";
  /** Plan 05e: the message id this trade-off closes (a fix names the
   * finding/blocker message it closes, `closes: F-3`), so the owner sees the
   * link on both messages. */
  closes?: string;
  /** Plan 05e: the round panel's per-seat votes on this trade-off or blocking
   * finding, kept on the message so its file shows the three reasons after
   * the round (and after a carry). */
  panelVotes?: Array<{ seat: number; verdict: string; reason: string }>;
  /** Plan 05e: the round panel's outcome for this message (`keep`, `drop` or
   * `downgrade`), kept beside the votes. */
  panelOutcome?: string;
  /** Plan 04b: this `blocker` message was raised through a reviewer's
   * separate `blockers` list — "stop the work until the owner decides". Only
   * such a blocker is voted by a panel: a blocking *finding* raised through
   * the ordinary `findings` list keeps its pre-04b meaning (it blocks
   * acceptance and forces a repair), and never parks the run on the owner
   * through a panel. */
  raisedAsBlocker?: boolean;
  /** Plan 04a item 4: the evaluator's report on an owner-refused message —
   * whether this candidate addressed the owner's reason. `addressed: false`
   * leaves the refusal standing but records the report, so the ledger can
   * tell "checked and not addressed" from "never checked". */
  addressedReport?: { addressed: boolean; reason?: string; at?: string };
  /** Plan 04a: the contentHash of the content RE-DERIVED from the backing
   * record when the message was last raised/carried. The evaluator may
   * rewrite the visible content, so a carry must compare the record against
   * this, not against `contentHash`, or an unchanged record would look
   * changed and lose the evaluator's wording. */
  sourceContentHash?: string;
  supersededBy?: string;
  /** Every past version's contentHash, so a verdict bound to a pre-carry
   * version of an unchanged message is still recognised as current. */
  versionContentHashes?: Record<number, string>;
  /** The (candidate, version) pairs this message was carried from, so a
   * verdict bound to a pre-carry candidate is still recognised. */
  carriedFrom?: Array<{ candidateSha: string; version: number }>;
}

// ---------------------------------------------------------------------------
// Owner requests
// ---------------------------------------------------------------------------

export type OwnerRequestStatus = "open" | "resolved" | "unneeded";

/** A resolvable choice on an owner request, named by a stable id (never by
 * matching its display text — round-3 review item 1). */
export interface OwnerRequestOption {
  id: string;
  label: string;
}

export interface OwnerRequest {
  id: string;
  version: number;
  phaseId: string;
  reason: string;
  origin:
    | "failed_vote"
    | "open_finding"
    | "repair_budget_exhausted"
    | "amend_conflict"
    | "reserved_decision"
    | "unaddressed_correction"
    | "blocker_panel";
  linkedDecisionId?: string; // set when origin is `reserved_decision` or `failed_vote`
  linkedFindingId?: string; // set when origin is `open_finding` or `blocker_panel`
  linkedCorrectionId?: string; // set when origin is `unaddressed_correction`
  /** Plan 04b: set when origin is `blocker_panel` — the raw `blocker` message
   * the panel escalated, so resolving the request resolves the message too. */
  linkedMessageId?: string;
  relatedBallots?: Ballot[]; // set when origin is `failed_vote` (design §5.2: "carrying every ballot")
  /** The (candidate, contract) this request was raised against, when a
   * candidate existed at the time (design §7.1). Absent only for a gate
   * failure with no candidate yet, e.g. every worker attempt timing out. */
  boundCandidateSha?: string;
  boundContractVersion?: ContractVersion;
  /** Every option has a defined effect (round-3 review item 1) — none is a
   * no-op. See owner-requests.ts for what each origin offers. */
  options: OwnerRequestOption[];
  status: OwnerRequestStatus;
  /** `option` is the chosen option's *id* (never its label). */
  resolution?: { option: string; note?: string };
  /** The (candidate, contract) the *resolution* was bound to — design §6.3's
   * "resolved by the owner bound to (C, K)". Set only once resolved. */
  resolvedBinding?: { candidateSha: string; contractVersion: ContractVersion };
}

// ---------------------------------------------------------------------------
// Decision briefs (owner-facing; see src/core/briefs.ts)
// ---------------------------------------------------------------------------

/** One plain-language option of a brief, carrying the underlying owner
 * request's own option id, so resolving from the brief sends exactly the
 * same resolve command as resolving the request. */
export interface BriefOption {
  id: string;
  label: string;
  /** What happens under this option. */
  effect: string;
  /** What it costs. */
  cost: string;
}

/** Another open item that touches the same concern as a brief, so the owner
 * sees the bigger risk beside the narrow request. */
export interface BriefRelated {
  id: string;
  question: string;
}

/** One owner-facing decision brief for one open item that needs the owner's
 * decision. Produced once per round after evaluation by the evaluator's
 * model; validated by `briefIssue` in src/core/briefs.ts. */
export interface DecisionBrief {
  /** The owner request (or entry/message id) this brief is for. */
  requestId: string;
  /** The owner command choosing an option sends. `resolve` (default) settles
   * an owner request; `override` approves or rejects a flagged reserved
   * decision; `entry` settles a live review entry (accept/refuse). A brief for
   * a reserved decision or an entry carries the command that item needs. */
  command?: "resolve" | "override" | "entry";
  /** One plain line, no code identifiers. */
  question: string;
  /** What the system does today, with one concrete example using real market
   * names and times from the plan's calendars. */
  today: string;
  /** What the owner would notice; always answers whether any market stops
   * publishing. */
  impact: string;
  options: BriefOption[];
  /** One option and why, citing the plan or an IC section. Absent only for
   * the deterministic backstop; it never recommends an option by position
   * without evidence. */
  recommendation?: { option: string; why: string };
  /** The reason the backstop has no recommendation (the writer timed out, its
   * tools did not match, or it did not run), rendered in plain words where the
   * recommendation would be (OD-3 / D-B-79). */
  noRecommendationReason?: string;
  related: BriefRelated[];
  /** The original message/finding/file:line, folded under TAB. */
  evidence: string[];
  /** The candidate this brief was written for, so a later candidate's round
   * rewrites it (the plan asks for one brief per round). Internal to the
   * conductor: the evaluator's `submit_brief` does not supply it. */
  candidateSha?: string;
}

// ---------------------------------------------------------------------------
// §7.5 Corrections (revise)
// ---------------------------------------------------------------------------

export type CorrectionStatus = "open" | "addressed" | "resolved" | "withdrawn";

export interface Correction {
  id: string;
  version: number;
  phaseId: string;
  targetRecordId: string; // the decision or finding id it corrects
  correctionText: string; // owner's words, verbatim
  contractChange: boolean; // ticked box in the revise buffer (§7.5)
  status: CorrectionStatus;
  boundContractVersion: ContractVersion; // the K it must be addressed under
  grantedRounds: number; // always 3 (design §7.5, §8.1) — the core sets this, not the caller
}

// ---------------------------------------------------------------------------
// §5 Ballots
// ---------------------------------------------------------------------------

export type Vote = "approve" | "reject";

export interface Ballot {
  reviewer: Reviewer;
  decisionId: string;
  vote: Vote;
  rationale: string;
  evidence: string[]; // at least one citation required to be valid
  contractObjection?: boolean; // opens a linked finding, suspends the vote
  boundCandidateSha: string;
  boundContractVersion: ContractVersion;
  boundRecordVersion: number; // design §7.1: bound to the decision's version, not just the candidate
  /** Skill fix 5: carried over from this earlier candidate because the
   * worker kept the decision unchanged and it had passed (core/rounds.ts). */
  carriedFrom?: string;
}

/** The owner's `override` command (design §7.4): "approve or reject a
 * delegated decision; recorded beside the ballots." It participates in
 * tally.ts's pass/fail exactly like a fourth, owner-authored ballot bound to
 * the decision's current version. */
export interface Override {
  decisionId: string;
  vote: Vote;
  boundCandidateSha: string;
  boundContractVersion: ContractVersion;
  boundRecordVersion: number;
}

// ---------------------------------------------------------------------------
// §6.3 Reviews
// ---------------------------------------------------------------------------

export type HonoredStatus = "honored" | "not_honored";
export type FindingStatement = "confirm" | "withdraw";

/** Work packet 2a addition (pure, additive): the second turn of a real
 * two-turn review (design §6.1's REVIEWING/§5) adds a ballot per votable
 * decision and any newly raised findings to the same submit_review call.
 * Both are plain-language disclosures the conductor assembles into bound
 * `Ballot`/`Finding` records (id/version/boundCandidateSha/... assigned by
 * the conductor, exactly like a worker's decision disclosure) — a reviewer
 * never supplies those binding fields itself. */
export interface BallotDisclosure {
  decisionId: string;
  vote: Vote;
  rationale: string;
  evidence: string[];
  contractObjection?: boolean;
}

export interface FindingDisclosure {
  kind: FindingKind;
  severity: FindingSeverity;
  evidence: string;
  linkedDecisionId?: string;
  /** Plan 01g: a reviewer may say the finding is that a criterion cannot be
   * met as written, naming it verbatim; the conductor records it as an
   * amendment record and the finding cites it. */
  criterionDispute?: CriterionDispute;
  reproduction?: { command: string };
  /** Plan 2c: the id of an already-open finding this one repeats; the
   * conductor records the reviewer on that finding instead of a duplicate. */
  sameAs?: string;
  /** Plan 05e: a runnable test or command that shows the claimed failure.
   * The conductor re-runs it in the candidate's checkout; the finding is
   * published only if it exits non-zero (and dropped with the run recorded
   * when it passes). */
  runnable?: string;
}

/** Plan 04b: one entry of a reviewer's `blockers` list. Exactly a finding
 * disclosure without a severity — a blocker is always `blocking` (there is
 * no such thing as an advisory blocker), so the reviewer never states one.
 *
 * Deliberately no `sameAs`: a blocker is NEVER folded into an existing
 * finding. Doing so could drop the stop-the-work request entirely (no
 * blocker message, no panel, and no blocking force at all when the target
 * finding was advisory), so the reviewer states the issue's evidence instead
 * (round-3 review, findings B-1/A-3/M-4). */
export interface BlockerDisclosure {
  kind: FindingKind;
  evidence: string;
  linkedDecisionId?: string;
  criterionDispute?: CriterionDispute;
  reproduction?: { command: string };
  /** Plan 05e: a runnable test or command, checked by the same 3a/3b rule as
   * an ordinary finding. */
  runnable?: string;
}

export interface Review {
  reviewer: Reviewer;
  phaseId: string;
  candidateSha: string;
  contractVersion: ContractVersion;
  correctionStatements: { correctionId: string; status: HonoredStatus }[];
  findingStatements: { findingId: string; status: FindingStatement; evidence?: string }[];
  /** Plan 04b: a reviewer's separate `blockers` list — "stop the work until
   * the owner decides". Each entry is a finding in substance (kind, evidence,
   * optional linked decision / criterion dispute / reproduction) and is
   * always raised at `blocking` severity: a raised blocker is, from that
   * moment, a `blocker` message (raw) AND a blocking finding, effective at
   * once, exactly like today's blocking findings. */
  blockers?: BlockerDisclosure[];
  /** Work packet 2a addition: present on a real reviewer's turn-2
   * submission; absent (or empty) for phase 1's stub reviews, which cast
   * ballots outside the Review record entirely (see conductor.ts's
   * `#castStubBallots`, kept for `stubReviews: true` runs). */
  ballots?: BallotDisclosure[];
  findings?: FindingDisclosure[];
  /** Plan 2c: this reviewer's own turn-1 discoveries that are the same
   * choice as another listed record. */
  discoveryMatches?: Array<{ discoveryId: string; sameAs: string }>;
  /** Plan 05e: this reviewer's mark on every open finding/blocker from an
   * earlier round that the turn-2 prompt listed — `resolved` or `open`, with
   * evidence. A majority `resolved` moves the message to `resolved`; a
   * majority `open` (or no majority) leaves it live. */
  resolutionStatements?: FindingResolutionStatement[];
  /** Plan 06b: this reviewer's verdict on every requirement and constraint
   * (`met`/`unmet`/`partial`) and every architecture item
   * (`fits`/`deviates`/`unclear`), each with evidence. A review that omits an
   * id is incomplete under the existing complete-ballot rule. */
  items?: import("./items.ts").ItemVerdict[];
  arch?: import("./items.ts").ArchVerdict[];
}

/** Plan 05e: one reviewer's mark on an earlier round's open finding/blocker
 * message. */
export interface FindingResolutionStatement {
  messageId: string;
  status: "resolved" | "open";
  evidence?: string;
}

// ---------------------------------------------------------------------------
// §7.4 Owner commands
// ---------------------------------------------------------------------------

export interface SteerCommand {
  kind: "steer";
  text: string;
  boundAttemptId: string;
}

export interface NoteCommand {
  kind: "note";
  text: string;
  phaseId: string;
}

export interface ResolveCommand {
  kind: "resolve";
  requestId: string;
  requestVersion: number;
  option: string;
  /** Optional scope note for an option that needs one (the open_finding
   * `accept_risk` option, design §4.2). `EvOwnerRequestResolved` already
   * carries `note?`; this is the command-side plumbing so the decision
   * view's `1` key can supply it and the option actually applies. */
  note?: string;
  boundCandidateSha: string;
  boundContractVersion: ContractVersion;
}

export interface OverrideCommand {
  kind: "override";
  decisionId: string;
  decisionVersion: number;
  vote: Vote;
  boundCandidateSha: string;
  boundContractVersion: ContractVersion;
}

export interface AcceptFindingCommand {
  kind: "accept-finding";
  findingId: string;
  findingVersion: number;
  scope: string;
  boundCandidateSha: string;
  boundContractVersion: ContractVersion;
}

export interface ReviseCommand {
  kind: "revise";
  targetRecordId: string;
  targetRecordVersion: number;
  correctionText: string;
  contractChange: boolean;
  boundCandidateSha: string;
  boundContractVersion: ContractVersion;
}

export interface AmendCommand {
  kind: "amend";
  phaseId: string;
  replacingContractVersion: ContractVersion;
  newContractVersion: ContractVersion;
}

export interface UnneededCommand {
  kind: "unneeded";
  requestId: string;
}

/** Contract v1 §3: the owner's verdict on a published message. Bound exactly
 * like every other record-level command (the message id is the record id and
 * the message version is the record version). */
export interface VerdictCommand {
  kind: "verdict";
  messageId: string;
  verdict: "accept" | "refuse";
  reason?: string;
  boundCandidateSha: string;
  boundContractVersion: ContractVersion;
  messageVersion: number;
}

/** design §3.5/§10.4 (`s`): "on a sampled item: 'should have been surfaced'
 * (records a miss)". The sampled item is either a record the conductor
 * sampled (a `detail` decision, an unreferenced hunk, ...) or an
 * unreferenced change; `recordId` names it, `sample` optionally names the
 * sample kind so the pilot can break the miss rate down. */
export interface MissCommand {
  kind: "miss";
  recordId: string;
  sample?: string;
}

export interface PauseResumeModeCommand {
  kind: "pause" | "resume" | "mode";
  mode?: "delegate" | "co-work";
}

/** §7.4/§9.3 (plan 2d): an owner's correction typed into the input box while
 * the phase is AWAITING_OWNER. It is not bound to a single record (the owner
 * is answering whatever the phase is parked on), so it carries only the
 * text and the conductor resolves the open owner requests itself. */
export interface CorrectionCommand {
  kind: "correction";
  text: string;
  phaseId: string;
}

export type OwnerCommand =
  | SteerCommand
  | CorrectionCommand
  | NoteCommand
  | ResolveCommand
  | OverrideCommand
  | AcceptFindingCommand
  | ReviseCommand
  | AmendCommand
  | UnneededCommand
  | VerdictCommand
  | MissCommand
  | PauseResumeModeCommand;

// ---------------------------------------------------------------------------
// §6.1 / §8 / §9 Phase and run state
// ---------------------------------------------------------------------------

export type PhaseStateName =
  | "READY"
  | "BASELINE"
  | "IMPLEMENTING"
  | "FREEZING"
  | "CHECKING"
  | "PROBING"
  | "REVIEWING"
  | "EVALUATING"
  | "RESOLVING"
  | "FINAL_CHECKING"
  | "GATING"
  | "ACCEPTED"
  | "PUBLISHING"
  | "DONE"
  | "REPAIRING"
  | "AWAITING_OWNER"
  | "BLOCKED";

export type RunStateName = "RUN_ACTIVE" | "RUN_PAUSED_BUDGET" | "ENV_BLOCKED";

// ---------------------------------------------------------------------------
// Plan 05i: environment preflight
// ---------------------------------------------------------------------------

/** One executable the preflight resolved (or could not). */
export interface EnvTool {
  name: string;
  path?: string;
}

/** The reason a run is `ENV_BLOCKED`: a declared command's executable is not
 * on the conductor's PATH (`preflight`), or a check/gate exited 126/127
 * (`check`). The details are shown in `tt status`, the status view and the
 * program buffer, and are what `tt resume` retries the preflight over. */
export interface EnvBlockInfo {
  kind: "preflight" | "check";
  at?: string;
  /** preflight: the executables not found; check: the command that exited. */
  missing?: string[];
  path?: string;
  stage?: "baseline" | "checks" | "probe" | "gate" | "worker";
  command?: string;
  exitCode?: number | null;
  tail?: string;
}

/** Plan 05i: what the run recorded about its environment — every tool the
 * preflight resolved at start, and the block when one is missing. */
export interface PhaseEnv {
  path?: string;
  tools?: EnvTool[];
  blocked?: EnvBlockInfo;
  /** Plan 05i / finding M-6: the run-axis state to restore when the
   * environment block clears. A run that was paused for budget returns to
   * `RUN_PAUSED_BUDGET`, so clearing the block never silently runs past an
   * exhausted budget. Absent defaults to `RUN_ACTIVE`. */
  resumeRun?: RunStateName;
}

// ---------------------------------------------------------------------------
// §7.4/§9.3 (plan 2d): owner input, recorded
// ---------------------------------------------------------------------------

export type OwnerInputKind = "steer" | "note" | "correction" | "directive" | "withdraw";

/** The state the conductor actually recorded for one owner input. `sent` is
 * never logged: it is derived by the read-only views from a pending inbox
 * file that the conductor has not picked up yet ("not picked up" once 30 s
 * have passed). Every other state is an explicit `OWNER_INPUT_RECORDED`
 * event. */
export type OwnerInputState =
  | "delivered" // steer acknowledged by Pi (deliver.done)
  | "noted" // note queued for the next worker attempt
  | "queued" // plan 06d: a correction outside AWAITING_OWNER, or a steer with no
  // running worker, queued for the next worker attempt (never refused)
  | "correction-started" // AWAITING_OWNER correction: requests resolved, repair started
  | "delivery-uncertain" // steer intent recorded, no acknowledgement (never resent)
  | "reverted" // plan 01g: a correction naming an amendment id restored its criterion
  | "refused"; // conductor refused: terminal phase, or no running worker for a steer

export interface OwnerInputRecord {
  id: string; // the inbox command id (<run>/inbox/<id>.json)
  kind: OwnerInputKind;
  text: string; // the text the owner sent, verbatim
  state: OwnerInputState;
  attemptId?: string; // steer: the worker attempt id it was bound to
  reason?: string; // refused / delivery-uncertain detail
  at: string; // ISO timestamp the conductor recorded
}

// ---------------------------------------------------------------------------
// Plan 01i: owner directives (design 01_ref_design.md D5, runtime §8)
// ---------------------------------------------------------------------------

/** How far a directive reaches: its own phase (every agent now, and in every
 * later prompt) or the whole program (every running node now, every node
 * started later). */
export type DirectiveScope = "phase" | "program";

/** The outcome of one directive's immediate Pi steer to one live agent of
 * this run, keyed by the agent's target label ("worker", "M", "A", "B"). A
 * target with no entry yet is still in flight. */
export type DirectiveDeliveryState = "delivered" | "delivery-uncertain";

/** Plan 01i: every text the owner sent through the input box, recorded as a
 * numbered directive (`OD-1`, `OD-2`, …) that is part of the phase until
 * withdrawn. It is steered at once to every live agent of the run (`targets`,
 * `deliveries`) and included verbatim, newest last, under "Owner directives
 * (binding)" in every later prompt — worker attempts and repairs, reviewer
 * turns 1 and 2, and re-dispatched or fresh agents. It binds reviewers as
 * part of the contract: a candidate that follows a directive cannot be
 * faulted for doing so, even where the plan's text says otherwise, and a
 * candidate that violates one is a blocking contract finding citing the id. */
export interface OwnerDirective {
  id: string; // "OD-1"
  seq: number; // 1
  text: string; // what the owner typed, verbatim
  scope: DirectiveScope;
  status: "in-force" | "withdrawn";
  commandId: string; // the inbox command id it arrived as
  at: string; // ISO timestamp the conductor recorded it
  /** The live agents it was steered to when sent ("worker", "M", "A", "B";
   * a reviewer letter per live reviewer). Empty when no agent was live — it
   * still reaches every later prompt. */
  targets: string[];
  /** Per-target outcome of that steer; a target in `targets` with no entry
   * has not acknowledged yet (shown `⧗`). */
  deliveries: Record<string, DirectiveDeliveryState>;
  withdrawnAt?: string;
  /** A program-wide directive that reached this phase through its plan (a
   * node started after the directive was issued), not through its inbox. */
  seeded?: boolean;
}

export interface Attempt {
  n: number;
  interrupted?: boolean;
}

export interface CandidateRef {
  sha: string;
  contractVersion: ContractVersion;
}

export interface ProbeResult {
  candidateSha: string;
  head: string;
  probedI?: string;
  passed?: boolean;
  interrupted?: boolean;
}

/** Plan 05d: one new failing test of a candidate's checks, labelled by its
 * own single-test re-run: `reproducesAlone` is a real failure; `loadOnly`
 * passed when re-run alone and is never a repair item. */
export interface CheckFailureClass {
  name: string;
  /** The single-test command that was re-run, when one could be built. */
  rerunCommand?: string;
  reproducesAlone: boolean;
  loadOnly: boolean;
  failingExitCode: number | null;
  rerunExitCodes: Array<number | null>;
}

export interface ChecksResult {
  candidateSha: string;
  passed?: boolean;
  interrupted?: boolean;
  /** Plan 05d: every new failing test of the candidate's last check (each
   * re-run alone), so the worker's repair prompt labels a real failure and a
   * flake apart (finding #35). Cleared when the next candidate freezes. */
  failures?: CheckFailureClass[];
}

/** Plan 05d: the last check failure of a candidate, kept across the repair
 * freeze (`checks` is cleared then), so the reviewers of the repaired
 * candidate are told which tests failed and how each was classified — the
 * reviewer half of requirement (2), which would otherwise never see a failed
 * check (finding A-5). */
export interface LastCheckFailures {
  candidateSha: string;
  failures: CheckFailureClass[];
  /** Plan 05d: the candidate whose freeze followed this failure — the only
   * candidate whose reviewers are told about it. Set once, when that
   * candidate freezes, so a LATER candidate (which repairs a review finding,
   * not the check) is never shown a two-candidates-stale split. */
  repairedBy?: string;
}

export interface ReviewSlot {
  review?: Review;
  timedOutOnce?: boolean;
}

/** One outstanding dispatch the conductor has started but not yet gotten a
 * completion event for (design §9.3's "intent" half of intent/completion).
 * Keyed by a fixed set of action kinds — see next.ts. */
export interface InFlightEntry {
  actionId: string;
}

export type InFlightKey =
  | "run_baseline"
  | "dispatch_worker"
  | "freeze"
  | "run_checks"
  | "dispatch_probe"
  | `review_${string}`
  | "dispatch_evaluation_tradeoff"
  | "dispatch_evaluation_finding"
  | "dispatch_evaluation_blocker"
  | `dispatch_panel_${string}_${number}`
  | `dispatch_round_panel_${number}`
  | "run_gate"
  | "run_final_checks"
  | "publish_cas";

/** The full state of one phase, as reduce()/next() see it. */
export interface PhaseState {
  runId: string;
  phaseId: string;
  contract: PhaseContract;
  phase: PhaseStateName;
  blockedReason?: string;
  attempt: Attempt;
  integrationHead: string; // H: the current integration head this phase publishes onto
  candidate?: CandidateRef; // set once FREEZING completes
  checks?: ChecksResult;
  probe?: ProbeResult;
  /** Plan 06h: keyed by seat name, so any `#+TT_REVIEWERS` list works. An
   * old state/fixture with `M`/`A`/`B` keeps working unchanged. */
  reviews: Record<string, ReviewSlot>;
  decisions: Decision[];
  findings: Finding[];
  ownerRequests: OwnerRequest[];
  corrections: Correction[];
  ballots: Ballot[];
  overrides: Override[];
  /** Outstanding dispatches next() must not re-emit (design §9.3's intent
   * half). Cleared by the matching completion event, or by REVISE's
   * cancel_in_flight (design §7.5 step 2). */
  inFlight: Partial<Record<InFlightKey, InFlightEntry>>;
  /** Phase 1b addition (pure, additive): the raw decision disclosures a
   * SUBMIT_PHASE carried, held here until FREEZE_COMPLETED assigns each one
   * an id/version/boundCandidateSha/boundContractVersion and moves it into
   * `decisions` (design §6.2's freeze boundary — a decision is not bound to
   * a real candidate until the freeze that produces that candidate
   * completes). Cleared once FREEZE_COMPLETED consumes it. */
  pendingDisclosures?: DecisionDisclosure[];
  /** Plan 2c: the worker's kept/changed/withdrawn statements about its prior
   * decisions, carried by SUBMIT_PHASE and consumed by FREEZE_COMPLETED. */
  pendingPrior?: PriorDecisionStatement[];
  /** Plan 2c: the number of candidates frozen in this phase so far (the
   * review round); 0 before the first freeze. */
  round?: number;
  worktreeTainted?: boolean;
  integrityViolated?: boolean;
  repairRoundsUsed: number;
  repairRoundsGranted: number; // 3 base, +3 per correction (§7.5, §8.1)
  publishedI?: string;
  /** §7.4 `note`: owner notes queued from the inbox, delivered verbatim in
   * the next worker attempt's prompt (see `deliveredNoteCount` for the
   * prefix already sent). Optional/absent on the many test fixtures that
   * predate the inbox. */
  ownerNotes?: string[];
  /** §7.4 `note`: how many of `ownerNotes` have already been delivered in a
   * worker attempt's prompt. The conductor sends `ownerNotes.slice(count)`
   * and then logs NOTES_DELIVERED, so a note reaches the *next* attempt
   * (§7.4) instead of every later one. Optional/absent = none delivered. */
  deliveredNoteCount?: number;
  /** §3.5/§10.4 `s`: record ids the owner marked "should have been
   * surfaced" — the observed miss sample, recorded as a pilot metric. */
  misses?: string[];
  /** §7.4 `unneeded` / §11.4's "unnecessary escalations" metric: owner
   * request ids the owner marked "did not need me". The request itself
   * stays `open` (so it can still be resolved and is never duplicated or
   * stranded); this list is the metric, not a resolution. */
  unneededRequestIds?: string[];
  /** Plan 2d (§7.4/§9.3): every text the owner sent through the input box,
   * with the effect the conductor actually recorded for it. The status
   * buffer's "Owner input" section renders this — never an inferred or
   * optimistic state. Ordered by id (stable across a restart). */
  ownerInputs?: OwnerInputRecord[];
  /** Plan 01g: the raw `criterionDispute` a SUBMIT_PHASE carried, held here
   * until FREEZE_COMPLETED assembles it into an amendment record bound to
   * the new candidate (exactly like `pendingDisclosures`). */
  pendingDispute?: CriterionDispute;
  /** Plan 01i: the owner directives in force (or withdrawn) in this phase,
   * in the order the owner sent them. Rebuilt by folding the log, so a
   * directive survives a conductor restart; included, newest last, in every
   * later prompt. */
  ownerDirectives?: OwnerDirective[];
  /** Contract v1 (core/messages.ts): every trade-off, finding and blocker
   * raised in this phase, in raise order. Folded from MESSAGE_* events, so a
   * conductor restart rebuilds it from the log alone. */
  messages?: Message[];
  /** Plan 05j: the review ledger's entries — one topic each, a projection of
   * the ENTRY_* events (core/entries.ts). Folded from the log, so a conductor
   * restart rebuilds it, and the views are pure projections of it. */
  entries?: Entry[];
  /** Plan 05j: the candidate whose round the entry curator has passed. The
   * conductor runs the curator once per round, after the reviews and before
   * the evaluators; evaluators wait until this names their candidate. */
  curatedFor?: string;
  /** Plan 04a: the base-baseline stage's own recovery bookkeeping. Set once
   * an interrupted `run_baseline` has been re-dispatched, so a second loss
   * takes the timed-out path instead of re-dispatching again. */
  baseline?: { interruptedOnce?: boolean };
  /** Plan 05i: what the run recorded about its environment at start — every
   * tool the preflight resolved, and the block when one is missing. */
  env?: PhaseEnv;
  /** Plan 04a: the EVALUATING stage's own state, PER MESSAGE TYPE (one
   * fresh evaluator per type that has raw messages this round): `settled` is
   * set by that type's `EVALUATOR_FINISHED` (or its timeout); `timedOut`
   * records that only that type's raw messages were published unevaluated;
   * `interruptedOnce` is the same one-redispatch bookkeeping as `baseline`. */
  evaluation?: { types?: Partial<Record<MessageType, EvaluatorOutcome>> };
  /** Plan 04b: the blocker panel's state, one entry per raw blocker message
   * of this round (keyed by the blocker message id). Plan 05e adds the
   * round panel for trade-offs and blocking findings. EVALUATING completes
   * only once every evaluator AND every panel has settled. */
  panel?: { blockers?: Record<string, PanelState>; round?: RoundPanelState };
  /** Plan 05e: candidates the three reviewers approved (all three reviews
   * in, no open blocking finding bound to the candidate), keyed by their
   * full git tree object id. A resubmission whose tree matches an approved
   * one re-reviews only the amended criterion (finding #34). */
  approvedCandidates?: Array<{ candidateSha: string; tree: string }>;
  /** Plan 05d: every flake observed in this phase (a new failing test that
   * passed when re-run alone), folded from FLAKE_OBSERVED events. Evidence
   * for the status, `tt summary` and a restart. */
  flakes?: FlakeObservation[];
  /** Plan 05d: the previous candidate's check failures and their
   * classifications, kept across the repair freeze so the reviewers of the
   * repaired candidate see them (finding A-5), and marked with that candidate
   * so a later one is not shown a stale split (finding A-9). */
  lastCheckFailures?: LastCheckFailures;
  /** Decision briefs (one per open owner item), produced once per round after
   * evaluation and folded from BRIEFS_RECORDED. The views render these above
   * the evidence; the status `needs you` line shows the first one's
   * question. */
  briefs?: DecisionBrief[];
  /** Plan 05k (OD-6): the `<candidateSha>::<itemId>` keys whose only brief on
   * that candidate is a backstop and for which the brief writer has already
   * been re-dispatched once. Folded from BRIEF_RETRY_ATTEMPTED, so the retry
   * is once per candidate even across a restart. */
  briefRetries?: string[];
  /** Plan 06g: the phase's rounds, in order — each round's base, lanes,
   * candidates, pick votes and winner. Absent for a plan without
   * `#+TT_WORKERS`, where no round event is ever recorded. */
  rounds?: RoundRecord[];
  /** Plan 06g (A6): the owner's `accept_carried` decision at budget
   * exhaustion — "accept with carried items". Set only by that owner option;
   * `accept()` honours it, and the views list `carriedItems`. */
  acceptedWithCarried?: boolean;
  /** Plan 06g: the open items carried past this phase, by finding id. */
  carriedItems?: string[];
  /** Plan 06g (ODP-2): the candidate sha the owner's carry was given for.
   * `accept()` accepts only when it equals the candidate under acceptance, so
   * a carry can never accept a stale or failing candidate. */
  carriedCandidateSha?: string;
  /** Plan 06g (A4): the test names that failed at THIS round's base — the
   * phase base in round 1, the previous candidate in a repair. Recorded at
   * the freeze, so `#regressionTests` compares against the round's base, not
   * the phase base. Absent in round 1 (the phase baseline is the base then). */
  roundBaseFailures?: string[];
  /** Plan 06g (A5): the target phase each carried item goes to, keyed by the
   * finding/message id (`tt carry <run> <id> --to <phase-id>`). `accept_carried`
   * carries the open items without a named target, so an id may be absent. */
  carriedTo?: Record<string, string>;
  /** Plan 06b: the worker's `submit_coverage` payload. The freeze is refused
   * until it covers every R, C and A. */
  coverage?: import("./items.ts").Coverage;
  /** OD-2 A2 / owner steer: the attempt number this coverage was submitted
   * in. The freeze requires it to equal the current `attempt.n`, so an
   * earlier attempt's report can never satisfy a later attempt's gate. */
  coverageAttempt?: number;
  /** Plan 06b: every `test` verify resolved against the candidate's check
   * run. A missing or failed one is a blocking finding anchored to its item. */
  checkResolution?: import("./items.ts").VerifyResolution[];
  /** Plan 06b: the items the owner recorded with `tt evidence`, and the
   * recording. The phase parks AWAITING_OWNER until every `evidence` item is
   * here. */
  itemEvidence?: Array<{ id: string; text: string; at?: string; commandId?: string }>;
  /** Plan 06b: architecture item ids whose deviation the owner accepted as a
   * trade-off. */
  acceptedDeviations?: string[];
  /** Plan 06b: architecture items whose `:WHERE:` symbol the conductor
   * grepped for and did not find in the candidate. Recorded as `deviates`
   * before any reviewer is asked. */
  archSymbolDeviations?: string[];
  /** Plan 06b: verdicts the evaluator's re-verification overturned, each
   * counted against its seat. */
  overturns?: import("./items.ts").Overturn[];
  /** Plan 06b (OD-1 R3b): the evaluator's substantive re-check of an item's
   * majority verdict, recorded with what it checked. A `contradicted` check
   * overturns the majority (the FINDING_VERIFIED path). */
  itemChecks?: Array<{ itemId: string; verdict: "confirmed" | "contradicted" | "unchecked"; evidence: string }>;
  /** Plan 06c: the candidate whose `final` check run already passed, so
   * RESOLVING accepts it without asking for the final check again. Cleared
   * when a new candidate freezes. */
  finalChecksPassedFor?: string;
}

/** Plan 05d: one recorded flake, as folded from a FLAKE_OBSERVED event. */
// ---------------------------------------------------------------------------
// Plan 06g: the round (K lanes, one base, one winner)
// ---------------------------------------------------------------------------

/** One lane's result inside a round. `sha` is set when the lane froze a
 * candidate; `ok` when its checks ran (true = passed). A lane that crashed,
 * timed out or produced no submission leaves both unset and carries `note`,
 * so the status can say why. */
export interface LaneCandidateRecord {
  lane: string; // "a", "b", …
  sha?: string;
  ok?: boolean;
  note?: string;
  /** Plan 06g2 (A3): the reviews this candidate drew, one per seat — the
   * round's own per-candidate record, so the review buffer groups them by
   * candidate. Set only once a review arrives; absent on a candidate whose
   * reviews have not been recorded (or on a plan without lanes). */
  reviews?: Array<{ seat: string; review: Review }>;
}

/** One seat's vote in a round's pick turn. `why` is the one-line reason. */
export interface PickVote {
  seat: string; // "M", "A", "B"
  lane: string;
  why: string;
}

/** Plan 06h (A3): the one revote a 3+-candidate round runs when no lane has
 * a strict majority. `lanes` are the top two by first-round votes. */
export interface RoundRevote {
  lanes: string[];
  votes: PickVote[];
}

/** One round of a phase: the base it started from, its lanes, their
 * candidates, the pick turn's votes and the winner. Reduced from the
 * ROUND_STARTED / CANDIDATE_SUBMITTED / CANDIDATE_CHECKED / PICK_VOTE /
 * CANDIDATE_PICKED record events, so a restart rebuilds it from the log. */
export interface RoundRecord {
  round: number;
  base: string;
  lanes: string[];
  candidates: LaneCandidateRecord[];
  votes: PickVote[];
  /** Plan 06h (A3): present only when 3+ candidates split with no strict
   * majority and the top two went to a revote. */
  revote?: RoundRevote;
  picked?: { lane: string; sha: string; votes: number };
}

export interface FlakeObservation {
  name: string;
  command: string;
  rerunCommand?: string;
  failingExitCode: number | null;
  rerunExitCodes: Array<number | null>;
  loadAverage?: number | null;
  savedRound: boolean;
  candidateSha?: string;
}

export type RunStatus = RunStateName;

/** The top-level reducer state: one phase's state plus the run-level budget axis. */
export interface State {
  run: RunStatus;
  phase: PhaseState;
}

// ---------------------------------------------------------------------------
// Events — the total vocabulary reduce() understands. Anything else, or an
// event that arrives when its guard does not hold, is rejected explicitly.
// ---------------------------------------------------------------------------

export interface EvAttemptStarted {
  type: "ATTEMPT_STARTED";
  /** Plan 04a: whether this attempt must take the base baseline first. The
   * conductor decides it (a baseline already on disk for this exact base
   * tree skips the stage — the 01e reuse rule); reduce() only routes it. */
  baselineNeeded?: boolean;
}
/** Phase 1b addition (pure, additive — round of review item 3): carries the
 * *raw* disclosures, not assembled Decision records. A worker's submit_phase
 * happens before any candidate exists (the freeze it triggers is what
 * produces one), so a Decision's binding fields cannot be filled in yet.
 * reduce.ts stashes these in `phase.pendingDisclosures`; FREEZE_COMPLETED is
 * what assembles and binds them (design §6.2, §7.1). */
export interface EvSubmitPhase {
  type: "SUBMIT_PHASE";
  disclosures: DecisionDisclosure[];
  /** Plan 2c: statements about prior decisions (repair attempts only). */
  prior?: PriorDecisionStatement[];
  /** Plan 01g: a criterion the worker says cannot be met as written; freeze
   * turns it into an amendment record (a `reserved` decision) bound to the
   * new candidate. */
  dispute?: CriterionDispute;
}
/** Plan 04a: the base baseline finished (run or reused). */
export interface EvBaselineCompleted {
  type: "BASELINE_COMPLETED";
  /** Plan 06c (A3): the parent candidate whose passing check record this run
   * reused as its baseline, when it reused one. */
  reusedFrom?: string;
}
/** Plan 04a: the base baseline could not be taken in time; the checks stay
 * strict and the work continues. */
export interface EvBaselineTimedOut {
  type: "BASELINE_TIMED_OUT";
}
/** Plan 04a: a conductor died during BASELINE. Re-dispatched once; a second
 * loss is BASELINE_TIMED_OUT. */
export interface EvBaselineInterrupted {
  type: "BASELINE_INTERRUPTED";
}
export interface EvAttemptTimedOut {
  type: "ATTEMPT_TIMED_OUT";
}

/** Plan 04a: one message type's evaluator state inside EVALUATING. */
export interface EvaluatorOutcome {
  settled?: boolean;
  timedOut?: boolean;
  interruptedOnce?: boolean;
}

// ---------------------------------------------------------------------------
// Plan 04b: the blocker panel (inside EVALUATING)
// ---------------------------------------------------------------------------

/** One option a `block` vote puts to the owner when the panel escalates. */
export interface PanelOption {
  id: string;
  label: string;
}

export type PanelVoteValue = "block" | "downgrade";

/** One panel seat's state for one raw blocker. `dispatches` is the retry
 * bookkeeping: a seat that times out once is re-dispatched (2), and a second
 * loss makes it `unavailable` — the same one-retry rule REVIEWING uses, kept
 * in state so it survives a conductor restart. */
export interface PanelSeatState {
  dispatches: number;
  vote?: PanelVoteValue;
  reason?: string;
  /** A `block` vote's two or three options for the owner. */
  options?: PanelOption[];
  unavailable?: boolean;
}

/** The panel's verdict on one blocker: `escalate` (2 of 3 voted `block`),
 * `downgrade` (2 of 3 voted `downgrade`), or `incomplete` (no two seats
 * agreed — including two seats unavailable after their retry). */
export type PanelOutcome = "escalate" | "downgrade" | "incomplete";

export interface PanelDecision {
  outcome: PanelOutcome;
  reason?: string;
  /** Escalations only: the options the owner chooses from. */
  options?: PanelOption[];
}

/** One raw blocker's panel. The entry exists from the moment the phase
 * enters EVALUATING (transitions.ts records the raw blocker ids there), so
 * "which blockers need a panel" is a fact of state, never a race with the
 * blocker evaluator publishing the message. */
export interface PanelState {
  seats?: Record<string, PanelSeatState>;
  decided?: PanelDecision;
}

// ---------------------------------------------------------------------------
// Plan 05e: the round panel (trade-offs and blocking findings)
// ---------------------------------------------------------------------------

/** One item's verdict from one round-panel seat. `keep` publishes a trade-off
 * to the owner (and blocks acceptance for a finding); `drop` drops a
 * trade-off and makes a finding advisory; `downgrade` makes a finding
 * advisory. */
export type RoundPanelVerdict = "keep" | "drop" | "downgrade";
export interface RoundPanelItemVote {
  messageId: string;
  verdict: RoundPanelVerdict;
  reason: string;
}

/** One round-panel seat: it votes on EVERY pending item in one batch. A seat
 * that times out once is re-dispatched, exactly like a blocker seat. */
export interface RoundPanelSeatState {
  dispatches: number;
  votes?: RoundPanelItemVote[];
  unavailable?: boolean;
}

export interface RoundPanelState {
  seats?: Record<string, RoundPanelSeatState>;
  /** Set once the seats' votes have been counted and applied. */
  decided?: boolean;
}

/** The round panel's outcome for one item. */
export type RoundPanelOutcome = "keep" | "drop" | "downgrade";

/** Plan 04a: one type's evaluator finished its round. A record event inside
 * EVALUATING; the phase completes only once every dispatched type has. */
export interface EvEvaluatorFinished {
  type: "EVALUATOR_FINISHED";
  messageType: MessageType;
  evaluated: number;
}

/** Plan 04a: one type's evaluator did not settle in time; only that type's
 * raw messages are published unchanged, marked `unevaluated`. A record event
 * inside EVALUATING. */
export interface EvEvaluationTimedOut {
  type: "EVALUATION_TIMED_OUT";
  messageType: MessageType;
}

/** Plan 04a: a conductor died while one type's evaluator ran; that dispatch
 * is re-dispatched once. A record event inside EVALUATING. */
export interface EvEvaluationInterrupted {
  type: "EVALUATION_INTERRUPTED";
  messageType: MessageType;
}

/** Plan 04a: all dispatched evaluators settled; EVALUATING -> RESOLVING. */
export interface EvEvaluationCompleted {
  type: "EVALUATION_COMPLETED";
}

/** Plan 04b: one panel seat's vote on one raw blocker, a record event inside
 * EVALUATING. A `block` vote must propose two or three options for the
 * owner. The seat must not have voted or been marked unavailable already. */
export interface EvPanelVote {
  type: "PANEL_VOTE";
  blockerId: string;
  seat: number; // 1, 2 or 3
  vote: PanelVoteValue;
  reason: string;
  options?: PanelOption[];
}

/** Plan 04b: one seat is unavailable after its own retry (a second timeout,
 * or a conductor crash during its dispatch). A record event; the other seats
 * still decide the panel. */
export interface EvPanelSeatUnavailable {
  type: "PANEL_SEAT_UNAVAILABLE";
  blockerId: string;
  seat: number;
  reason?: string;
}

/** Plan 04b: the panel has counted its seats and decided. A record event
 * inside EVALUATING — the SINGLE exit from EVALUATING stays
 * EVALUATION_COMPLETED, so the phase cannot leave while an evaluator is
 * still working; the recorded outcome picks which row that exit takes
 * (escalate -> AWAITING_OWNER, downgrade -> REPAIRING, incomplete ->
 * RESOLVING). `outcome` must equal what the recorded votes imply. */
export interface EvPanelDecided {
  type: "PANEL_DECIDED";
  blockerId: string;
  outcome: PanelOutcome;
  reason?: string;
  /** Escalations only: the two or three options the owner chooses from. */
  options?: PanelOption[];
}

// ---------------------------------------------------------------------------
// Plan 05e: the round panel, finding verification and approved candidates
// ---------------------------------------------------------------------------

/** One round-panel seat's batched votes on every pending trade-off and
 * blocking finding. Record event inside EVALUATING. */
export interface EvRoundPanelVote {
  type: "ROUND_PANEL_VOTE";
  seat: number;
  votes: RoundPanelItemVote[];
}

/** One round-panel seat is unavailable after its own retry. */
export interface EvRoundPanelSeatUnavailable {
  type: "ROUND_PANEL_SEAT_UNAVAILABLE";
  seat: number;
  reason?: string;
}

/** The round panel's votes have been counted and applied. Record event. */
export interface EvRoundPanelDecided {
  type: "ROUND_PANEL_DECIDED";
  decisions: Array<{ messageId: string; outcome: RoundPanelOutcome; reason?: string }>;
}

/** Plan 05e: a finding's severity changed against the plan (the evaluator's
 * check) or by the round panel's vote. */
export interface EvFindingSeverityChanged {
  type: "FINDING_SEVERITY_CHANGED";
  findingId: string;
  severity: FindingSeverity;
  reason: string;
  /** `reviewer` is a `sameAs` re-raise, which takes the re-raiser's
   * severity (plan 05e, finding #32). */
  by: "evaluator" | "panel" | "reviewer";
}

/** Plan 05e: a majority of reviewers marked the finding's message resolved,
 * so the finding itself is repaired on the current candidate. */
export interface EvFindingResolvedByVote {
  type: "FINDING_RESOLVED_BY_VOTE";
  findingId: string;
  candidateSha: string;
  reason?: string;
}

/** Plan 05e: what validated a finding — the check record, a run (with its
 * command and exit status), a file:line citation the evaluator checked, or
 * the round panel's vote. */
export interface EvFindingVerified {
  type: "FINDING_VERIFIED";
  findingId: string;
  verified: string;
}

/** Plan 05e: all three reviewers reviewed this candidate with no open
 * blocking finding bound to it. `tree` (the candidate's git tree object id)
 * is what a later amendment-only resubmission is compared against. */
export interface EvCandidateApproved {
  type: "CANDIDATE_APPROVED";
  candidateSha: string;
  tree: string;
}
export interface EvAttemptNoSubmission {
  type: "ATTEMPT_NO_SUBMISSION";
}
export interface EvAttemptInterrupted {
  type: "ATTEMPT_INTERRUPTED";
}
/** Phase 1b addition (pure, additive): `decisions` are the fully assembled,
 * bound Decision records the conductor built from `phase.pendingDisclosures`
 * once `candidateSha` was known (each must validate against
 * schemas/decision.schema.json — the conductor's job, not reduce.ts's).
 * `tainted` is design §2.2's "a sweep that found survivors marks the
 * worktree tainted" outcome for *this* freeze; omitted or `false` clears any
 * earlier taint (a clean freeze means the worktree is trustworthy again). */
export interface EvFreezeCompleted {
  type: "FREEZE_COMPLETED";
  candidateSha: string;
  decisions: Decision[];
  tainted?: boolean;
}
export interface EvFreezeTimedOut {
  type: "FREEZE_TIMED_OUT";
}
export interface EvFreezeInterrupted {
  type: "FREEZE_INTERRUPTED";
}
export interface EvChecksPassed {
  type: "CHECKS_PASSED";
}
export interface EvChecksFailed {
  type: "CHECKS_FAILED";
  /** Plan 05d: the new failing tests, each with its single-test re-run's
   * classification. Absent for a check whose output named no test (the strict
   * rule), which stores no classifications. */
  failures?: CheckFailureClass[];
}
export interface EvChecksInterrupted {
  type: "CHECKS_INTERRUPTED";
}

/** Plan 05d: one new failing test that passed when re-run alone — a flake,
 * recorded per test with the command, both exit statuses and the machine's
 * load average at the failing run. `savedRound` says the check passed only
 * because every new failure was load-only (the repair round it saved). A
 * record-only event: it moves no phase state. */
export interface EvFlakeObserved {
  type: "FLAKE_OBSERVED";
  name: string;
  /** The check command that failed. */
  command: string;
  /** The single-test command re-run alone, when one could be built. */
  rerunCommand?: string;
  /** The failing check's exit status. */
  failingExitCode: number | null;
  /** The exit status of every re-run that was attempted, in order. */
  rerunExitCodes: Array<number | null>;
  /** The machine's 1-minute load average when the check failed, when read. */
  loadAverage?: number | null;
  /** True when the check passed because every new failure was load-only. */
  savedRound: boolean;
  candidateSha?: string;
}

/** Plan 05d / finding #33: a worker (or other agent) launch missed its hello
 * and was retried once with a longer limit. A record-only event, so the
 * status and `tt summary` can count launch retries. */
export interface EvLaunchRetried {
  type: "LAUNCH_RETRIED";
  role: string;
  /** The first limit that timed out, in ms. */
  timeoutMs: number;
  /** The longer limit the retry used, in ms. */
  retryTimeoutMs: number;
  /** Bytes in the session being continued, the retry limit's scale. */
  sessionBytes: number;
}

// ---------------------------------------------------------------------------
// Plan 05i: environment preflight / environment failures
// ---------------------------------------------------------------------------

/** The run's tools were resolved at start (a record-only event: it stores the
 * resolved paths in `phase.env` for the status views, and never moves the
 * phase). */
export interface EvEnvChecked {
  type: "ENV_CHECKED";
  path: string;
  tools: EnvTool[];
  at?: string;
}

/** The preflight found declared command(s) whose executable is not on the
 * conductor's PATH: RUN_ACTIVE -> ENV_BLOCKED, before a baseline or any agent
 * launch. `missing` and `path` are the visible reason. */
export interface EvEnvPreflightFailed {
  type: "ENV_PREFLIGHT_FAILED";
  missing: string[];
  path: string;
  at?: string;
}

/** A check, baseline, probe or gate command exited 126/127 — the shell could
 * not execute it. RUN_ACTIVE -> ENV_BLOCKED, with the command and the log
 * tail; never a baseline, a `checks failed`, a repair or a finding. */
export interface EvEnvCheckFailed {
  type: "ENV_CHECK_FAILED";
  stage: "baseline" | "checks" | "probe" | "gate" | "worker";
  command: string;
  exitCode: number | null;
  tail: string;
  at?: string;
}
export interface EvProbePassed {
  type: "PROBE_PASSED";
  probedI: string;
}
export interface EvProbeFailed {
  type: "PROBE_FAILED";
  evidence: string;
}
export interface EvProbeInterrupted {
  type: "PROBE_INTERRUPTED";
}
export interface EvReviewSubmitted {
  type: "REVIEW_SUBMITTED";
  review: Review;
  /** Plan 06g: the candidate this review judged, when the round reviewed more
   * than one (K ≥ 2). Absent on the single-lane loop, where the phase's own
   * candidate is the only one there could be. */
  candidate?: string;
  /** Plan 06g2: the round whose winner this review is promoted into the
   * phase's review slots for. The round already recorded it as a
   * ROUND_REVIEW_SUBMITTED when it was submitted, so a view that counts the
   * round's reviews must not count this promotion a second time. Absent on
   * the single-lane loop. */
  promotedFrom?: number;
}
export interface EvReviewTimedOut {
  type: "REVIEW_TIMED_OUT";
  reviewer: Reviewer;
}
export interface EvActionStarted {
  type: "ACTION_STARTED";
  action: string; // one of the Action["type"] values next() emits
  actionId: string;
  reviewer?: Reviewer; // required when action === "dispatch_review"
  messageType?: MessageType; // required when action === "dispatch_evaluation"
  blockerId?: string; // required when action === "dispatch_panel"
  seat?: number; // required when action === "dispatch_panel"
}
export interface EvBallotCast {
  type: "BALLOT_CAST";
  ballot: Ballot;
}
export interface EvOverrideCast {
  type: "OVERRIDE_CAST";
  override: Override;
}
export interface EvFindingRaised {
  type: "FINDING_RAISED";
  finding: Finding;
}
export interface EvFindingConfirmedRepaired {
  type: "FINDING_CONFIRMED_REPAIRED";
  findingId: string;
  byReviewer: Reviewer;
  candidateSha: string;
}
export interface EvFindingDisproved {
  type: "FINDING_DISPROVED";
  findingId: string;
  byReviewer: Reviewer;
  evidence: string;
}
export interface EvFindingAcceptedByOwner extends RecordBinding {
  type: "FINDING_ACCEPTED_BY_OWNER";
  findingId: string;
  scope: string;
  by: "owner"; // design §4.2: accepted requires the owner, and only the owner
}
export interface EvFindingSeverityLowered extends RecordBinding {
  type: "FINDING_SEVERITY_LOWERED";
  findingId: string;
  severity: FindingSeverity;
  by: "owner"; // design §3.4: only the owner may lower a finding's severity
}
export interface EvDecisionClassLowered extends RecordBinding {
  type: "DECISION_CLASS_LOWERED";
  decisionId: string;
  class: DecisionClass;
  by: "owner"; // design §3.4: only the owner may lower a decision's class
}
export interface EvOwnerRequestOpened {
  type: "OWNER_REQUEST_OPENED";
  request: OwnerRequest;
}
export interface EvOwnerRequestResolved extends RecordBinding {
  type: "OWNER_REQUEST_RESOLVED";
  requestId: string;
  option: string;
  note?: string;
}
export interface EvAccepted {
  type: "ACCEPTED";
  resolvedCorrectionIds: string[]; // must equal what predicate.ts computes; reduce verifies
}

/** Plan 01f: the phase has nothing left open (`accept(C, K)` holds) and its
 * contract declares a gate, so the phase gates the candidate before
 * accepting it: RESOLVING -> GATING. Emitted by the conductor exactly when
 * next() asks for it, never for a gate-less phase (which keeps the old
 * RESOLVING --ACCEPTED--> ACCEPTED edge). */
export interface EvGateRequired {
  type: "GATE_REQUIRED";
}

/** Plan 06c: the candidate is acceptable and its contract declares a final
 * check — so the final check runs first, once. Emitted by the conductor
 * exactly when next() asks for it, never for a phase without a final check
 * (which keeps the old RESOLVING --ACCEPTED--> ACCEPTED edge). */
export interface EvFinalCheckRequired {
  type: "FINAL_CHECK_REQUIRED";
}

/** Plan 06c: the final check ran and passed, so the candidate is accepted. */
export interface EvFinalChecksPassed {
  type: "FINAL_CHECKS_PASSED";
  candidateSha: string;
}

/** Plan 06c: the final check failed. An ordinary check failure: the phase
 * returns to REPAIRING (or AWAITING_OWNER when the repair budget is spent),
 * with a blocking finding carrying `evidence` (the failing test's name and
 * the check log's tail). */
export interface EvFinalChecksFailed {
  type: "FINAL_CHECKS_FAILED";
  evidence: string;
  /** Plan 06c: the final run's failing tests, classified like a normal check
   * failure, so the repair prompt names them. */
  failures?: CheckFailureClass[];
}

/** Plan 06c: the conductor died while running the final check. Like an
 * interrupted gate, it is rerun rather than counted as passed or failed. */
export interface EvFinalChecksInterrupted {
  type: "FINAL_CHECKS_INTERRUPTED";
}

/** Plan 01f: the gate command failed — a non-zero exit, or a kill at the
 * limit. The phase returns to REPAIRING (or AWAITING_OWNER when the repair
 * budget is exhausted) with a blocking `integration` finding carrying
 * `evidence`: the log's last lines, so the worker is shown the failure. */
export interface EvGateFailed {
  type: "GATE_FAILED";
  evidence: string;
}

/** Plan 01f / design §9.3: the conductor died while gating. An interrupted
 * gate is never a passing one, and never a failing one either: the gate is
 * rerun ("interrupted gates: interrupted, never passed — rerun"). */
export interface EvGateInterrupted {
  type: "GATE_INTERRUPTED";
}
export interface EvPublishIntent {
  type: "PUBLISH_INTENT";
  expectedHead: string;
  candidateI: string;
}
export interface EvPublishCompleted {
  type: "PUBLISH_COMPLETED";
  newHead: string;
}
export interface EvPublishStale {
  type: "PUBLISH_STALE";
  actualHead: string;
}
export interface EvRepairAttemptStarted {
  type: "REPAIR_ATTEMPT_STARTED";
}
export interface EvRepairBudgetExhausted {
  type: "REPAIR_BUDGET_EXHAUSTED";
}
export interface EvResolvingIncomplete {
  type: "RESOLVING_INCOMPLETE";
}
export interface EvRevise extends RecordBinding {
  type: "REVISE";
  correctionId: string;
  targetRecordId: string;
  correctionText: string;
  contractChange: boolean;
}
/** Plan 01g: an amendment decision passed the normal tally (M, plus one of
 * A/B), so the conductor replaces one acceptance item for this phase. The
 * phase returns to a fresh attempt under the new contract version so the
 * *next* candidate is judged against the new wording; no repair round is
 * consumed by the amendment itself. Emitted by the conductor when next()
 * asks for `apply_amendment`. */
export interface EvCriterionAmended {
  type: "CRITERION_AMENDED";
  decisionId: string; // the amendment decision that passed
  newAcceptance: string[]; // the full replacement acceptance list
  newContractVersion: ContractVersion;
}

/** Plan 01g: the owner's correction naming an amendment id restores the
 * criterion's original wording. A phase that already has a candidate takes
 * the AMEND-like transition to CHECKING and clears the evidence bound to the
 * replaced contract version; a phase with no candidate yet (IMPLEMENTING,
 * FREEZING, REPAIRING) is updated record-only. Either way the run never
 * waits for the owner and the input is recorded as state `reverted`. */
export interface EvCriterionReverted {
  type: "CRITERION_REVERTED";
  amendmentId: string;
  newAcceptance: string[]; // the restored acceptance list
  newContractVersion: ContractVersion;
  /** OD-2: the time of the revert, carried on the event so reduce() is pure. */
  at?: string;
}

export interface EvAmend {
  type: "AMEND";
  replacingContractVersion: ContractVersion; // what the sender saw — staleness check
  newContractVersion: ContractVersion;
  /** design §4.2/§4.3: contract findings of this phase the amend itself
   * resolves (e.g. "acknowledge means queued" resolves the finding that
   * said acknowledgement must follow the fill). Each must be an open
   * `contract`-kind finding of this phase. */
  resolvesFindingIds?: string[];
}
export interface EvRunBudgetExceeded {
  type: "RUN_BUDGET_EXCEEDED";
}
export interface EvRunResumed {
  type: "RUN_RESUMED";
}
/** Phase 1b addition (pure, additive — round of review item 4): a launch
 * failure (design §2.1: "a mismatch is a launch failure, not a warning") for
 * a worker or a reviewer's tool set, moving the phase straight to BLOCKED
 * rather than being retried as an ordinary attempt/review failure — a wrong
 * `--tools` allowlist cannot be fixed by another attempt, so consuming a
 * repair round on it is pointless. Carries the full detail (expected,
 * missing, extra) for whoever looks at BLOCKED next. */
export interface EvLaunchFailed {
  type: "LAUNCH_FAILED";
  role: "worker" | "reviewer" | "evaluator" | "panel";
  reviewer?: Reviewer; // set when role === "reviewer"
  expected: string[];
  missing: string[];
  extra: string[];
}

/** §7.4 `note`: an owner note delivered to the next worker attempt's
 * prompt. Record-only (handled by reduce.ts's applyRecordEvent): it moves
 * no phase state, it only appends to `phase.ownerNotes`. */
export interface EvNoteAdded {
  type: "NOTE_ADDED";
  phaseId: string;
  text: string;
}

/** Plan 2d (§7.4/§9.3): records — or updates, keyed by `input.id` — one
 * owner input and the effect the conductor actually observed for it. A
 * record-only event (no phase-state-name change): the status buffer shows
 * exactly what happened, never what was hoped for. */
export interface EvOwnerInputRecorded {
  type: "OWNER_INPUT_RECORDED";
  input: OwnerInputRecord;
}

/** Plan 01i: records one owner directive (`OD-<seq>`) the moment it is
 * accepted. Record-only: it moves no phase-state name, it appends a binding
 * record the phase carries until withdrawn and every later prompt quotes. */
export interface EvDirectiveAdded {
  type: "DIRECTIVE_ADDED";
  directive: OwnerDirective;
}

/** Plan 01i: `withdraw OD-n` — the directive no longer applies. Record-only:
 * the record stays (the status shows it withdrawn) and later prompts omit
 * it. */
export interface EvDirectiveWithdrawn {
  type: "DIRECTIVE_WITHDRAWN";
  directiveId: string;
  at?: string;
}

/** Plan 01i: the observed outcome of one directive's immediate steer to one
 * live agent, keyed by its target label. Record-only. */
export interface EvDirectiveDelivered {
  type: "DIRECTIVE_DELIVERED";
  directiveId: string;
  target: string;
  state: DirectiveDeliveryState;
}

/** Plan 2d (§7.5/§7.4): the owner's correction typed while the phase is
 * AWAITING_OWNER. It resolves every open owner request, grants a fresh
 * 3-round repair allowance (independent of any exhausted budget), queues
 * the text as an owner note so the very next worker attempt's prompt
 * carries it verbatim, and moves the phase to REPAIRING. */
export interface EvOwnerCorrection {
  type: "OWNER_CORRECTION";
  correctionId: string; // the inbox command id
  text: string;
}

/** §7.4 `unneeded` / §11.4's "unnecessary escalations" metric: marks an
 * OPEN owner request as `unneeded` ("did not need me"). Record-only. */
export interface EvOwnerRequestMarkedUnneeded {
  type: "OWNER_REQUEST_MARKED_UNNEEDED";
  requestId: string;
}

/** §3.5/§10.4 `s`: records that a sampled item should have been surfaced.
 * Record-only; the miss rate within the sample is the observed rate. */
export interface EvMissRecorded {
  type: "MISS_RECORDED";
  recordId: string;
  sample?: string;
}

/** §7.4 `note`: records that the first `count` entries of `phase.ownerNotes`
 * were delivered in a worker attempt's prompt, so the next attempt sends
 * only the rest (a note is queued for the NEXT attempt, not every later
 * one). Record-only: it moves no phase state. */
export interface EvNotesDelivered {
  type: "NOTES_DELIVERED";
  phaseId: string;
  count: number;
}

/** Plan 2c: a reviewer matched its own discovery to another listed record
 * (design §3.3): the discovery is superseded by that record, and the
 * reviewer is recorded on it as "also seen by". Record-only. */
export interface EvDecisionMatched {
  type: "DECISION_MATCHED";
  decisionId: string;
  sameAs: string;
  reviewer: Reviewer;
}

/** Plan 2c: a reviewer raised a finding that repeats an already-open one;
 * the reviewer is recorded on the existing finding. Record-only. */
export interface EvFindingAlsoRaised {
  type: "FINDING_ALSO_RAISED";
  findingId: string;
  reviewer: Reviewer;
}

/** Phase 1b work-packet addition (pure, additive — item 3): design §2.2's
 * "a mismatch invalidates that gate's result and marks the run
 * integrity-violated for the owner." A record-only event (no phase-state-
 * name change; handled by reduce.ts's applyRecordEvent, like BALLOT_CAST) so
 * `phase.integrityViolated` is a *logged* fact — it survives a conductor
 * restart via `rebuildState` folding the log, unlike an in-memory-only flag.
 * `stage` names what was being verified (e.g. "checks"); the conductor
 * emits this *before* the stage's own outcome event (e.g. CHECKS_FAILED),
 * so the affected gate's own result already reflects "not passed" — see
 * conductor.ts's `#runChecks`. */
export interface EvIntegrityViolated {
  type: "INTEGRITY_VIOLATED";
  stage: string;
  evidence?: string;
}

/** Work packet 2a addition (pure, additive — a record-only event, like
 * BALLOT_CAST/FINDING_RAISED: no phase-state-name change, no new
 * transitions.ts row). Design §3.3's other two decision sources besides a
 * worker's disclosure: a reviewer-discovered decision (source
 * `reviewer-discovered`, from `submit_discovery`'s second decision source)
 * or a conductor-computed boundary trigger (source `trigger`, design §3.3's
 * "boundary triggers ... a trigger record a reviewer must classify"). The
 * conductor assembles+binds `decision` (id/version/boundCandidateSha/
 * boundContractVersion) exactly as it does for a worker's disclosure at
 * freeze time — see `Conductor#assembleDiscoveredDecision`/
 * `#computeBoundaryTriggers` — and validates it against
 * schemas/decision.schema.json before emitting this. */
export interface EvDecisionAdded {
  type: "DECISION_ADDED";
  decision: Decision;
}

// ---------------------------------------------------------------------------
// Contract v1: message events (core/messages.ts)
// ---------------------------------------------------------------------------

/** A trade-off, finding or blocker is raised as a raw message bound to the
 * phase's current candidate/contract. */
export interface EvMessageRaised {
  type: "MESSAGE_RAISED";
  message: Message;
}
/** A raw message is published (reviewable by the owner). Plan 04a: the
 * evaluator's clean wording rides along as `content`; a bare publish (no
 * evaluator) leaves the raised fields as they were. */
export interface EvMessagePublished {
  type: "MESSAGE_PUBLISHED";
  messageId: string;
  boundCandidateSha: string;
  boundContractVersion: ContractVersion;
  boundRecordVersion: number;
  content?: {
    type: MessageType;
    title: string;
    summary: string;
    context: string;
    evidence: string[];
    planRef?: string;
    importance?: "high" | "medium" | "low";
  };
  /** Set when the evaluator did not evaluate this message (it missed it, or
   * the evaluation timed out): it is published unchanged, marked so. */
  unevaluated?: boolean;
}
/** An evaluator merged a message into another (or into the plan). */
export interface EvMessageMerged {
  type: "MESSAGE_MERGED";
  messageId: string;
  by: "owner" | "evaluator" | "panel" | "vote";
  reason?: string;
  boundCandidateSha: string;
  boundContractVersion: ContractVersion;
  boundRecordVersion: number;
}
/** An evaluator dropped a message as not reviewable. */
export interface EvMessageDropped {
  type: "MESSAGE_DROPPED";
  messageId: string;
  by: "owner" | "evaluator" | "panel" | "vote";
  reason?: string;
  boundCandidateSha: string;
  boundContractVersion: ContractVersion;
  boundRecordVersion: number;
}
/** The owner's verdict on a published message (contract §3). */
export interface EvOwnerVerdict {
  type: "OWNER_VERDICT";
  messageId: string;
  verdict: "accept" | "refuse";
  reason?: string;
  boundCandidateSha: string;
  boundContractVersion: ContractVersion;
  boundRecordVersion: number;
}
/** A message is resolved without an owner verdict. */
export interface EvMessageResolved {
  type: "MESSAGE_RESOLVED";
  messageId: string;
  by: "owner" | "evaluator" | "panel" | "vote";
  reason?: string;
  boundCandidateSha: string;
  boundContractVersion: ContractVersion;
  boundRecordVersion: number;
}
/** Plan 04a item 4: the evaluator reports whether an owner-refused message
 * was addressed. A record event on the message, not a state change. */
export interface EvMessageAddressReported {
  type: "MESSAGE_ADDRESS_REPORTED";
  messageId: string;
  addressed: boolean;
  reason?: string;
  /** Plan 04a / OD-2: the time the report was made. Carried on the event (the
   * conductor stamps it) so reduce() stays a pure function of (state, event)
   * and a rebuild from events.jsonl is byte-identical. */
  at?: string;
  boundCandidateSha: string;
  boundContractVersion: ContractVersion;
  boundRecordVersion: number;
}

/** A message is superseded (never votable again). */
export interface EvMessageSuperseded {
  type: "MESSAGE_SUPERSEDED";
  messageId: string;
  reason?: string;
  boundCandidateSha: string;
  boundContractVersion: ContractVersion;
  boundRecordVersion: number;
}
/** Plan 05j: one round's curator pass is done for `candidateSha`. */
export interface EvEntryCurated {
  type: "ENTRY_CURATED";
  candidateSha: string;
  count: number;
}

/** Plan 05j: a review lint violation, recorded so the view's first line and
 * the log agree. Record-only: the lint never repairs an entry. */
export interface EvReviewLintFailed {
  type: "REVIEW_LINT_FAILED";
  rule: string;
  detail: string;
  at?: string;
}

/** Conductor-emitted at each FREEZE_COMPLETED, one per live message: pins the
 * version bump and whether the content (and contract) survived unchanged. */
export interface EvMessageCarried {
  type: "MESSAGE_CARRIED";
  messageId: string;
  fromCandidate: string;
  toCandidate: string;
  fromVersion: number;
  toVersion: number;
  contentHash: string;
  unchanged: boolean;
  /** Plan 04a: the record-derived content hash this carry compares against,
   * so a later carry can tell an unchanged record from a changed one even
   * when the evaluator rewrote the visible content. */
  sourceContentHash?: string;
}

/** Decision briefs: one per open owner item, produced once per round after
 * evaluation. A record-only event (no phase-state-name change; handled by
 * reduce.ts's applyRecordEvent) merged by requestId, so a conductor restart
 * rebuilds the briefs the views render from the log alone. */
export interface EvBriefsRecorded {
  type: "BRIEFS_RECORDED";
  briefs: DecisionBrief[];
}

/** Plan 05k (OD-6): the brief writer was re-dispatched once for the named
 * items, whose only brief on `candidateSha` was a backstop. A record-only
 * event (handled by reduce.ts's applyRecordEvent) so the once-per-candidate
 * bound survives a conductor restart. */
export interface EvBriefRetryAttempted {
  type: "BRIEF_RETRY_ATTEMPTED";
  candidateSha: string;
  requestIds: string[];
}

/** Plan 06b: the item-loop state, as one record-only event so a conductor
 * restart rebuilds the worker's coverage, the check resolution, the owner's
 * evidence recordings, the accepted deviations and the evaluator's overturns
 * from the log alone. Fields are replaced when present. */
export interface EvItemStateUpdated {
  type: "ITEM_STATE_UPDATED";
  coverage?: import("./items.ts").Coverage;
  coverageAttempt?: number;
  checkResolution?: import("./items.ts").VerifyResolution[];
  itemEvidence?: Array<{ id: string; text: string; at?: string; commandId?: string }>;
  acceptedDeviations?: string[];
  archSymbolDeviations?: string[];
  overturns?: import("./items.ts").Overturn[];
}

/** Plan 06b: every `evidence` item is now recorded, so the phase the owner
 * parked resumes to RESOLVING (and acceptance, if nothing else is open). */
export interface EvEvidenceRecorded {
  type: "EVIDENCE_RECORDED";
  itemId: string;
}

/** Plan 06b (OD-1 R3b): the evaluator's substantive re-check of one item's
 * majority verdict, with the evidence of what it checked. */
export interface EvItemCheckRecorded {
  type: "ITEM_CHECK_RECORDED";
  itemId: string;
  /** `unchecked` is the conductor's own record that the evaluator gave no
   * check for a required item after one re-prompt. */
  verdict: "confirmed" | "contradicted" | "unchecked";
  evidence: string;
}

/** Plan 06g (A5): the owner carries one finding or message to a later
 * phase's plan (`tt carry <run> <id> --to <phase-id>`). Record-only unless
 * the phase is AWAITING_OWNER: there it answers the open request about the
 * record, and once no blocking item remains uncarried it accepts the
 * candidate (`acceptedWithCarried`). owner-commands.ts is the only place
 * that decides what a carry does. */
export interface EvItemCarried {
  type: "ITEM_CARRIED";
  recordId: string;
  /** Which record the id names; absent means the lookup tries a finding
   * first, then a message. */
  recordKind?: "finding" | "message";
  /** The phase id the item is carried to. */
  toPhase: string;
  boundCandidateSha: string;
  boundContractVersion: ContractVersion;
  boundRecordVersion: number;
}

// ---------------------------------------------------------------------------
// Plan 06g: the round's record events (all absent when TT_WORKERS is absent)
// ---------------------------------------------------------------------------

/** A round began: `lanes` are its lane ids, `base` the one version every lane
 * starts from (round 1: the integration branch head). Record-only. */
export interface EvRoundStarted {
  type: "ROUND_STARTED";
  round: number;
  base: string;
  lanes: string[];
}

/** One lane froze a candidate. Record-only; `sha` is the lane's commit. */
export interface EvCandidateSubmitted {
  type: "CANDIDATE_SUBMITTED";
  round: number;
  lane: string;
  sha: string;
}

/** One lane's checks ran. `ok` is false for a failure; a lane that crashed,
 * timed out or submitted nothing records no candidate at all (the status
 * shows its `note` instead). Record-only. */
export interface EvCandidateChecked {
  type: "CANDIDATE_CHECKED";
  round: number;
  lane: string;
  ok: boolean;
  note?: string;
}

/** One seat's vote in a round's pick turn, with its one-line why. A seat
 * votes once; a later PICK_VOTE from the same seat replaces the earlier one.
 * Record-only. */
export interface EvPickVote {
  type: "PICK_VOTE";
  round: number;
  seat: string;
  lane: string;
  why: string;
  /** Plan 06h (A3): true when this vote is in the top-two revote rather than
   * the first pick turn. Absent means the first turn (old logs unchanged). */
  revote?: boolean;
}

/** Plan 06h (A3): the top two lanes go to one revote. Record-only. */
export interface EvRevoteStarted {
  type: "REVOTE_STARTED";
  round: number;
  lanes: string[];
}

/** The round's winner, decided by core/rounds.ts's `pickWinner` (never by a
 * model) and recorded here. `votes` is the number of pick votes the winner
 * took (0 when it was the only passing candidate and won without a vote).
 * Record-only. */
export interface EvCandidatePicked {
  type: "CANDIDATE_PICKED";
  round: number;
  lane: string;
  sha: string;
  votes: number;
}

/** Plan 06g2: one seat's review of one lane candidate, recorded while the
 * round runs (the phase is still IMPLEMENTING, so no review slot exists to
 * hold it). Record-only: the round's own per-candidate review record. The
 * winner's reviews are promoted into the phase's review slots at hand-off
 * (a REVIEW_SUBMITTED carrying `promotedFrom`), so the acceptance rule reads
 * the winner's reviews and nothing else. */
export interface EvRoundReviewSubmitted {
  type: "ROUND_REVIEW_SUBMITTED";
  round: number;
  lane: string;
  seat: string;
  review: Review;
}

export type Event =
  | EvRoundStarted
  | EvCandidateSubmitted
  | EvCandidateChecked
  | EvPickVote
  | EvRevoteStarted
  | EvCandidatePicked
  | EvRoundReviewSubmitted
  | EvItemCarried
  | EvItemStateUpdated
  | EvEvidenceRecorded
  | EvItemCheckRecorded
  | EvReviewLintFailed
  | EvBriefsRecorded
  | EvBriefRetryAttempted
  | EvEntryCurated
  | EntryEvent
  | EvAttemptStarted
  | EvBaselineCompleted
  | EvBaselineTimedOut
  | EvBaselineInterrupted
  | EvEvaluationCompleted
  | EvEvaluationTimedOut
  | EvEvaluationInterrupted
  | EvEvaluatorFinished
  | EvPanelVote
  | EvPanelSeatUnavailable
  | EvPanelDecided
  | EvRoundPanelVote
  | EvRoundPanelSeatUnavailable
  | EvRoundPanelDecided
  | EvFindingSeverityChanged
  | EvFindingResolvedByVote
  | EvFindingVerified
  | EvCandidateApproved
  | EvSubmitPhase
  | EvAttemptTimedOut
  | EvAttemptNoSubmission
  | EvAttemptInterrupted
  | EvFreezeCompleted
  | EvFreezeTimedOut
  | EvFreezeInterrupted
  | EvChecksPassed
  | EvChecksFailed
  | EvChecksInterrupted
  | EvEnvChecked
  | EvEnvPreflightFailed
  | EvEnvCheckFailed
  | EvProbePassed
  | EvProbeFailed
  | EvProbeInterrupted
  | EvReviewSubmitted
  | EvReviewTimedOut
  | EvActionStarted
  | EvBallotCast
  | EvOverrideCast
  | EvFindingRaised
  | EvFindingConfirmedRepaired
  | EvFindingDisproved
  | EvFindingAcceptedByOwner
  | EvFindingSeverityLowered
  | EvDecisionClassLowered
  | EvOwnerRequestOpened
  | EvOwnerRequestResolved
  | EvAccepted
  | EvGateRequired
  | EvGateFailed
  | EvGateInterrupted
  | EvFinalCheckRequired
  | EvFinalChecksPassed
  | EvFinalChecksFailed
  | EvFinalChecksInterrupted
  | EvPublishIntent
  | EvPublishCompleted
  | EvPublishStale
  | EvRepairAttemptStarted
  | EvRepairBudgetExhausted
  | EvResolvingIncomplete
  | EvRevise
  | EvAmend
  | EvCriterionAmended
  | EvCriterionReverted
  | EvRunBudgetExceeded
  | EvRunResumed
  | EvLaunchFailed
  | EvIntegrityViolated
  | EvDecisionAdded
  | EvNoteAdded
  | EvOwnerInputRecorded
  | EvDirectiveAdded
  | EvDirectiveWithdrawn
  | EvDirectiveDelivered
  | EvOwnerCorrection
  | EvOwnerRequestMarkedUnneeded
  | EvMissRecorded
  | EvNotesDelivered
  | EvDecisionMatched
  | EvFindingAlsoRaised
  | EvMessageRaised
  | EvMessagePublished
  | EvMessageMerged
  | EvMessageDropped
  | EvOwnerVerdict
  | EvMessageResolved
  | EvMessageAddressReported
  | EvMessageSuperseded
  | EvMessageCarried
  | EvFlakeObserved
  | EvLaunchRetried;

export type EventType = Event["type"];

// ---------------------------------------------------------------------------
// reduce() result
// ---------------------------------------------------------------------------

export interface ReduceOk {
  ok: true;
  state: State;
}

export interface ReduceRejected {
  ok: false;
  reason: string;
  state: State; // the unchanged input state
}

export type ReduceResult = ReduceOk | ReduceRejected;

// ---------------------------------------------------------------------------
// next() actions
// ---------------------------------------------------------------------------

export type Action = { type: string; [key: string]: unknown };
