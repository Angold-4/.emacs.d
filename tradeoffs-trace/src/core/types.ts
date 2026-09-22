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

export interface PlanPhase {
  id: string;
  goal: string;
  acceptance: string[];
  checks: string[];
  boundaries: string[];
  reserved: string[];
  provisional: boolean;
}

export interface Plan {
  title: string;
  checks: string[]; // TT_CHECKS
  phases: PlanPhase[];
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
}

// ---------------------------------------------------------------------------
// §4 Findings
// ---------------------------------------------------------------------------

export type FindingKind = "defect" | "contract" | "integration";
export type FindingSeverity = "blocking" | "advisory";
export type FindingStatus = "open" | "repaired" | "disproved" | "accepted";
export type Reviewer = "M" | "A" | "B";

export interface Finding {
  id: string;
  version: number;
  phaseId: string;
  kind: FindingKind;
  severity: FindingSeverity;
  evidence: string; // file:line, scenario, check result or plan clause — required, non-empty
  raisedBy: Reviewer | "conductor"; // conductor raises `integration` findings itself
  linkedDecisionId?: string;
  status: FindingStatus;
  boundCandidateSha: string; // the candidate the finding was raised against
  reproduction?: { command: string; result: "reproduced" | "inconclusive" };
  repairedByCandidateSha?: string;
  disprovedEvidence?: string;
  acceptedScope?: string; // required scope note (§4.2, §10.4 `x`)
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
    | "unaddressed_correction";
  linkedDecisionId?: string; // set when origin is `reserved_decision` or `failed_vote`
  linkedFindingId?: string; // set when origin is `open_finding`
  linkedCorrectionId?: string; // set when origin is `unaddressed_correction`
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

export interface Review {
  reviewer: Reviewer;
  phaseId: string;
  candidateSha: string;
  contractVersion: ContractVersion;
  correctionStatements: { correctionId: string; status: HonoredStatus }[];
  findingStatements: { findingId: string; status: FindingStatement; evidence?: string }[];
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

export interface PauseResumeModeCommand {
  kind: "pause" | "resume" | "mode";
  mode?: "delegate" | "co-work";
}

export type OwnerCommand =
  | SteerCommand
  | NoteCommand
  | ResolveCommand
  | OverrideCommand
  | AcceptFindingCommand
  | ReviseCommand
  | AmendCommand
  | UnneededCommand
  | PauseResumeModeCommand;

// ---------------------------------------------------------------------------
// §6.1 / §8 / §9 Phase and run state
// ---------------------------------------------------------------------------

export type PhaseStateName =
  | "READY"
  | "IMPLEMENTING"
  | "FREEZING"
  | "CHECKING"
  | "PROBING"
  | "REVIEWING"
  | "RESOLVING"
  | "ACCEPTED"
  | "PUBLISHING"
  | "DONE"
  | "REPAIRING"
  | "AWAITING_OWNER"
  | "BLOCKED";

export type RunStateName = "RUN_ACTIVE" | "RUN_PAUSED_BUDGET";

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

export interface ChecksResult {
  candidateSha: string;
  passed?: boolean;
  interrupted?: boolean;
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
  | "dispatch_worker"
  | "freeze"
  | "run_checks"
  | "dispatch_probe"
  | "review_M"
  | "review_A"
  | "review_B"
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
  reviews: { M?: ReviewSlot; A?: ReviewSlot; B?: ReviewSlot };
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
  worktreeTainted?: boolean;
  integrityViolated?: boolean;
  repairRoundsUsed: number;
  repairRoundsGranted: number; // 3 base, +3 per correction (§7.5, §8.1)
  publishedI?: string;
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
}
export interface EvSubmitPhase {
  type: "SUBMIT_PHASE";
  decisions: Decision[];
}
export interface EvAttemptTimedOut {
  type: "ATTEMPT_TIMED_OUT";
}
export interface EvAttemptNoSubmission {
  type: "ATTEMPT_NO_SUBMISSION";
}
export interface EvAttemptInterrupted {
  type: "ATTEMPT_INTERRUPTED";
}
export interface EvFreezeCompleted {
  type: "FREEZE_COMPLETED";
  candidateSha: string;
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
}
export interface EvChecksInterrupted {
  type: "CHECKS_INTERRUPTED";
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

export type Event =
  | EvAttemptStarted
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
  | EvPublishIntent
  | EvPublishCompleted
  | EvPublishStale
  | EvRepairAttemptStarted
  | EvRepairBudgetExhausted
  | EvResolvingIncomplete
  | EvRevise
  | EvAmend
  | EvRunBudgetExceeded
  | EvRunResumed;

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
