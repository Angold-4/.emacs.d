// design §5.1: the master-veto two-of-three vote, and its validity rules.
//
//   A decision passes iff M approves and at least one of A, B approves.
//
// - Voting applies only to `delegated` decisions with no open linked finding.
// - A missing, malformed or evidence-free ballot counts as reject ("missing
//   support is FAIL", OrgBrain #96).
// - A ballot bound to a superseded candidate, contract version or record
//   (decision) version is discarded — treated exactly as if it were
//   missing, which already counts as reject, so "discarded" and "counts as
//   reject" are the same rule seen from two angles, not a contradiction
//   (design §7.1: every ballot carries the full binding tuple).
// - A ballot carrying a contract objection opens a linked finding and
//   suspends the vote (handled where ballots are recorded — see
//   reduce.ts's BALLOT_CAST case — tally() only reports `suspended` once
//   that link exists).

import { DEFAULT_SEATS } from "./seats.ts";
import type { Ballot, ContractVersion, Decision, Finding, Reviewer } from "./types.ts";

export type TallyResult = "pass" | "fail" | "suspended" | "not_votable";

export function isValidBallot(b: Ballot | undefined): b is Ballot {
  if (!b) return false;
  if (b.vote !== "approve" && b.vote !== "reject") return false;
  if (typeof b.rationale !== "string" || b.rationale.trim().length === 0) return false;
  if (!Array.isArray(b.evidence) || b.evidence.length === 0) return false;
  if (b.evidence.some((c) => typeof c !== "string" || c.trim().length === 0)) return false;
  return true;
}

/** design §7.1: a ballot must be bound to the current candidate, contract
 * version AND the decision's current record version — not just the
 * candidate. A ballot cast on decision D v2 does not count once D is v3. */
export function isBoundCurrent(
  b: Ballot,
  candidateSha: string,
  contractVersion: ContractVersion,
  decisionVersion: number,
): boolean {
  return (
    b.boundCandidateSha === candidateSha &&
    b.boundContractVersion.snapshot === contractVersion.snapshot &&
    b.boundContractVersion.sectionSha256 === contractVersion.sectionSha256 &&
    b.boundRecordVersion === decisionVersion
  );
}

/** Reviewers vote on `delegated` and `reserved` decisions (owner-optional:
 * a reserved decision is voted like any other and only flagged for the
 * owner, never held for them); a `detail` is not voted on. A decision with
 * an *open* linked finding is not votable. */
export function isVotedClass(decision: Decision): boolean {
  return decision.class === "delegated" || decision.class === "reserved";
}

export function isVotable(decision: Decision, findings: Finding[]): boolean {
  if (!isVotedClass(decision)) return false;
  if (!decision.linkedFindingId) return true;
  const finding = findings.find((f) => f.id === decision.linkedFindingId);
  return Boolean(finding) && finding!.status !== "open";
}

/** The one ballot from `reviewer` on `decisionId` that is bound to the
 * current (candidateSha, contractVersion, decisionVersion), if any. A
 * ballot bound to a superseded version is not returned here — it is
 * discarded, not counted. */
export function currentBallot(
  ballots: Ballot[],
  decisionId: string,
  reviewer: Reviewer,
  candidateSha: string,
  contractVersion: ContractVersion,
  decisionVersion: number,
): Ballot | undefined {
  const bound = ballots.filter(
    (b) =>
      b.decisionId === decisionId &&
      b.reviewer === reviewer &&
      isBoundCurrent(b, candidateSha, contractVersion, decisionVersion),
  );
  return bound[bound.length - 1];
}

/** Plan 06h (A2): the vote rule generalises the master-veto two-of-three
 * rule. A decision passes iff the LEADER approves and a strict majority of
 * the seats approve (2 of 3, 3 of 5, 4 of 7). With the default `M A B` and
 * leader `M` this is exactly the old rule, so every 06g test is unchanged. */
export function tally(
  decision: Decision,
  ballots: Ballot[],
  findings: Finding[],
  candidateSha: string,
  contractVersion: ContractVersion,
  seats: readonly string[] = DEFAULT_SEATS,
): TallyResult {
  if (!isVotedClass(decision)) return "not_votable";
  if (decision.linkedFindingId) {
    const finding = findings.find((f) => f.id === decision.linkedFindingId);
    if (!finding || finding.status === "open") return "suspended";
  }
  const list = seats.length > 0 ? seats : DEFAULT_SEATS;
  const leader = list[0];
  const needed = Math.floor(list.length / 2) + 1;
  const leaderBallot = currentBallot(ballots, decision.id, leader, candidateSha, contractVersion, decision.version);
  if (!isValidBallot(leaderBallot) || leaderBallot.vote !== "approve") return "fail";
  const approvals = list.filter((seat) => {
    const b = currentBallot(ballots, decision.id, seat, candidateSha, contractVersion, decision.version);
    return isValidBallot(b) && b.vote === "approve";
  }).length;
  return approvals >= needed ? "pass" : "fail";
}
