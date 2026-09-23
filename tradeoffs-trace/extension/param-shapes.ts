// Plain-data field lists for the submission tools' typebox parameter
// schemas — deliberately with NO import of `typebox` or any Pi package, so
// this module can be imported from a plain `node --test` run outside Pi's
// extension loader (typebox is only resolvable inside Pi's own jiti-loaded
// extension context; a standalone `node` process cannot resolve it — see
// test/contract/submission-schema.test.ts).
//
// tradeoffs-trace.ts builds each tool's actual typebox `Type.Object(...)`
// by mapping over these same arrays (`Object.fromEntries(shape.properties
// .map(...))`), so the property-name/required-key set the model actually
// sees is mechanically derived from this file, not just kept in a
// separately-maintained parallel list that could drift from it. The
// contract test checks these plain shapes against schemas/submission.
// schema.json and schemas/review.schema.json, which is equivalent to
// checking the real typebox schemas without needing typebox itself.

export interface ParamShape {
  /** Property (key) names, in the order the tool declares them. */
  properties: string[];
  /** Which of `properties` are required — every one of them, for every
   * submission tool in this packet (none is `Type.Optional`). */
  required: string[];
}

/** design §3.2's plain-language decision-disclosure fields, shared by
 * `submit_phase`'s `decisions[]` and `submit_discovery`'s `discoveries[]`.
 * Matches schemas/submission.schema.json's `$defs.decisionDisclosure`. */
export const DECISION_DISCLOSURE_PARAMS: ParamShape = {
  properties: ["choice", "whyItMatters", "alternatives", "recommendation", "classProposal"],
  required: ["choice", "whyItMatters", "alternatives", "recommendation", "classProposal"],
};

/** Matches schemas/submission.schema.json's `$defs.submitPhase`. */
export const SUBMIT_PHASE_PARAMS: ParamShape = {
  properties: ["decisions", "assumptions", "deviations"],
  required: ["decisions", "assumptions", "deviations"],
};

/** Matches schemas/submission.schema.json's `$defs.submitDiscovery`. */
export const SUBMIT_DISCOVERY_PARAMS: ParamShape = {
  properties: ["discoveries"],
  required: ["discoveries"],
};

/** Matches schemas/review.schema.json's top level exactly — a Review
 * carries no conductor-assigned binding fields, so the model supplies
 * every field the record itself needs. `ballots`/`findings` (work packet
 * 2a) are optional: a real reviewer's turn-2 submission includes them; a
 * phase-1 stub review omits them (see conductor.ts's `stubReviews`). */
export const SUBMIT_REVIEW_PARAMS: ParamShape = {
  properties: [
    "reviewer",
    "phaseId",
    "candidateSha",
    "contractVersion",
    "correctionStatements",
    "findingStatements",
    "ballots",
    "findings",
  ],
  required: ["reviewer", "phaseId", "candidateSha", "contractVersion", "correctionStatements", "findingStatements"],
};
