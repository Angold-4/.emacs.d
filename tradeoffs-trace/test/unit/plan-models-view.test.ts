// #+TT_MODELS must be visible: the status view names the models only when the
// plan set them, and `tt summary`'s PR body lists the model used per role.
// These use a plan object directly (buildView/prSummary are pure over the run
// directory), so no conductor process is needed.

import assert from "node:assert/strict";
import * as fs from "node:fs";
import * as path from "node:path";
import { test } from "node:test";

import type { RunPlanFile } from "../../src/conductor.ts";
import type { PlanModels } from "../../src/core/roles.ts";
import { renderStatusText, renderStatusView, statusViewInput } from "../../src/render.ts";
import { buildView, modelEntries, modelsLineText, prSummary } from "../../src/view.ts";

const MODELS: PlanModels = {
  worker: { model: "deepseek/deepseek-v4.1-flash" },
  reviewer: { provider: "vercel-ai-gateway", model: "anthropic/claude-sonnet-5" },
  evaluator: { provider: "p-e", model: "anthropic/claude-opus" },
  panel: { model: "deepseek/deepseek-v4.1-flash" },
};

function plan(models?: PlanModels): RunPlanFile {
  return {
    title: "plan models",
    repo: "/tmp/tt-plan-models-view",
    integrationBranch: "main",
    checks: ["true"],
    phases: [{ id: "p1", goal: "g", acceptance: ["a"], checks: ["true"], boundaries: [], reserved: [] }],
    ...(models ? { models } : {}),
  };
}

test("modelEntries: pipeline order, provider only when declared, slash model kept whole", () => {
  assert.deepEqual(modelEntries(MODELS), [
    { role: "worker", value: "deepseek/deepseek-v4.1-flash" },
    { role: "reviewer", value: "vercel-ai-gateway:anthropic/claude-sonnet-5" },
    { role: "evaluator", value: "p-e:anthropic/claude-opus" },
    { role: "panel", value: "deepseek/deepseek-v4.1-flash" },
  ]);
  assert.deepEqual(modelEntries(undefined), []);
  assert.equal(modelsLineText(undefined), undefined);
});

test("the status view has a models line only when the plan set models", () => {
  const dir = fs.mkdtempSync("/tmp/tt-plan-models-view-");
  try {
    const withModels = plan(MODELS);
    const view = buildView(dir, withModels, false);
    assert.equal(view.models, "models: worker=deepseek/deepseek-v4.1-flash reviewer=vercel-ai-gateway:anthropic/claude-sonnet-5 evaluator=p-e:anthropic/claude-opus panel=deepseek/deepseek-v4.1-flash");
    const text = renderStatusView(statusViewInput({ runDir: dir, plan: withModels, state: { phase: view.timeline.state.phase }, view, alive: false }));
    assert.match(text, /^models: worker=/m);
    assert.match(renderStatusText(dir, { run: "RUN_ACTIVE", phase: view.timeline.state.phase }, view), /^models: worker=/m);

    // A plan without models: the line is absent from both status views.
    const without = plan();
    const bareView = buildView(dir, without, false);
    assert.equal(bareView.models, undefined);
    const bare = renderStatusView(statusViewInput({ runDir: dir, plan: without, state: { phase: bareView.timeline.state.phase }, view: bareView, alive: false }));
    assert.doesNotMatch(bare, /models:/);
    assert.doesNotMatch(renderStatusText(dir, { run: "RUN_ACTIVE", phase: bareView.timeline.state.phase }, bareView), /models:/);
  } finally {
    fs.rmSync(dir, { recursive: true, force: true });
  }
});

test("tt summary's PR body lists the models used per role", () => {
  const dir = fs.mkdtempSync("/tmp/tt-plan-models-view-");
  try {
    const md = prSummary(dir, plan(MODELS));
    assert.match(md, /### Models per role/);
    assert.match(md, /- worker: deepseek\/deepseek-v4\.1-flash/);
    assert.match(md, /- reviewer: vercel-ai-gateway:anthropic\/claude-sonnet-5/);
    assert.match(md, /- evaluator: p-e:anthropic\/claude-opus/);
    assert.match(md, /- panel: deepseek\/deepseek-v4\.1-flash/);

    const bare = prSummary(dir, plan());
    assert.doesNotMatch(bare, /Models per role/);
  } finally {
    fs.rmSync(dir, { recursive: true, force: true });
  }
});

// The owner's per-seat configuration: the status line and the PR body must
// name every seat's model, not just the role's shared default.
const SEAT_MODELS: PlanModels = {
  worker: { model: "deepseek/deepseek-v4.1-flash" },
  reviewerSeats: {
    M: { provider: "vercel-ai-gateway", model: "anthropic/claude-opus-5.5" },
    A: { model: "deepseek/deepseek-v4.1-flash" },
    B: { provider: "vercel-ai-gateway", model: "spacexai/grok-4.6" },
  },
  evaluator: { provider: "vercel-ai-gateway", model: "anthropic/claude-opus-5.5" },
  panelFrom: "reviewers",
};

test("modelEntries: per-seat models expand to one entry per seat", () => {
  assert.deepEqual(modelEntries(SEAT_MODELS), [
    { role: "worker", value: "deepseek/deepseek-v4.1-flash" },
    { role: "reviewer.M", value: "vercel-ai-gateway:anthropic/claude-opus-5.5" },
    { role: "reviewer.A", value: "deepseek/deepseek-v4.1-flash" },
    { role: "reviewer.B", value: "vercel-ai-gateway:spacexai/grok-4.6" },
    { role: "evaluator", value: "vercel-ai-gateway:anthropic/claude-opus-5.5" },
    { role: "panel.1", value: "vercel-ai-gateway:anthropic/claude-opus-5.5" },
    { role: "panel.2", value: "deepseek/deepseek-v4.1-flash" },
    { role: "panel.3", value: "vercel-ai-gateway:spacexai/grok-4.6" },
  ]);
});

test("the status line and tt summary name every seat's model", () => {
  const dir = fs.mkdtempSync("/tmp/tt-plan-models-view-");
  try {
    const withSeats = plan(SEAT_MODELS);
    const view = buildView(dir, withSeats, false);
    assert.match(
      view.models ?? "",
      /^models: worker=deepseek\/deepseek-v4\.1-flash reviewer\.M=vercel-ai-gateway:anthropic\/claude-opus-5\.5 reviewer\.A=deepseek\/deepseek-v4\.1-flash reviewer\.B=vercel-ai-gateway:spacexai\/grok-4\.6 evaluator=vercel-ai-gateway:anthropic\/claude-opus-5\.5 panel\.1=vercel-ai-gateway:anthropic\/claude-opus-5\.5 panel\.2=deepseek\/deepseek-v4\.1-flash panel\.3=vercel-ai-gateway:spacexai\/grok-4\.6$/,
    );
    const text = renderStatusText(dir, { run: "RUN_ACTIVE", phase: view.timeline.state.phase }, view);
    assert.match(text, /^models: worker=.*reviewer\.M=.*panel\.3=/m);
    const md = prSummary(dir, withSeats);
    assert.match(md, /- reviewer\.M: vercel-ai-gateway:anthropic\/claude-opus-5\.5/);
    assert.match(md, /- reviewer\.B: vercel-ai-gateway:spacexai\/grok-4\.6/);
    assert.match(md, /- panel\.3: vercel-ai-gateway:spacexai\/grok-4\.6/);
  } finally {
    fs.rmSync(dir, { recursive: true, force: true });
  }
});
