// design §2.1's "Tool-set assertion", plan Phase 0's `role-tool-sets` exit
// gate: against the REAL installed Pi, no model call (never send `prompt`),
// each role's launch reports exactly its ROLE_TOOLS set. Also documents the
// `--exclude-tools` pitfall the design calls out.
//
// Must complete within ~30s total and leave no `pi` processes behind.

import assert from "node:assert/strict";
import { execFileSync, spawn, type ChildProcessWithoutNullStreams } from "node:child_process";
import { mkdtempSync, rmSync } from "node:fs";
import { createServer, type Server, type Socket } from "node:net";
import { tmpdir } from "node:os";
import { join } from "node:path";
import { after, test } from "node:test";

import { JSONLDecoder } from "../../src/core/protocol.ts";
import { assertToolSet, defaultExtensionPath, launchArgs, PI_VERSION, ROLE_TOOLS, type Role } from "../../src/core/roles.ts";

const PI_BIN = "pi";

function requirePi(): void {
  let version: string;
  try {
    version = execFileSync(PI_BIN, ["--version"], { encoding: "utf8" }).trim();
  } catch (err) {
    throw new Error(`'pi' is not on PATH — role-tool-sets requires the real installed Pi (${String(err)})`);
  }
  assert.equal(version, PI_VERSION, `installed pi is ${version}, expected the pinned ${PI_VERSION}`);
}

// Runs once, up front, and fails (not skips) the whole file if it doesn't hold.
requirePi();

interface Hello {
  type: "hello";
  agentId: string;
  role: string;
  tools: string[];
  piVersion?: string;
}

/** A minimal stand-in for the conductor's run-socket server: accepts one
 * connection, decodes JSONL, and resolves the first `hello` it sees. */
function startHelloServer(socketPath: string): { server: Server; hello: Promise<Hello> } {
  let resolveHello!: (h: Hello) => void;
  const hello = new Promise<Hello>((resolve) => {
    resolveHello = resolve;
  });
  const server = createServer((socket: Socket) => {
    const decoder = new JSONLDecoder();
    socket.on("data", (chunk) => {
      const messages = decoder.push(chunk) as Hello[];
      for (const msg of messages) {
        if (msg.type === "hello") resolveHello(msg);
      }
    });
  });
  server.listen(socketPath);
  return { server, hello };
}

const liveProcesses = new Set<ChildProcessWithoutNullStreams>();

function spawnPi(args: string[], env: NodeJS.ProcessEnv): ChildProcessWithoutNullStreams {
  const child = spawn(PI_BIN, args, {
    env: { ...process.env, ...env },
    stdio: ["pipe", "pipe", "pipe"],
    detached: true, // own process group, so we can kill it and its group
  });
  liveProcesses.add(child);
  child.on("exit", () => liveProcesses.delete(child));
  // Keep stdin open (piped, never closed) — Pi exits on EOF otherwise.
  return child;
}

function killGroup(child: ChildProcessWithoutNullStreams): void {
  if (child.pid === undefined) return;
  try {
    process.kill(-child.pid, "SIGKILL");
  } catch {
    // already gone
  }
  try {
    child.stdin.end();
  } catch {
    // ignore
  }
}

function isAlive(pid: number): boolean {
  try {
    process.kill(pid, 0);
    return true;
  } catch {
    return false;
  }
}

async function waitForExit(child: ChildProcessWithoutNullStreams, timeoutMs = 5000): Promise<void> {
  if (child.exitCode !== null || child.signalCode !== null) return;
  await new Promise<void>((resolve) => {
    const timer = setTimeout(resolve, timeoutMs);
    child.once("exit", () => {
      clearTimeout(timer);
      resolve();
    });
  });
}

after(() => {
  // Belt-and-braces cleanup in case an assertion threw before a test's own
  // kill ran.
  for (const child of [...liveProcesses]) killGroup(child);
});

const tmpRoot = mkdtempSync(join(tmpdir(), "tt-role-tool-sets-"));
after(() => {
  try {
    rmSync(tmpRoot, { recursive: true, force: true });
  } catch {
    // best-effort
  }
});

async function launchAndGetHello(role: Role, args: string[]): Promise<{ hello: Hello; child: ChildProcessWithoutNullStreams; socketPath: string }> {
  const socketPath = join(tmpRoot, `${role}-${Math.random().toString(36).slice(2)}.sock`);
  const { server, hello } = startHelloServer(socketPath);
  const child = spawnPi(args, {
    TT_SOCKET: socketPath,
    TT_AGENT_ID: `${role}-1`,
    TT_ROLE: role,
  });
  let stderr = "";
  child.stderr.on("data", (d) => {
    stderr += d.toString();
  });
  const timeout = new Promise<never>((_, reject) =>
    setTimeout(() => reject(new Error(`timed out waiting for hello from ${role} launch; stderr: ${stderr}`)), 20000),
  );
  const helloMsg = await Promise.race([hello, timeout]);
  server.close();
  return { hello: helloMsg, child, socketPath };
}

for (const role of Object.keys(ROLE_TOOLS) as Role[]) {
  test(`role-tool-sets: ${role} launch reports exactly ROLE_TOOLS.${role}`, async () => {
    const args = launchArgs(role, { extensionPath: defaultExtensionPath() });
    const { hello, child } = await launchAndGetHello(role, args);

    assert.equal(hello.role, role);
    assert.deepEqual([...hello.tools].sort(), [...ROLE_TOOLS[role]].sort());
    const result = assertToolSet(role, hello.tools);
    assert.deepEqual(result, { ok: true });

    const pid = child.pid!;
    killGroup(child);
    await waitForExit(child);
    assert.equal(isAlive(pid), false, "pi process must be gone after killing its group");
  });
}

// Negative case (design §2.1's pitfall): a worker launched with
// `--exclude-tools bash` instead of the `--tools` allowlist. Pi's default
// tool set omits `grep`, `find`, `ls`, so a denylist leaves the worker
// without them, and — because the extension registers all four submission
// tools in every agent — with the reviewer's own `submit_discovery` and
// `submit_review` too (nothing excludes them).
test("role-tool-sets: --exclude-tools bash (denylist pitfall) fails assertToolSet, naming the actual mismatch", async () => {
  const args = [
    "--mode",
    "rpc",
    "--exclude-tools",
    "bash",
    "--extension",
    defaultExtensionPath(),
    "--no-extensions",
    "--no-skills",
    "--no-session",
  ];
  const { hello, child } = await launchAndGetHello("worker", args);

  // Observed against Pi 0.87.0 (recorded here per the brief): hello.tools
  // was exactly
  //   ["read","edit","write","submit_phase","submit_discovery","submit_review","sh"]
  // — Pi's default active set minus `bash`, plus every extension tool this
  // skeleton registers (sh, submit_phase, submit_discovery, submit_review)
  // since none of them is named by --exclude-tools. `grep`, `find` and `ls`
  // are not in Pi's default set at all, so they stay missing regardless.
  const result = assertToolSet("worker", hello.tools);
  assert.equal(result.ok, false, `expected a mismatch; Pi reported tools: ${JSON.stringify(hello.tools)}`);
  if (!result.ok) {
    // grep/find/ls are not in Pi's default active set, so the worker
    // launches without them.
    assert.ok(result.missing.includes("grep"), `expected 'grep' missing; got ${JSON.stringify(result.missing)}`);
    assert.ok(result.missing.includes("find"), `expected 'find' missing; got ${JSON.stringify(result.missing)}`);
    assert.ok(result.missing.includes("ls"), `expected 'ls' missing; got ${JSON.stringify(result.missing)}`);
    // The reviewer's submission tools are extra, since --exclude-tools
    // never removes them and the extension registers them unconditionally.
    assert.ok(result.extra.includes("submit_discovery"), `expected 'submit_discovery' extra; got ${JSON.stringify(result.extra)}`);
    assert.ok(result.extra.includes("submit_review"), `expected 'submit_review' extra; got ${JSON.stringify(result.extra)}`);
  }

  const pid = child.pid!;
  killGroup(child);
  await waitForExit(child);
  assert.equal(isAlive(pid), false, "pi process must be gone after killing its group");
});
