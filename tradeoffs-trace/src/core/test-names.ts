// Skill fix 4: tests removed from files that still exist. Pure: extraction
// from file text and the set difference. The effect layer feeds it the
// base and candidate versions of each changed file.
//
// Deleting a test along with the code it tested is fine (run 9120dca7
// removed 40 tests with the dormant School A code). Deleting a test from a
// file that still exists may drop coverage of live behaviour, so reviewers
// are shown each such test and asked to confirm it was replaced or that its
// behaviour is gone. A test whose name appears in any changed file of the
// candidate counts as moved, not removed.

const PATTERNS: Array<{ ext: RegExp; re: RegExp }> = [
  // Rust: #[test] / #[tokio::test(...)] then any further attributes, then fn.
  { ext: /\.rs$/, re: /#\[(?:tokio::)?test[^\]]*\]\s*(?:#\[[^\]]*\]\s*)*(?:pub(?:\([^)]*\))?\s+)?(?:async\s+)?fn\s+([A-Za-z0-9_]+)/g },
  // JS/TS node:test, jest, mocha: test("name", …) / it('name', …)
  { ext: /\.(?:[cm]?[jt]sx?)$/, re: /\b(?:test|it)\(\s*(["'`])((?:(?!\1).)+)\1/g },
  // Emacs Lisp ERT
  { ext: /\.el$/, re: /\(ert-deftest\s+([^\s()]+)/g },
  // Python
  { ext: /\.py$/, re: /^\s*(?:async\s+)?def\s+(test_[A-Za-z0-9_]+)/gm },
  // Go
  { ext: /\.go$/, re: /^func\s+(Test[A-Za-z0-9_]+)\s*\(/gm },
];

/** The test names declared in `text`, a file at `path`. */
export function testNames(path: string, text: string): string[] {
  const p = PATTERNS.find((x) => x.ext.test(path));
  if (!p) return [];
  const names: string[] = [];
  for (const m of text.matchAll(p.re)) names.push(m[m.length - 1]);
  return names;
}

export interface FileVersions {
  path: string;
  /** Text at the base, or undefined when the file is new. */
  base?: string;
  /** Text in the candidate, or undefined when the file was deleted. */
  candidate?: string;
}

/** "<path>: <test>" for every test declared in a file at the base that
 * still exists in the candidate, whose name is declared nowhere in the
 * candidate's changed files. */
export function removedTests(files: FileVersions[]): string[] {
  const inCandidate = new Set<string>();
  for (const f of files) if (f.candidate !== undefined) for (const n of testNames(f.path, f.candidate)) inCandidate.add(n);
  const out: string[] = [];
  for (const f of files) {
    if (f.base === undefined || f.candidate === undefined) continue;
    for (const n of testNames(f.path, f.base)) if (!inCandidate.has(n)) out.push(`${f.path}: ${n}`);
  }
  return out;
}
