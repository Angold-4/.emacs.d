// A minimal JSON Schema validator supporting only the keywords the schemas
// under schemas/*.schema.json actually use: type, required, properties,
// additionalProperties, items, enum, const, minLength, minItems, maxItems, pattern,
// oneOf, anyOf, and $ref to a local $defs entry ("#/$defs/Name"). No ajv:
// tradeoffs-trace runs with zero npm dependencies (see README).

export type JSONSchema = Record<string, unknown>;

export interface ValidationResult {
  valid: boolean;
  errors: string[];
}

function typeOf(value: unknown): string {
  if (value === null) return "null";
  if (Array.isArray(value)) return "array";
  return typeof value;
}

function resolveRef(ref: string, root: JSONSchema): JSONSchema {
  const prefix = "#/$defs/";
  if (!ref.startsWith(prefix)) {
    throw new Error(`unsupported $ref (only local #/$defs/... refs are supported): ${ref}`);
  }
  const name = ref.slice(prefix.length);
  const defs = root.$defs as Record<string, JSONSchema> | undefined;
  const resolved = defs?.[name];
  if (!resolved) throw new Error(`$ref not found: ${ref}`);
  return resolved;
}

function validateNode(schema: JSONSchema, data: unknown, root: JSONSchema, path: string, errors: string[]): void {
  if (schema.$ref) {
    validateNode(resolveRef(schema.$ref as string, root), data, root, path, errors);
    return;
  }

  if (schema.oneOf) {
    const branches = schema.oneOf as JSONSchema[];
    const matchCount = branches.filter((b) => {
      const sub: string[] = [];
      validateNode(b, data, root, path, sub);
      return sub.length === 0;
    }).length;
    if (matchCount !== 1) {
      errors.push(`${path || "<root>"}: expected exactly one oneOf branch to match, ${matchCount} did`);
    }
    return;
  }

  if (schema.anyOf) {
    const branches = schema.anyOf as JSONSchema[];
    const matches = branches.some((b) => {
      const sub: string[] = [];
      validateNode(b, data, root, path, sub);
      return sub.length === 0;
    });
    if (!matches) {
      errors.push(`${path || "<root>"}: expected at least one anyOf branch to match, none did`);
    }
    return;
  }

  if (schema.const !== undefined) {
    if (JSON.stringify(data) !== JSON.stringify(schema.const)) {
      errors.push(`${path || "<root>"}: expected const ${JSON.stringify(schema.const)}, got ${JSON.stringify(data)}`);
    }
  }

  if (schema.enum) {
    const allowed = schema.enum as unknown[];
    if (!allowed.some((v) => JSON.stringify(v) === JSON.stringify(data))) {
      errors.push(`${path || "<root>"}: ${JSON.stringify(data)} is not one of ${JSON.stringify(allowed)}`);
    }
  }

  if (schema.type) {
    const expected = Array.isArray(schema.type) ? (schema.type as string[]) : [schema.type as string];
    const actual = typeOf(data);
    const matches = expected.some((t) => (t === "integer" ? actual === "number" && Number.isInteger(data) : t === actual));
    if (!matches) {
      errors.push(`${path || "<root>"}: expected type ${expected.join(" | ")}, got ${actual}`);
      return; // further structural checks would be meaningless
    }
  }

  if (typeof data === "string") {
    if (typeof schema.minLength === "number" && data.length < schema.minLength) {
      errors.push(`${path}: length ${data.length} is below minLength ${schema.minLength}`);
    }
    if (typeof schema.pattern === "string" && !new RegExp(schema.pattern).test(data)) {
      errors.push(`${path}: does not match pattern ${schema.pattern}`);
    }
  }

  if (Array.isArray(data)) {
    if (typeof schema.minItems === "number" && data.length < schema.minItems) {
      errors.push(`${path}: has ${data.length} items, below minItems ${schema.minItems}`);
    }
    if (typeof schema.maxItems === "number" && data.length > schema.maxItems) {
      errors.push(`${path}: has ${data.length} items, above maxItems ${schema.maxItems}`);
    }
    if (schema.items) {
      data.forEach((item, i) => validateNode(schema.items as JSONSchema, item, root, `${path}[${i}]`, errors));
    }
  }

  if (typeOf(data) === "object") {
    const obj = data as Record<string, unknown>;
    const required = (schema.required as string[]) ?? [];
    for (const key of required) {
      if (!(key in obj)) {
        errors.push(`${path || "<root>"}: missing required property '${key}'`);
      }
    }
    const properties = (schema.properties as Record<string, JSONSchema>) ?? {};
    for (const [key, propSchema] of Object.entries(properties)) {
      if (key in obj) {
        validateNode(propSchema, obj[key], root, path ? `${path}.${key}` : key, errors);
      }
    }
    if (schema.additionalProperties === false) {
      for (const key of Object.keys(obj)) {
        if (!(key in properties)) {
          errors.push(`${path || "<root>"}: unexpected additional property '${key}'`);
        }
      }
    }
  }
}

export function validate(schema: JSONSchema, data: unknown): ValidationResult {
  const errors: string[] = [];
  try {
    validateNode(schema, data, schema, "", errors);
  } catch (err) {
    errors.push(`schema error: ${String((err as Error)?.message ?? err)}`);
  }
  return { valid: errors.length === 0, errors };
}
