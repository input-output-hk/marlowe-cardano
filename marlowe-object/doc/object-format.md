# Marlowe Object Format

The Marlowe Object Format is a way to describe a Marlowe contract as a set of
labelled objects that reference one-another which can be linked to create a
Marlowe contract. The term comes from the object files produced by a compiler,
which can be linked to create an executable program.

The object format extends the core Marlowe contract with references - names
that identify objects defined elsewhere. For example, a contract can use a
reference as a sub-contract instead of inlining it, allowing commonly repeated
contracts to be defined once and referenced many times. Contracts, values, and
observations can be defined as objects.

## Specification

A bundle is a finite sequence of pairs of labels and objects:

```
bundle := (label, obj)*
```

A label is a text value that is unique in the bundle.

```
label := text
```

Most constructs in a Marlowe contract can be defined as objects. Here is the
full list of constructs that can be defined as objects:

```
obj := value_obj
     | observation_obj
     | contract_obj
     | party_obj
     | token_obj
     | action_obj
```

Each of the object cases are the same as the core marlowe construct with an
additional reference case, which can refer to another object by its label.

Because Marlowe programs are not allowed to contain loops, in order to
reference another object, that object must have been previously defined. A
consequence of this is that object bundle must be defined bottom-up.

## JSON encoding

A bundle is encoded as an array of object definitions:

```typescript
type Bundle = ObjectDefinition[];
```

An object definition is encoded as an object with a `label` property and
`value` property.

```typescript
type ObjectDefinition = {
  label: string;
  type: "value" | "observation" | "contract" | "party" | "token" | "action";
  value: Obj;
};
```

An object is encoded the same as the corresponding core construct. A reference
is encoded as an object with a `ref` property, whose value contains the
referenced label.
