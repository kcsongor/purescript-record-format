# purescript-record-format

Experimental record formatting from type-level format strings, based on [Justin Woo](https://github.com/justinwoo)'s idea.

This library uses the unreleased 0.12 version of the compiler.

## Example

```purescript
format @"Hi {name}! Your favourite number is {number}" {name : "Bill", number : 16}
```

produces the string

```
"Hi Bill! Your favourite number is 16"
```

A missing field results in a type-error:

```purescript
format @"Hi {name}! Your favourite number is {number}" {name : "Bill"}
```

```
  Could not match type

    ( number :: t2
    | t3
    )

  with type

    ( name :: String
    )
```

The only requirement is that all the types in the record have `Show`
instances.
