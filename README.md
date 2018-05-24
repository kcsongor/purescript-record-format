# purescript-record-format

Record formatting from type-level format strings, based on [Justin Woo](https://github.com/justinwoo)'s idea.

This library uses the 0.12 version of the compiler.

## Example

```purescript
format
  (SProxy :: SProxy "Hi {name}! Your favourite number is {number}")
  {name : "Bill", number : 16}
```

produces the string

```
"Hi Bill! Your favourite number is 16"
```

A missing field results in a type-error:

```purescript
format
  (SProxy "Hi {name}! Your favourite number is {number}")
  {name : "Bill"}
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
