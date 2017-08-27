# purescript-record-format

Experimental record formatting from type-level format strings, based on [Justin Woo](https://github.com/justinwoo)'s idea.

This package uses a modified version of the compiler (and the typelevel-prelude
package) that supports `Uncons (s :: Symbol) (h :: Symbol) (t :: Symbol) | s -> h t` - breaking a type-level string to its head and tail.

It also relies on overlapping instances being chosen in alphabetical order.

## Example

```purescript
format (SProxy :: SProxy "Hi {name}! Your favourite number is {number}") {name : "Bill", number : 16}
```

produces the string

```
"Hi Bill! Your favourite number is 16"
```

A missing field results in a type-error:

```purescript
format (SProxy :: SProxy "Hi {name}! Your favourite number is {number}") {name : "Bill"}
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
