# purescript-record-format

Record formatting from type-level format strings, based on [Justin Woo](https://github.com/justinwoo)'s idea.

This library uses the 0.15.10 version of the compiler.

## Example

```purescript
format @"Hi {name}! Your favourite number is {number}" {name : "Bill", number : 16}
```

produces the string

```console
"Hi Bill! Your favourite number is 16"
```

A missing field results in a type-error:

```purescript
format @"Hi {name}! Your favourite number is {number}" {name : "Bill"}
```

```console
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

## Nix flake

1. Install Nix.
   - Use [single-user installation](https://nixos.org/download.html), then [enable flakes](https://nixos.wiki/wiki/Flakes).
   - Alternatively, use an [unofficial](https://github.com/DeterminateSystems/nix-installer#the-determinate-nix-installer) installer.

2. Run the default `devShell`.

    ```console
    nix develop
    ```

3. Test  the project

    ```console
    spago test
    ```
