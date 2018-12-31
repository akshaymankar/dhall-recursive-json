# dhall-recursive-json

## What is this project?

Doing recursion is dhall isn't really obvious. But with [Boehm Berducci Encoding](https://github.com/dhall-lang/dhall-lang/wiki/How-to-translate-recursive-code-to-Dhall), it is possible to represent recursive data.
The `dhall-to-json` tool still cannot convert this recursive data to json out of the box (or at least I couldn't figure it out).

This tool is an extension on dhall-to-json, it adds a `JSON` type and a function called `toJSON` to make anything into a JSON.
`toJSON` is of type `forall(t : Type) -> (x : t) -> JSON`.

## How to install

1. Clone this repository
1. Install [stack](https://docs.haskellstack.org/en/stable/README/)
1. `cd dhall-recursive-json`
1. stack install

### Examples

#### Simple Example

```bash
dhall-to-recursive-json <<< 'toJSON {x : Text, y : JSON} {x = "exxx", y = toJSON (List Text) ["y1", "y2", "y3"]}'
```

produces

```json
{
    "x": "exxx",
    "y": [
        "y1",
        "y2",
        "y3"
    ]
}
```

#### More practical example

```dhall
let Expr =
      ∀(Expr : Type)
    → ∀(Lit : Natural → Expr)
    → ∀(Add : { op1 : Expr, op2 : Expr } → Expr)
    → ∀(Mul : { op1 : Expr, op2 : Expr } → Expr)
    → Expr

let BinaryOperands = λ(Expr : Type) → { op1 : Expr, op2 : Expr }

let example =
      λ(Expr : Type)
    → λ(Lit : Natural → Expr)
    → λ(Add : BinaryOperands Expr → Expr)
    → λ(Mul : BinaryOperands Expr → Expr)
    → Mul
    { op1 =
      Add { op1 = Mul { op1 = Lit 10, op2 = Lit 2 }, op2 = Lit 1 }
    , op2 =
      Lit 2
    }

in  example
  JSON
  (toJSON Natural)
  (   λ(e : BinaryOperands JSON)
    → toJSON { add : JSON } { add = toJSON (BinaryOperands JSON) e }
  )
  (   λ(e : BinaryOperands JSON)
    → toJSON { mul : JSON } { mul = toJSON (BinaryOperands JSON) e }
  )
```

produces

```json
{
    "mul": {
        "op2": 2,
        "op1": {
            "add": {
                "op2": 1,
                "op1": {
                    "mul": {
                        "op2": 2,
                        "op1": 10
                    }
                }
            }
        }
    }
}
```


## Support for YAML?

Coming soon!
