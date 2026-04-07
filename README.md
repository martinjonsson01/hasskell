# Hasskell

A framework for declarative Home Assistant (**HASS**) automations in Has**kell** - **Hasskell**.

## Example

```haskell
runHasskell
  ( Config
      { baseUrl = "home-assistant-instance.test.com",
        token = "api-token-1234",
        logging = logging,
        workingDir = Just $ currentDir </> "hasskell-example"
      }
  )
  $
  policy "toggle light"
  ( if_ (toggledStateOf test `is` on)
      `then_` (flaktlampa `shouldBe` on)
      `else_` (flaktlampa `shouldBe` off)
  )
```

When run in debug mode will describe the decision provenance chain in detail:

<details>
  <summary>Click to view</summary>

```
will turn light `light.flaktlampa` on
reason:
  entity `light.flaktlampa` should be on
       ╭──▶ Run.hs@43:31-43:41
       │
    43 │           `then_` (flaktlampa `shouldBe` on)
       •                               ┬───────── 
       •                               ╰╸ entity `light.flaktlampa` should be on
  ─────╯

      │
      ├─▶ because literal
           ╭──▶ Run.hs@43:42-43:44
           │
        43 │           `then_` (flaktlampa `shouldBe` on)
           •                                          ┬─
           •                                          ╰╸ literal
      ─────╯

      │
      ├─▶ because entity `light.flaktlampa` is currently off
      │
      ├─▶ because branch was taken
           ╭──▶ Run.hs@43:11-43:18
           │
        43 │           `then_` (flaktlampa `shouldBe` on)
           •           ┬──────
           •           ╰╸ branch was taken
      ─────╯

          │
          ├─▶ because True
               ╭──▶ Run.hs@42:34-42:38
               │
            42 │       ( if_ (toggledStateOf test `is` on)
               •                                  ┬───
               •                                  ╰╸ True
          ─────╯

              │
              ├─▶ because on
                   ╭──▶ Run.hs@42:14-42:28
                   │
                42 │       ( if_ (toggledStateOf test `is` on)
                   •              ┬─────────────
                   •              ╰╸ on
              ─────╯

                  │
                  ├─▶ because entity `input_boolean.test` is currently on
              │
              ├─▶ because on
                   ╭──▶ Run.hs@42:39-42:41
                   │
                42 │       ( if_ (toggledStateOf test `is` on)
                   •                                       ┬─
                   •                                       ╰╸ on
              ─────╯

                  │
                  ├─▶ because literal
                       ╭──▶ Run.hs@42:39-42:41
                       │
                    42 │       ( if_ (toggledStateOf test `is` on)
                       •                                       ┬─
                       •                                       ╰╸ literal
                  ─────╯
```
</details>

## Features

- Declarative style of desired home state
- Reconciliation loop driving entities towards desired state
- Composable and resuable user-written functions thanks to it being an eDSL
- Historical replay of previous states
- Full traceability for each entity action
