A first pass at a `reflex` API for working with local storage.

```haskell
body :: MonadWidget t m => m ()
body = do
  text "Testing storage"
  void . runStorageT LocalStorage $ do
    -- sets the default value for Tag1, only if none is already present
    initializeTag Tag1 0
    counter

counter :: (MonadWidget t m, HasStorage t ExampleTag m) => m ()
counter = el "div" $ do
  -- ask for the current value in local storage under the Tag1 key, defaulting to 0
  dTag1 <- askStorageTagDef Tag1 0

  eAdd <- button "Add"
  eClear <- button "Clear"
  display dTag1

  let
    eChange = mergeWith (.) [
        succ <$ eAdd
      , const 0 <$ eClear
      ]

  -- insert the new value for Tag1 into local storage
  tellStorageInsert Tag1 $ (&) <$> current dTag1 <@> eChange
```
