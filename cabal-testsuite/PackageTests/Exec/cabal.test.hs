import Test.Cabal.Prelude
main = cabalTest $ do
    -- NB: cabal-version: >= 1.2 in my.cabal means we exercise
    -- the non per-component code path
    cabal "v2-build" ["my-executable"]
    withPlan $ runPlanExe "my" "my-executable" []
