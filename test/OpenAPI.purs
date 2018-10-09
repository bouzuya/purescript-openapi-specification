module Test.OpenAPI
  ( tests
  ) where

import Data.Either (Either, fromRight)
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Foreign (MultipleErrors)
import Node.Buffer as Buffer
import Node.Encoding as Encoding
import Node.FS.Sync as FS
import OpenAPI (OpenAPI)
import Partial.Unsafe (unsafePartial)
import Prelude (bind, discard, pure)
import Simple.JSON (readJSON)
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert as Assert

readExample :: String -> Aff (Either MultipleErrors OpenAPI)
readExample p = do
  b <- liftEffect (FS.readFile "./examples/api-with-examples.json")
  s <- liftEffect (Buffer.toString Encoding.UTF8 b)
  pure (readJSON s :: Either MultipleErrors OpenAPI)

tests :: TestSuite
tests = suite "OpenAPI" do
  suite "examples/api-with-examples.json" do
    let
      f = do
        e <- readExample "./examples/api-with-examples.json"
        pure (unsafePartial (fromRight e))
    test "openapi" do
      r <- f
      Assert.equal "3.0.0" r.openapi
    test "info" do
      r <- f
      Assert.equal
        { title: "Simple API overview"
        , description: Nothing
        , termsOfService: Nothing
        , contact: Nothing
        , license: Nothing
        , version: "v2"
        }
        r.info
