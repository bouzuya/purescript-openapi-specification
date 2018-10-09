module Test.SimpleJson
  ( tests
  ) where

import Data.Either (Either(..), isLeft)
import Data.Maybe (Maybe(..))
import Data.Nullable (Nullable, toNullable)
import Foreign (MultipleErrors)
import Prelude (discard)
import Simple.JSON (readJSON, writeJSON)
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert as Assert

type MaybeJson = { a :: Maybe String }
type NullableJson = { a :: Nullable String }

tests :: TestSuite
tests = suite "Simple.JSON" do
  test "readJSON (Maybe a)" do
    let f s = readJSON s :: Either MultipleErrors MaybeJson
    Assert.equal (Right { a: Just "b" }) (f """{"a":"b"}""")
    Assert.equal (Right { a: Nothing }) (f """{"a":null}""")
    Assert.equal (Right { a: Nothing }) (f """{}""")
  test "readJSON (Nullable a)" do
    let f s = readJSON s :: Either MultipleErrors NullableJson
    Assert.equal (Right { a: toNullable (Just "b") }) (f """{"a":"b"}""")
    Assert.equal (Right { a: toNullable Nothing }) (f """{"a":null}""")
    Assert.equal true (isLeft (f """{}"""))
  test "writeJSON (Maybe a)" do
    Assert.equal """{"a":"b"}""" (writeJSON ({ a: Just "b" } :: MaybeJson))
    Assert.equal """{}""" (writeJSON ({ a: Nothing } :: MaybeJson))
  test "writeJSON (Nullable a)" do
    Assert.equal """{"a":"b"}""" (writeJSON ({ a: toNullable (Just "b") } :: NullableJson))
    Assert.equal """{"a":null}""" (writeJSON ({ a: toNullable Nothing } :: NullableJson))
