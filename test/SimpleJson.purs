module Test.SimpleJson
  ( tests
  ) where

import Data.Either (Either(..), isLeft)
import Data.Maybe (Maybe(..))
import Data.Nullable (Nullable, toNullable)
import Data.Tuple (Tuple(..))
import Foreign (MultipleErrors)
import Foreign.Object (Object)
import Foreign.Object as Object
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
  test "readJSON Boolean" do
    let f s = readJSON s :: Either MultipleErrors { a :: Boolean }
    Assert.equal (Right { a: true }) (f """{"a":true}""")
  test "readJSON Number" do
    let f s = readJSON s :: Either MultipleErrors { a :: Number }
    Assert.equal (Right { a: 1.0 }) (f """{"a":1}""")
    Assert.equal (Right { a: 1.5 }) (f """{"a":1.5}""")
  test "readJSON Int" do
    let f s = readJSON s :: Either MultipleErrors { a :: Int }
    Assert.equal (Right { a: 1 }) (f """{"a":1}""")
    Assert.equal true (isLeft (f """{"a":1.5}"""))
  test "readJSON String" do
    let f s = readJSON s :: Either MultipleErrors { a :: String }
    Assert.equal (Right { a: "b" }) (f """{"a":"b"}""")
  test "readJSON (Array a)" do
    let f s = readJSON s :: Either MultipleErrors { a :: Array Int }
    Assert.equal (Right { a: [1] }) (f """{"a":[1]}""")
  test "readJSON (Object a)" do
    let f s = readJSON s :: Either MultipleErrors { a :: Object String }
    Assert.equal (Right { a: Object.fromFoldable [ Tuple "a" "b" ] }) (f """{"a":{"a":"b"}}""")
    Assert.equal (Right { a: Object.fromFoldable [ Tuple "a" "b", Tuple "c" "d" ] }) (f """{"a":{"a":"b","c":"d"}}""")
  test "readJSON Record" do
    let f s = readJSON s :: Either MultipleErrors { a :: { a :: String } }
    Assert.equal (Right { a: { a: "b" } }) (f """{"a":{"a":"b"}}""")
    -- NOTE: { a :: String | r }
    Assert.equal (Right { a: { a: "b" } }) (f """{"a":{"a":"b","c":"d"}}""")
