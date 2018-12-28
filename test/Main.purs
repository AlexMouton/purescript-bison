module Test.Main where

import Prelude
import Effect (Effect)
import Effect.Console (log, logShow)
import Effect.Class (liftEffect)
import Data.Either (isRight)
import Data.String.Common (joinWith)

import Text.Parsing.StringParser (Parser, runParser)

import Test.Spec (Spec, describe, describeOnly, it)
import Test.Spec.Assertions (fail, shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (run)

import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)

import Bison

main :: Effect Unit
main = run [consoleReporter] do
  describe "Bison" do
    describe "examples" do
      -- file "simple.bison" parseGrammar
      file "webidl2.bison" parseGrammar

    describe "Rule" do
      -- parses parseRule "Haha:Abcdf abcd ABCD;"
      -- parses parseRule "Haha:Abcdf abcd ABCD;\n"
      -- parses parseRule "Haha : Abcdf abcd ABCD | interface NONONO\n  | ε\n;"
      -- parses parseRule "Haha : Abcdf abcd ABCD; Haha : Abcdf abcd ABCD | interface NONONO;"
      -- parses parseRule "Haha : Abcdf abcd ABCD | interface NONONO | ε\n;\n"
      -- parses parseRule "InterfaceRest\n : IDENTIFIER Inheritance '{' InterfaceMembers '}' ';'\n  ;"
      parses parseRule "BufferRelatedType\n : Int8Array\n ;\n"

    describe "Terms" do
      parses parseTerms "haha;"
      parses parseTerms "haha | Abcdf | ε;"


    describe "Term" do
      describe "Single" do
        parses parseTerm "abcdf"

      describe "Multiple" do
        parses parseTerm "Abcdf abcd ABCD ε"

    describe "Point" do
      describe "Token" do
        parses parsePoint "abcdf"

      describe "Regx" do
        parses parsePoint "ABCDF"

      describe "Ref" do
        parses parsePoint "Abcdf"
        parses parsePoint "Int8Array"

      describe "Literal" do
        parses parsePoint "abcdf"
        parses parseLiteral "'+'"

      describe "Epsilon" do
        parses parsePoint "ε"


    describe "Token" do
      parses parseToken "abcdf"

    describe "Regx" do
      parses parseRegx "ABCDF"

    describe "Ref" do
      parses parseRef "Abcdf"
      parses parseRef "Int8Array"

    describe "Literal" do
      parses parseLiteral "\"abcdf\""
      parses parseLiteral "'+'"

parses :: ∀ a. Show a => Parser a -> String -> Spec Unit
parses parser src =
  describe src do
    it "parses" do
      let res = runParser parser src
      _ <- liftEffect $ log $ joinWith "\n\n" [src, show res]
      when (not $ isRight res) $
        fail $ joinWith "\n\n" [src, show res]

file :: ∀ a. Show a => String -> Parser a -> Spec Unit
file f parser =
    describe f do
      it "parses" do
        src <- liftEffect $ readTextFile UTF8 f
        let res = runParser parser src
        _ <- liftEffect $ log $ joinWith "\n\n" [src, show res]
        when (not $ isRight res) $
          fail $ joinWith "\n\n" [src, show res]
