module SetopSpec (spec) where

import Setop
import Protolude

import Test.Hspec
import Test.Hspec.QuickCheck

import qualified Data.Set as S

spec :: Spec
spec = do
    describe "union" $ do
        it "on two given sets" $ do
          let a = fromList [1, 3, 5, 7] :: Set Int
              b = fromList [0, 3, 5, 9] :: Set Int
              s = fromList [0, 1, 3, 5, 7, 9] :: Set Int
          (a `union` b) == s && (b `union` a) == s

        prop "property: A ∪ ∅ = A" $
          \ (a::Set Int) ->
            a `union` S.empty == a

        prop "property: A ∪ A = A" $
          \ (a::Set Int) ->
            a `union` a == a

        prop "property: A ⊆ (A ∪ B)" $
          \ (a::Set Int) (b::Set Int) ->
            a `S.isSubsetOf` (a `union` b)

        prop "property: A ∪ B = B ∪ A" $
          \ (a::Set Int) (b::Set Int) ->
            a `union` b == b `union` a

        prop "property: A ∪ (B ∪ C) = (A ∪ B) ∪ C" $
          \ (a::Set Int) (b::Set Int) (c::Set Int) ->
            a `union` (b `union` c) == (a `union` b) `union` c

        prop "property: A ⊆ B if and only if A ∪ B = B" $
          \ (a::Set Int) (b::Set Int) ->
            if a `union` b == b
            then a `S.isSubsetOf` b
            else not (a `S.isSubsetOf` b)

    describe "difference" $ do
        it "on two given sets" $ do
          let a = fromList [1, 3, 5, 7] :: Set Int
              b = fromList [0, 3, 5, 9] :: Set Int
              sab = fromList [1, 7] :: Set Int
              sba = fromList [0, 9] :: Set Int
          (a `difference` b) == sab && (b `difference` a) == sba

        prop "property: ∅ - A = ∅" $
          \ (a::Set Int) ->
            S.empty `difference` a == S.empty

        prop "property: A - ∅ = A" $
          \ (a::Set Int) ->
            a `difference` S.empty == a

        prop "property: A - A = ∅" $
          \ (a::Set Int) ->
            a `difference` a == S.empty

        prop "property: A - B ≠ B - A for A ≠ B" $
          \ (a::Set Int) (b::Set Int) ->
            if a /= b
            then a `difference` b /= b `difference` a
            else a `difference` a == S.empty

        prop "property: if A ⊆ B then A - B = ∅" $
          \ (a::Set Int) (b::Set Int) ->
            if a `S.isSubsetOf` b
            then a `difference` b == S.empty
            else a `difference` b /= S.empty

    describe "disjunction" $ do
        it "on two given sets" $ do
          let a = fromList [1, 3, 5, 7] :: Set Int
              b = fromList [0, 3, 5, 9] :: Set Int
              s = fromList [0, 1, 7, 9] :: Set Int
          (a `disjunction` b) == s && (b `disjunction` a) == s

        prop "property: A △ ∅ = A" $
          \ (a::Set Int) ->
            a `disjunction` S.empty == a

        prop "property: A △ A = ∅" $
          \ (a::Set Int) ->
            a `disjunction` a == S.empty

        prop "property: A △ B = B △ A" $
          \ (a::Set Int) (b::Set Int) ->
            a `disjunction` b == b `disjunction` a

        prop "property: A △ (B △ C) = (A △ B) △ C" $
          \ (a::Set Int) (b::Set Int) (c::Set Int)->
            a `disjunction` (b `disjunction` c) == (a `disjunction` b) `disjunction` c

        prop "property: if A ⊆ B then A △ B = B - a" $
          \ (a::Set Int) (b::Set Int) ->
            if a `S.isSubsetOf` b
            then a `disjunction` b == b `difference` a
            else a `disjunction` b /= b `difference` a

    describe "intersection" $ do
        it "on two given sets" $ do
          let a = fromList [1, 3, 5, 7] :: Set Int
              b = fromList [0, 3, 5, 9] :: Set Int
              s = fromList [3, 5] :: Set Int
          (a `intersection` b) == s && (b `intersection` a) == s

        prop "property: A ∩ ∅ = ∅" $
          \ (a::Set Int) ->
            a `intersection` S.empty == S.empty

        prop "property: A ∩ A = A" $
          \ (a::Set Int) ->
            a `intersection` a == a

        prop "property: A ∩ B ⊆ A" $
          \ (a::Set Int) (b::Set Int) ->
            (a `intersection` b) `S.isSubsetOf` a

        prop "property: A ∩ B = B ∩ A" $
          \ (a::Set Int) (b::Set Int) ->
            a `intersection` b == b `intersection` a

        prop "property: A ∩ (B ∩ C) = (A ∩ B) ∩ C" $
          \ (a::Set Int) (b::Set Int) (c::Set Int) ->
            a `intersection` (b `intersection` c) == (a `intersection` b) `intersection` c

        prop "property: A ⊆ B if and only if A ∩ B = A" $
          \ (a::Set Int) (b::Set Int) ->
            if a `intersection` b == a
            then a `S.isSubsetOf` b
            else not (a `S.isSubsetOf` b)
