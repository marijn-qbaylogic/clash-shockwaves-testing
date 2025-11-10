
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

import Prelude

import Test.Tasty
import Test.Tasty.HUnit

import Clash.Shockwaves.Internal.Types
import Clash.Shockwaves.Internal.Util
import Clash.Shockwaves.Internal.Translator
import Clash.Shockwaves.Internal.Waveform
import Clash.Prelude
import qualified Data.List as L
import qualified Data.Map as M
import Data.Bifunctor (second)
import Data.Maybe (fromMaybe)

import Tests.Structure
import Tests.Types

main :: IO ()
main = defaultMain tests

{-

- test testStructure with some cases
- test that for all types, for all values, the translation matches the structure
- test that for some complex values, the render string is exactly as expected
- test that for all types, for some values, the output structure and style is exactly as expected
- test that LUT types actually produce LUT tables, even when nested
- test that all subvalues get added to the type tables?

-}


isR :: Either String () -> Assertion
isR x = case x of
  Right () -> return ()
  Left msg -> assertFailure msg

isL :: Either String () -> Assertion
isL x = case x of
  Left _ -> return ()
  Right () -> assertFailure "passed, but should have failed"

tests :: TestTree
tests = testGroup "Tests" [testStructureT,structureT,renderT,translationT,lutT]

undef :: a
undef = Clash.Prelude.undefined

testStructureT :: TestTree
testStructureT = testGroup "TEST testStructure FUNCTION"
  [ testCase "is empty, get empty"  $ isR $ testStructure
    (Structure           [])
    (Translation Nothing [])
  , testCase "is empty, get some"   $ isL $ testStructure
    (Structure           [                            ])
    (Translation Nothing [("a",Translation Nothing [])])
  , testCase "is some, get that"    $ isR $ testStructure
    (Structure           [("a",Structure           [])])
    (Translation Nothing [("a",Translation Nothing [])])
  , testCase "is some, get none"    $ isR $ testStructure
    (Structure           [("a",Structure           [])])
    (Translation Nothing [                            ])
  , testCase "is some, get other"   $ isL $ testStructure
    (Structure           [("a",Structure           [])])
    (Translation Nothing [("b",Translation Nothing [])])
  , testCase "recursive true"       $ isR $ testStructure
    (Structure           [ ("a",Structure           [])
                         , ("b",Structure           [ ("ba",Structure [])
                                                    , ("bb",Structure [])])])
    (Translation Nothing [ ("b",Translation Nothing [ ("ba",Translation Nothing [])
                                                    , ("bb",Translation Nothing [])])])
  , testCase "recursive false"      $ isL $ testStructure
    (Structure           [ ("a",Structure           [])
                         , ("b",Structure           [ ("ba",Structure [])
                                                    , ("bb",Structure [])])])
    (Translation Nothing [ ("b",Translation Nothing [ ("ba",Translation Nothing [])
                                                    , ("bc",Translation Nothing [])])])
  ]








testAll :: ShowX a => (a -> Assertion) -> [a] -> [TestTree]
testAll f = L.map go
  where go x = testCase (showX x) (f x)

testS :: Waveform a => a -> Assertion
testS (x::a) = isR $ testStructure (structure $ translator @a) $ translate x

structureT :: TestTree
structureT = testGroup "TRANSLATION MATCHES TRANSLATOR STRUCTURE"
  [ testGroup "S"  $ testAll testS [S,undef]
  , testGroup "M"  $ testAll testS [Ma,Mb,Mc,undef]
  , testGroup "F"  $ testAll testS [M True 3,M False 3,undef]
  , testGroup "Op" $ testAll testS [True ://: (False ://: False), undef ://: (True ://: False), False ://: (True://:undef),undef]
  , testGroup "St" $ testAll testS [St{b=3,a=False},St{a=undef,b=0},undef]
  , testGroup "C"  $ testAll testS [Red,Green,Blue,undef]
  , testGroup "Mix"$ testAll testS [A,B True,C False 1,D{x=True,y= -1},0 :**: (True ://: False),undef]
  , testGroup "L"  $ testAll testS [La True False,Lb False True,undef]
  , testGroup "Maybe" $ testAll testS [Nothing, Just True, undef]
  , testGroup "Vec 2" $ testAll testS [True :> False :> Nil, undef :> undef :> Nil, undef]
  , testGroup "Vec 0" $ testAll testS [Nil @Bool, undef]
  ]









renders :: (Waveform a, ShowX a) => [a] -> [String] -> [TestTree]
renders xs = L.zipWith go rs'
  where
    getRen (Translation Nothing _) = ""
    getRen (Translation (Just (s,_,_)) _) = s
    rs' = L.map (\x -> (showX x, getRen $ translate x)) xs
    go (n,x) y = testCase n $ x @?= y

renderT :: TestTree
renderT = testGroup "RENDERED STRING IS CORRECT"
  [ testGroup "S"  $ renders [ S , undef]
                             ["S","S"   ]
  , testGroup "M"  $ renders [ Ma , Mb , Mc , undef     ]
                             ["Ma","Mb","Mc","undefined"]
  , testGroup "F"  $ renders [ M True 3 , M False 3 , undef     ]
                             ["M True 3","M False 3","M undefined undefined"]
  , testGroup "Op" $ renders [ True ://: (False ://: False) , undef     ://: (True ://: False) , False ://: (True ://: undef)     , undef                                     ]
                             ["True ://: (False ://: False)","undefined ://: (True ://: False)","False ://: (True ://: undefined)","undefined ://: (undefined ://: undefined)"]
  , testGroup "St" $ renders [ St{b=3,a=False}      , St{a=undef,b=0}          , undef     ]
                             ["St{a = False, b = 3}","St{a = undefined, b = 0}","St{a = undefined, b = undefined}"]
  , testGroup "C"  $ renders [ Red , Green , Blue , undef     ]
                             ["Red","Green","Blue","undefined"]
  , testGroup "Mix"$ renders [ A , B True , C False 1 , D{x=True,y= -1}     , 0 :**: (True ://: False) , undef     ]
                             ["A","B True","C False 1","D{x = True, y = -1}","0 :**: (True ://: False)","undefined"]
  , testGroup "L"  $ renders [ La True False  , Lb False True  , undef     ]
                             ["True <A> False","False <B> True","undefined"]
  , testGroup "Maybe" $ renders [ Nothing , Just True , undef     ]
                                ["Nothing","Just True","undefined"]
  , testGroup "Vec 2" $ renders [ True :> False :> Nil , undef     :> undef     :> Nil , True :> undef            , undef                         ]
                                ["True :> False :> Nil","undefined :> undefined :> Nil","True :> undefined :> Nil","undefined :> undefined :> Nil"]
  , testGroup "Vec 0" $ renders [ Nil @Bool, undef]
                                ["Nil"     ,"Nil" ]
  , testGroup "Maybe L" $ renders [ Just (La False False)  ,  Just undef     , undef     ]
                                  ["Just (False <A> False)", "Just undefined","undefined"]
  , testGroup "Signed 32" $ renders [ 0 ,  12345 ,   -123456789 :: Signed 32]
                                    ["0","12_345","-123_456_789"            ]
  ]










data T = T (String,WaveStyle) [(String,T)] deriving (Show)

pattern (:@) :: a -> b -> (a,b)
pattern (:@) x y <- (x,y)

toT :: Translation -> T
toT (Translation ren subs) = T d $ L.map (second toT) subs
  where d = case ren of
          Just (v,s,_) -> (v,s)
          Nothing -> ("",WSNormal)


pat :: (T -> Int) -> T -> Assertion
pat f v = case safeVal (f v) of
  Right _ -> return ()
  Left e -> assertFailure $ show v <> ": " <> fromMaybe "error" e

pats :: (Waveform a, ShowX a) => [a] -> [T->Int] -> [TestTree]
pats = L.zipWith go
  where
    go :: (Waveform a, ShowX a) => a -> (T -> Int) -> TestTree
    go x f = testCase (showX x) $ pat f $ toT $ translate x

translationT :: TestTree
translationT = testGroup "TRANSLATION STRUCTURE/STYLE IS CORRECT"
  [ testGroup "S"  $ pats [S           ,undef]
                          [\(T _ [])->0,\(T _ [])->0]
  , testGroup "M"  $ pats [ Ma                              , Mb                      , undef       ]
                          [\(T _ ["Ma" :@ T ("Ma",_) []])->0, \(T _ ["Mb":@T _ []])->0, \(T _ [])->0]
  , testGroup "F"  $ pats [ M True 3                            , undef                              ]
                          [\(T _ ["0" :@ T _ [],"1":@T _ []])->0, \(T _ ["0":@T _ [],"1":@T _ []])->0]
  , testGroup "Op" $ pats [ True ://: (False ://: False)                           , undef                                                  ]
                          [\(T _ ["0":@T _ _,"1":@T _ ["0":@T _ _, "1":@T _ _]])->0,\(T _ ["0":@T _ _,"1":@T _ ["0":@T _ _, "1":@T _ _]])->0]
  , testGroup "St" $ pats [ St{b=3,a=False}                    , undef                              ]
                          [\(T _ ["a":@T _ [],"b":@T _ []])->0, \(T _ ["a":@T _ [],"b":@T _ []])->0]
  , testGroup "C"  $ pats [ Red                                             , Green                                                    ]
                          [\(T ("Red",WSVar "red" "red") ["Red":@T ("Red",WSVar "red" "red") []])->0, \(T ("Green",WSVar "green" "lime") ["Green":@T ("Green",WSVar "green" "lime") []])->0]
  , testGroup "L"  $ pats [ La True False                                            , undef       ]
                             [\(T (_,"red") ["La":@T (_,"red") ["0":@ _,"1":@ _]])->0, \(T _ [])->0]
  , testGroup "Maybe" $ pats [ Nothing    , Just True               , undef       ]
                             [\(T _ [])->0, \(T _ ["Just.0":@ _])->0, \(T _ [])->0]
  , testGroup "Vec 2" $ pats [ True :> False :> Nil      , undef :> undef :> Nil      , True :> undef              , undef                      ]
                             [\(T _ ["0":@ _,"1":@ _])->0, \(T _ ["0":@ _,"1":@ _])->0, \(T _ ["0":@ _,"1":@ _])->0, \(T _ ["0":@ _,"1":@ _])->0]
--   , testCase "debug Maybe L" $ assertFailure . show $ translator @(Maybe L)
--   , testCase "debug L" $ assertFailure . show $ translator @L
--   , testCase "debug L" $ assertFailure . show $ translate (La True False)
--   , testCase "debug L" $ assertFailure . show $ precL (La True False)
  ]

lutT :: TestTree
lutT = testGroup "LUT VALUES ARE STORED"
  [ testCase "True <A> True" $ addValue (La True True) M.empty @?= M.fromList [(typeName @L,M.fromList [("011",translate $ La True True)])]
  , testCase "Just (True <A> True)" $ addValue (Just $ La True True) M.empty @?= M.fromList [(typeName @L,M.fromList [("011",translate $ La True True)])]
  , testCase "Just (undefined @L)" $ addValue (Just $ undef @L) M.empty @?= M.fromList [(typeName @L,M.fromList [("xxx",translate $ undef @L)])]
  , testCase "undefined @(Maybe L)" $ addValue (undef @(Maybe L)) M.empty @?= M.empty
  ]