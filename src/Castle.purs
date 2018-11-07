module Castle where

import Prelude

import Control.Monad.State (State, execState, put)
import Control.Monad.State.Class (get)
import Data.Array (index, singleton, updateAt, (!!), (..))
import Data.Array as A
import Data.Foldable (sum, foldl, length, traverse_)
import Data.List (List(Nil), reverse, (:))
import Data.List as L
import Data.Maybe (fromMaybe)
import Data.Set (Set, empty, insert, member, size)
import Data.String (Pattern(..), joinWith)
import Data.String as S
import Data.Tuple (Tuple(..))

type Width = Int
type Height = Int

data Castle = Castle
  { values :: Array (Array Boolean)
  , width :: Width
  , height :: Height
  }

derive instance eqCastle :: Eq Castle
derive instance ordCastle :: Ord Castle

instance showCastle :: Show Castle where
  show cast@(Castle xy) = "\n" <> (joinWith "\n" do
    row <- (xy.height - 1) .. 0
    [stringifyRow row cast]) <> "\n"

stringifyRow :: Int -> Castle -> String
stringifyRow row cast = joinWith "" $ map showCastleSpace $ getCastleRow row cast

getCastleRow :: Int -> Castle -> Array Boolean
getCastleRow row (Castle x) = fromMaybe (A.replicate x.width true) (index x.values row)

showCastleSpace :: Boolean -> String
showCastleSpace = (if _ then "X" else ".")

newCastle :: Width -> Height -> Castle
newCastle w h = Castle
  { values: makeInitialArray w h
  , width: w
  , height: h
  }

makeInitialArray :: Width -> Height -> Array (Array Boolean)
makeInitialArray w h = [A.replicate w true] <> (A.replicate (h - 1) <<< A.replicate w) false

pb :: Width -> Height -> Castle -> Castle
pb w h cObj@(Castle c) = Castle
  { values: updateValue w h c.values
  , width: c.width
  , height: c.height
  }

updateValue :: Int -> Int -> Array (Array Boolean) -> Array (Array Boolean)
updateValue w h orig = fromMaybe orig $ do
  column <- index orig h
  updatedColumn <- updateAt w true column
  updateAt h updatedColumn orig

getRowTuples :: Int -> Castle -> Array (Tuple Boolean Boolean)
getRowTuples row (Castle {values}) = fromMaybe [] $ do
  myRow <- values !! row
  prevRow <- values !! (row - 1)
  pure $ A.zip myRow prevRow

getNeighborIndices :: Castle -> Array (Tuple Width Height)
getNeighborIndices c@(Castle cast) = foldl (\acc row ->
  acc <> getNeighborIndicesForRow row c
  ) [] (1..cast.height)
  where
    getNeighborIndicesForRow row c' = foldl (\acc (Tuple i (Tuple curr prev)) ->
      if prev && not curr then A.snoc acc (Tuple i row) else acc
      ) [] $ A.mapWithIndex Tuple (getRowTuples row c')

getTotalBlocks :: Castle -> Int
getTotalBlocks cast@(Castle c) = sum $
  map (\x -> getBlocksInRow x cast) (0 .. (c.height - 1))
  where
    getBlocksInRow row c' = length $ A.filter (\x -> S.length x > 0)
      $ S.split (Pattern ".") (stringifyRow row c')

isEvenCastle :: Castle -> Boolean
isEvenCastle c = mod (getTotalBlocks c) 2 == 0

applyNeighbors :: Castle -> Array (Tuple Width Height) -> Castle
applyNeighbors castle changeList = foldl
  (\acc (Tuple x y) -> acc >>> pb x y) id changeList castle

sublistofsize' :: forall a. Int -> List a -> List a -> List (List a) -> List (List a)
sublistofsize' 0 _ prefix otherResults = reverse prefix : otherResults
sublistofsize' _ Nil prefix otherResults = otherResults
sublistofsize' n (x : xs) prefix otherResults =
   sublistofsize' (n-1) xs (x:prefix) (sublistofsize' n xs prefix otherResults)

sublistofsize :: forall a. Int -> Array a -> Array (Array a)
sublistofsize n xs = A.fromFoldable $ map A.fromFoldable $ sublistofsize' n (L.fromFoldable xs) Nil Nil

combs :: forall a. Array a -> Array (Array a)
combs = sublistofsize 2

twoStackNeighbor :: Tuple Width Height -> Array (Tuple Width Height)
twoStackNeighbor (Tuple x y) = [(Tuple x y), (Tuple x (y + 1))]

getAllNeighborStates :: Castle -> Array (Array (Tuple Width Height))
getAllNeighborStates c =
  let neighbors = getNeighborIndices c
      subsets = combs neighbors
      twoStacks = map twoStackNeighbor neighbors
  in map singleton neighbors <> subsets <> twoStacks

getCastleCount :: Castle -> Int
getCastleCount castle = getTotalBlocks castle

visitNode :: Castle -> State (Set Castle) Unit
visitNode c = do
  s <- get
  if not (member c s) && isEvenCastle c
    then do
      put $ insert c s
      getAllCastles c
    else pure unit

getAllCastles :: Castle -> State (Set Castle) Unit
getAllCastles c = do
  traverse_ visitNode (map (applyNeighbors c) (getAllNeighborStates c))

getResult :: Width -> Height -> Array Castle
getResult w h = A.fromFoldable $ execState (getAllCastles (newCastle w h)) empty

getCount :: Width -> Height -> Int
getCount w h = size $ execState (getAllCastles (newCastle w h)) empty
