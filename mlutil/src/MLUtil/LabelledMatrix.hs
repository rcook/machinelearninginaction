module MLUtil.LabelledMatrix
    ( LabelId
    , LabelledMatrix (..)
    , mkLabelledMatrix
    , readLabelledMatrix
    ) where

import           Control.Exception
import           Control.Monad
import           Control.Monad.ST
import           Data.List.Split
import qualified Data.Map as M
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Storable.Mutable as VSM
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import           MLUtil.Imports
import           MLUtil.Util
import           Numeric.LinearAlgebra.Devel

type LabelId = Int

data LabelledMatrix = LabelledMatrix
    { lmValues :: Matrix R
    , lmLabelIds :: VU.Vector LabelId
    , lmLabelIdMap :: M.Map String LabelId
    , lmLabelMap :: M.Map LabelId String
    } deriving Show

splitLine :: String -> [String]
splitLine = splitOneOf [' ', '\t']

swapPair :: (a, b) -> (b, a)
swapPair (a, b) = (b, a)

swapMap :: (Ord k, Ord v) => M.Map k v -> M.Map v k
swapMap = M.fromList . map swapPair . M.toList

-- cf kNN.file2matrix
readLabelledMatrix :: FilePath -> IO (Maybe LabelledMatrix)
readLabelledMatrix path = (mkLabelledMatrix . lines) <$> readFile path

-- cf kNN.file2matrix
mkLabelledMatrix :: [String] -> Maybe LabelledMatrix
mkLabelledMatrix [] = Nothing
mkLabelledMatrix ls =
    let rowCount = length ls
        tokenCount = length $ splitLine (head ls)
        columnCount = tokenCount - 1

        (valuesV, labelIds, labels) = runST $ do
            mValues <- VSM.new (rowCount * columnCount)
            mLabelIds <- VUM.new rowCount

            labels' <- forFoldM M.empty (zip [0..] ls) $ \labels'' (r, l) -> do
                let allTokens = splitLine l
                    labelToken = last allTokens
                    valueTokens = init $ allTokens
                    (labels''', labelId) = case M.lookup labelToken labels'' of
                        Just labelId -> (labels'', labelId)
                        Nothing ->
                            let newLabelId = length labels''
                            in (M.insert labelToken newLabelId labels'', newLabelId)

                VUM.unsafeWrite mLabelIds r labelId

                forM_ (zip [0..] valueTokens) $ \(c, valueToken) ->
                    VSM.unsafeWrite mValues (r * columnCount + c) (read valueToken)

                return labels'''

            values' <- VS.unsafeFreeze mValues
            labelIds' <- VU.unsafeFreeze mLabelIds
            return (values', labelIds', labels')

        values = matrixFromVector RowMajor rowCount columnCount valuesV
    in Just $ LabelledMatrix values labelIds labels (swapMap labels)
