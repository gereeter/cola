import Control.DeepSeq
import Data.List (foldl')
import Control.Exception (evaluate)
import Data.Maybe (fromMaybe)

import Data.Vector.Unboxed (Vector)
import qualified Data.COLA as C
import qualified Data.Map as M

import Criterion.Main

import Prelude hiding (lookup)

main = do
    let m = M.fromAscList elems :: M.Map Int Int
        m_even = M.fromAscList elems_even :: M.Map Int Int
        m_odd = M.fromAscList elems_odd :: M.Map Int Int
        c = C.fromList elems :: C.COLA Vector Vector Int Int
        c_even = C.fromList elems_even :: C.COLA Vector Vector Int Int
        c_odd = C.fromList elems_odd :: C.COLA Vector Vector Int Int
    evaluate $ rnf [m, m_even, m_odd]
    evaluate c
    evaluate c_even
    evaluate c_odd
    defaultMain
        [ bgroup "lookup"
            [ bgroup "present"
                [ bench "Map"  $ whnf (\m -> foldl' (\n k -> fromMaybe n (M.lookup k m)) 0 evens) m_even
                , bench "COLA" $ whnf (\m -> foldl' (\n k -> fromMaybe n (C.lookup k m)) 0 evens) c_even
                ]
            , bgroup "absent"
                [ bench "Map"  $ whnf (\m -> foldl' (\n k -> fromMaybe n (M.lookup k m)) 0 evens) m_odd
                , bench "COLA" $ whnf (\m -> foldl' (\n k -> fromMaybe n (C.lookup k m)) 0 evens) c_odd
                ]
            ]
        , bgroup "insert"
            [ bgroup "present"
                [ bench "Map"  $ whnf (\m -> foldl' (\m (k, v) -> M.insert k v m) m elems_even) m_even
                , bench "COLA" $ whnf (\m -> foldl' (\m (k, v) -> C.insert k v m) m elems_even) c_even
                ]
            , bgroup "absent"
                [ bench "Map"  $ whnf (\m -> foldl' (\m (k, v) -> M.insert k v m) m elems_even) m_odd
                , bench "COLA" $ whnf (\m -> foldl' (\m (k, v) -> C.insert k v m) m elems_even) c_odd
                ]
            ]
        ]
  where
    bound = 2^12
    elems = zip keys values
    elems_even = zip evens evens
    elems_odd = zip odds odds
    keys = [1..bound]
    evens = [2,4..bound]
    odds = [1,3..bound]
    values = [1..bound]
