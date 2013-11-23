module Data.COLA where

import Control.Monad.ST
import Control.Monad.Partial
import Control.Monad.Partial.ST
import Control.Monad.Trans.Iter

import Control.Monad.Trans

import qualified Data.Vector.Generic as V
import qualified Data.Vector.Generic.Mutable as VM
import qualified Data.Vector.Fusion.Stream as S
import Data.Vector.Fusion.Stream.Merge

data COLA vk va k a = Zero
                    | One                     !(vk k) !(va a)
                    | Two   !(COLA vk va k a) !(vk k) !(va a) !(vk k) !(va a)                 !(Partial (vk k, va a))
                    | Three !(COLA vk va k a) !(vk k) !(va a) !(vk k) !(va a) !(vk k) !(va a) !(Partial (vk k, va a))

empty :: COLA vk va k a
empty = Zero

lookup :: (V.Vector vk k, V.Vector va a, Ord k) => k -> COLA vk va k a -> Maybe a
lookup k = k `seq` go
  where
    go Zero = Nothing
    go (One vk1 va1) = bsearch vk1 va1 Nothing
    go (Two rest vk1 va1 vk2 va2 _) = bsearch vk1 va1 (bsearch vk2 va2 (go rest))
    go (Three rest vk1 va1 vk2 va2 vk3 va3 _) = bsearch vk1 va1 (bsearch vk2 va2 (bsearch vk3 va3 (go rest)))
    
    bsearch vk va fallback = goSearch 0 (V.length vk)
      where
        goSearch start end
            | start + 1 == end = if vk V.! start == k
                                 then Just (va V.! start)
                                 else fallback
            | otherwise = case compare k (vk V.! mid) of
                LT -> goSearch start mid
                EQ -> Just (va V.! mid)
                GT -> goSearch mid end
          where
            mid = (start + end) `div` 2

insert :: (V.Vector vk k, V.Vector va a, Ord k) => k -> a -> COLA vk va k a -> COLA vk va k a
insert k a = go (V.singleton k) (V.singleton a)
  where
    go vk va Zero = One vk va
    go vk va (One vk1 va1) = Two Zero vk va vk1 va1 (merge vk va vk1 va1)
    go vk va (Two rest vk1 va1 vk2 va2 part) = Three (doSteps rest) vk va vk1 va1 vk2 va2 (run 1 part)
    go vk va (Three rest vk1 va1 vk2 va2 vk3 va3 part) = Two (uncurry go (finish part) rest) vk va vk1 va1 (merge vk va vk1 va1)
    
    doSteps Zero = Zero
    doSteps (One vk1 va1) = One vk1 va1
    doSteps (Two rest vk1 va1 vk2 va2 part) = Two (doSteps rest) vk1 va1 vk2 va2 (run 1 part)
    doSteps (Three rest vk1 va1 vk2 va2 vk3 va3 part) = Three (doSteps rest) vk1 va1 vk2 va2 vk3 va3 (run 1 part)
    
    merge vk1 va1 vk2 va2 = walkST
                          $ partialDoubleUnstream (V.length vk1 + V.length vk2)
                          $ mergeStreamsWith (\a _ -> Just a) (S.zip (V.stream vk1) (V.stream va1)) (S.zip (V.stream vk2) (V.stream va2))
    
    partialDoubleUnstream :: (V.Vector vk k, V.Vector va a) => Int -> S.Stream (k, a) -> IterT (ST s) (vk k, va a)
    partialDoubleUnstream n s
      = do
          vk <- lift $ VM.unsafeNew n
          va <- lift $ VM.unsafeNew n
          let put i (k, a) = do lift $ VM.unsafeWrite vk i k
                                lift $ VM.unsafeWrite va i a
                                IterT (return (Iter (return ())))
                                return (i+1)
          n' <- S.foldM' put 0 s
          vk' <- lift $ V.unsafeFreeze $ VM.unsafeSlice 0 n' vk
          va' <- lift $ V.unsafeFreeze $ VM.unsafeSlice 0 n' va
          return (vk', va')


fromList :: (V.Vector vk k, V.Vector va a, Ord k) => [(k, a)] -> COLA vk va k a
fromList = foldr (uncurry insert) empty
