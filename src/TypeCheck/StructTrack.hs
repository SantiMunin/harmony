module TypeCheck.StructTrack where

import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.List as L
import TypeCheck.ApiSpec(Id)

type StructTrack = M.Map Id [Id]

isValidStructReference :: Id -> Id -> StructTrack -> Bool
isValidStructReference from to structTrack
  | from == to = False
  | (not (M.member from structTrack)) || (not (M.member to structTrack)) = False
  | L.elem from (referedBy structTrack to) = False
  | otherwise = True
    where
      referedBy :: StructTrack -> Id -> [Id]
      referedBy structTrack to = go structTrack S.empty S.empty to
        where
          go :: StructTrack -> S.Set Id -> S.Set Id -> Id -> [Id]
          go structTrack seen refered str
            | S.member str seen = S.toList refered
            | otherwise = let strPointsTo = M.lookup str structTrack in
                              case strPointsTo of
                                Nothing -> []
                                Just ids ->
                                  concatMap (\x -> go structTrack (S.insert x seen) (S.insert x refered) x) ids


addStructReference :: StructTrack -> Id -> Id -> StructTrack
addStructReference structTrack from to = M.insertWith (\to ids -> to ++ ids) from [to] structTrack

newStructTrack :: StructTrack
newStructTrack = M.empty
