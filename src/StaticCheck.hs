module StaticCheck where

import qualified Data.Set      as S

import           Language.Abs
import           Language.ErrM

staticCheck :: Specification -> Err Specification
staticCheck spec@(Spec _ _ enums structs resources) =
  checkEnums enums >> checkStructs structs enums >> checkResources resources structs >> return spec

checkEnums :: [EnumType] -> Err ()
checkEnums enums =
  let duplicates = findDuplicates enums in
    case duplicates of
       Nothing -> return ()
       Just (DefEnum (Ident name) _) ->
           fail $ "Name clash in the enum declaration: "
               ++ name
               ++ " is declared more than once."

checkStructs :: [StructType] -> [EnumType] -> Err ()
checkStructs structs enums = undefined

checkResources :: [Resource] -> [StructType] -> Err ()
checkResources = undefined


findDuplicates :: Ord a => [a] -> Maybe a
findDuplicates list = go list S.empty S.empty
  where
    go [] _ repeatedSet = Nothing
    go (x:xs) seen repeatedSet | x `S.member` seen = return x
    go (x:xs) seen repeatedSet = go xs (x `S.insert` seen) repeatedSet

