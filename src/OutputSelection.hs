module OutputSelection where

import qualified ApiSpec                    as AS
import qualified Generation.OutputGenerator as OG

-- | Supported implementations (the preffix 'C' stands for 'Client' where as 'S' stands for
-- 'Server').
data Target =
    CPython
  | CJavascript
  | SJavascript
  deriving Show

-- | Maps targets to generation information.
getGenInfo :: Target -> OG.GenerationInfo
getGenInfo SJavascript = OG.createGenInfo files templates fieldMapping
  where
    files = []
    templates = [ ("templates/server/js/server.tpl", "js")
                , ("templates/server/js/package.tpl", "json") ]
    fieldMapping AS.TString = "String"
    fieldMapping AS.TInt = "Number"
    fieldMapping AS.TDouble = "Number"
    fieldMapping _ = error "Custom types not implemented yet"
getGenInfo other = error $ "Couldn't process " ++ show other ++ " flag."
