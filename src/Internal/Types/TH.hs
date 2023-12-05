-- | Helper functions so I don't need to write lots of dhall instances
module Internal.Types.TH where

import Language.Haskell.TH.Syntax (Dec (..), Q (..), Type (..), mkName)
import RIO

deriveProduct :: [String] -> [String] -> Q [Dec]
deriveProduct cs ss =
  return $
    [ StandaloneDerivD Nothing [] (AppT x y)
    | x <- ConT . mkName <$> cs
    , y <- ConT . mkName <$> ss
    ]
