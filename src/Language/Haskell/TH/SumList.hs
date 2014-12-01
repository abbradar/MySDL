module Language.Haskell.TH.SumList where

import Language.Haskell.TH

makeSumList :: Name -> Q Exp
makeSumList n = do
  (TyConI (DataD _ _ _ ctors _)) <- reify n
  return $ ListE $ map (\(NormalC cn []) -> ConE cn) ctors
