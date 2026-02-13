{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSDerivedAttributeDescription@.
module ObjC.CoreData.NSDerivedAttributeDescription
  ( NSDerivedAttributeDescription
  , IsNSDerivedAttributeDescription(..)
  , derivationExpression
  , setDerivationExpression
  , derivationExpressionSelector
  , setDerivationExpressionSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CoreData.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- derivationExpression@
derivationExpression :: IsNSDerivedAttributeDescription nsDerivedAttributeDescription => nsDerivedAttributeDescription -> IO (Id NSExpression)
derivationExpression nsDerivedAttributeDescription =
  sendMessage nsDerivedAttributeDescription derivationExpressionSelector

-- | @- setDerivationExpression:@
setDerivationExpression :: (IsNSDerivedAttributeDescription nsDerivedAttributeDescription, IsNSExpression value) => nsDerivedAttributeDescription -> value -> IO ()
setDerivationExpression nsDerivedAttributeDescription value =
  sendMessage nsDerivedAttributeDescription setDerivationExpressionSelector (toNSExpression value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @derivationExpression@
derivationExpressionSelector :: Selector '[] (Id NSExpression)
derivationExpressionSelector = mkSelector "derivationExpression"

-- | @Selector@ for @setDerivationExpression:@
setDerivationExpressionSelector :: Selector '[Id NSExpression] ()
setDerivationExpressionSelector = mkSelector "setDerivationExpression:"

