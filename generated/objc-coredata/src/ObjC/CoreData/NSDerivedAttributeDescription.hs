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

import Foreign.Ptr (Ptr, nullPtr, castPtr)
import Foreign.LibFFI
import Foreign.C.Types
import Data.Int (Int8, Int16)
import Data.Word (Word16)
import Data.Coerce (coerce)

import ObjC.Runtime.Types
import ObjC.Runtime.MsgSend (sendMsg, sendClassMsg)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CoreData.Internal.Classes
import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- derivationExpression@
derivationExpression :: IsNSDerivedAttributeDescription nsDerivedAttributeDescription => nsDerivedAttributeDescription -> IO (Id NSExpression)
derivationExpression nsDerivedAttributeDescription  =
  sendMsg nsDerivedAttributeDescription (mkSelector "derivationExpression") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setDerivationExpression:@
setDerivationExpression :: (IsNSDerivedAttributeDescription nsDerivedAttributeDescription, IsNSExpression value) => nsDerivedAttributeDescription -> value -> IO ()
setDerivationExpression nsDerivedAttributeDescription  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsDerivedAttributeDescription (mkSelector "setDerivationExpression:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @derivationExpression@
derivationExpressionSelector :: Selector
derivationExpressionSelector = mkSelector "derivationExpression"

-- | @Selector@ for @setDerivationExpression:@
setDerivationExpressionSelector :: Selector
setDerivationExpressionSelector = mkSelector "setDerivationExpression:"

