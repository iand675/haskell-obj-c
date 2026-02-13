{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @SKAttributeValue@.
module ObjC.SpriteKit.SKAttributeValue
  ( SKAttributeValue
  , IsSKAttributeValue(..)
  , valueWithFloat
  , init_
  , floatValue
  , setFloatValue
  , floatValueSelector
  , initSelector
  , setFloatValueSelector
  , valueWithFloatSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.SpriteKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ valueWithFloat:@
valueWithFloat :: CFloat -> IO (Id SKAttributeValue)
valueWithFloat value =
  do
    cls' <- getRequiredClass "SKAttributeValue"
    sendClassMessage cls' valueWithFloatSelector value

-- | @- init@
init_ :: IsSKAttributeValue skAttributeValue => skAttributeValue -> IO (Id SKAttributeValue)
init_ skAttributeValue =
  sendOwnedMessage skAttributeValue initSelector

-- | @- floatValue@
floatValue :: IsSKAttributeValue skAttributeValue => skAttributeValue -> IO CFloat
floatValue skAttributeValue =
  sendMessage skAttributeValue floatValueSelector

-- | @- setFloatValue:@
setFloatValue :: IsSKAttributeValue skAttributeValue => skAttributeValue -> CFloat -> IO ()
setFloatValue skAttributeValue value =
  sendMessage skAttributeValue setFloatValueSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @valueWithFloat:@
valueWithFloatSelector :: Selector '[CFloat] (Id SKAttributeValue)
valueWithFloatSelector = mkSelector "valueWithFloat:"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id SKAttributeValue)
initSelector = mkSelector "init"

-- | @Selector@ for @floatValue@
floatValueSelector :: Selector '[] CFloat
floatValueSelector = mkSelector "floatValue"

-- | @Selector@ for @setFloatValue:@
setFloatValueSelector :: Selector '[CFloat] ()
setFloatValueSelector = mkSelector "setFloatValue:"

