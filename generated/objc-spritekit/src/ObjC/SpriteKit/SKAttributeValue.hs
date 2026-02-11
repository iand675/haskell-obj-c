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
  , valueWithFloatSelector
  , initSelector
  , floatValueSelector
  , setFloatValueSelector


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

import ObjC.SpriteKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ valueWithFloat:@
valueWithFloat :: CFloat -> IO (Id SKAttributeValue)
valueWithFloat value =
  do
    cls' <- getRequiredClass "SKAttributeValue"
    sendClassMsg cls' (mkSelector "valueWithFloat:") (retPtr retVoid) [argCFloat (fromIntegral value)] >>= retainedObject . castPtr

-- | @- init@
init_ :: IsSKAttributeValue skAttributeValue => skAttributeValue -> IO (Id SKAttributeValue)
init_ skAttributeValue  =
  sendMsg skAttributeValue (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- floatValue@
floatValue :: IsSKAttributeValue skAttributeValue => skAttributeValue -> IO CFloat
floatValue skAttributeValue  =
  sendMsg skAttributeValue (mkSelector "floatValue") retCFloat []

-- | @- setFloatValue:@
setFloatValue :: IsSKAttributeValue skAttributeValue => skAttributeValue -> CFloat -> IO ()
setFloatValue skAttributeValue  value =
  sendMsg skAttributeValue (mkSelector "setFloatValue:") retVoid [argCFloat (fromIntegral value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @valueWithFloat:@
valueWithFloatSelector :: Selector
valueWithFloatSelector = mkSelector "valueWithFloat:"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @floatValue@
floatValueSelector :: Selector
floatValueSelector = mkSelector "floatValue"

-- | @Selector@ for @setFloatValue:@
setFloatValueSelector :: Selector
setFloatValueSelector = mkSelector "setFloatValue:"

