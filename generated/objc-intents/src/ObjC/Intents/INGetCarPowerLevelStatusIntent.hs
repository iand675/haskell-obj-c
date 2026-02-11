{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INGetCarPowerLevelStatusIntent@.
module ObjC.Intents.INGetCarPowerLevelStatusIntent
  ( INGetCarPowerLevelStatusIntent
  , IsINGetCarPowerLevelStatusIntent(..)
  , initWithCarName
  , carName
  , initWithCarNameSelector
  , carNameSelector


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

import ObjC.Intents.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithCarName:@
initWithCarName :: (IsINGetCarPowerLevelStatusIntent inGetCarPowerLevelStatusIntent, IsINSpeakableString carName) => inGetCarPowerLevelStatusIntent -> carName -> IO (Id INGetCarPowerLevelStatusIntent)
initWithCarName inGetCarPowerLevelStatusIntent  carName =
withObjCPtr carName $ \raw_carName ->
    sendMsg inGetCarPowerLevelStatusIntent (mkSelector "initWithCarName:") (retPtr retVoid) [argPtr (castPtr raw_carName :: Ptr ())] >>= ownedObject . castPtr

-- | @- carName@
carName :: IsINGetCarPowerLevelStatusIntent inGetCarPowerLevelStatusIntent => inGetCarPowerLevelStatusIntent -> IO (Id INSpeakableString)
carName inGetCarPowerLevelStatusIntent  =
  sendMsg inGetCarPowerLevelStatusIntent (mkSelector "carName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithCarName:@
initWithCarNameSelector :: Selector
initWithCarNameSelector = mkSelector "initWithCarName:"

-- | @Selector@ for @carName@
carNameSelector :: Selector
carNameSelector = mkSelector "carName"

