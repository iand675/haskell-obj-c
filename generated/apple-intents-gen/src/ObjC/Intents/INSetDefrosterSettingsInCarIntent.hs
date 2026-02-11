{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INSetDefrosterSettingsInCarIntent@.
module ObjC.Intents.INSetDefrosterSettingsInCarIntent
  ( INSetDefrosterSettingsInCarIntent
  , IsINSetDefrosterSettingsInCarIntent(..)
  , initWithEnable_defroster_carName
  , initWithEnable_defroster
  , enable
  , defroster
  , carName
  , initWithEnable_defroster_carNameSelector
  , initWithEnable_defrosterSelector
  , enableSelector
  , defrosterSelector
  , carNameSelector

  -- * Enum types
  , INCarDefroster(INCarDefroster)
  , pattern INCarDefrosterUnknown
  , pattern INCarDefrosterFront
  , pattern INCarDefrosterRear
  , pattern INCarDefrosterAll

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
import ObjC.Intents.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- initWithEnable:defroster:carName:@
initWithEnable_defroster_carName :: (IsINSetDefrosterSettingsInCarIntent inSetDefrosterSettingsInCarIntent, IsNSNumber enable, IsINSpeakableString carName) => inSetDefrosterSettingsInCarIntent -> enable -> INCarDefroster -> carName -> IO (Id INSetDefrosterSettingsInCarIntent)
initWithEnable_defroster_carName inSetDefrosterSettingsInCarIntent  enable defroster carName =
  withObjCPtr enable $ \raw_enable ->
    withObjCPtr carName $ \raw_carName ->
        sendMsg inSetDefrosterSettingsInCarIntent (mkSelector "initWithEnable:defroster:carName:") (retPtr retVoid) [argPtr (castPtr raw_enable :: Ptr ()), argCLong (coerce defroster), argPtr (castPtr raw_carName :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithEnable:defroster:@
initWithEnable_defroster :: (IsINSetDefrosterSettingsInCarIntent inSetDefrosterSettingsInCarIntent, IsNSNumber enable) => inSetDefrosterSettingsInCarIntent -> enable -> INCarDefroster -> IO (Id INSetDefrosterSettingsInCarIntent)
initWithEnable_defroster inSetDefrosterSettingsInCarIntent  enable defroster =
  withObjCPtr enable $ \raw_enable ->
      sendMsg inSetDefrosterSettingsInCarIntent (mkSelector "initWithEnable:defroster:") (retPtr retVoid) [argPtr (castPtr raw_enable :: Ptr ()), argCLong (coerce defroster)] >>= ownedObject . castPtr

-- | @- enable@
enable :: IsINSetDefrosterSettingsInCarIntent inSetDefrosterSettingsInCarIntent => inSetDefrosterSettingsInCarIntent -> IO (Id NSNumber)
enable inSetDefrosterSettingsInCarIntent  =
    sendMsg inSetDefrosterSettingsInCarIntent (mkSelector "enable") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- defroster@
defroster :: IsINSetDefrosterSettingsInCarIntent inSetDefrosterSettingsInCarIntent => inSetDefrosterSettingsInCarIntent -> IO INCarDefroster
defroster inSetDefrosterSettingsInCarIntent  =
    fmap (coerce :: CLong -> INCarDefroster) $ sendMsg inSetDefrosterSettingsInCarIntent (mkSelector "defroster") retCLong []

-- | @- carName@
carName :: IsINSetDefrosterSettingsInCarIntent inSetDefrosterSettingsInCarIntent => inSetDefrosterSettingsInCarIntent -> IO (Id INSpeakableString)
carName inSetDefrosterSettingsInCarIntent  =
    sendMsg inSetDefrosterSettingsInCarIntent (mkSelector "carName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithEnable:defroster:carName:@
initWithEnable_defroster_carNameSelector :: Selector
initWithEnable_defroster_carNameSelector = mkSelector "initWithEnable:defroster:carName:"

-- | @Selector@ for @initWithEnable:defroster:@
initWithEnable_defrosterSelector :: Selector
initWithEnable_defrosterSelector = mkSelector "initWithEnable:defroster:"

-- | @Selector@ for @enable@
enableSelector :: Selector
enableSelector = mkSelector "enable"

-- | @Selector@ for @defroster@
defrosterSelector :: Selector
defrosterSelector = mkSelector "defroster"

-- | @Selector@ for @carName@
carNameSelector :: Selector
carNameSelector = mkSelector "carName"

