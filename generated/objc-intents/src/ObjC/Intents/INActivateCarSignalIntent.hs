{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INActivateCarSignalIntent@.
module ObjC.Intents.INActivateCarSignalIntent
  ( INActivateCarSignalIntent
  , IsINActivateCarSignalIntent(..)
  , initWithCarName_signals
  , carName
  , signals
  , initWithCarName_signalsSelector
  , carNameSelector
  , signalsSelector

  -- * Enum types
  , INCarSignalOptions(INCarSignalOptions)
  , pattern INCarSignalOptionAudible
  , pattern INCarSignalOptionVisible

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

-- | @- initWithCarName:signals:@
initWithCarName_signals :: (IsINActivateCarSignalIntent inActivateCarSignalIntent, IsINSpeakableString carName) => inActivateCarSignalIntent -> carName -> INCarSignalOptions -> IO (Id INActivateCarSignalIntent)
initWithCarName_signals inActivateCarSignalIntent  carName signals =
withObjCPtr carName $ \raw_carName ->
    sendMsg inActivateCarSignalIntent (mkSelector "initWithCarName:signals:") (retPtr retVoid) [argPtr (castPtr raw_carName :: Ptr ()), argCULong (coerce signals)] >>= ownedObject . castPtr

-- | @- carName@
carName :: IsINActivateCarSignalIntent inActivateCarSignalIntent => inActivateCarSignalIntent -> IO (Id INSpeakableString)
carName inActivateCarSignalIntent  =
  sendMsg inActivateCarSignalIntent (mkSelector "carName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- signals@
signals :: IsINActivateCarSignalIntent inActivateCarSignalIntent => inActivateCarSignalIntent -> IO INCarSignalOptions
signals inActivateCarSignalIntent  =
  fmap (coerce :: CULong -> INCarSignalOptions) $ sendMsg inActivateCarSignalIntent (mkSelector "signals") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithCarName:signals:@
initWithCarName_signalsSelector :: Selector
initWithCarName_signalsSelector = mkSelector "initWithCarName:signals:"

-- | @Selector@ for @carName@
carNameSelector :: Selector
carNameSelector = mkSelector "carName"

-- | @Selector@ for @signals@
signalsSelector :: Selector
signalsSelector = mkSelector "signals"

