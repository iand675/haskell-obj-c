{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @SRTextInputSession@.
module ObjC.SensorKit.SRTextInputSession
  ( SRTextInputSession
  , IsSRTextInputSession(..)
  , duration
  , sessionType
  , sessionIdentifier
  , durationSelector
  , sessionTypeSelector
  , sessionIdentifierSelector

  -- * Enum types
  , SRTextInputSessionType(SRTextInputSessionType)
  , pattern SRTextInputSessionTypeKeyboard
  , pattern SRTextInputSessionTypeThirdPartyKeyboard
  , pattern SRTextInputSessionTypePencil
  , pattern SRTextInputSessionTypeDictation

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

import ObjC.SensorKit.Internal.Classes
import ObjC.SensorKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- duration@
duration :: IsSRTextInputSession srTextInputSession => srTextInputSession -> IO CDouble
duration srTextInputSession  =
    sendMsg srTextInputSession (mkSelector "duration") retCDouble []

-- | @- sessionType@
sessionType :: IsSRTextInputSession srTextInputSession => srTextInputSession -> IO SRTextInputSessionType
sessionType srTextInputSession  =
    fmap (coerce :: CLong -> SRTextInputSessionType) $ sendMsg srTextInputSession (mkSelector "sessionType") retCLong []

-- | sessionIdentifier
--
-- Unique identifier of keyboard session
--
-- ObjC selector: @- sessionIdentifier@
sessionIdentifier :: IsSRTextInputSession srTextInputSession => srTextInputSession -> IO (Id NSString)
sessionIdentifier srTextInputSession  =
    sendMsg srTextInputSession (mkSelector "sessionIdentifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @duration@
durationSelector :: Selector
durationSelector = mkSelector "duration"

-- | @Selector@ for @sessionType@
sessionTypeSelector :: Selector
sessionTypeSelector = mkSelector "sessionType"

-- | @Selector@ for @sessionIdentifier@
sessionIdentifierSelector :: Selector
sessionIdentifierSelector = mkSelector "sessionIdentifier"

