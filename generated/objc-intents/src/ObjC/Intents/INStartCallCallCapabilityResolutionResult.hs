{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INStartCallCallCapabilityResolutionResult@.
module ObjC.Intents.INStartCallCallCapabilityResolutionResult
  ( INStartCallCallCapabilityResolutionResult
  , IsINStartCallCallCapabilityResolutionResult(..)
  , unsupportedForReason
  , initWithCallCapabilityResolutionResult
  , unsupportedForReasonSelector
  , initWithCallCapabilityResolutionResultSelector

  -- * Enum types
  , INStartCallCallCapabilityUnsupportedReason(INStartCallCallCapabilityUnsupportedReason)
  , pattern INStartCallCallCapabilityUnsupportedReasonVideoCallUnsupported
  , pattern INStartCallCallCapabilityUnsupportedReasonMicrophoneNotAccessible
  , pattern INStartCallCallCapabilityUnsupportedReasonCameraNotAccessible

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

-- | @+ unsupportedForReason:@
unsupportedForReason :: INStartCallCallCapabilityUnsupportedReason -> IO (Id INStartCallCallCapabilityResolutionResult)
unsupportedForReason reason =
  do
    cls' <- getRequiredClass "INStartCallCallCapabilityResolutionResult"
    sendClassMsg cls' (mkSelector "unsupportedForReason:") (retPtr retVoid) [argCLong (coerce reason)] >>= retainedObject . castPtr

-- | @- initWithCallCapabilityResolutionResult:@
initWithCallCapabilityResolutionResult :: (IsINStartCallCallCapabilityResolutionResult inStartCallCallCapabilityResolutionResult, IsINCallCapabilityResolutionResult callCapabilityResolutionResult) => inStartCallCallCapabilityResolutionResult -> callCapabilityResolutionResult -> IO (Id INStartCallCallCapabilityResolutionResult)
initWithCallCapabilityResolutionResult inStartCallCallCapabilityResolutionResult  callCapabilityResolutionResult =
withObjCPtr callCapabilityResolutionResult $ \raw_callCapabilityResolutionResult ->
    sendMsg inStartCallCallCapabilityResolutionResult (mkSelector "initWithCallCapabilityResolutionResult:") (retPtr retVoid) [argPtr (castPtr raw_callCapabilityResolutionResult :: Ptr ())] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @unsupportedForReason:@
unsupportedForReasonSelector :: Selector
unsupportedForReasonSelector = mkSelector "unsupportedForReason:"

-- | @Selector@ for @initWithCallCapabilityResolutionResult:@
initWithCallCapabilityResolutionResultSelector :: Selector
initWithCallCapabilityResolutionResultSelector = mkSelector "initWithCallCapabilityResolutionResult:"

