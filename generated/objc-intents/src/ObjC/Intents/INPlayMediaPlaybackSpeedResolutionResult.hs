{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INPlayMediaPlaybackSpeedResolutionResult@.
module ObjC.Intents.INPlayMediaPlaybackSpeedResolutionResult
  ( INPlayMediaPlaybackSpeedResolutionResult
  , IsINPlayMediaPlaybackSpeedResolutionResult(..)
  , unsupportedForReason
  , initWithDoubleResolutionResult
  , unsupportedForReasonSelector
  , initWithDoubleResolutionResultSelector

  -- * Enum types
  , INPlayMediaPlaybackSpeedUnsupportedReason(INPlayMediaPlaybackSpeedUnsupportedReason)
  , pattern INPlayMediaPlaybackSpeedUnsupportedReasonBelowMinimum
  , pattern INPlayMediaPlaybackSpeedUnsupportedReasonAboveMaximum

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
unsupportedForReason :: INPlayMediaPlaybackSpeedUnsupportedReason -> IO (Id INPlayMediaPlaybackSpeedResolutionResult)
unsupportedForReason reason =
  do
    cls' <- getRequiredClass "INPlayMediaPlaybackSpeedResolutionResult"
    sendClassMsg cls' (mkSelector "unsupportedForReason:") (retPtr retVoid) [argCLong (coerce reason)] >>= retainedObject . castPtr

-- | @- initWithDoubleResolutionResult:@
initWithDoubleResolutionResult :: (IsINPlayMediaPlaybackSpeedResolutionResult inPlayMediaPlaybackSpeedResolutionResult, IsINDoubleResolutionResult doubleResolutionResult) => inPlayMediaPlaybackSpeedResolutionResult -> doubleResolutionResult -> IO (Id INPlayMediaPlaybackSpeedResolutionResult)
initWithDoubleResolutionResult inPlayMediaPlaybackSpeedResolutionResult  doubleResolutionResult =
withObjCPtr doubleResolutionResult $ \raw_doubleResolutionResult ->
    sendMsg inPlayMediaPlaybackSpeedResolutionResult (mkSelector "initWithDoubleResolutionResult:") (retPtr retVoid) [argPtr (castPtr raw_doubleResolutionResult :: Ptr ())] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @unsupportedForReason:@
unsupportedForReasonSelector :: Selector
unsupportedForReasonSelector = mkSelector "unsupportedForReason:"

-- | @Selector@ for @initWithDoubleResolutionResult:@
initWithDoubleResolutionResultSelector :: Selector
initWithDoubleResolutionResultSelector = mkSelector "initWithDoubleResolutionResult:"

