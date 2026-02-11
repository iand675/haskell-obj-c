{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INCallDestinationTypeResolutionResult@.
module ObjC.Intents.INCallDestinationTypeResolutionResult
  ( INCallDestinationTypeResolutionResult
  , IsINCallDestinationTypeResolutionResult(..)
  , successWithResolvedCallDestinationType
  , confirmationRequiredWithCallDestinationTypeToConfirm
  , successWithResolvedCallDestinationTypeSelector
  , confirmationRequiredWithCallDestinationTypeToConfirmSelector

  -- * Enum types
  , INCallDestinationType(INCallDestinationType)
  , pattern INCallDestinationTypeUnknown
  , pattern INCallDestinationTypeNormal
  , pattern INCallDestinationTypeEmergency
  , pattern INCallDestinationTypeVoicemail
  , pattern INCallDestinationTypeRedial
  , pattern INCallDestinationTypeCallBack
  , pattern INCallDestinationTypeNormalDestination
  , pattern INCallDestinationTypeEmergencyDestination
  , pattern INCallDestinationTypeVoicemailDestination
  , pattern INCallDestinationTypeRedialDestination

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

-- | @+ successWithResolvedCallDestinationType:@
successWithResolvedCallDestinationType :: INCallDestinationType -> IO (Id INCallDestinationTypeResolutionResult)
successWithResolvedCallDestinationType resolvedCallDestinationType =
  do
    cls' <- getRequiredClass "INCallDestinationTypeResolutionResult"
    sendClassMsg cls' (mkSelector "successWithResolvedCallDestinationType:") (retPtr retVoid) [argCLong (coerce resolvedCallDestinationType)] >>= retainedObject . castPtr

-- | @+ confirmationRequiredWithCallDestinationTypeToConfirm:@
confirmationRequiredWithCallDestinationTypeToConfirm :: INCallDestinationType -> IO (Id INCallDestinationTypeResolutionResult)
confirmationRequiredWithCallDestinationTypeToConfirm callDestinationTypeToConfirm =
  do
    cls' <- getRequiredClass "INCallDestinationTypeResolutionResult"
    sendClassMsg cls' (mkSelector "confirmationRequiredWithCallDestinationTypeToConfirm:") (retPtr retVoid) [argCLong (coerce callDestinationTypeToConfirm)] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @successWithResolvedCallDestinationType:@
successWithResolvedCallDestinationTypeSelector :: Selector
successWithResolvedCallDestinationTypeSelector = mkSelector "successWithResolvedCallDestinationType:"

-- | @Selector@ for @confirmationRequiredWithCallDestinationTypeToConfirm:@
confirmationRequiredWithCallDestinationTypeToConfirmSelector :: Selector
confirmationRequiredWithCallDestinationTypeToConfirmSelector = mkSelector "confirmationRequiredWithCallDestinationTypeToConfirm:"

