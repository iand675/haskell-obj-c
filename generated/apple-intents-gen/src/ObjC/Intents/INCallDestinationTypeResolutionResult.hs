{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INCallDestinationTypeResolutionResult@.
module ObjC.Intents.INCallDestinationTypeResolutionResult
  ( INCallDestinationTypeResolutionResult
  , IsINCallDestinationTypeResolutionResult(..)
  , successWithResolvedCallDestinationType
  , confirmationRequiredWithCallDestinationTypeToConfirm
  , confirmationRequiredWithCallDestinationTypeToConfirmSelector
  , successWithResolvedCallDestinationTypeSelector

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

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
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
    sendClassMessage cls' successWithResolvedCallDestinationTypeSelector resolvedCallDestinationType

-- | @+ confirmationRequiredWithCallDestinationTypeToConfirm:@
confirmationRequiredWithCallDestinationTypeToConfirm :: INCallDestinationType -> IO (Id INCallDestinationTypeResolutionResult)
confirmationRequiredWithCallDestinationTypeToConfirm callDestinationTypeToConfirm =
  do
    cls' <- getRequiredClass "INCallDestinationTypeResolutionResult"
    sendClassMessage cls' confirmationRequiredWithCallDestinationTypeToConfirmSelector callDestinationTypeToConfirm

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @successWithResolvedCallDestinationType:@
successWithResolvedCallDestinationTypeSelector :: Selector '[INCallDestinationType] (Id INCallDestinationTypeResolutionResult)
successWithResolvedCallDestinationTypeSelector = mkSelector "successWithResolvedCallDestinationType:"

-- | @Selector@ for @confirmationRequiredWithCallDestinationTypeToConfirm:@
confirmationRequiredWithCallDestinationTypeToConfirmSelector :: Selector '[INCallDestinationType] (Id INCallDestinationTypeResolutionResult)
confirmationRequiredWithCallDestinationTypeToConfirmSelector = mkSelector "confirmationRequiredWithCallDestinationTypeToConfirm:"

