{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INAddMediaMediaDestinationResolutionResult@.
module ObjC.Intents.INAddMediaMediaDestinationResolutionResult
  ( INAddMediaMediaDestinationResolutionResult
  , IsINAddMediaMediaDestinationResolutionResult(..)
  , unsupportedForReason
  , initWithMediaDestinationResolutionResult
  , initWithMediaDestinationResolutionResultSelector
  , unsupportedForReasonSelector

  -- * Enum types
  , INAddMediaMediaDestinationUnsupportedReason(INAddMediaMediaDestinationUnsupportedReason)
  , pattern INAddMediaMediaDestinationUnsupportedReasonPlaylistNameNotFound
  , pattern INAddMediaMediaDestinationUnsupportedReasonPlaylistNotEditable

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

-- | @+ unsupportedForReason:@
unsupportedForReason :: INAddMediaMediaDestinationUnsupportedReason -> IO (Id INAddMediaMediaDestinationResolutionResult)
unsupportedForReason reason =
  do
    cls' <- getRequiredClass "INAddMediaMediaDestinationResolutionResult"
    sendClassMessage cls' unsupportedForReasonSelector reason

-- | @- initWithMediaDestinationResolutionResult:@
initWithMediaDestinationResolutionResult :: (IsINAddMediaMediaDestinationResolutionResult inAddMediaMediaDestinationResolutionResult, IsINMediaDestinationResolutionResult mediaDestinationResolutionResult) => inAddMediaMediaDestinationResolutionResult -> mediaDestinationResolutionResult -> IO (Id INAddMediaMediaDestinationResolutionResult)
initWithMediaDestinationResolutionResult inAddMediaMediaDestinationResolutionResult mediaDestinationResolutionResult =
  sendOwnedMessage inAddMediaMediaDestinationResolutionResult initWithMediaDestinationResolutionResultSelector (toINMediaDestinationResolutionResult mediaDestinationResolutionResult)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @unsupportedForReason:@
unsupportedForReasonSelector :: Selector '[INAddMediaMediaDestinationUnsupportedReason] (Id INAddMediaMediaDestinationResolutionResult)
unsupportedForReasonSelector = mkSelector "unsupportedForReason:"

-- | @Selector@ for @initWithMediaDestinationResolutionResult:@
initWithMediaDestinationResolutionResultSelector :: Selector '[Id INMediaDestinationResolutionResult] (Id INAddMediaMediaDestinationResolutionResult)
initWithMediaDestinationResolutionResultSelector = mkSelector "initWithMediaDestinationResolutionResult:"

