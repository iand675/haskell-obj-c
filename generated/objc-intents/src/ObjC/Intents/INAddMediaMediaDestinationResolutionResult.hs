{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INAddMediaMediaDestinationResolutionResult@.
module ObjC.Intents.INAddMediaMediaDestinationResolutionResult
  ( INAddMediaMediaDestinationResolutionResult
  , IsINAddMediaMediaDestinationResolutionResult(..)
  , unsupportedForReason
  , initWithMediaDestinationResolutionResult
  , unsupportedForReasonSelector
  , initWithMediaDestinationResolutionResultSelector

  -- * Enum types
  , INAddMediaMediaDestinationUnsupportedReason(INAddMediaMediaDestinationUnsupportedReason)
  , pattern INAddMediaMediaDestinationUnsupportedReasonPlaylistNameNotFound
  , pattern INAddMediaMediaDestinationUnsupportedReasonPlaylistNotEditable

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
unsupportedForReason :: INAddMediaMediaDestinationUnsupportedReason -> IO (Id INAddMediaMediaDestinationResolutionResult)
unsupportedForReason reason =
  do
    cls' <- getRequiredClass "INAddMediaMediaDestinationResolutionResult"
    sendClassMsg cls' (mkSelector "unsupportedForReason:") (retPtr retVoid) [argCLong (coerce reason)] >>= retainedObject . castPtr

-- | @- initWithMediaDestinationResolutionResult:@
initWithMediaDestinationResolutionResult :: (IsINAddMediaMediaDestinationResolutionResult inAddMediaMediaDestinationResolutionResult, IsINMediaDestinationResolutionResult mediaDestinationResolutionResult) => inAddMediaMediaDestinationResolutionResult -> mediaDestinationResolutionResult -> IO (Id INAddMediaMediaDestinationResolutionResult)
initWithMediaDestinationResolutionResult inAddMediaMediaDestinationResolutionResult  mediaDestinationResolutionResult =
withObjCPtr mediaDestinationResolutionResult $ \raw_mediaDestinationResolutionResult ->
    sendMsg inAddMediaMediaDestinationResolutionResult (mkSelector "initWithMediaDestinationResolutionResult:") (retPtr retVoid) [argPtr (castPtr raw_mediaDestinationResolutionResult :: Ptr ())] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @unsupportedForReason:@
unsupportedForReasonSelector :: Selector
unsupportedForReasonSelector = mkSelector "unsupportedForReason:"

-- | @Selector@ for @initWithMediaDestinationResolutionResult:@
initWithMediaDestinationResolutionResultSelector :: Selector
initWithMediaDestinationResolutionResultSelector = mkSelector "initWithMediaDestinationResolutionResult:"

