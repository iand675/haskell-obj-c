{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSHapticFeedbackManager@.
module ObjC.AppKit.NSHapticFeedbackManager
  ( NSHapticFeedbackManager
  , IsNSHapticFeedbackManager(..)
  , defaultPerformer
  , defaultPerformerSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ defaultPerformer@
defaultPerformer :: IO RawId
defaultPerformer  =
  do
    cls' <- getRequiredClass "NSHapticFeedbackManager"
    sendClassMessage cls' defaultPerformerSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @defaultPerformer@
defaultPerformerSelector :: Selector '[] RawId
defaultPerformerSelector = mkSelector "defaultPerformer"

