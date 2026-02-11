{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @SWCollaborationCoordinator@.
module ObjC.SharedWithYouCore.SWCollaborationCoordinator
  ( SWCollaborationCoordinator
  , IsSWCollaborationCoordinator(..)
  , sharedCoordinator
  , sharedCoordinatorSelector


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

import ObjC.SharedWithYouCore.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ sharedCoordinator@
sharedCoordinator :: IO (Id SWCollaborationCoordinator)
sharedCoordinator  =
  do
    cls' <- getRequiredClass "SWCollaborationCoordinator"
    sendClassMsg cls' (mkSelector "sharedCoordinator") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @sharedCoordinator@
sharedCoordinatorSelector :: Selector
sharedCoordinatorSelector = mkSelector "sharedCoordinator"

