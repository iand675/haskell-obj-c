{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @SWCollaborationCoordinator@.
module ObjC.SharedWithYouCore.SWCollaborationCoordinator
  ( SWCollaborationCoordinator
  , IsSWCollaborationCoordinator(..)
  , sharedCoordinator
  , actionHandler
  , setActionHandler
  , actionHandlerSelector
  , setActionHandlerSelector
  , sharedCoordinatorSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.SharedWithYouCore.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ sharedCoordinator@
sharedCoordinator :: IO (Id SWCollaborationCoordinator)
sharedCoordinator  =
  do
    cls' <- getRequiredClass "SWCollaborationCoordinator"
    sendClassMessage cls' sharedCoordinatorSelector

-- | @- actionHandler@
actionHandler :: IsSWCollaborationCoordinator swCollaborationCoordinator => swCollaborationCoordinator -> IO RawId
actionHandler swCollaborationCoordinator =
  sendMessage swCollaborationCoordinator actionHandlerSelector

-- | @- setActionHandler:@
setActionHandler :: IsSWCollaborationCoordinator swCollaborationCoordinator => swCollaborationCoordinator -> RawId -> IO ()
setActionHandler swCollaborationCoordinator value =
  sendMessage swCollaborationCoordinator setActionHandlerSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @sharedCoordinator@
sharedCoordinatorSelector :: Selector '[] (Id SWCollaborationCoordinator)
sharedCoordinatorSelector = mkSelector "sharedCoordinator"

-- | @Selector@ for @actionHandler@
actionHandlerSelector :: Selector '[] RawId
actionHandlerSelector = mkSelector "actionHandler"

-- | @Selector@ for @setActionHandler:@
setActionHandlerSelector :: Selector '[RawId] ()
setActionHandlerSelector = mkSelector "setActionHandler:"

