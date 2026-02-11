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
  , sharedCoordinatorSelector
  , actionHandlerSelector
  , setActionHandlerSelector


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

-- | @- actionHandler@
actionHandler :: IsSWCollaborationCoordinator swCollaborationCoordinator => swCollaborationCoordinator -> IO RawId
actionHandler swCollaborationCoordinator  =
    fmap (RawId . castPtr) $ sendMsg swCollaborationCoordinator (mkSelector "actionHandler") (retPtr retVoid) []

-- | @- setActionHandler:@
setActionHandler :: IsSWCollaborationCoordinator swCollaborationCoordinator => swCollaborationCoordinator -> RawId -> IO ()
setActionHandler swCollaborationCoordinator  value =
    sendMsg swCollaborationCoordinator (mkSelector "setActionHandler:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @sharedCoordinator@
sharedCoordinatorSelector :: Selector
sharedCoordinatorSelector = mkSelector "sharedCoordinator"

-- | @Selector@ for @actionHandler@
actionHandlerSelector :: Selector
actionHandlerSelector = mkSelector "actionHandler"

-- | @Selector@ for @setActionHandler:@
setActionHandlerSelector :: Selector
setActionHandlerSelector = mkSelector "setActionHandler:"

