{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @SWStartCollaborationAction@.
module ObjC.SharedWithYouCore.SWStartCollaborationAction
  ( SWStartCollaborationAction
  , IsSWStartCollaborationAction(..)
  , fulfillUsingURL_collaborationIdentifier
  , init_
  , new
  , collaborationMetadata
  , collaborationMetadataSelector
  , fulfillUsingURL_collaborationIdentifierSelector
  , initSelector
  , newSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.SharedWithYouCore.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- fulfillUsingURL:collaborationIdentifier:@
fulfillUsingURL_collaborationIdentifier :: (IsSWStartCollaborationAction swStartCollaborationAction, IsNSURL url, IsNSString collaborationIdentifier) => swStartCollaborationAction -> url -> collaborationIdentifier -> IO ()
fulfillUsingURL_collaborationIdentifier swStartCollaborationAction url collaborationIdentifier =
  sendMessage swStartCollaborationAction fulfillUsingURL_collaborationIdentifierSelector (toNSURL url) (toNSString collaborationIdentifier)

-- | @- init@
init_ :: IsSWStartCollaborationAction swStartCollaborationAction => swStartCollaborationAction -> IO (Id SWStartCollaborationAction)
init_ swStartCollaborationAction =
  sendOwnedMessage swStartCollaborationAction initSelector

-- | @+ new@
new :: IO (Id SWStartCollaborationAction)
new  =
  do
    cls' <- getRequiredClass "SWStartCollaborationAction"
    sendOwnedClassMessage cls' newSelector

-- | @- collaborationMetadata@
collaborationMetadata :: IsSWStartCollaborationAction swStartCollaborationAction => swStartCollaborationAction -> IO (Id SWCollaborationMetadata)
collaborationMetadata swStartCollaborationAction =
  sendMessage swStartCollaborationAction collaborationMetadataSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @fulfillUsingURL:collaborationIdentifier:@
fulfillUsingURL_collaborationIdentifierSelector :: Selector '[Id NSURL, Id NSString] ()
fulfillUsingURL_collaborationIdentifierSelector = mkSelector "fulfillUsingURL:collaborationIdentifier:"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id SWStartCollaborationAction)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id SWStartCollaborationAction)
newSelector = mkSelector "new"

-- | @Selector@ for @collaborationMetadata@
collaborationMetadataSelector :: Selector '[] (Id SWCollaborationMetadata)
collaborationMetadataSelector = mkSelector "collaborationMetadata"

