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
  , fulfillUsingURL_collaborationIdentifierSelector
  , initSelector
  , newSelector
  , collaborationMetadataSelector


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

-- | @- fulfillUsingURL:collaborationIdentifier:@
fulfillUsingURL_collaborationIdentifier :: (IsSWStartCollaborationAction swStartCollaborationAction, IsNSURL url, IsNSString collaborationIdentifier) => swStartCollaborationAction -> url -> collaborationIdentifier -> IO ()
fulfillUsingURL_collaborationIdentifier swStartCollaborationAction  url collaborationIdentifier =
withObjCPtr url $ \raw_url ->
  withObjCPtr collaborationIdentifier $ \raw_collaborationIdentifier ->
      sendMsg swStartCollaborationAction (mkSelector "fulfillUsingURL:collaborationIdentifier:") retVoid [argPtr (castPtr raw_url :: Ptr ()), argPtr (castPtr raw_collaborationIdentifier :: Ptr ())]

-- | @- init@
init_ :: IsSWStartCollaborationAction swStartCollaborationAction => swStartCollaborationAction -> IO (Id SWStartCollaborationAction)
init_ swStartCollaborationAction  =
  sendMsg swStartCollaborationAction (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id SWStartCollaborationAction)
new  =
  do
    cls' <- getRequiredClass "SWStartCollaborationAction"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- collaborationMetadata@
collaborationMetadata :: IsSWStartCollaborationAction swStartCollaborationAction => swStartCollaborationAction -> IO (Id SWCollaborationMetadata)
collaborationMetadata swStartCollaborationAction  =
  sendMsg swStartCollaborationAction (mkSelector "collaborationMetadata") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @fulfillUsingURL:collaborationIdentifier:@
fulfillUsingURL_collaborationIdentifierSelector :: Selector
fulfillUsingURL_collaborationIdentifierSelector = mkSelector "fulfillUsingURL:collaborationIdentifier:"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @collaborationMetadata@
collaborationMetadataSelector :: Selector
collaborationMetadataSelector = mkSelector "collaborationMetadata"

