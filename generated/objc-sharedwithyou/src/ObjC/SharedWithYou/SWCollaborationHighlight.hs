{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | SWCollaborationHighlight
--
-- A SWHighlight object that represents an active collaboration
--
-- Generated bindings for @SWCollaborationHighlight@.
module ObjC.SharedWithYou.SWCollaborationHighlight
  ( SWCollaborationHighlight
  , IsSWCollaborationHighlight(..)
  , collaborationIdentifier
  , title
  , creationDate
  , collaborationIdentifierSelector
  , titleSelector
  , creationDateSelector


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

import ObjC.SharedWithYou.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Unique identifier as provided by the app hosting the collaboration
--
-- This identifier is unique across platforms and shares
--
-- ObjC selector: @- collaborationIdentifier@
collaborationIdentifier :: IsSWCollaborationHighlight swCollaborationHighlight => swCollaborationHighlight -> IO (Id NSString)
collaborationIdentifier swCollaborationHighlight  =
  sendMsg swCollaborationHighlight (mkSelector "collaborationIdentifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Title of the collaboration highlight
--
-- Title of the collaboration if provided by the app hosting the collaboration
--
-- ObjC selector: @- title@
title :: IsSWCollaborationHighlight swCollaborationHighlight => swCollaborationHighlight -> IO (Id NSString)
title swCollaborationHighlight  =
  sendMsg swCollaborationHighlight (mkSelector "title") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Date when the file was created
--
-- ObjC selector: @- creationDate@
creationDate :: IsSWCollaborationHighlight swCollaborationHighlight => swCollaborationHighlight -> IO (Id NSDate)
creationDate swCollaborationHighlight  =
  sendMsg swCollaborationHighlight (mkSelector "creationDate") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @collaborationIdentifier@
collaborationIdentifierSelector :: Selector
collaborationIdentifierSelector = mkSelector "collaborationIdentifier"

-- | @Selector@ for @title@
titleSelector :: Selector
titleSelector = mkSelector "title"

-- | @Selector@ for @creationDate@
creationDateSelector :: Selector
creationDateSelector = mkSelector "creationDate"

