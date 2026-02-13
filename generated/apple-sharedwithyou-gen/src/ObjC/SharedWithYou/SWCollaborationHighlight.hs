{-# LANGUAGE DataKinds #-}
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
  , creationDateSelector
  , titleSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
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
collaborationIdentifier swCollaborationHighlight =
  sendMessage swCollaborationHighlight collaborationIdentifierSelector

-- | Title of the collaboration highlight
--
-- Title of the collaboration if provided by the app hosting the collaboration
--
-- ObjC selector: @- title@
title :: IsSWCollaborationHighlight swCollaborationHighlight => swCollaborationHighlight -> IO (Id NSString)
title swCollaborationHighlight =
  sendMessage swCollaborationHighlight titleSelector

-- | Date when the file was created
--
-- ObjC selector: @- creationDate@
creationDate :: IsSWCollaborationHighlight swCollaborationHighlight => swCollaborationHighlight -> IO (Id NSDate)
creationDate swCollaborationHighlight =
  sendMessage swCollaborationHighlight creationDateSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @collaborationIdentifier@
collaborationIdentifierSelector :: Selector '[] (Id NSString)
collaborationIdentifierSelector = mkSelector "collaborationIdentifier"

-- | @Selector@ for @title@
titleSelector :: Selector '[] (Id NSString)
titleSelector = mkSelector "title"

-- | @Selector@ for @creationDate@
creationDateSelector :: Selector '[] (Id NSDate)
creationDateSelector = mkSelector "creationDate"

