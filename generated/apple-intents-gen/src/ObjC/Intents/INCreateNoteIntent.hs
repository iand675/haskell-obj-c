{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INCreateNoteIntent@.
module ObjC.Intents.INCreateNoteIntent
  ( INCreateNoteIntent
  , IsINCreateNoteIntent(..)
  , initWithTitle_content_groupName
  , title
  , content
  , groupName
  , contentSelector
  , groupNameSelector
  , initWithTitle_content_groupNameSelector
  , titleSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Intents.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithTitle:content:groupName:@
initWithTitle_content_groupName :: (IsINCreateNoteIntent inCreateNoteIntent, IsINSpeakableString title, IsINNoteContent content, IsINSpeakableString groupName) => inCreateNoteIntent -> title -> content -> groupName -> IO (Id INCreateNoteIntent)
initWithTitle_content_groupName inCreateNoteIntent title content groupName =
  sendOwnedMessage inCreateNoteIntent initWithTitle_content_groupNameSelector (toINSpeakableString title) (toINNoteContent content) (toINSpeakableString groupName)

-- | @- title@
title :: IsINCreateNoteIntent inCreateNoteIntent => inCreateNoteIntent -> IO (Id INSpeakableString)
title inCreateNoteIntent =
  sendMessage inCreateNoteIntent titleSelector

-- | @- content@
content :: IsINCreateNoteIntent inCreateNoteIntent => inCreateNoteIntent -> IO (Id INNoteContent)
content inCreateNoteIntent =
  sendMessage inCreateNoteIntent contentSelector

-- | @- groupName@
groupName :: IsINCreateNoteIntent inCreateNoteIntent => inCreateNoteIntent -> IO (Id INSpeakableString)
groupName inCreateNoteIntent =
  sendMessage inCreateNoteIntent groupNameSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithTitle:content:groupName:@
initWithTitle_content_groupNameSelector :: Selector '[Id INSpeakableString, Id INNoteContent, Id INSpeakableString] (Id INCreateNoteIntent)
initWithTitle_content_groupNameSelector = mkSelector "initWithTitle:content:groupName:"

-- | @Selector@ for @title@
titleSelector :: Selector '[] (Id INSpeakableString)
titleSelector = mkSelector "title"

-- | @Selector@ for @content@
contentSelector :: Selector '[] (Id INNoteContent)
contentSelector = mkSelector "content"

-- | @Selector@ for @groupName@
groupNameSelector :: Selector '[] (Id INSpeakableString)
groupNameSelector = mkSelector "groupName"

