{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INAppendToNoteIntent@.
module ObjC.Intents.INAppendToNoteIntent
  ( INAppendToNoteIntent
  , IsINAppendToNoteIntent(..)
  , initWithTargetNote_content
  , targetNote
  , content
  , contentSelector
  , initWithTargetNote_contentSelector
  , targetNoteSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Intents.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithTargetNote:content:@
initWithTargetNote_content :: (IsINAppendToNoteIntent inAppendToNoteIntent, IsINNote targetNote, IsINNoteContent content) => inAppendToNoteIntent -> targetNote -> content -> IO (Id INAppendToNoteIntent)
initWithTargetNote_content inAppendToNoteIntent targetNote content =
  sendOwnedMessage inAppendToNoteIntent initWithTargetNote_contentSelector (toINNote targetNote) (toINNoteContent content)

-- | @- targetNote@
targetNote :: IsINAppendToNoteIntent inAppendToNoteIntent => inAppendToNoteIntent -> IO (Id INNote)
targetNote inAppendToNoteIntent =
  sendMessage inAppendToNoteIntent targetNoteSelector

-- | @- content@
content :: IsINAppendToNoteIntent inAppendToNoteIntent => inAppendToNoteIntent -> IO (Id INNoteContent)
content inAppendToNoteIntent =
  sendMessage inAppendToNoteIntent contentSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithTargetNote:content:@
initWithTargetNote_contentSelector :: Selector '[Id INNote, Id INNoteContent] (Id INAppendToNoteIntent)
initWithTargetNote_contentSelector = mkSelector "initWithTargetNote:content:"

-- | @Selector@ for @targetNote@
targetNoteSelector :: Selector '[] (Id INNote)
targetNoteSelector = mkSelector "targetNote"

-- | @Selector@ for @content@
contentSelector :: Selector '[] (Id INNoteContent)
contentSelector = mkSelector "content"

