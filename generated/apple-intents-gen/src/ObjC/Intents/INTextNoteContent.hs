{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INTextNoteContent@.
module ObjC.Intents.INTextNoteContent
  ( INTextNoteContent
  , IsINTextNoteContent(..)
  , initWithText
  , text
  , initWithTextSelector
  , textSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Intents.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithText:@
initWithText :: (IsINTextNoteContent inTextNoteContent, IsNSString text) => inTextNoteContent -> text -> IO (Id INTextNoteContent)
initWithText inTextNoteContent text =
  sendOwnedMessage inTextNoteContent initWithTextSelector (toNSString text)

-- | @- text@
text :: IsINTextNoteContent inTextNoteContent => inTextNoteContent -> IO (Id NSString)
text inTextNoteContent =
  sendMessage inTextNoteContent textSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithText:@
initWithTextSelector :: Selector '[Id NSString] (Id INTextNoteContent)
initWithTextSelector = mkSelector "initWithText:"

-- | @Selector@ for @text@
textSelector :: Selector '[] (Id NSString)
textSelector = mkSelector "text"

