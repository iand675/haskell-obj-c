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

import ObjC.Intents.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithText:@
initWithText :: (IsINTextNoteContent inTextNoteContent, IsNSString text) => inTextNoteContent -> text -> IO (Id INTextNoteContent)
initWithText inTextNoteContent  text =
withObjCPtr text $ \raw_text ->
    sendMsg inTextNoteContent (mkSelector "initWithText:") (retPtr retVoid) [argPtr (castPtr raw_text :: Ptr ())] >>= ownedObject . castPtr

-- | @- text@
text :: IsINTextNoteContent inTextNoteContent => inTextNoteContent -> IO (Id NSString)
text inTextNoteContent  =
  sendMsg inTextNoteContent (mkSelector "text") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithText:@
initWithTextSelector :: Selector
initWithTextSelector = mkSelector "initWithText:"

-- | @Selector@ for @text@
textSelector :: Selector
textSelector = mkSelector "text"

