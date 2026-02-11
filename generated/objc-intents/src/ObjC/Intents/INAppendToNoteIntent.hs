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
  , initWithTargetNote_contentSelector
  , targetNoteSelector
  , contentSelector


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

-- | @- initWithTargetNote:content:@
initWithTargetNote_content :: (IsINAppendToNoteIntent inAppendToNoteIntent, IsINNote targetNote, IsINNoteContent content) => inAppendToNoteIntent -> targetNote -> content -> IO (Id INAppendToNoteIntent)
initWithTargetNote_content inAppendToNoteIntent  targetNote content =
withObjCPtr targetNote $ \raw_targetNote ->
  withObjCPtr content $ \raw_content ->
      sendMsg inAppendToNoteIntent (mkSelector "initWithTargetNote:content:") (retPtr retVoid) [argPtr (castPtr raw_targetNote :: Ptr ()), argPtr (castPtr raw_content :: Ptr ())] >>= ownedObject . castPtr

-- | @- targetNote@
targetNote :: IsINAppendToNoteIntent inAppendToNoteIntent => inAppendToNoteIntent -> IO (Id INNote)
targetNote inAppendToNoteIntent  =
  sendMsg inAppendToNoteIntent (mkSelector "targetNote") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- content@
content :: IsINAppendToNoteIntent inAppendToNoteIntent => inAppendToNoteIntent -> IO (Id INNoteContent)
content inAppendToNoteIntent  =
  sendMsg inAppendToNoteIntent (mkSelector "content") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithTargetNote:content:@
initWithTargetNote_contentSelector :: Selector
initWithTargetNote_contentSelector = mkSelector "initWithTargetNote:content:"

-- | @Selector@ for @targetNote@
targetNoteSelector :: Selector
targetNoteSelector = mkSelector "targetNote"

-- | @Selector@ for @content@
contentSelector :: Selector
contentSelector = mkSelector "content"

