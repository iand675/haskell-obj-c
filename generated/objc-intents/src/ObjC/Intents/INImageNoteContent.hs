{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INImageNoteContent@.
module ObjC.Intents.INImageNoteContent
  ( INImageNoteContent
  , IsINImageNoteContent(..)
  , initWithImage
  , image
  , initWithImageSelector
  , imageSelector


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

-- | @- initWithImage:@
initWithImage :: (IsINImageNoteContent inImageNoteContent, IsINImage image) => inImageNoteContent -> image -> IO (Id INImageNoteContent)
initWithImage inImageNoteContent  image =
withObjCPtr image $ \raw_image ->
    sendMsg inImageNoteContent (mkSelector "initWithImage:") (retPtr retVoid) [argPtr (castPtr raw_image :: Ptr ())] >>= ownedObject . castPtr

-- | @- image@
image :: IsINImageNoteContent inImageNoteContent => inImageNoteContent -> IO (Id INImage)
image inImageNoteContent  =
  sendMsg inImageNoteContent (mkSelector "image") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithImage:@
initWithImageSelector :: Selector
initWithImageSelector = mkSelector "initWithImage:"

-- | @Selector@ for @image@
imageSelector :: Selector
imageSelector = mkSelector "image"

