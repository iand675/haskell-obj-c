{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INImageNoteContent@.
module ObjC.Intents.INImageNoteContent
  ( INImageNoteContent
  , IsINImageNoteContent(..)
  , initWithImage
  , image
  , imageSelector
  , initWithImageSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Intents.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithImage:@
initWithImage :: (IsINImageNoteContent inImageNoteContent, IsINImage image) => inImageNoteContent -> image -> IO (Id INImageNoteContent)
initWithImage inImageNoteContent image =
  sendOwnedMessage inImageNoteContent initWithImageSelector (toINImage image)

-- | @- image@
image :: IsINImageNoteContent inImageNoteContent => inImageNoteContent -> IO (Id INImage)
image inImageNoteContent =
  sendMessage inImageNoteContent imageSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithImage:@
initWithImageSelector :: Selector '[Id INImage] (Id INImageNoteContent)
initWithImageSelector = mkSelector "initWithImage:"

-- | @Selector@ for @image@
imageSelector :: Selector '[] (Id INImage)
imageSelector = mkSelector "image"

