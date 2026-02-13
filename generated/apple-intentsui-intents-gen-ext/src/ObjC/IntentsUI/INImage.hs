{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INImage@.
module ObjC.IntentsUI.INImage
  ( INImage
  , IsINImage(..)
  , imageWithNSImage
  , imageWithNSImageSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.IntentsUI.Internal.Classes
import ObjC.AppKit.Internal.Classes
import ObjC.Intents.Internal.Classes

-- | @+ imageWithNSImage:@
imageWithNSImage :: IsNSImage image => image -> IO (Id INImage)
imageWithNSImage image =
  do
    cls' <- getRequiredClass "INImage"
    sendClassMessage cls' imageWithNSImageSelector (toNSImage image)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @imageWithNSImage:@
imageWithNSImageSelector :: Selector '[Id NSImage] (Id INImage)
imageWithNSImageSelector = mkSelector "imageWithNSImage:"

