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

import ObjC.IntentsUI.Internal.Classes
import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes
import ObjC.Intents.Internal.Classes

-- | @+ imageWithNSImage:@
imageWithNSImage :: IsNSImage image => image -> IO (Id INImage)
imageWithNSImage image =
  do
    cls' <- getRequiredClass "INImage"
    withObjCPtr image $ \raw_image ->
      sendClassMsg cls' (mkSelector "imageWithNSImage:") (retPtr retVoid) [argPtr (castPtr raw_image :: Ptr ())] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @imageWithNSImage:@
imageWithNSImageSelector :: Selector
imageWithNSImageSelector = mkSelector "imageWithNSImage:"

