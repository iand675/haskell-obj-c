{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSPathComponentCell@.
module ObjC.AppKit.NSPathComponentCell
  ( NSPathComponentCell
  , IsNSPathComponentCell(..)
  , image
  , setImage
  , url
  , setURL
  , imageSelector
  , setImageSelector
  , setURLSelector
  , urlSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- image@
image :: IsNSPathComponentCell nsPathComponentCell => nsPathComponentCell -> IO (Id NSImage)
image nsPathComponentCell =
  sendMessage nsPathComponentCell imageSelector

-- | @- setImage:@
setImage :: (IsNSPathComponentCell nsPathComponentCell, IsNSImage value) => nsPathComponentCell -> value -> IO ()
setImage nsPathComponentCell value =
  sendMessage nsPathComponentCell setImageSelector (toNSImage value)

-- | @- URL@
url :: IsNSPathComponentCell nsPathComponentCell => nsPathComponentCell -> IO (Id NSURL)
url nsPathComponentCell =
  sendMessage nsPathComponentCell urlSelector

-- | @- setURL:@
setURL :: (IsNSPathComponentCell nsPathComponentCell, IsNSURL value) => nsPathComponentCell -> value -> IO ()
setURL nsPathComponentCell value =
  sendMessage nsPathComponentCell setURLSelector (toNSURL value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @image@
imageSelector :: Selector '[] (Id NSImage)
imageSelector = mkSelector "image"

-- | @Selector@ for @setImage:@
setImageSelector :: Selector '[Id NSImage] ()
setImageSelector = mkSelector "setImage:"

-- | @Selector@ for @URL@
urlSelector :: Selector '[] (Id NSURL)
urlSelector = mkSelector "URL"

-- | @Selector@ for @setURL:@
setURLSelector :: Selector '[Id NSURL] ()
setURLSelector = mkSelector "setURL:"

