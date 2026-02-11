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
  , urlSelector
  , setURLSelector


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

import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- image@
image :: IsNSPathComponentCell nsPathComponentCell => nsPathComponentCell -> IO (Id NSImage)
image nsPathComponentCell  =
  sendMsg nsPathComponentCell (mkSelector "image") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setImage:@
setImage :: (IsNSPathComponentCell nsPathComponentCell, IsNSImage value) => nsPathComponentCell -> value -> IO ()
setImage nsPathComponentCell  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsPathComponentCell (mkSelector "setImage:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- URL@
url :: IsNSPathComponentCell nsPathComponentCell => nsPathComponentCell -> IO (Id NSURL)
url nsPathComponentCell  =
  sendMsg nsPathComponentCell (mkSelector "URL") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setURL:@
setURL :: (IsNSPathComponentCell nsPathComponentCell, IsNSURL value) => nsPathComponentCell -> value -> IO ()
setURL nsPathComponentCell  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsPathComponentCell (mkSelector "setURL:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @image@
imageSelector :: Selector
imageSelector = mkSelector "image"

-- | @Selector@ for @setImage:@
setImageSelector :: Selector
setImageSelector = mkSelector "setImage:"

-- | @Selector@ for @URL@
urlSelector :: Selector
urlSelector = mkSelector "URL"

-- | @Selector@ for @setURL:@
setURLSelector :: Selector
setURLSelector = mkSelector "setURL:"

