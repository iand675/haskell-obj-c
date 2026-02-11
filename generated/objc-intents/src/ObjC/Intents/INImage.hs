{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INImage@.
module ObjC.Intents.INImage
  ( INImage
  , IsINImage(..)
  , imageNamed
  , systemImageNamed
  , imageWithImageData
  , imageWithURL
  , imageWithURL_width_height
  , imageNamedSelector
  , systemImageNamedSelector
  , imageWithImageDataSelector
  , imageWithURLSelector
  , imageWithURL_width_heightSelector


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

-- | @+ imageNamed:@
imageNamed :: IsNSString name => name -> IO (Id INImage)
imageNamed name =
  do
    cls' <- getRequiredClass "INImage"
    withObjCPtr name $ \raw_name ->
      sendClassMsg cls' (mkSelector "imageNamed:") (retPtr retVoid) [argPtr (castPtr raw_name :: Ptr ())] >>= retainedObject . castPtr

-- | @+ systemImageNamed:@
systemImageNamed :: IsNSString systemImageName => systemImageName -> IO (Id INImage)
systemImageNamed systemImageName =
  do
    cls' <- getRequiredClass "INImage"
    withObjCPtr systemImageName $ \raw_systemImageName ->
      sendClassMsg cls' (mkSelector "systemImageNamed:") (retPtr retVoid) [argPtr (castPtr raw_systemImageName :: Ptr ())] >>= retainedObject . castPtr

-- | @+ imageWithImageData:@
imageWithImageData :: IsNSData imageData => imageData -> IO (Id INImage)
imageWithImageData imageData =
  do
    cls' <- getRequiredClass "INImage"
    withObjCPtr imageData $ \raw_imageData ->
      sendClassMsg cls' (mkSelector "imageWithImageData:") (retPtr retVoid) [argPtr (castPtr raw_imageData :: Ptr ())] >>= retainedObject . castPtr

-- | @+ imageWithURL:@
imageWithURL :: IsNSURL url => url -> IO (Id INImage)
imageWithURL url =
  do
    cls' <- getRequiredClass "INImage"
    withObjCPtr url $ \raw_url ->
      sendClassMsg cls' (mkSelector "imageWithURL:") (retPtr retVoid) [argPtr (castPtr raw_url :: Ptr ())] >>= retainedObject . castPtr

-- | @+ imageWithURL:width:height:@
imageWithURL_width_height :: IsNSURL url => url -> CDouble -> CDouble -> IO (Id INImage)
imageWithURL_width_height url width height =
  do
    cls' <- getRequiredClass "INImage"
    withObjCPtr url $ \raw_url ->
      sendClassMsg cls' (mkSelector "imageWithURL:width:height:") (retPtr retVoid) [argPtr (castPtr raw_url :: Ptr ()), argCDouble (fromIntegral width), argCDouble (fromIntegral height)] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @imageNamed:@
imageNamedSelector :: Selector
imageNamedSelector = mkSelector "imageNamed:"

-- | @Selector@ for @systemImageNamed:@
systemImageNamedSelector :: Selector
systemImageNamedSelector = mkSelector "systemImageNamed:"

-- | @Selector@ for @imageWithImageData:@
imageWithImageDataSelector :: Selector
imageWithImageDataSelector = mkSelector "imageWithImageData:"

-- | @Selector@ for @imageWithURL:@
imageWithURLSelector :: Selector
imageWithURLSelector = mkSelector "imageWithURL:"

-- | @Selector@ for @imageWithURL:width:height:@
imageWithURL_width_heightSelector :: Selector
imageWithURL_width_heightSelector = mkSelector "imageWithURL:width:height:"

