{-# LANGUAGE DataKinds #-}
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
  , imageWithImageDataSelector
  , imageWithURLSelector
  , imageWithURL_width_heightSelector
  , systemImageNamedSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Intents.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ imageNamed:@
imageNamed :: IsNSString name => name -> IO (Id INImage)
imageNamed name =
  do
    cls' <- getRequiredClass "INImage"
    sendClassMessage cls' imageNamedSelector (toNSString name)

-- | @+ systemImageNamed:@
systemImageNamed :: IsNSString systemImageName => systemImageName -> IO (Id INImage)
systemImageNamed systemImageName =
  do
    cls' <- getRequiredClass "INImage"
    sendClassMessage cls' systemImageNamedSelector (toNSString systemImageName)

-- | @+ imageWithImageData:@
imageWithImageData :: IsNSData imageData => imageData -> IO (Id INImage)
imageWithImageData imageData =
  do
    cls' <- getRequiredClass "INImage"
    sendClassMessage cls' imageWithImageDataSelector (toNSData imageData)

-- | @+ imageWithURL:@
imageWithURL :: IsNSURL url => url -> IO (Id INImage)
imageWithURL url =
  do
    cls' <- getRequiredClass "INImage"
    sendClassMessage cls' imageWithURLSelector (toNSURL url)

-- | @+ imageWithURL:width:height:@
imageWithURL_width_height :: IsNSURL url => url -> CDouble -> CDouble -> IO (Id INImage)
imageWithURL_width_height url width height =
  do
    cls' <- getRequiredClass "INImage"
    sendClassMessage cls' imageWithURL_width_heightSelector (toNSURL url) width height

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @imageNamed:@
imageNamedSelector :: Selector '[Id NSString] (Id INImage)
imageNamedSelector = mkSelector "imageNamed:"

-- | @Selector@ for @systemImageNamed:@
systemImageNamedSelector :: Selector '[Id NSString] (Id INImage)
systemImageNamedSelector = mkSelector "systemImageNamed:"

-- | @Selector@ for @imageWithImageData:@
imageWithImageDataSelector :: Selector '[Id NSData] (Id INImage)
imageWithImageDataSelector = mkSelector "imageWithImageData:"

-- | @Selector@ for @imageWithURL:@
imageWithURLSelector :: Selector '[Id NSURL] (Id INImage)
imageWithURLSelector = mkSelector "imageWithURL:"

-- | @Selector@ for @imageWithURL:width:height:@
imageWithURL_width_heightSelector :: Selector '[Id NSURL, CDouble, CDouble] (Id INImage)
imageWithURL_width_heightSelector = mkSelector "imageWithURL:width:height:"

