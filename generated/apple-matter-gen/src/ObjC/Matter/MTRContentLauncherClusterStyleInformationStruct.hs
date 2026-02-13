{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRContentLauncherClusterStyleInformationStruct@.
module ObjC.Matter.MTRContentLauncherClusterStyleInformationStruct
  ( MTRContentLauncherClusterStyleInformationStruct
  , IsMTRContentLauncherClusterStyleInformationStruct(..)
  , imageURL
  , setImageURL
  , imageUrl
  , setImageUrl
  , color
  , setColor
  , size
  , setSize
  , colorSelector
  , imageURLSelector
  , imageUrlSelector
  , setColorSelector
  , setImageURLSelector
  , setImageUrlSelector
  , setSizeSelector
  , sizeSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- imageURL@
imageURL :: IsMTRContentLauncherClusterStyleInformationStruct mtrContentLauncherClusterStyleInformationStruct => mtrContentLauncherClusterStyleInformationStruct -> IO (Id NSString)
imageURL mtrContentLauncherClusterStyleInformationStruct =
  sendMessage mtrContentLauncherClusterStyleInformationStruct imageURLSelector

-- | @- setImageURL:@
setImageURL :: (IsMTRContentLauncherClusterStyleInformationStruct mtrContentLauncherClusterStyleInformationStruct, IsNSString value) => mtrContentLauncherClusterStyleInformationStruct -> value -> IO ()
setImageURL mtrContentLauncherClusterStyleInformationStruct value =
  sendMessage mtrContentLauncherClusterStyleInformationStruct setImageURLSelector (toNSString value)

-- | @- imageUrl@
imageUrl :: IsMTRContentLauncherClusterStyleInformationStruct mtrContentLauncherClusterStyleInformationStruct => mtrContentLauncherClusterStyleInformationStruct -> IO (Id NSString)
imageUrl mtrContentLauncherClusterStyleInformationStruct =
  sendMessage mtrContentLauncherClusterStyleInformationStruct imageUrlSelector

-- | @- setImageUrl:@
setImageUrl :: (IsMTRContentLauncherClusterStyleInformationStruct mtrContentLauncherClusterStyleInformationStruct, IsNSString value) => mtrContentLauncherClusterStyleInformationStruct -> value -> IO ()
setImageUrl mtrContentLauncherClusterStyleInformationStruct value =
  sendMessage mtrContentLauncherClusterStyleInformationStruct setImageUrlSelector (toNSString value)

-- | @- color@
color :: IsMTRContentLauncherClusterStyleInformationStruct mtrContentLauncherClusterStyleInformationStruct => mtrContentLauncherClusterStyleInformationStruct -> IO (Id NSString)
color mtrContentLauncherClusterStyleInformationStruct =
  sendMessage mtrContentLauncherClusterStyleInformationStruct colorSelector

-- | @- setColor:@
setColor :: (IsMTRContentLauncherClusterStyleInformationStruct mtrContentLauncherClusterStyleInformationStruct, IsNSString value) => mtrContentLauncherClusterStyleInformationStruct -> value -> IO ()
setColor mtrContentLauncherClusterStyleInformationStruct value =
  sendMessage mtrContentLauncherClusterStyleInformationStruct setColorSelector (toNSString value)

-- | @- size@
size :: IsMTRContentLauncherClusterStyleInformationStruct mtrContentLauncherClusterStyleInformationStruct => mtrContentLauncherClusterStyleInformationStruct -> IO (Id MTRContentLauncherClusterDimensionStruct)
size mtrContentLauncherClusterStyleInformationStruct =
  sendMessage mtrContentLauncherClusterStyleInformationStruct sizeSelector

-- | @- setSize:@
setSize :: (IsMTRContentLauncherClusterStyleInformationStruct mtrContentLauncherClusterStyleInformationStruct, IsMTRContentLauncherClusterDimensionStruct value) => mtrContentLauncherClusterStyleInformationStruct -> value -> IO ()
setSize mtrContentLauncherClusterStyleInformationStruct value =
  sendMessage mtrContentLauncherClusterStyleInformationStruct setSizeSelector (toMTRContentLauncherClusterDimensionStruct value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @imageURL@
imageURLSelector :: Selector '[] (Id NSString)
imageURLSelector = mkSelector "imageURL"

-- | @Selector@ for @setImageURL:@
setImageURLSelector :: Selector '[Id NSString] ()
setImageURLSelector = mkSelector "setImageURL:"

-- | @Selector@ for @imageUrl@
imageUrlSelector :: Selector '[] (Id NSString)
imageUrlSelector = mkSelector "imageUrl"

-- | @Selector@ for @setImageUrl:@
setImageUrlSelector :: Selector '[Id NSString] ()
setImageUrlSelector = mkSelector "setImageUrl:"

-- | @Selector@ for @color@
colorSelector :: Selector '[] (Id NSString)
colorSelector = mkSelector "color"

-- | @Selector@ for @setColor:@
setColorSelector :: Selector '[Id NSString] ()
setColorSelector = mkSelector "setColor:"

-- | @Selector@ for @size@
sizeSelector :: Selector '[] (Id MTRContentLauncherClusterDimensionStruct)
sizeSelector = mkSelector "size"

-- | @Selector@ for @setSize:@
setSizeSelector :: Selector '[Id MTRContentLauncherClusterDimensionStruct] ()
setSizeSelector = mkSelector "setSize:"

