{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRContentLauncherClusterStyleInformation@.
module ObjC.Matter.MTRContentLauncherClusterStyleInformation
  ( MTRContentLauncherClusterStyleInformation
  , IsMTRContentLauncherClusterStyleInformation(..)
  , color
  , setColor
  , size
  , setSize
  , colorSelector
  , setColorSelector
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

-- | @- color@
color :: IsMTRContentLauncherClusterStyleInformation mtrContentLauncherClusterStyleInformation => mtrContentLauncherClusterStyleInformation -> IO (Id NSString)
color mtrContentLauncherClusterStyleInformation =
  sendMessage mtrContentLauncherClusterStyleInformation colorSelector

-- | @- setColor:@
setColor :: (IsMTRContentLauncherClusterStyleInformation mtrContentLauncherClusterStyleInformation, IsNSString value) => mtrContentLauncherClusterStyleInformation -> value -> IO ()
setColor mtrContentLauncherClusterStyleInformation value =
  sendMessage mtrContentLauncherClusterStyleInformation setColorSelector (toNSString value)

-- | @- size@
size :: IsMTRContentLauncherClusterStyleInformation mtrContentLauncherClusterStyleInformation => mtrContentLauncherClusterStyleInformation -> IO (Id MTRContentLauncherClusterDimensionStruct)
size mtrContentLauncherClusterStyleInformation =
  sendMessage mtrContentLauncherClusterStyleInformation sizeSelector

-- | @- setSize:@
setSize :: (IsMTRContentLauncherClusterStyleInformation mtrContentLauncherClusterStyleInformation, IsMTRContentLauncherClusterDimensionStruct value) => mtrContentLauncherClusterStyleInformation -> value -> IO ()
setSize mtrContentLauncherClusterStyleInformation value =
  sendMessage mtrContentLauncherClusterStyleInformation setSizeSelector (toMTRContentLauncherClusterDimensionStruct value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

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

