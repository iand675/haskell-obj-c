{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @CAMetalLayer@.
module ObjC.QuartzCore.CAMetalLayer
  ( CAMetalLayer
  , IsCAMetalLayer(..)
  , nextDrawable
  , device
  , setDevice
  , preferredDevice
  , pixelFormat
  , setPixelFormat
  , framebufferOnly
  , setFramebufferOnly
  , maximumDrawableCount
  , setMaximumDrawableCount
  , presentsWithTransaction
  , setPresentsWithTransaction
  , colorspace
  , setColorspace
  , wantsExtendedDynamicRangeContent
  , setWantsExtendedDynamicRangeContent
  , edrMetadata
  , setEDRMetadata
  , displaySyncEnabled
  , setDisplaySyncEnabled
  , allowsNextDrawableTimeout
  , setAllowsNextDrawableTimeout
  , developerHUDProperties
  , setDeveloperHUDProperties
  , residencySet
  , nextDrawableSelector
  , deviceSelector
  , setDeviceSelector
  , preferredDeviceSelector
  , pixelFormatSelector
  , setPixelFormatSelector
  , framebufferOnlySelector
  , setFramebufferOnlySelector
  , maximumDrawableCountSelector
  , setMaximumDrawableCountSelector
  , presentsWithTransactionSelector
  , setPresentsWithTransactionSelector
  , colorspaceSelector
  , setColorspaceSelector
  , wantsExtendedDynamicRangeContentSelector
  , setWantsExtendedDynamicRangeContentSelector
  , edrMetadataSelector
  , setEDRMetadataSelector
  , displaySyncEnabledSelector
  , setDisplaySyncEnabledSelector
  , allowsNextDrawableTimeoutSelector
  , setAllowsNextDrawableTimeoutSelector
  , developerHUDPropertiesSelector
  , setDeveloperHUDPropertiesSelector
  , residencySetSelector


  ) where

import Foreign.Ptr (Ptr, nullPtr, castPtr)
import Foreign.LibFFI
import Foreign.C.Types
import Data.Int (Int8, Int16)
import Data.Word (Word16)
import Data.Coerce (coerce)

import ObjC.Runtime.Types
import ObjC.Runtime.MsgSend (sendMsg, sendClassMsg, sendMsgStret, sendClassMsgStret)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.QuartzCore.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- nextDrawable@
nextDrawable :: IsCAMetalLayer caMetalLayer => caMetalLayer -> IO RawId
nextDrawable caMetalLayer  =
    fmap (RawId . castPtr) $ sendMsg caMetalLayer (mkSelector "nextDrawable") (retPtr retVoid) []

-- | @- device@
device :: IsCAMetalLayer caMetalLayer => caMetalLayer -> IO RawId
device caMetalLayer  =
    fmap (RawId . castPtr) $ sendMsg caMetalLayer (mkSelector "device") (retPtr retVoid) []

-- | @- setDevice:@
setDevice :: IsCAMetalLayer caMetalLayer => caMetalLayer -> RawId -> IO ()
setDevice caMetalLayer  value =
    sendMsg caMetalLayer (mkSelector "setDevice:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- | @- preferredDevice@
preferredDevice :: IsCAMetalLayer caMetalLayer => caMetalLayer -> IO RawId
preferredDevice caMetalLayer  =
    fmap (RawId . castPtr) $ sendMsg caMetalLayer (mkSelector "preferredDevice") (retPtr retVoid) []

-- | @- pixelFormat@
pixelFormat :: IsCAMetalLayer caMetalLayer => caMetalLayer -> IO CInt
pixelFormat caMetalLayer  =
    sendMsg caMetalLayer (mkSelector "pixelFormat") retCInt []

-- | @- setPixelFormat:@
setPixelFormat :: IsCAMetalLayer caMetalLayer => caMetalLayer -> CInt -> IO ()
setPixelFormat caMetalLayer  value =
    sendMsg caMetalLayer (mkSelector "setPixelFormat:") retVoid [argCInt (fromIntegral value)]

-- | @- framebufferOnly@
framebufferOnly :: IsCAMetalLayer caMetalLayer => caMetalLayer -> IO Bool
framebufferOnly caMetalLayer  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg caMetalLayer (mkSelector "framebufferOnly") retCULong []

-- | @- setFramebufferOnly:@
setFramebufferOnly :: IsCAMetalLayer caMetalLayer => caMetalLayer -> Bool -> IO ()
setFramebufferOnly caMetalLayer  value =
    sendMsg caMetalLayer (mkSelector "setFramebufferOnly:") retVoid [argCULong (if value then 1 else 0)]

-- | @- maximumDrawableCount@
maximumDrawableCount :: IsCAMetalLayer caMetalLayer => caMetalLayer -> IO CULong
maximumDrawableCount caMetalLayer  =
    sendMsg caMetalLayer (mkSelector "maximumDrawableCount") retCULong []

-- | @- setMaximumDrawableCount:@
setMaximumDrawableCount :: IsCAMetalLayer caMetalLayer => caMetalLayer -> CULong -> IO ()
setMaximumDrawableCount caMetalLayer  value =
    sendMsg caMetalLayer (mkSelector "setMaximumDrawableCount:") retVoid [argCULong value]

-- | @- presentsWithTransaction@
presentsWithTransaction :: IsCAMetalLayer caMetalLayer => caMetalLayer -> IO Bool
presentsWithTransaction caMetalLayer  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg caMetalLayer (mkSelector "presentsWithTransaction") retCULong []

-- | @- setPresentsWithTransaction:@
setPresentsWithTransaction :: IsCAMetalLayer caMetalLayer => caMetalLayer -> Bool -> IO ()
setPresentsWithTransaction caMetalLayer  value =
    sendMsg caMetalLayer (mkSelector "setPresentsWithTransaction:") retVoid [argCULong (if value then 1 else 0)]

-- | @- colorspace@
colorspace :: IsCAMetalLayer caMetalLayer => caMetalLayer -> IO (Ptr ())
colorspace caMetalLayer  =
    fmap castPtr $ sendMsg caMetalLayer (mkSelector "colorspace") (retPtr retVoid) []

-- | @- setColorspace:@
setColorspace :: IsCAMetalLayer caMetalLayer => caMetalLayer -> Ptr () -> IO ()
setColorspace caMetalLayer  value =
    sendMsg caMetalLayer (mkSelector "setColorspace:") retVoid [argPtr value]

-- | @- wantsExtendedDynamicRangeContent@
wantsExtendedDynamicRangeContent :: IsCAMetalLayer caMetalLayer => caMetalLayer -> IO Bool
wantsExtendedDynamicRangeContent caMetalLayer  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg caMetalLayer (mkSelector "wantsExtendedDynamicRangeContent") retCULong []

-- | @- setWantsExtendedDynamicRangeContent:@
setWantsExtendedDynamicRangeContent :: IsCAMetalLayer caMetalLayer => caMetalLayer -> Bool -> IO ()
setWantsExtendedDynamicRangeContent caMetalLayer  value =
    sendMsg caMetalLayer (mkSelector "setWantsExtendedDynamicRangeContent:") retVoid [argCULong (if value then 1 else 0)]

-- | @- EDRMetadata@
edrMetadata :: IsCAMetalLayer caMetalLayer => caMetalLayer -> IO (Id CAEDRMetadata)
edrMetadata caMetalLayer  =
    sendMsg caMetalLayer (mkSelector "EDRMetadata") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setEDRMetadata:@
setEDRMetadata :: (IsCAMetalLayer caMetalLayer, IsCAEDRMetadata value) => caMetalLayer -> value -> IO ()
setEDRMetadata caMetalLayer  value =
  withObjCPtr value $ \raw_value ->
      sendMsg caMetalLayer (mkSelector "setEDRMetadata:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- displaySyncEnabled@
displaySyncEnabled :: IsCAMetalLayer caMetalLayer => caMetalLayer -> IO Bool
displaySyncEnabled caMetalLayer  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg caMetalLayer (mkSelector "displaySyncEnabled") retCULong []

-- | @- setDisplaySyncEnabled:@
setDisplaySyncEnabled :: IsCAMetalLayer caMetalLayer => caMetalLayer -> Bool -> IO ()
setDisplaySyncEnabled caMetalLayer  value =
    sendMsg caMetalLayer (mkSelector "setDisplaySyncEnabled:") retVoid [argCULong (if value then 1 else 0)]

-- | @- allowsNextDrawableTimeout@
allowsNextDrawableTimeout :: IsCAMetalLayer caMetalLayer => caMetalLayer -> IO Bool
allowsNextDrawableTimeout caMetalLayer  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg caMetalLayer (mkSelector "allowsNextDrawableTimeout") retCULong []

-- | @- setAllowsNextDrawableTimeout:@
setAllowsNextDrawableTimeout :: IsCAMetalLayer caMetalLayer => caMetalLayer -> Bool -> IO ()
setAllowsNextDrawableTimeout caMetalLayer  value =
    sendMsg caMetalLayer (mkSelector "setAllowsNextDrawableTimeout:") retVoid [argCULong (if value then 1 else 0)]

-- | @- developerHUDProperties@
developerHUDProperties :: IsCAMetalLayer caMetalLayer => caMetalLayer -> IO (Id NSDictionary)
developerHUDProperties caMetalLayer  =
    sendMsg caMetalLayer (mkSelector "developerHUDProperties") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setDeveloperHUDProperties:@
setDeveloperHUDProperties :: (IsCAMetalLayer caMetalLayer, IsNSDictionary value) => caMetalLayer -> value -> IO ()
setDeveloperHUDProperties caMetalLayer  value =
  withObjCPtr value $ \raw_value ->
      sendMsg caMetalLayer (mkSelector "setDeveloperHUDProperties:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- residencySet@
residencySet :: IsCAMetalLayer caMetalLayer => caMetalLayer -> IO RawId
residencySet caMetalLayer  =
    fmap (RawId . castPtr) $ sendMsg caMetalLayer (mkSelector "residencySet") (retPtr retVoid) []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @nextDrawable@
nextDrawableSelector :: Selector
nextDrawableSelector = mkSelector "nextDrawable"

-- | @Selector@ for @device@
deviceSelector :: Selector
deviceSelector = mkSelector "device"

-- | @Selector@ for @setDevice:@
setDeviceSelector :: Selector
setDeviceSelector = mkSelector "setDevice:"

-- | @Selector@ for @preferredDevice@
preferredDeviceSelector :: Selector
preferredDeviceSelector = mkSelector "preferredDevice"

-- | @Selector@ for @pixelFormat@
pixelFormatSelector :: Selector
pixelFormatSelector = mkSelector "pixelFormat"

-- | @Selector@ for @setPixelFormat:@
setPixelFormatSelector :: Selector
setPixelFormatSelector = mkSelector "setPixelFormat:"

-- | @Selector@ for @framebufferOnly@
framebufferOnlySelector :: Selector
framebufferOnlySelector = mkSelector "framebufferOnly"

-- | @Selector@ for @setFramebufferOnly:@
setFramebufferOnlySelector :: Selector
setFramebufferOnlySelector = mkSelector "setFramebufferOnly:"

-- | @Selector@ for @maximumDrawableCount@
maximumDrawableCountSelector :: Selector
maximumDrawableCountSelector = mkSelector "maximumDrawableCount"

-- | @Selector@ for @setMaximumDrawableCount:@
setMaximumDrawableCountSelector :: Selector
setMaximumDrawableCountSelector = mkSelector "setMaximumDrawableCount:"

-- | @Selector@ for @presentsWithTransaction@
presentsWithTransactionSelector :: Selector
presentsWithTransactionSelector = mkSelector "presentsWithTransaction"

-- | @Selector@ for @setPresentsWithTransaction:@
setPresentsWithTransactionSelector :: Selector
setPresentsWithTransactionSelector = mkSelector "setPresentsWithTransaction:"

-- | @Selector@ for @colorspace@
colorspaceSelector :: Selector
colorspaceSelector = mkSelector "colorspace"

-- | @Selector@ for @setColorspace:@
setColorspaceSelector :: Selector
setColorspaceSelector = mkSelector "setColorspace:"

-- | @Selector@ for @wantsExtendedDynamicRangeContent@
wantsExtendedDynamicRangeContentSelector :: Selector
wantsExtendedDynamicRangeContentSelector = mkSelector "wantsExtendedDynamicRangeContent"

-- | @Selector@ for @setWantsExtendedDynamicRangeContent:@
setWantsExtendedDynamicRangeContentSelector :: Selector
setWantsExtendedDynamicRangeContentSelector = mkSelector "setWantsExtendedDynamicRangeContent:"

-- | @Selector@ for @EDRMetadata@
edrMetadataSelector :: Selector
edrMetadataSelector = mkSelector "EDRMetadata"

-- | @Selector@ for @setEDRMetadata:@
setEDRMetadataSelector :: Selector
setEDRMetadataSelector = mkSelector "setEDRMetadata:"

-- | @Selector@ for @displaySyncEnabled@
displaySyncEnabledSelector :: Selector
displaySyncEnabledSelector = mkSelector "displaySyncEnabled"

-- | @Selector@ for @setDisplaySyncEnabled:@
setDisplaySyncEnabledSelector :: Selector
setDisplaySyncEnabledSelector = mkSelector "setDisplaySyncEnabled:"

-- | @Selector@ for @allowsNextDrawableTimeout@
allowsNextDrawableTimeoutSelector :: Selector
allowsNextDrawableTimeoutSelector = mkSelector "allowsNextDrawableTimeout"

-- | @Selector@ for @setAllowsNextDrawableTimeout:@
setAllowsNextDrawableTimeoutSelector :: Selector
setAllowsNextDrawableTimeoutSelector = mkSelector "setAllowsNextDrawableTimeout:"

-- | @Selector@ for @developerHUDProperties@
developerHUDPropertiesSelector :: Selector
developerHUDPropertiesSelector = mkSelector "developerHUDProperties"

-- | @Selector@ for @setDeveloperHUDProperties:@
setDeveloperHUDPropertiesSelector :: Selector
setDeveloperHUDPropertiesSelector = mkSelector "setDeveloperHUDProperties:"

-- | @Selector@ for @residencySet@
residencySetSelector :: Selector
residencySetSelector = mkSelector "residencySet"

