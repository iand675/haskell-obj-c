{-# LANGUAGE DataKinds #-}
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
  , allowsNextDrawableTimeoutSelector
  , colorspaceSelector
  , developerHUDPropertiesSelector
  , deviceSelector
  , displaySyncEnabledSelector
  , edrMetadataSelector
  , framebufferOnlySelector
  , maximumDrawableCountSelector
  , nextDrawableSelector
  , pixelFormatSelector
  , preferredDeviceSelector
  , presentsWithTransactionSelector
  , residencySetSelector
  , setAllowsNextDrawableTimeoutSelector
  , setColorspaceSelector
  , setDeveloperHUDPropertiesSelector
  , setDeviceSelector
  , setDisplaySyncEnabledSelector
  , setEDRMetadataSelector
  , setFramebufferOnlySelector
  , setMaximumDrawableCountSelector
  , setPixelFormatSelector
  , setPresentsWithTransactionSelector
  , setWantsExtendedDynamicRangeContentSelector
  , wantsExtendedDynamicRangeContentSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.QuartzCore.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- nextDrawable@
nextDrawable :: IsCAMetalLayer caMetalLayer => caMetalLayer -> IO RawId
nextDrawable caMetalLayer =
  sendMessage caMetalLayer nextDrawableSelector

-- | @- device@
device :: IsCAMetalLayer caMetalLayer => caMetalLayer -> IO RawId
device caMetalLayer =
  sendMessage caMetalLayer deviceSelector

-- | @- setDevice:@
setDevice :: IsCAMetalLayer caMetalLayer => caMetalLayer -> RawId -> IO ()
setDevice caMetalLayer value =
  sendMessage caMetalLayer setDeviceSelector value

-- | @- preferredDevice@
preferredDevice :: IsCAMetalLayer caMetalLayer => caMetalLayer -> IO RawId
preferredDevice caMetalLayer =
  sendMessage caMetalLayer preferredDeviceSelector

-- | @- pixelFormat@
pixelFormat :: IsCAMetalLayer caMetalLayer => caMetalLayer -> IO CInt
pixelFormat caMetalLayer =
  sendMessage caMetalLayer pixelFormatSelector

-- | @- setPixelFormat:@
setPixelFormat :: IsCAMetalLayer caMetalLayer => caMetalLayer -> CInt -> IO ()
setPixelFormat caMetalLayer value =
  sendMessage caMetalLayer setPixelFormatSelector value

-- | @- framebufferOnly@
framebufferOnly :: IsCAMetalLayer caMetalLayer => caMetalLayer -> IO Bool
framebufferOnly caMetalLayer =
  sendMessage caMetalLayer framebufferOnlySelector

-- | @- setFramebufferOnly:@
setFramebufferOnly :: IsCAMetalLayer caMetalLayer => caMetalLayer -> Bool -> IO ()
setFramebufferOnly caMetalLayer value =
  sendMessage caMetalLayer setFramebufferOnlySelector value

-- | @- maximumDrawableCount@
maximumDrawableCount :: IsCAMetalLayer caMetalLayer => caMetalLayer -> IO CULong
maximumDrawableCount caMetalLayer =
  sendMessage caMetalLayer maximumDrawableCountSelector

-- | @- setMaximumDrawableCount:@
setMaximumDrawableCount :: IsCAMetalLayer caMetalLayer => caMetalLayer -> CULong -> IO ()
setMaximumDrawableCount caMetalLayer value =
  sendMessage caMetalLayer setMaximumDrawableCountSelector value

-- | @- presentsWithTransaction@
presentsWithTransaction :: IsCAMetalLayer caMetalLayer => caMetalLayer -> IO Bool
presentsWithTransaction caMetalLayer =
  sendMessage caMetalLayer presentsWithTransactionSelector

-- | @- setPresentsWithTransaction:@
setPresentsWithTransaction :: IsCAMetalLayer caMetalLayer => caMetalLayer -> Bool -> IO ()
setPresentsWithTransaction caMetalLayer value =
  sendMessage caMetalLayer setPresentsWithTransactionSelector value

-- | @- colorspace@
colorspace :: IsCAMetalLayer caMetalLayer => caMetalLayer -> IO (Ptr ())
colorspace caMetalLayer =
  sendMessage caMetalLayer colorspaceSelector

-- | @- setColorspace:@
setColorspace :: IsCAMetalLayer caMetalLayer => caMetalLayer -> Ptr () -> IO ()
setColorspace caMetalLayer value =
  sendMessage caMetalLayer setColorspaceSelector value

-- | @- wantsExtendedDynamicRangeContent@
wantsExtendedDynamicRangeContent :: IsCAMetalLayer caMetalLayer => caMetalLayer -> IO Bool
wantsExtendedDynamicRangeContent caMetalLayer =
  sendMessage caMetalLayer wantsExtendedDynamicRangeContentSelector

-- | @- setWantsExtendedDynamicRangeContent:@
setWantsExtendedDynamicRangeContent :: IsCAMetalLayer caMetalLayer => caMetalLayer -> Bool -> IO ()
setWantsExtendedDynamicRangeContent caMetalLayer value =
  sendMessage caMetalLayer setWantsExtendedDynamicRangeContentSelector value

-- | @- EDRMetadata@
edrMetadata :: IsCAMetalLayer caMetalLayer => caMetalLayer -> IO (Id CAEDRMetadata)
edrMetadata caMetalLayer =
  sendMessage caMetalLayer edrMetadataSelector

-- | @- setEDRMetadata:@
setEDRMetadata :: (IsCAMetalLayer caMetalLayer, IsCAEDRMetadata value) => caMetalLayer -> value -> IO ()
setEDRMetadata caMetalLayer value =
  sendMessage caMetalLayer setEDRMetadataSelector (toCAEDRMetadata value)

-- | @- displaySyncEnabled@
displaySyncEnabled :: IsCAMetalLayer caMetalLayer => caMetalLayer -> IO Bool
displaySyncEnabled caMetalLayer =
  sendMessage caMetalLayer displaySyncEnabledSelector

-- | @- setDisplaySyncEnabled:@
setDisplaySyncEnabled :: IsCAMetalLayer caMetalLayer => caMetalLayer -> Bool -> IO ()
setDisplaySyncEnabled caMetalLayer value =
  sendMessage caMetalLayer setDisplaySyncEnabledSelector value

-- | @- allowsNextDrawableTimeout@
allowsNextDrawableTimeout :: IsCAMetalLayer caMetalLayer => caMetalLayer -> IO Bool
allowsNextDrawableTimeout caMetalLayer =
  sendMessage caMetalLayer allowsNextDrawableTimeoutSelector

-- | @- setAllowsNextDrawableTimeout:@
setAllowsNextDrawableTimeout :: IsCAMetalLayer caMetalLayer => caMetalLayer -> Bool -> IO ()
setAllowsNextDrawableTimeout caMetalLayer value =
  sendMessage caMetalLayer setAllowsNextDrawableTimeoutSelector value

-- | @- developerHUDProperties@
developerHUDProperties :: IsCAMetalLayer caMetalLayer => caMetalLayer -> IO (Id NSDictionary)
developerHUDProperties caMetalLayer =
  sendMessage caMetalLayer developerHUDPropertiesSelector

-- | @- setDeveloperHUDProperties:@
setDeveloperHUDProperties :: (IsCAMetalLayer caMetalLayer, IsNSDictionary value) => caMetalLayer -> value -> IO ()
setDeveloperHUDProperties caMetalLayer value =
  sendMessage caMetalLayer setDeveloperHUDPropertiesSelector (toNSDictionary value)

-- | @- residencySet@
residencySet :: IsCAMetalLayer caMetalLayer => caMetalLayer -> IO RawId
residencySet caMetalLayer =
  sendMessage caMetalLayer residencySetSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @nextDrawable@
nextDrawableSelector :: Selector '[] RawId
nextDrawableSelector = mkSelector "nextDrawable"

-- | @Selector@ for @device@
deviceSelector :: Selector '[] RawId
deviceSelector = mkSelector "device"

-- | @Selector@ for @setDevice:@
setDeviceSelector :: Selector '[RawId] ()
setDeviceSelector = mkSelector "setDevice:"

-- | @Selector@ for @preferredDevice@
preferredDeviceSelector :: Selector '[] RawId
preferredDeviceSelector = mkSelector "preferredDevice"

-- | @Selector@ for @pixelFormat@
pixelFormatSelector :: Selector '[] CInt
pixelFormatSelector = mkSelector "pixelFormat"

-- | @Selector@ for @setPixelFormat:@
setPixelFormatSelector :: Selector '[CInt] ()
setPixelFormatSelector = mkSelector "setPixelFormat:"

-- | @Selector@ for @framebufferOnly@
framebufferOnlySelector :: Selector '[] Bool
framebufferOnlySelector = mkSelector "framebufferOnly"

-- | @Selector@ for @setFramebufferOnly:@
setFramebufferOnlySelector :: Selector '[Bool] ()
setFramebufferOnlySelector = mkSelector "setFramebufferOnly:"

-- | @Selector@ for @maximumDrawableCount@
maximumDrawableCountSelector :: Selector '[] CULong
maximumDrawableCountSelector = mkSelector "maximumDrawableCount"

-- | @Selector@ for @setMaximumDrawableCount:@
setMaximumDrawableCountSelector :: Selector '[CULong] ()
setMaximumDrawableCountSelector = mkSelector "setMaximumDrawableCount:"

-- | @Selector@ for @presentsWithTransaction@
presentsWithTransactionSelector :: Selector '[] Bool
presentsWithTransactionSelector = mkSelector "presentsWithTransaction"

-- | @Selector@ for @setPresentsWithTransaction:@
setPresentsWithTransactionSelector :: Selector '[Bool] ()
setPresentsWithTransactionSelector = mkSelector "setPresentsWithTransaction:"

-- | @Selector@ for @colorspace@
colorspaceSelector :: Selector '[] (Ptr ())
colorspaceSelector = mkSelector "colorspace"

-- | @Selector@ for @setColorspace:@
setColorspaceSelector :: Selector '[Ptr ()] ()
setColorspaceSelector = mkSelector "setColorspace:"

-- | @Selector@ for @wantsExtendedDynamicRangeContent@
wantsExtendedDynamicRangeContentSelector :: Selector '[] Bool
wantsExtendedDynamicRangeContentSelector = mkSelector "wantsExtendedDynamicRangeContent"

-- | @Selector@ for @setWantsExtendedDynamicRangeContent:@
setWantsExtendedDynamicRangeContentSelector :: Selector '[Bool] ()
setWantsExtendedDynamicRangeContentSelector = mkSelector "setWantsExtendedDynamicRangeContent:"

-- | @Selector@ for @EDRMetadata@
edrMetadataSelector :: Selector '[] (Id CAEDRMetadata)
edrMetadataSelector = mkSelector "EDRMetadata"

-- | @Selector@ for @setEDRMetadata:@
setEDRMetadataSelector :: Selector '[Id CAEDRMetadata] ()
setEDRMetadataSelector = mkSelector "setEDRMetadata:"

-- | @Selector@ for @displaySyncEnabled@
displaySyncEnabledSelector :: Selector '[] Bool
displaySyncEnabledSelector = mkSelector "displaySyncEnabled"

-- | @Selector@ for @setDisplaySyncEnabled:@
setDisplaySyncEnabledSelector :: Selector '[Bool] ()
setDisplaySyncEnabledSelector = mkSelector "setDisplaySyncEnabled:"

-- | @Selector@ for @allowsNextDrawableTimeout@
allowsNextDrawableTimeoutSelector :: Selector '[] Bool
allowsNextDrawableTimeoutSelector = mkSelector "allowsNextDrawableTimeout"

-- | @Selector@ for @setAllowsNextDrawableTimeout:@
setAllowsNextDrawableTimeoutSelector :: Selector '[Bool] ()
setAllowsNextDrawableTimeoutSelector = mkSelector "setAllowsNextDrawableTimeout:"

-- | @Selector@ for @developerHUDProperties@
developerHUDPropertiesSelector :: Selector '[] (Id NSDictionary)
developerHUDPropertiesSelector = mkSelector "developerHUDProperties"

-- | @Selector@ for @setDeveloperHUDProperties:@
setDeveloperHUDPropertiesSelector :: Selector '[Id NSDictionary] ()
setDeveloperHUDPropertiesSelector = mkSelector "setDeveloperHUDProperties:"

-- | @Selector@ for @residencySet@
residencySetSelector :: Selector '[] RawId
residencySetSelector = mkSelector "residencySet"

