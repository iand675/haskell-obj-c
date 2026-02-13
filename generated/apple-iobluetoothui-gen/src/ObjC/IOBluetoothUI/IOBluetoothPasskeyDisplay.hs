{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @IOBluetoothPasskeyDisplay@.
module ObjC.IOBluetoothUI.IOBluetoothPasskeyDisplay
  ( IOBluetoothPasskeyDisplay
  , IsIOBluetoothPasskeyDisplay(..)
  , sharedDisplayView
  , setPasskey_forDevice_usingSSP
  , advancePasskeyIndicator
  , retreatPasskeyIndicator
  , resetPasskeyIndicator
  , setupUIForDevice
  , setupUIForSSPDevice
  , setPasskeyString
  , setPasskeyIndicatorEnabled
  , resetAll
  , usePasskeyNotificaitons
  , setUsePasskeyNotificaitons
  , isIncomingRequest
  , setIsIncomingRequest
  , passkey
  , setPasskey
  , returnImage
  , setReturnImage
  , returnHighlightImage
  , setReturnHighlightImage
  , centeredView
  , setCenteredView
  , backgroundImageConstraint
  , setBackgroundImageConstraint
  , advancePasskeyIndicatorSelector
  , backgroundImageConstraintSelector
  , centeredViewSelector
  , isIncomingRequestSelector
  , passkeySelector
  , resetAllSelector
  , resetPasskeyIndicatorSelector
  , retreatPasskeyIndicatorSelector
  , returnHighlightImageSelector
  , returnImageSelector
  , setBackgroundImageConstraintSelector
  , setCenteredViewSelector
  , setIsIncomingRequestSelector
  , setPasskeyIndicatorEnabledSelector
  , setPasskeySelector
  , setPasskeyStringSelector
  , setPasskey_forDevice_usingSSPSelector
  , setReturnHighlightImageSelector
  , setReturnImageSelector
  , setUsePasskeyNotificaitonsSelector
  , setupUIForDeviceSelector
  , setupUIForSSPDeviceSelector
  , sharedDisplayViewSelector
  , usePasskeyNotificaitonsSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.IOBluetoothUI.Internal.Classes
import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes
import ObjC.IOBluetooth.Internal.Classes

-- | @+ sharedDisplayView@
sharedDisplayView :: IO (Id IOBluetoothPasskeyDisplay)
sharedDisplayView  =
  do
    cls' <- getRequiredClass "IOBluetoothPasskeyDisplay"
    sendClassMessage cls' sharedDisplayViewSelector

-- | @- setPasskey:forDevice:usingSSP:@
setPasskey_forDevice_usingSSP :: (IsIOBluetoothPasskeyDisplay ioBluetoothPasskeyDisplay, IsNSString inString, IsIOBluetoothDevice device) => ioBluetoothPasskeyDisplay -> inString -> device -> Bool -> IO ()
setPasskey_forDevice_usingSSP ioBluetoothPasskeyDisplay inString device isSSP =
  sendMessage ioBluetoothPasskeyDisplay setPasskey_forDevice_usingSSPSelector (toNSString inString) (toIOBluetoothDevice device) isSSP

-- | @- advancePasskeyIndicator@
advancePasskeyIndicator :: IsIOBluetoothPasskeyDisplay ioBluetoothPasskeyDisplay => ioBluetoothPasskeyDisplay -> IO ()
advancePasskeyIndicator ioBluetoothPasskeyDisplay =
  sendMessage ioBluetoothPasskeyDisplay advancePasskeyIndicatorSelector

-- | @- retreatPasskeyIndicator@
retreatPasskeyIndicator :: IsIOBluetoothPasskeyDisplay ioBluetoothPasskeyDisplay => ioBluetoothPasskeyDisplay -> IO ()
retreatPasskeyIndicator ioBluetoothPasskeyDisplay =
  sendMessage ioBluetoothPasskeyDisplay retreatPasskeyIndicatorSelector

-- | @- resetPasskeyIndicator@
resetPasskeyIndicator :: IsIOBluetoothPasskeyDisplay ioBluetoothPasskeyDisplay => ioBluetoothPasskeyDisplay -> IO ()
resetPasskeyIndicator ioBluetoothPasskeyDisplay =
  sendMessage ioBluetoothPasskeyDisplay resetPasskeyIndicatorSelector

-- | @- setupUIForDevice:@
setupUIForDevice :: (IsIOBluetoothPasskeyDisplay ioBluetoothPasskeyDisplay, IsIOBluetoothDevice device) => ioBluetoothPasskeyDisplay -> device -> IO ()
setupUIForDevice ioBluetoothPasskeyDisplay device =
  sendMessage ioBluetoothPasskeyDisplay setupUIForDeviceSelector (toIOBluetoothDevice device)

-- | @- setupUIForSSPDevice:@
setupUIForSSPDevice :: (IsIOBluetoothPasskeyDisplay ioBluetoothPasskeyDisplay, IsIOBluetoothDevice device) => ioBluetoothPasskeyDisplay -> device -> IO ()
setupUIForSSPDevice ioBluetoothPasskeyDisplay device =
  sendMessage ioBluetoothPasskeyDisplay setupUIForSSPDeviceSelector (toIOBluetoothDevice device)

-- | @- setPasskeyString:@
setPasskeyString :: (IsIOBluetoothPasskeyDisplay ioBluetoothPasskeyDisplay, IsNSString inString) => ioBluetoothPasskeyDisplay -> inString -> IO ()
setPasskeyString ioBluetoothPasskeyDisplay inString =
  sendMessage ioBluetoothPasskeyDisplay setPasskeyStringSelector (toNSString inString)

-- | @- setPasskeyIndicatorEnabled:@
setPasskeyIndicatorEnabled :: IsIOBluetoothPasskeyDisplay ioBluetoothPasskeyDisplay => ioBluetoothPasskeyDisplay -> Bool -> IO ()
setPasskeyIndicatorEnabled ioBluetoothPasskeyDisplay inEnabled =
  sendMessage ioBluetoothPasskeyDisplay setPasskeyIndicatorEnabledSelector inEnabled

-- | @- resetAll@
resetAll :: IsIOBluetoothPasskeyDisplay ioBluetoothPasskeyDisplay => ioBluetoothPasskeyDisplay -> IO ()
resetAll ioBluetoothPasskeyDisplay =
  sendMessage ioBluetoothPasskeyDisplay resetAllSelector

-- | @- usePasskeyNotificaitons@
usePasskeyNotificaitons :: IsIOBluetoothPasskeyDisplay ioBluetoothPasskeyDisplay => ioBluetoothPasskeyDisplay -> IO Bool
usePasskeyNotificaitons ioBluetoothPasskeyDisplay =
  sendMessage ioBluetoothPasskeyDisplay usePasskeyNotificaitonsSelector

-- | @- setUsePasskeyNotificaitons:@
setUsePasskeyNotificaitons :: IsIOBluetoothPasskeyDisplay ioBluetoothPasskeyDisplay => ioBluetoothPasskeyDisplay -> Bool -> IO ()
setUsePasskeyNotificaitons ioBluetoothPasskeyDisplay value =
  sendMessage ioBluetoothPasskeyDisplay setUsePasskeyNotificaitonsSelector value

-- | @- isIncomingRequest@
isIncomingRequest :: IsIOBluetoothPasskeyDisplay ioBluetoothPasskeyDisplay => ioBluetoothPasskeyDisplay -> IO Bool
isIncomingRequest ioBluetoothPasskeyDisplay =
  sendMessage ioBluetoothPasskeyDisplay isIncomingRequestSelector

-- | @- setIsIncomingRequest:@
setIsIncomingRequest :: IsIOBluetoothPasskeyDisplay ioBluetoothPasskeyDisplay => ioBluetoothPasskeyDisplay -> Bool -> IO ()
setIsIncomingRequest ioBluetoothPasskeyDisplay value =
  sendMessage ioBluetoothPasskeyDisplay setIsIncomingRequestSelector value

-- | @- passkey@
passkey :: IsIOBluetoothPasskeyDisplay ioBluetoothPasskeyDisplay => ioBluetoothPasskeyDisplay -> IO (Id NSString)
passkey ioBluetoothPasskeyDisplay =
  sendMessage ioBluetoothPasskeyDisplay passkeySelector

-- | @- setPasskey:@
setPasskey :: (IsIOBluetoothPasskeyDisplay ioBluetoothPasskeyDisplay, IsNSString value) => ioBluetoothPasskeyDisplay -> value -> IO ()
setPasskey ioBluetoothPasskeyDisplay value =
  sendMessage ioBluetoothPasskeyDisplay setPasskeySelector (toNSString value)

-- | @- returnImage@
returnImage :: IsIOBluetoothPasskeyDisplay ioBluetoothPasskeyDisplay => ioBluetoothPasskeyDisplay -> IO (Id NSImage)
returnImage ioBluetoothPasskeyDisplay =
  sendMessage ioBluetoothPasskeyDisplay returnImageSelector

-- | @- setReturnImage:@
setReturnImage :: (IsIOBluetoothPasskeyDisplay ioBluetoothPasskeyDisplay, IsNSImage value) => ioBluetoothPasskeyDisplay -> value -> IO ()
setReturnImage ioBluetoothPasskeyDisplay value =
  sendMessage ioBluetoothPasskeyDisplay setReturnImageSelector (toNSImage value)

-- | @- returnHighlightImage@
returnHighlightImage :: IsIOBluetoothPasskeyDisplay ioBluetoothPasskeyDisplay => ioBluetoothPasskeyDisplay -> IO (Id NSImage)
returnHighlightImage ioBluetoothPasskeyDisplay =
  sendMessage ioBluetoothPasskeyDisplay returnHighlightImageSelector

-- | @- setReturnHighlightImage:@
setReturnHighlightImage :: (IsIOBluetoothPasskeyDisplay ioBluetoothPasskeyDisplay, IsNSImage value) => ioBluetoothPasskeyDisplay -> value -> IO ()
setReturnHighlightImage ioBluetoothPasskeyDisplay value =
  sendMessage ioBluetoothPasskeyDisplay setReturnHighlightImageSelector (toNSImage value)

-- | @- centeredView@
centeredView :: IsIOBluetoothPasskeyDisplay ioBluetoothPasskeyDisplay => ioBluetoothPasskeyDisplay -> IO (Id NSView)
centeredView ioBluetoothPasskeyDisplay =
  sendMessage ioBluetoothPasskeyDisplay centeredViewSelector

-- | @- setCenteredView:@
setCenteredView :: (IsIOBluetoothPasskeyDisplay ioBluetoothPasskeyDisplay, IsNSView value) => ioBluetoothPasskeyDisplay -> value -> IO ()
setCenteredView ioBluetoothPasskeyDisplay value =
  sendMessage ioBluetoothPasskeyDisplay setCenteredViewSelector (toNSView value)

-- | @- backgroundImageConstraint@
backgroundImageConstraint :: IsIOBluetoothPasskeyDisplay ioBluetoothPasskeyDisplay => ioBluetoothPasskeyDisplay -> IO (Id NSLayoutConstraint)
backgroundImageConstraint ioBluetoothPasskeyDisplay =
  sendMessage ioBluetoothPasskeyDisplay backgroundImageConstraintSelector

-- | @- setBackgroundImageConstraint:@
setBackgroundImageConstraint :: (IsIOBluetoothPasskeyDisplay ioBluetoothPasskeyDisplay, IsNSLayoutConstraint value) => ioBluetoothPasskeyDisplay -> value -> IO ()
setBackgroundImageConstraint ioBluetoothPasskeyDisplay value =
  sendMessage ioBluetoothPasskeyDisplay setBackgroundImageConstraintSelector (toNSLayoutConstraint value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @sharedDisplayView@
sharedDisplayViewSelector :: Selector '[] (Id IOBluetoothPasskeyDisplay)
sharedDisplayViewSelector = mkSelector "sharedDisplayView"

-- | @Selector@ for @setPasskey:forDevice:usingSSP:@
setPasskey_forDevice_usingSSPSelector :: Selector '[Id NSString, Id IOBluetoothDevice, Bool] ()
setPasskey_forDevice_usingSSPSelector = mkSelector "setPasskey:forDevice:usingSSP:"

-- | @Selector@ for @advancePasskeyIndicator@
advancePasskeyIndicatorSelector :: Selector '[] ()
advancePasskeyIndicatorSelector = mkSelector "advancePasskeyIndicator"

-- | @Selector@ for @retreatPasskeyIndicator@
retreatPasskeyIndicatorSelector :: Selector '[] ()
retreatPasskeyIndicatorSelector = mkSelector "retreatPasskeyIndicator"

-- | @Selector@ for @resetPasskeyIndicator@
resetPasskeyIndicatorSelector :: Selector '[] ()
resetPasskeyIndicatorSelector = mkSelector "resetPasskeyIndicator"

-- | @Selector@ for @setupUIForDevice:@
setupUIForDeviceSelector :: Selector '[Id IOBluetoothDevice] ()
setupUIForDeviceSelector = mkSelector "setupUIForDevice:"

-- | @Selector@ for @setupUIForSSPDevice:@
setupUIForSSPDeviceSelector :: Selector '[Id IOBluetoothDevice] ()
setupUIForSSPDeviceSelector = mkSelector "setupUIForSSPDevice:"

-- | @Selector@ for @setPasskeyString:@
setPasskeyStringSelector :: Selector '[Id NSString] ()
setPasskeyStringSelector = mkSelector "setPasskeyString:"

-- | @Selector@ for @setPasskeyIndicatorEnabled:@
setPasskeyIndicatorEnabledSelector :: Selector '[Bool] ()
setPasskeyIndicatorEnabledSelector = mkSelector "setPasskeyIndicatorEnabled:"

-- | @Selector@ for @resetAll@
resetAllSelector :: Selector '[] ()
resetAllSelector = mkSelector "resetAll"

-- | @Selector@ for @usePasskeyNotificaitons@
usePasskeyNotificaitonsSelector :: Selector '[] Bool
usePasskeyNotificaitonsSelector = mkSelector "usePasskeyNotificaitons"

-- | @Selector@ for @setUsePasskeyNotificaitons:@
setUsePasskeyNotificaitonsSelector :: Selector '[Bool] ()
setUsePasskeyNotificaitonsSelector = mkSelector "setUsePasskeyNotificaitons:"

-- | @Selector@ for @isIncomingRequest@
isIncomingRequestSelector :: Selector '[] Bool
isIncomingRequestSelector = mkSelector "isIncomingRequest"

-- | @Selector@ for @setIsIncomingRequest:@
setIsIncomingRequestSelector :: Selector '[Bool] ()
setIsIncomingRequestSelector = mkSelector "setIsIncomingRequest:"

-- | @Selector@ for @passkey@
passkeySelector :: Selector '[] (Id NSString)
passkeySelector = mkSelector "passkey"

-- | @Selector@ for @setPasskey:@
setPasskeySelector :: Selector '[Id NSString] ()
setPasskeySelector = mkSelector "setPasskey:"

-- | @Selector@ for @returnImage@
returnImageSelector :: Selector '[] (Id NSImage)
returnImageSelector = mkSelector "returnImage"

-- | @Selector@ for @setReturnImage:@
setReturnImageSelector :: Selector '[Id NSImage] ()
setReturnImageSelector = mkSelector "setReturnImage:"

-- | @Selector@ for @returnHighlightImage@
returnHighlightImageSelector :: Selector '[] (Id NSImage)
returnHighlightImageSelector = mkSelector "returnHighlightImage"

-- | @Selector@ for @setReturnHighlightImage:@
setReturnHighlightImageSelector :: Selector '[Id NSImage] ()
setReturnHighlightImageSelector = mkSelector "setReturnHighlightImage:"

-- | @Selector@ for @centeredView@
centeredViewSelector :: Selector '[] (Id NSView)
centeredViewSelector = mkSelector "centeredView"

-- | @Selector@ for @setCenteredView:@
setCenteredViewSelector :: Selector '[Id NSView] ()
setCenteredViewSelector = mkSelector "setCenteredView:"

-- | @Selector@ for @backgroundImageConstraint@
backgroundImageConstraintSelector :: Selector '[] (Id NSLayoutConstraint)
backgroundImageConstraintSelector = mkSelector "backgroundImageConstraint"

-- | @Selector@ for @setBackgroundImageConstraint:@
setBackgroundImageConstraintSelector :: Selector '[Id NSLayoutConstraint] ()
setBackgroundImageConstraintSelector = mkSelector "setBackgroundImageConstraint:"

