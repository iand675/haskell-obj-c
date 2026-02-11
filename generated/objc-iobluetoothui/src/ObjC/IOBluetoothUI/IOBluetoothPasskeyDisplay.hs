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
  , sharedDisplayViewSelector
  , setPasskey_forDevice_usingSSPSelector
  , advancePasskeyIndicatorSelector
  , retreatPasskeyIndicatorSelector
  , resetPasskeyIndicatorSelector
  , setupUIForDeviceSelector
  , setupUIForSSPDeviceSelector
  , setPasskeyStringSelector
  , setPasskeyIndicatorEnabledSelector
  , resetAllSelector
  , usePasskeyNotificaitonsSelector
  , setUsePasskeyNotificaitonsSelector
  , isIncomingRequestSelector
  , setIsIncomingRequestSelector
  , passkeySelector
  , setPasskeySelector
  , returnImageSelector
  , setReturnImageSelector
  , returnHighlightImageSelector
  , setReturnHighlightImageSelector
  , centeredViewSelector
  , setCenteredViewSelector
  , backgroundImageConstraintSelector
  , setBackgroundImageConstraintSelector


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

import ObjC.IOBluetoothUI.Internal.Classes
import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes
import ObjC.IOBluetooth.Internal.Classes

-- | @+ sharedDisplayView@
sharedDisplayView :: IO (Id IOBluetoothPasskeyDisplay)
sharedDisplayView  =
  do
    cls' <- getRequiredClass "IOBluetoothPasskeyDisplay"
    sendClassMsg cls' (mkSelector "sharedDisplayView") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setPasskey:forDevice:usingSSP:@
setPasskey_forDevice_usingSSP :: (IsIOBluetoothPasskeyDisplay ioBluetoothPasskeyDisplay, IsNSString inString, IsIOBluetoothDevice device) => ioBluetoothPasskeyDisplay -> inString -> device -> Bool -> IO ()
setPasskey_forDevice_usingSSP ioBluetoothPasskeyDisplay  inString device isSSP =
withObjCPtr inString $ \raw_inString ->
  withObjCPtr device $ \raw_device ->
      sendMsg ioBluetoothPasskeyDisplay (mkSelector "setPasskey:forDevice:usingSSP:") retVoid [argPtr (castPtr raw_inString :: Ptr ()), argPtr (castPtr raw_device :: Ptr ()), argCULong (if isSSP then 1 else 0)]

-- | @- advancePasskeyIndicator@
advancePasskeyIndicator :: IsIOBluetoothPasskeyDisplay ioBluetoothPasskeyDisplay => ioBluetoothPasskeyDisplay -> IO ()
advancePasskeyIndicator ioBluetoothPasskeyDisplay  =
  sendMsg ioBluetoothPasskeyDisplay (mkSelector "advancePasskeyIndicator") retVoid []

-- | @- retreatPasskeyIndicator@
retreatPasskeyIndicator :: IsIOBluetoothPasskeyDisplay ioBluetoothPasskeyDisplay => ioBluetoothPasskeyDisplay -> IO ()
retreatPasskeyIndicator ioBluetoothPasskeyDisplay  =
  sendMsg ioBluetoothPasskeyDisplay (mkSelector "retreatPasskeyIndicator") retVoid []

-- | @- resetPasskeyIndicator@
resetPasskeyIndicator :: IsIOBluetoothPasskeyDisplay ioBluetoothPasskeyDisplay => ioBluetoothPasskeyDisplay -> IO ()
resetPasskeyIndicator ioBluetoothPasskeyDisplay  =
  sendMsg ioBluetoothPasskeyDisplay (mkSelector "resetPasskeyIndicator") retVoid []

-- | @- setupUIForDevice:@
setupUIForDevice :: (IsIOBluetoothPasskeyDisplay ioBluetoothPasskeyDisplay, IsIOBluetoothDevice device) => ioBluetoothPasskeyDisplay -> device -> IO ()
setupUIForDevice ioBluetoothPasskeyDisplay  device =
withObjCPtr device $ \raw_device ->
    sendMsg ioBluetoothPasskeyDisplay (mkSelector "setupUIForDevice:") retVoid [argPtr (castPtr raw_device :: Ptr ())]

-- | @- setupUIForSSPDevice:@
setupUIForSSPDevice :: (IsIOBluetoothPasskeyDisplay ioBluetoothPasskeyDisplay, IsIOBluetoothDevice device) => ioBluetoothPasskeyDisplay -> device -> IO ()
setupUIForSSPDevice ioBluetoothPasskeyDisplay  device =
withObjCPtr device $ \raw_device ->
    sendMsg ioBluetoothPasskeyDisplay (mkSelector "setupUIForSSPDevice:") retVoid [argPtr (castPtr raw_device :: Ptr ())]

-- | @- setPasskeyString:@
setPasskeyString :: (IsIOBluetoothPasskeyDisplay ioBluetoothPasskeyDisplay, IsNSString inString) => ioBluetoothPasskeyDisplay -> inString -> IO ()
setPasskeyString ioBluetoothPasskeyDisplay  inString =
withObjCPtr inString $ \raw_inString ->
    sendMsg ioBluetoothPasskeyDisplay (mkSelector "setPasskeyString:") retVoid [argPtr (castPtr raw_inString :: Ptr ())]

-- | @- setPasskeyIndicatorEnabled:@
setPasskeyIndicatorEnabled :: IsIOBluetoothPasskeyDisplay ioBluetoothPasskeyDisplay => ioBluetoothPasskeyDisplay -> Bool -> IO ()
setPasskeyIndicatorEnabled ioBluetoothPasskeyDisplay  inEnabled =
  sendMsg ioBluetoothPasskeyDisplay (mkSelector "setPasskeyIndicatorEnabled:") retVoid [argCULong (if inEnabled then 1 else 0)]

-- | @- resetAll@
resetAll :: IsIOBluetoothPasskeyDisplay ioBluetoothPasskeyDisplay => ioBluetoothPasskeyDisplay -> IO ()
resetAll ioBluetoothPasskeyDisplay  =
  sendMsg ioBluetoothPasskeyDisplay (mkSelector "resetAll") retVoid []

-- | @- usePasskeyNotificaitons@
usePasskeyNotificaitons :: IsIOBluetoothPasskeyDisplay ioBluetoothPasskeyDisplay => ioBluetoothPasskeyDisplay -> IO Bool
usePasskeyNotificaitons ioBluetoothPasskeyDisplay  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg ioBluetoothPasskeyDisplay (mkSelector "usePasskeyNotificaitons") retCULong []

-- | @- setUsePasskeyNotificaitons:@
setUsePasskeyNotificaitons :: IsIOBluetoothPasskeyDisplay ioBluetoothPasskeyDisplay => ioBluetoothPasskeyDisplay -> Bool -> IO ()
setUsePasskeyNotificaitons ioBluetoothPasskeyDisplay  value =
  sendMsg ioBluetoothPasskeyDisplay (mkSelector "setUsePasskeyNotificaitons:") retVoid [argCULong (if value then 1 else 0)]

-- | @- isIncomingRequest@
isIncomingRequest :: IsIOBluetoothPasskeyDisplay ioBluetoothPasskeyDisplay => ioBluetoothPasskeyDisplay -> IO Bool
isIncomingRequest ioBluetoothPasskeyDisplay  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg ioBluetoothPasskeyDisplay (mkSelector "isIncomingRequest") retCULong []

-- | @- setIsIncomingRequest:@
setIsIncomingRequest :: IsIOBluetoothPasskeyDisplay ioBluetoothPasskeyDisplay => ioBluetoothPasskeyDisplay -> Bool -> IO ()
setIsIncomingRequest ioBluetoothPasskeyDisplay  value =
  sendMsg ioBluetoothPasskeyDisplay (mkSelector "setIsIncomingRequest:") retVoid [argCULong (if value then 1 else 0)]

-- | @- passkey@
passkey :: IsIOBluetoothPasskeyDisplay ioBluetoothPasskeyDisplay => ioBluetoothPasskeyDisplay -> IO (Id NSString)
passkey ioBluetoothPasskeyDisplay  =
  sendMsg ioBluetoothPasskeyDisplay (mkSelector "passkey") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setPasskey:@
setPasskey :: (IsIOBluetoothPasskeyDisplay ioBluetoothPasskeyDisplay, IsNSString value) => ioBluetoothPasskeyDisplay -> value -> IO ()
setPasskey ioBluetoothPasskeyDisplay  value =
withObjCPtr value $ \raw_value ->
    sendMsg ioBluetoothPasskeyDisplay (mkSelector "setPasskey:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- returnImage@
returnImage :: IsIOBluetoothPasskeyDisplay ioBluetoothPasskeyDisplay => ioBluetoothPasskeyDisplay -> IO (Id NSImage)
returnImage ioBluetoothPasskeyDisplay  =
  sendMsg ioBluetoothPasskeyDisplay (mkSelector "returnImage") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setReturnImage:@
setReturnImage :: (IsIOBluetoothPasskeyDisplay ioBluetoothPasskeyDisplay, IsNSImage value) => ioBluetoothPasskeyDisplay -> value -> IO ()
setReturnImage ioBluetoothPasskeyDisplay  value =
withObjCPtr value $ \raw_value ->
    sendMsg ioBluetoothPasskeyDisplay (mkSelector "setReturnImage:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- returnHighlightImage@
returnHighlightImage :: IsIOBluetoothPasskeyDisplay ioBluetoothPasskeyDisplay => ioBluetoothPasskeyDisplay -> IO (Id NSImage)
returnHighlightImage ioBluetoothPasskeyDisplay  =
  sendMsg ioBluetoothPasskeyDisplay (mkSelector "returnHighlightImage") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setReturnHighlightImage:@
setReturnHighlightImage :: (IsIOBluetoothPasskeyDisplay ioBluetoothPasskeyDisplay, IsNSImage value) => ioBluetoothPasskeyDisplay -> value -> IO ()
setReturnHighlightImage ioBluetoothPasskeyDisplay  value =
withObjCPtr value $ \raw_value ->
    sendMsg ioBluetoothPasskeyDisplay (mkSelector "setReturnHighlightImage:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- centeredView@
centeredView :: IsIOBluetoothPasskeyDisplay ioBluetoothPasskeyDisplay => ioBluetoothPasskeyDisplay -> IO (Id NSView)
centeredView ioBluetoothPasskeyDisplay  =
  sendMsg ioBluetoothPasskeyDisplay (mkSelector "centeredView") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setCenteredView:@
setCenteredView :: (IsIOBluetoothPasskeyDisplay ioBluetoothPasskeyDisplay, IsNSView value) => ioBluetoothPasskeyDisplay -> value -> IO ()
setCenteredView ioBluetoothPasskeyDisplay  value =
withObjCPtr value $ \raw_value ->
    sendMsg ioBluetoothPasskeyDisplay (mkSelector "setCenteredView:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- backgroundImageConstraint@
backgroundImageConstraint :: IsIOBluetoothPasskeyDisplay ioBluetoothPasskeyDisplay => ioBluetoothPasskeyDisplay -> IO (Id NSLayoutConstraint)
backgroundImageConstraint ioBluetoothPasskeyDisplay  =
  sendMsg ioBluetoothPasskeyDisplay (mkSelector "backgroundImageConstraint") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setBackgroundImageConstraint:@
setBackgroundImageConstraint :: (IsIOBluetoothPasskeyDisplay ioBluetoothPasskeyDisplay, IsNSLayoutConstraint value) => ioBluetoothPasskeyDisplay -> value -> IO ()
setBackgroundImageConstraint ioBluetoothPasskeyDisplay  value =
withObjCPtr value $ \raw_value ->
    sendMsg ioBluetoothPasskeyDisplay (mkSelector "setBackgroundImageConstraint:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @sharedDisplayView@
sharedDisplayViewSelector :: Selector
sharedDisplayViewSelector = mkSelector "sharedDisplayView"

-- | @Selector@ for @setPasskey:forDevice:usingSSP:@
setPasskey_forDevice_usingSSPSelector :: Selector
setPasskey_forDevice_usingSSPSelector = mkSelector "setPasskey:forDevice:usingSSP:"

-- | @Selector@ for @advancePasskeyIndicator@
advancePasskeyIndicatorSelector :: Selector
advancePasskeyIndicatorSelector = mkSelector "advancePasskeyIndicator"

-- | @Selector@ for @retreatPasskeyIndicator@
retreatPasskeyIndicatorSelector :: Selector
retreatPasskeyIndicatorSelector = mkSelector "retreatPasskeyIndicator"

-- | @Selector@ for @resetPasskeyIndicator@
resetPasskeyIndicatorSelector :: Selector
resetPasskeyIndicatorSelector = mkSelector "resetPasskeyIndicator"

-- | @Selector@ for @setupUIForDevice:@
setupUIForDeviceSelector :: Selector
setupUIForDeviceSelector = mkSelector "setupUIForDevice:"

-- | @Selector@ for @setupUIForSSPDevice:@
setupUIForSSPDeviceSelector :: Selector
setupUIForSSPDeviceSelector = mkSelector "setupUIForSSPDevice:"

-- | @Selector@ for @setPasskeyString:@
setPasskeyStringSelector :: Selector
setPasskeyStringSelector = mkSelector "setPasskeyString:"

-- | @Selector@ for @setPasskeyIndicatorEnabled:@
setPasskeyIndicatorEnabledSelector :: Selector
setPasskeyIndicatorEnabledSelector = mkSelector "setPasskeyIndicatorEnabled:"

-- | @Selector@ for @resetAll@
resetAllSelector :: Selector
resetAllSelector = mkSelector "resetAll"

-- | @Selector@ for @usePasskeyNotificaitons@
usePasskeyNotificaitonsSelector :: Selector
usePasskeyNotificaitonsSelector = mkSelector "usePasskeyNotificaitons"

-- | @Selector@ for @setUsePasskeyNotificaitons:@
setUsePasskeyNotificaitonsSelector :: Selector
setUsePasskeyNotificaitonsSelector = mkSelector "setUsePasskeyNotificaitons:"

-- | @Selector@ for @isIncomingRequest@
isIncomingRequestSelector :: Selector
isIncomingRequestSelector = mkSelector "isIncomingRequest"

-- | @Selector@ for @setIsIncomingRequest:@
setIsIncomingRequestSelector :: Selector
setIsIncomingRequestSelector = mkSelector "setIsIncomingRequest:"

-- | @Selector@ for @passkey@
passkeySelector :: Selector
passkeySelector = mkSelector "passkey"

-- | @Selector@ for @setPasskey:@
setPasskeySelector :: Selector
setPasskeySelector = mkSelector "setPasskey:"

-- | @Selector@ for @returnImage@
returnImageSelector :: Selector
returnImageSelector = mkSelector "returnImage"

-- | @Selector@ for @setReturnImage:@
setReturnImageSelector :: Selector
setReturnImageSelector = mkSelector "setReturnImage:"

-- | @Selector@ for @returnHighlightImage@
returnHighlightImageSelector :: Selector
returnHighlightImageSelector = mkSelector "returnHighlightImage"

-- | @Selector@ for @setReturnHighlightImage:@
setReturnHighlightImageSelector :: Selector
setReturnHighlightImageSelector = mkSelector "setReturnHighlightImage:"

-- | @Selector@ for @centeredView@
centeredViewSelector :: Selector
centeredViewSelector = mkSelector "centeredView"

-- | @Selector@ for @setCenteredView:@
setCenteredViewSelector :: Selector
setCenteredViewSelector = mkSelector "setCenteredView:"

-- | @Selector@ for @backgroundImageConstraint@
backgroundImageConstraintSelector :: Selector
backgroundImageConstraintSelector = mkSelector "backgroundImageConstraint"

-- | @Selector@ for @setBackgroundImageConstraint:@
setBackgroundImageConstraintSelector :: Selector
setBackgroundImageConstraintSelector = mkSelector "setBackgroundImageConstraint:"

