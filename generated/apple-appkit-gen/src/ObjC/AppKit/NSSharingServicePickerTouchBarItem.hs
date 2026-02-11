{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSSharingServicePickerTouchBarItem@.
module ObjC.AppKit.NSSharingServicePickerTouchBarItem
  ( NSSharingServicePickerTouchBarItem
  , IsNSSharingServicePickerTouchBarItem(..)
  , delegate
  , setDelegate
  , enabled
  , setEnabled
  , buttonTitle
  , setButtonTitle
  , buttonImage
  , setButtonImage
  , delegateSelector
  , setDelegateSelector
  , enabledSelector
  , setEnabledSelector
  , buttonTitleSelector
  , setButtonTitleSelector
  , buttonImageSelector
  , setButtonImageSelector


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

-- | @- delegate@
delegate :: IsNSSharingServicePickerTouchBarItem nsSharingServicePickerTouchBarItem => nsSharingServicePickerTouchBarItem -> IO RawId
delegate nsSharingServicePickerTouchBarItem  =
    fmap (RawId . castPtr) $ sendMsg nsSharingServicePickerTouchBarItem (mkSelector "delegate") (retPtr retVoid) []

-- | @- setDelegate:@
setDelegate :: IsNSSharingServicePickerTouchBarItem nsSharingServicePickerTouchBarItem => nsSharingServicePickerTouchBarItem -> RawId -> IO ()
setDelegate nsSharingServicePickerTouchBarItem  value =
    sendMsg nsSharingServicePickerTouchBarItem (mkSelector "setDelegate:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- | @- enabled@
enabled :: IsNSSharingServicePickerTouchBarItem nsSharingServicePickerTouchBarItem => nsSharingServicePickerTouchBarItem -> IO Bool
enabled nsSharingServicePickerTouchBarItem  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsSharingServicePickerTouchBarItem (mkSelector "enabled") retCULong []

-- | @- setEnabled:@
setEnabled :: IsNSSharingServicePickerTouchBarItem nsSharingServicePickerTouchBarItem => nsSharingServicePickerTouchBarItem -> Bool -> IO ()
setEnabled nsSharingServicePickerTouchBarItem  value =
    sendMsg nsSharingServicePickerTouchBarItem (mkSelector "setEnabled:") retVoid [argCULong (if value then 1 else 0)]

-- | @- buttonTitle@
buttonTitle :: IsNSSharingServicePickerTouchBarItem nsSharingServicePickerTouchBarItem => nsSharingServicePickerTouchBarItem -> IO (Id NSString)
buttonTitle nsSharingServicePickerTouchBarItem  =
    sendMsg nsSharingServicePickerTouchBarItem (mkSelector "buttonTitle") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setButtonTitle:@
setButtonTitle :: (IsNSSharingServicePickerTouchBarItem nsSharingServicePickerTouchBarItem, IsNSString value) => nsSharingServicePickerTouchBarItem -> value -> IO ()
setButtonTitle nsSharingServicePickerTouchBarItem  value =
  withObjCPtr value $ \raw_value ->
      sendMsg nsSharingServicePickerTouchBarItem (mkSelector "setButtonTitle:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- buttonImage@
buttonImage :: IsNSSharingServicePickerTouchBarItem nsSharingServicePickerTouchBarItem => nsSharingServicePickerTouchBarItem -> IO (Id NSImage)
buttonImage nsSharingServicePickerTouchBarItem  =
    sendMsg nsSharingServicePickerTouchBarItem (mkSelector "buttonImage") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setButtonImage:@
setButtonImage :: (IsNSSharingServicePickerTouchBarItem nsSharingServicePickerTouchBarItem, IsNSImage value) => nsSharingServicePickerTouchBarItem -> value -> IO ()
setButtonImage nsSharingServicePickerTouchBarItem  value =
  withObjCPtr value $ \raw_value ->
      sendMsg nsSharingServicePickerTouchBarItem (mkSelector "setButtonImage:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @delegate@
delegateSelector :: Selector
delegateSelector = mkSelector "delegate"

-- | @Selector@ for @setDelegate:@
setDelegateSelector :: Selector
setDelegateSelector = mkSelector "setDelegate:"

-- | @Selector@ for @enabled@
enabledSelector :: Selector
enabledSelector = mkSelector "enabled"

-- | @Selector@ for @setEnabled:@
setEnabledSelector :: Selector
setEnabledSelector = mkSelector "setEnabled:"

-- | @Selector@ for @buttonTitle@
buttonTitleSelector :: Selector
buttonTitleSelector = mkSelector "buttonTitle"

-- | @Selector@ for @setButtonTitle:@
setButtonTitleSelector :: Selector
setButtonTitleSelector = mkSelector "setButtonTitle:"

-- | @Selector@ for @buttonImage@
buttonImageSelector :: Selector
buttonImageSelector = mkSelector "buttonImage"

-- | @Selector@ for @setButtonImage:@
setButtonImageSelector :: Selector
setButtonImageSelector = mkSelector "setButtonImage:"

