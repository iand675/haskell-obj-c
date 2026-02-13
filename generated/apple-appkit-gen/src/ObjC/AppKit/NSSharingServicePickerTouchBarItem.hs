{-# LANGUAGE DataKinds #-}
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
  , buttonImageSelector
  , buttonTitleSelector
  , delegateSelector
  , enabledSelector
  , setButtonImageSelector
  , setButtonTitleSelector
  , setDelegateSelector
  , setEnabledSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- delegate@
delegate :: IsNSSharingServicePickerTouchBarItem nsSharingServicePickerTouchBarItem => nsSharingServicePickerTouchBarItem -> IO RawId
delegate nsSharingServicePickerTouchBarItem =
  sendMessage nsSharingServicePickerTouchBarItem delegateSelector

-- | @- setDelegate:@
setDelegate :: IsNSSharingServicePickerTouchBarItem nsSharingServicePickerTouchBarItem => nsSharingServicePickerTouchBarItem -> RawId -> IO ()
setDelegate nsSharingServicePickerTouchBarItem value =
  sendMessage nsSharingServicePickerTouchBarItem setDelegateSelector value

-- | @- enabled@
enabled :: IsNSSharingServicePickerTouchBarItem nsSharingServicePickerTouchBarItem => nsSharingServicePickerTouchBarItem -> IO Bool
enabled nsSharingServicePickerTouchBarItem =
  sendMessage nsSharingServicePickerTouchBarItem enabledSelector

-- | @- setEnabled:@
setEnabled :: IsNSSharingServicePickerTouchBarItem nsSharingServicePickerTouchBarItem => nsSharingServicePickerTouchBarItem -> Bool -> IO ()
setEnabled nsSharingServicePickerTouchBarItem value =
  sendMessage nsSharingServicePickerTouchBarItem setEnabledSelector value

-- | @- buttonTitle@
buttonTitle :: IsNSSharingServicePickerTouchBarItem nsSharingServicePickerTouchBarItem => nsSharingServicePickerTouchBarItem -> IO (Id NSString)
buttonTitle nsSharingServicePickerTouchBarItem =
  sendMessage nsSharingServicePickerTouchBarItem buttonTitleSelector

-- | @- setButtonTitle:@
setButtonTitle :: (IsNSSharingServicePickerTouchBarItem nsSharingServicePickerTouchBarItem, IsNSString value) => nsSharingServicePickerTouchBarItem -> value -> IO ()
setButtonTitle nsSharingServicePickerTouchBarItem value =
  sendMessage nsSharingServicePickerTouchBarItem setButtonTitleSelector (toNSString value)

-- | @- buttonImage@
buttonImage :: IsNSSharingServicePickerTouchBarItem nsSharingServicePickerTouchBarItem => nsSharingServicePickerTouchBarItem -> IO (Id NSImage)
buttonImage nsSharingServicePickerTouchBarItem =
  sendMessage nsSharingServicePickerTouchBarItem buttonImageSelector

-- | @- setButtonImage:@
setButtonImage :: (IsNSSharingServicePickerTouchBarItem nsSharingServicePickerTouchBarItem, IsNSImage value) => nsSharingServicePickerTouchBarItem -> value -> IO ()
setButtonImage nsSharingServicePickerTouchBarItem value =
  sendMessage nsSharingServicePickerTouchBarItem setButtonImageSelector (toNSImage value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @delegate@
delegateSelector :: Selector '[] RawId
delegateSelector = mkSelector "delegate"

-- | @Selector@ for @setDelegate:@
setDelegateSelector :: Selector '[RawId] ()
setDelegateSelector = mkSelector "setDelegate:"

-- | @Selector@ for @enabled@
enabledSelector :: Selector '[] Bool
enabledSelector = mkSelector "enabled"

-- | @Selector@ for @setEnabled:@
setEnabledSelector :: Selector '[Bool] ()
setEnabledSelector = mkSelector "setEnabled:"

-- | @Selector@ for @buttonTitle@
buttonTitleSelector :: Selector '[] (Id NSString)
buttonTitleSelector = mkSelector "buttonTitle"

-- | @Selector@ for @setButtonTitle:@
setButtonTitleSelector :: Selector '[Id NSString] ()
setButtonTitleSelector = mkSelector "setButtonTitle:"

-- | @Selector@ for @buttonImage@
buttonImageSelector :: Selector '[] (Id NSImage)
buttonImageSelector = mkSelector "buttonImage"

-- | @Selector@ for @setButtonImage:@
setButtonImageSelector :: Selector '[Id NSImage] ()
setButtonImageSelector = mkSelector "setButtonImage:"

