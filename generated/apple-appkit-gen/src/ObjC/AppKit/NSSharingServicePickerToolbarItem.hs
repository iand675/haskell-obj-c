{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSSharingServicePickerToolbarItem@.
module ObjC.AppKit.NSSharingServicePickerToolbarItem
  ( NSSharingServicePickerToolbarItem
  , IsNSSharingServicePickerToolbarItem(..)
  , delegate
  , setDelegate
  , delegateSelector
  , setDelegateSelector


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
delegate :: IsNSSharingServicePickerToolbarItem nsSharingServicePickerToolbarItem => nsSharingServicePickerToolbarItem -> IO RawId
delegate nsSharingServicePickerToolbarItem =
  sendMessage nsSharingServicePickerToolbarItem delegateSelector

-- | @- setDelegate:@
setDelegate :: IsNSSharingServicePickerToolbarItem nsSharingServicePickerToolbarItem => nsSharingServicePickerToolbarItem -> RawId -> IO ()
setDelegate nsSharingServicePickerToolbarItem value =
  sendMessage nsSharingServicePickerToolbarItem setDelegateSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @delegate@
delegateSelector :: Selector '[] RawId
delegateSelector = mkSelector "delegate"

-- | @Selector@ for @setDelegate:@
setDelegateSelector :: Selector '[RawId] ()
setDelegateSelector = mkSelector "setDelegate:"

