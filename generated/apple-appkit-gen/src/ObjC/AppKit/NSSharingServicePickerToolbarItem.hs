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
delegate :: IsNSSharingServicePickerToolbarItem nsSharingServicePickerToolbarItem => nsSharingServicePickerToolbarItem -> IO RawId
delegate nsSharingServicePickerToolbarItem  =
    fmap (RawId . castPtr) $ sendMsg nsSharingServicePickerToolbarItem (mkSelector "delegate") (retPtr retVoid) []

-- | @- setDelegate:@
setDelegate :: IsNSSharingServicePickerToolbarItem nsSharingServicePickerToolbarItem => nsSharingServicePickerToolbarItem -> RawId -> IO ()
setDelegate nsSharingServicePickerToolbarItem  value =
    sendMsg nsSharingServicePickerToolbarItem (mkSelector "setDelegate:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @delegate@
delegateSelector :: Selector
delegateSelector = mkSelector "delegate"

-- | @Selector@ for @setDelegate:@
setDelegateSelector :: Selector
setDelegateSelector = mkSelector "setDelegate:"

