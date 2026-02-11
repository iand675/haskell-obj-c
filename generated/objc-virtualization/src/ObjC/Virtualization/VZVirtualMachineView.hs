{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A view that allows user interaction with a virtual machine.
--
-- The VZVirtualMachineView shows the contents of the virtual machine framebuffer. If the virtual machine configuration includes a keyboard and a pointing device,    the view forwards keyboard and mouse events to the virtual machine via those devices.
--
-- VZVirtualMachine
--
-- Generated bindings for @VZVirtualMachineView@.
module ObjC.Virtualization.VZVirtualMachineView
  ( VZVirtualMachineView
  , IsVZVirtualMachineView(..)
  , virtualMachine
  , setVirtualMachine
  , capturesSystemKeys
  , setCapturesSystemKeys
  , automaticallyReconfiguresDisplay
  , setAutomaticallyReconfiguresDisplay
  , virtualMachineSelector
  , setVirtualMachineSelector
  , capturesSystemKeysSelector
  , setCapturesSystemKeysSelector
  , automaticallyReconfiguresDisplaySelector
  , setAutomaticallyReconfiguresDisplaySelector


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

import ObjC.Virtualization.Internal.Classes
import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | The virtual machine to display in the view.
--
-- ObjC selector: @- virtualMachine@
virtualMachine :: IsVZVirtualMachineView vzVirtualMachineView => vzVirtualMachineView -> IO (Id VZVirtualMachine)
virtualMachine vzVirtualMachineView  =
  sendMsg vzVirtualMachineView (mkSelector "virtualMachine") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The virtual machine to display in the view.
--
-- ObjC selector: @- setVirtualMachine:@
setVirtualMachine :: (IsVZVirtualMachineView vzVirtualMachineView, IsVZVirtualMachine value) => vzVirtualMachineView -> value -> IO ()
setVirtualMachine vzVirtualMachineView  value =
withObjCPtr value $ \raw_value ->
    sendMsg vzVirtualMachineView (mkSelector "setVirtualMachine:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Whether certain system hot keys should be sent to the guest instead of the host. Defaults to NO.
--
-- ObjC selector: @- capturesSystemKeys@
capturesSystemKeys :: IsVZVirtualMachineView vzVirtualMachineView => vzVirtualMachineView -> IO Bool
capturesSystemKeys vzVirtualMachineView  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg vzVirtualMachineView (mkSelector "capturesSystemKeys") retCULong []

-- | Whether certain system hot keys should be sent to the guest instead of the host. Defaults to NO.
--
-- ObjC selector: @- setCapturesSystemKeys:@
setCapturesSystemKeys :: IsVZVirtualMachineView vzVirtualMachineView => vzVirtualMachineView -> Bool -> IO ()
setCapturesSystemKeys vzVirtualMachineView  value =
  sendMsg vzVirtualMachineView (mkSelector "setCapturesSystemKeys:") retVoid [argCULong (if value then 1 else 0)]

-- | Automatically reconfigures the graphics display associated with this view with respect to view changes. Defaults to NO.
--
-- Automatically resize or reconfigure this graphics display when the view properties update.    For example, resizing the display when the view has a live resize operation. When enabled,    the graphics display will automatically be reconfigured to match the host display environment.
--
-- This property can only be set on a single VZVirtualMachineView targeting a particular VZGraphicsDisplay    at a time. If multiple VZVirtualMachineViews targeting the same VZGraphicsDisplay enable this property,    only one view will respect the property, and the other view will have had the property disabled.
--
-- ObjC selector: @- automaticallyReconfiguresDisplay@
automaticallyReconfiguresDisplay :: IsVZVirtualMachineView vzVirtualMachineView => vzVirtualMachineView -> IO Bool
automaticallyReconfiguresDisplay vzVirtualMachineView  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg vzVirtualMachineView (mkSelector "automaticallyReconfiguresDisplay") retCULong []

-- | Automatically reconfigures the graphics display associated with this view with respect to view changes. Defaults to NO.
--
-- Automatically resize or reconfigure this graphics display when the view properties update.    For example, resizing the display when the view has a live resize operation. When enabled,    the graphics display will automatically be reconfigured to match the host display environment.
--
-- This property can only be set on a single VZVirtualMachineView targeting a particular VZGraphicsDisplay    at a time. If multiple VZVirtualMachineViews targeting the same VZGraphicsDisplay enable this property,    only one view will respect the property, and the other view will have had the property disabled.
--
-- ObjC selector: @- setAutomaticallyReconfiguresDisplay:@
setAutomaticallyReconfiguresDisplay :: IsVZVirtualMachineView vzVirtualMachineView => vzVirtualMachineView -> Bool -> IO ()
setAutomaticallyReconfiguresDisplay vzVirtualMachineView  value =
  sendMsg vzVirtualMachineView (mkSelector "setAutomaticallyReconfiguresDisplay:") retVoid [argCULong (if value then 1 else 0)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @virtualMachine@
virtualMachineSelector :: Selector
virtualMachineSelector = mkSelector "virtualMachine"

-- | @Selector@ for @setVirtualMachine:@
setVirtualMachineSelector :: Selector
setVirtualMachineSelector = mkSelector "setVirtualMachine:"

-- | @Selector@ for @capturesSystemKeys@
capturesSystemKeysSelector :: Selector
capturesSystemKeysSelector = mkSelector "capturesSystemKeys"

-- | @Selector@ for @setCapturesSystemKeys:@
setCapturesSystemKeysSelector :: Selector
setCapturesSystemKeysSelector = mkSelector "setCapturesSystemKeys:"

-- | @Selector@ for @automaticallyReconfiguresDisplay@
automaticallyReconfiguresDisplaySelector :: Selector
automaticallyReconfiguresDisplaySelector = mkSelector "automaticallyReconfiguresDisplay"

-- | @Selector@ for @setAutomaticallyReconfiguresDisplay:@
setAutomaticallyReconfiguresDisplaySelector :: Selector
setAutomaticallyReconfiguresDisplaySelector = mkSelector "setAutomaticallyReconfiguresDisplay:"

