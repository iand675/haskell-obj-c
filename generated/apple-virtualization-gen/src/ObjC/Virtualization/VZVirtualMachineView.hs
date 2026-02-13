{-# LANGUAGE DataKinds #-}
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
  , automaticallyReconfiguresDisplaySelector
  , capturesSystemKeysSelector
  , setAutomaticallyReconfiguresDisplaySelector
  , setCapturesSystemKeysSelector
  , setVirtualMachineSelector
  , virtualMachineSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Virtualization.Internal.Classes
import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | The virtual machine to display in the view.
--
-- ObjC selector: @- virtualMachine@
virtualMachine :: IsVZVirtualMachineView vzVirtualMachineView => vzVirtualMachineView -> IO (Id VZVirtualMachine)
virtualMachine vzVirtualMachineView =
  sendMessage vzVirtualMachineView virtualMachineSelector

-- | The virtual machine to display in the view.
--
-- ObjC selector: @- setVirtualMachine:@
setVirtualMachine :: (IsVZVirtualMachineView vzVirtualMachineView, IsVZVirtualMachine value) => vzVirtualMachineView -> value -> IO ()
setVirtualMachine vzVirtualMachineView value =
  sendMessage vzVirtualMachineView setVirtualMachineSelector (toVZVirtualMachine value)

-- | Whether certain system hot keys should be sent to the guest instead of the host. Defaults to NO.
--
-- ObjC selector: @- capturesSystemKeys@
capturesSystemKeys :: IsVZVirtualMachineView vzVirtualMachineView => vzVirtualMachineView -> IO Bool
capturesSystemKeys vzVirtualMachineView =
  sendMessage vzVirtualMachineView capturesSystemKeysSelector

-- | Whether certain system hot keys should be sent to the guest instead of the host. Defaults to NO.
--
-- ObjC selector: @- setCapturesSystemKeys:@
setCapturesSystemKeys :: IsVZVirtualMachineView vzVirtualMachineView => vzVirtualMachineView -> Bool -> IO ()
setCapturesSystemKeys vzVirtualMachineView value =
  sendMessage vzVirtualMachineView setCapturesSystemKeysSelector value

-- | Automatically reconfigures the graphics display associated with this view with respect to view changes. Defaults to NO.
--
-- Automatically resize or reconfigure this graphics display when the view properties update.    For example, resizing the display when the view has a live resize operation. When enabled,    the graphics display will automatically be reconfigured to match the host display environment.
--
-- This property can only be set on a single VZVirtualMachineView targeting a particular VZGraphicsDisplay    at a time. If multiple VZVirtualMachineViews targeting the same VZGraphicsDisplay enable this property,    only one view will respect the property, and the other view will have had the property disabled.
--
-- ObjC selector: @- automaticallyReconfiguresDisplay@
automaticallyReconfiguresDisplay :: IsVZVirtualMachineView vzVirtualMachineView => vzVirtualMachineView -> IO Bool
automaticallyReconfiguresDisplay vzVirtualMachineView =
  sendMessage vzVirtualMachineView automaticallyReconfiguresDisplaySelector

-- | Automatically reconfigures the graphics display associated with this view with respect to view changes. Defaults to NO.
--
-- Automatically resize or reconfigure this graphics display when the view properties update.    For example, resizing the display when the view has a live resize operation. When enabled,    the graphics display will automatically be reconfigured to match the host display environment.
--
-- This property can only be set on a single VZVirtualMachineView targeting a particular VZGraphicsDisplay    at a time. If multiple VZVirtualMachineViews targeting the same VZGraphicsDisplay enable this property,    only one view will respect the property, and the other view will have had the property disabled.
--
-- ObjC selector: @- setAutomaticallyReconfiguresDisplay:@
setAutomaticallyReconfiguresDisplay :: IsVZVirtualMachineView vzVirtualMachineView => vzVirtualMachineView -> Bool -> IO ()
setAutomaticallyReconfiguresDisplay vzVirtualMachineView value =
  sendMessage vzVirtualMachineView setAutomaticallyReconfiguresDisplaySelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @virtualMachine@
virtualMachineSelector :: Selector '[] (Id VZVirtualMachine)
virtualMachineSelector = mkSelector "virtualMachine"

-- | @Selector@ for @setVirtualMachine:@
setVirtualMachineSelector :: Selector '[Id VZVirtualMachine] ()
setVirtualMachineSelector = mkSelector "setVirtualMachine:"

-- | @Selector@ for @capturesSystemKeys@
capturesSystemKeysSelector :: Selector '[] Bool
capturesSystemKeysSelector = mkSelector "capturesSystemKeys"

-- | @Selector@ for @setCapturesSystemKeys:@
setCapturesSystemKeysSelector :: Selector '[Bool] ()
setCapturesSystemKeysSelector = mkSelector "setCapturesSystemKeys:"

-- | @Selector@ for @automaticallyReconfiguresDisplay@
automaticallyReconfiguresDisplaySelector :: Selector '[] Bool
automaticallyReconfiguresDisplaySelector = mkSelector "automaticallyReconfiguresDisplay"

-- | @Selector@ for @setAutomaticallyReconfiguresDisplay:@
setAutomaticallyReconfiguresDisplaySelector :: Selector '[Bool] ()
setAutomaticallyReconfiguresDisplaySelector = mkSelector "setAutomaticallyReconfiguresDisplay:"

