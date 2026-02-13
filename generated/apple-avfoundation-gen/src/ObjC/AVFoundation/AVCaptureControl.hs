{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | AVCaptureControl
--
-- AVCaptureControl is an abstract class that defines an interface for a unique type of control which allows deep integration with the camera system through AVCaptureSession.
--
-- Various concrete subclasses of @AVCaptureControl@ are provided by AVFoundation to allow your application to both leverage common system controls and define unique custom controls.
--
-- Controls may be added to an @AVCaptureSession@ using @-[AVCaptureSession addControl:]@.
--
-- For controls that use symbols to represent them, only SF Symbols may be used.
--
-- Generated bindings for @AVCaptureControl@.
module ObjC.AVFoundation.AVCaptureControl
  ( AVCaptureControl
  , IsAVCaptureControl(..)
  , init_
  , new
  , enabled
  , setEnabled
  , enabledSelector
  , initSelector
  , newSelector
  , setEnabledSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AVFoundation.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsAVCaptureControl avCaptureControl => avCaptureControl -> IO (Id AVCaptureControl)
init_ avCaptureControl =
  sendOwnedMessage avCaptureControl initSelector

-- | @+ new@
new :: IO (Id AVCaptureControl)
new  =
  do
    cls' <- getRequiredClass "AVCaptureControl"
    sendOwnedClassMessage cls' newSelector

-- | enabled
--
-- Indicates whether the control should be enabled for user interaction.
--
-- The value of this property is a @BOOL@ that determines whether the control should be enabled for user interaction. Clients can set this property to keep a control added to an @AVCaptureSession@ but prevent it from being interacted with by the user. A control's value may still be changed while it is disabled. The default value is @YES@.
--
-- ObjC selector: @- enabled@
enabled :: IsAVCaptureControl avCaptureControl => avCaptureControl -> IO Bool
enabled avCaptureControl =
  sendMessage avCaptureControl enabledSelector

-- | enabled
--
-- Indicates whether the control should be enabled for user interaction.
--
-- The value of this property is a @BOOL@ that determines whether the control should be enabled for user interaction. Clients can set this property to keep a control added to an @AVCaptureSession@ but prevent it from being interacted with by the user. A control's value may still be changed while it is disabled. The default value is @YES@.
--
-- ObjC selector: @- setEnabled:@
setEnabled :: IsAVCaptureControl avCaptureControl => avCaptureControl -> Bool -> IO ()
setEnabled avCaptureControl value =
  sendMessage avCaptureControl setEnabledSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id AVCaptureControl)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id AVCaptureControl)
newSelector = mkSelector "new"

-- | @Selector@ for @enabled@
enabledSelector :: Selector '[] Bool
enabledSelector = mkSelector "enabled"

-- | @Selector@ for @setEnabled:@
setEnabledSelector :: Selector '[Bool] ()
setEnabledSelector = mkSelector "setEnabled:"

