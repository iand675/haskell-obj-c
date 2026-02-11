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
  , initSelector
  , newSelector
  , enabledSelector
  , setEnabledSelector


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

import ObjC.AVFoundation.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsAVCaptureControl avCaptureControl => avCaptureControl -> IO (Id AVCaptureControl)
init_ avCaptureControl  =
  sendMsg avCaptureControl (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id AVCaptureControl)
new  =
  do
    cls' <- getRequiredClass "AVCaptureControl"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | enabled
--
-- Indicates whether the control should be enabled for user interaction.
--
-- The value of this property is a @BOOL@ that determines whether the control should be enabled for user interaction. Clients can set this property to keep a control added to an @AVCaptureSession@ but prevent it from being interacted with by the user. A control's value may still be changed while it is disabled. The default value is @YES@.
--
-- ObjC selector: @- enabled@
enabled :: IsAVCaptureControl avCaptureControl => avCaptureControl -> IO Bool
enabled avCaptureControl  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg avCaptureControl (mkSelector "enabled") retCULong []

-- | enabled
--
-- Indicates whether the control should be enabled for user interaction.
--
-- The value of this property is a @BOOL@ that determines whether the control should be enabled for user interaction. Clients can set this property to keep a control added to an @AVCaptureSession@ but prevent it from being interacted with by the user. A control's value may still be changed while it is disabled. The default value is @YES@.
--
-- ObjC selector: @- setEnabled:@
setEnabled :: IsAVCaptureControl avCaptureControl => avCaptureControl -> Bool -> IO ()
setEnabled avCaptureControl  value =
  sendMsg avCaptureControl (mkSelector "setEnabled:") retVoid [argCULong (if value then 1 else 0)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @enabled@
enabledSelector :: Selector
enabledSelector = mkSelector "enabled"

-- | @Selector@ for @setEnabled:@
setEnabledSelector :: Selector
setEnabledSelector = mkSelector "setEnabled:"

