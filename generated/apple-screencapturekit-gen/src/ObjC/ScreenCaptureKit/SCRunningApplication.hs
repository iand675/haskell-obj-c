{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @SCRunningApplication@.
module ObjC.ScreenCaptureKit.SCRunningApplication
  ( SCRunningApplication
  , IsSCRunningApplication(..)
  , init_
  , new
  , bundleIdentifier
  , applicationName
  , processID
  , applicationNameSelector
  , bundleIdentifierSelector
  , initSelector
  , newSelector
  , processIDSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.ScreenCaptureKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsSCRunningApplication scRunningApplication => scRunningApplication -> IO (Id SCRunningApplication)
init_ scRunningApplication =
  sendOwnedMessage scRunningApplication initSelector

-- | @+ new@
new :: IO (Id SCRunningApplication)
new  =
  do
    cls' <- getRequiredClass "SCRunningApplication"
    sendOwnedClassMessage cls' newSelector

-- | bundleIdentifier the bundleIdentifier for the SCRunningApplication
--
-- ObjC selector: @- bundleIdentifier@
bundleIdentifier :: IsSCRunningApplication scRunningApplication => scRunningApplication -> IO (Id NSString)
bundleIdentifier scRunningApplication =
  sendMessage scRunningApplication bundleIdentifierSelector

-- | applicationName the application name for the SCRunningApplication
--
-- ObjC selector: @- applicationName@
applicationName :: IsSCRunningApplication scRunningApplication => scRunningApplication -> IO (Id NSString)
applicationName scRunningApplication =
  sendMessage scRunningApplication applicationNameSelector

-- | processID the SCRunningApplication
--
-- ObjC selector: @- processID@
processID :: IsSCRunningApplication scRunningApplication => scRunningApplication -> IO CInt
processID scRunningApplication =
  sendMessage scRunningApplication processIDSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id SCRunningApplication)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id SCRunningApplication)
newSelector = mkSelector "new"

-- | @Selector@ for @bundleIdentifier@
bundleIdentifierSelector :: Selector '[] (Id NSString)
bundleIdentifierSelector = mkSelector "bundleIdentifier"

-- | @Selector@ for @applicationName@
applicationNameSelector :: Selector '[] (Id NSString)
applicationNameSelector = mkSelector "applicationName"

-- | @Selector@ for @processID@
processIDSelector :: Selector '[] CInt
processIDSelector = mkSelector "processID"

