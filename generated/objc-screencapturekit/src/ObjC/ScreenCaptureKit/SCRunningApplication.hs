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
  , initSelector
  , newSelector
  , bundleIdentifierSelector
  , applicationNameSelector
  , processIDSelector


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

import ObjC.ScreenCaptureKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsSCRunningApplication scRunningApplication => scRunningApplication -> IO (Id SCRunningApplication)
init_ scRunningApplication  =
  sendMsg scRunningApplication (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id SCRunningApplication)
new  =
  do
    cls' <- getRequiredClass "SCRunningApplication"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | bundleIdentifier the bundleIdentifier for the SCRunningApplication
--
-- ObjC selector: @- bundleIdentifier@
bundleIdentifier :: IsSCRunningApplication scRunningApplication => scRunningApplication -> IO (Id NSString)
bundleIdentifier scRunningApplication  =
  sendMsg scRunningApplication (mkSelector "bundleIdentifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | applicationName the application name for the SCRunningApplication
--
-- ObjC selector: @- applicationName@
applicationName :: IsSCRunningApplication scRunningApplication => scRunningApplication -> IO (Id NSString)
applicationName scRunningApplication  =
  sendMsg scRunningApplication (mkSelector "applicationName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | processID the SCRunningApplication
--
-- ObjC selector: @- processID@
processID :: IsSCRunningApplication scRunningApplication => scRunningApplication -> IO CInt
processID scRunningApplication  =
  sendMsg scRunningApplication (mkSelector "processID") retCInt []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @bundleIdentifier@
bundleIdentifierSelector :: Selector
bundleIdentifierSelector = mkSelector "bundleIdentifier"

-- | @Selector@ for @applicationName@
applicationNameSelector :: Selector
applicationNameSelector = mkSelector "applicationName"

-- | @Selector@ for @processID@
processIDSelector :: Selector
processIDSelector = mkSelector "processID"

