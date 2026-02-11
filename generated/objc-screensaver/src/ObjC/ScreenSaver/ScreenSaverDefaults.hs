{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A class that defines a set of methods for saving and restoring user defaults for screen savers.
--
-- ``ScreenSaverDefaults`` gives you access to preference values you need to configure your screen saver. Because multiple apps can load a screen saver, you can’t use the standard <doc://com.apple.documentation/documentation/foundation/nsuserdefaults> object to store preferences. Instead, instantiate this class using the ``ScreenSaverDefaults/defaultsForModuleWithName:`` method, which takes your screen saver’s bundle identifier as a parameter. The resulting object gives you a way to store your preference values and associate them only with your screen saver. Use the inherited <doc://com.apple.documentation/documentation/foundation/nsuserdefaults> methods to load, store, or modify values.
--
-- Generated bindings for @ScreenSaverDefaults@.
module ObjC.ScreenSaver.ScreenSaverDefaults
  ( ScreenSaverDefaults
  , IsScreenSaverDefaults(..)
  , defaultsForModuleWithName
  , defaultsForModuleWithNameSelector


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

import ObjC.ScreenSaver.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Returns a screen saver defaults instance that reads and writes defaults for the specified module.
--
-- - Parameters:   - inModuleName: The bundle identifier for the module.
--
-- ObjC selector: @+ defaultsForModuleWithName:@
defaultsForModuleWithName :: IsNSString inModuleName => inModuleName -> IO (Id ScreenSaverDefaults)
defaultsForModuleWithName inModuleName =
  do
    cls' <- getRequiredClass "ScreenSaverDefaults"
    withObjCPtr inModuleName $ \raw_inModuleName ->
      sendClassMsg cls' (mkSelector "defaultsForModuleWithName:") (retPtr retVoid) [argPtr (castPtr raw_inModuleName :: Ptr ())] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @defaultsForModuleWithName:@
defaultsForModuleWithNameSelector :: Selector
defaultsForModuleWithNameSelector = mkSelector "defaultsForModuleWithName:"

