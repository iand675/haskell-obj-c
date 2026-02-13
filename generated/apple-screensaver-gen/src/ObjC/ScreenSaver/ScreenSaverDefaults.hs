{-# LANGUAGE DataKinds #-}
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

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
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
    sendClassMessage cls' defaultsForModuleWithNameSelector (toNSString inModuleName)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @defaultsForModuleWithName:@
defaultsForModuleWithNameSelector :: Selector '[Id NSString] (Id ScreenSaverDefaults)
defaultsForModuleWithNameSelector = mkSelector "defaultsForModuleWithName:"

