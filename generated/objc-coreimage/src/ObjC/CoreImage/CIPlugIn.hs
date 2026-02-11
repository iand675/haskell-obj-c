{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | The CIPlugIn class is responsible for loading Image Units.
--
-- The implementation of the CIPlugIn objects is private. An application can, however, call the 2 public class method to load plug-ins.
--
-- Loading executable CIFilter plugins is deprecated starting in macOS 10.15.
--
-- Generated bindings for @CIPlugIn@.
module ObjC.CoreImage.CIPlugIn
  ( CIPlugIn
  , IsCIPlugIn(..)
  , loadAllPlugIns
  , loadNonExecutablePlugIns
  , loadPlugIn_allowNonExecutable
  , loadPlugIn_allowExecutableCode
  , loadNonExecutablePlugIn
  , loadAllPlugInsSelector
  , loadNonExecutablePlugInsSelector
  , loadPlugIn_allowNonExecutableSelector
  , loadPlugIn_allowExecutableCodeSelector
  , loadNonExecutablePlugInSelector


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

import ObjC.CoreImage.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | This call will scan for plugins with the extension .plugin in 		/Library/Graphics/Image Units        ~Library/Graphics/Image Units If called more than once, newly added plug-ins will be loaded but you cannot remove a plug-in and its filters.
--
-- ObjC selector: @+ loadAllPlugIns@
loadAllPlugIns :: IO ()
loadAllPlugIns  =
  do
    cls' <- getRequiredClass "CIPlugIn"
    sendClassMsg cls' (mkSelector "loadAllPlugIns") retVoid []

-- | Same as loadAllPlugIns does not load filters that contain executable code.
--
-- ObjC selector: @+ loadNonExecutablePlugIns@
loadNonExecutablePlugIns :: IO ()
loadNonExecutablePlugIns  =
  do
    cls' <- getRequiredClass "CIPlugIn"
    sendClassMsg cls' (mkSelector "loadNonExecutablePlugIns") retVoid []

-- | Loads a plug-in specified by its URL.
--
-- ObjC selector: @+ loadPlugIn:allowNonExecutable:@
loadPlugIn_allowNonExecutable :: IsNSURL url => url -> Bool -> IO ()
loadPlugIn_allowNonExecutable url allowNonExecutable =
  do
    cls' <- getRequiredClass "CIPlugIn"
    withObjCPtr url $ \raw_url ->
      sendClassMsg cls' (mkSelector "loadPlugIn:allowNonExecutable:") retVoid [argPtr (castPtr raw_url :: Ptr ()), argCULong (if allowNonExecutable then 1 else 0)]

-- | Loads a plug-in specified by its URL. If allowExecutableCode is NO, filters containing executable code will not be loaded. If YES, any kind of filter will be loaded.
--
-- ObjC selector: @+ loadPlugIn:allowExecutableCode:@
loadPlugIn_allowExecutableCode :: IsNSURL url => url -> Bool -> IO ()
loadPlugIn_allowExecutableCode url allowExecutableCode =
  do
    cls' <- getRequiredClass "CIPlugIn"
    withObjCPtr url $ \raw_url ->
      sendClassMsg cls' (mkSelector "loadPlugIn:allowExecutableCode:") retVoid [argPtr (castPtr raw_url :: Ptr ()), argCULong (if allowExecutableCode then 1 else 0)]

-- | Loads a non-executable plug-in specified by its URL. If the filters containing executable code, it will not be loaded.
--
-- ObjC selector: @+ loadNonExecutablePlugIn:@
loadNonExecutablePlugIn :: IsNSURL url => url -> IO ()
loadNonExecutablePlugIn url =
  do
    cls' <- getRequiredClass "CIPlugIn"
    withObjCPtr url $ \raw_url ->
      sendClassMsg cls' (mkSelector "loadNonExecutablePlugIn:") retVoid [argPtr (castPtr raw_url :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @loadAllPlugIns@
loadAllPlugInsSelector :: Selector
loadAllPlugInsSelector = mkSelector "loadAllPlugIns"

-- | @Selector@ for @loadNonExecutablePlugIns@
loadNonExecutablePlugInsSelector :: Selector
loadNonExecutablePlugInsSelector = mkSelector "loadNonExecutablePlugIns"

-- | @Selector@ for @loadPlugIn:allowNonExecutable:@
loadPlugIn_allowNonExecutableSelector :: Selector
loadPlugIn_allowNonExecutableSelector = mkSelector "loadPlugIn:allowNonExecutable:"

-- | @Selector@ for @loadPlugIn:allowExecutableCode:@
loadPlugIn_allowExecutableCodeSelector :: Selector
loadPlugIn_allowExecutableCodeSelector = mkSelector "loadPlugIn:allowExecutableCode:"

-- | @Selector@ for @loadNonExecutablePlugIn:@
loadNonExecutablePlugInSelector :: Selector
loadNonExecutablePlugInSelector = mkSelector "loadNonExecutablePlugIn:"

