{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSPDFPanel@.
module ObjC.AppKit.NSPDFPanel
  ( NSPDFPanel
  , IsNSPDFPanel(..)
  , panel
  , beginSheetWithPDFInfo_modalForWindow_completionHandler
  , accessoryController
  , setAccessoryController
  , options
  , setOptions
  , defaultFileName
  , setDefaultFileName
  , accessoryControllerSelector
  , beginSheetWithPDFInfo_modalForWindow_completionHandlerSelector
  , defaultFileNameSelector
  , optionsSelector
  , panelSelector
  , setAccessoryControllerSelector
  , setDefaultFileNameSelector
  , setOptionsSelector

  -- * Enum types
  , NSPDFPanelOptions(NSPDFPanelOptions)
  , pattern NSPDFPanelShowsPaperSize
  , pattern NSPDFPanelShowsOrientation
  , pattern NSPDFPanelRequestsParentDirectory

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AppKit.Internal.Classes
import ObjC.AppKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @+ panel@
panel :: IO (Id NSPDFPanel)
panel  =
  do
    cls' <- getRequiredClass "NSPDFPanel"
    sendClassMessage cls' panelSelector

-- | @- beginSheetWithPDFInfo:modalForWindow:completionHandler:@
beginSheetWithPDFInfo_modalForWindow_completionHandler :: (IsNSPDFPanel nspdfPanel, IsNSPDFInfo pdfInfo, IsNSWindow docWindow) => nspdfPanel -> pdfInfo -> docWindow -> Ptr () -> IO ()
beginSheetWithPDFInfo_modalForWindow_completionHandler nspdfPanel pdfInfo docWindow completionHandler =
  sendMessage nspdfPanel beginSheetWithPDFInfo_modalForWindow_completionHandlerSelector (toNSPDFInfo pdfInfo) (toNSWindow docWindow) completionHandler

-- | @- accessoryController@
accessoryController :: IsNSPDFPanel nspdfPanel => nspdfPanel -> IO (Id NSViewController)
accessoryController nspdfPanel =
  sendMessage nspdfPanel accessoryControllerSelector

-- | @- setAccessoryController:@
setAccessoryController :: (IsNSPDFPanel nspdfPanel, IsNSViewController value) => nspdfPanel -> value -> IO ()
setAccessoryController nspdfPanel value =
  sendMessage nspdfPanel setAccessoryControllerSelector (toNSViewController value)

-- | @- options@
options :: IsNSPDFPanel nspdfPanel => nspdfPanel -> IO NSPDFPanelOptions
options nspdfPanel =
  sendMessage nspdfPanel optionsSelector

-- | @- setOptions:@
setOptions :: IsNSPDFPanel nspdfPanel => nspdfPanel -> NSPDFPanelOptions -> IO ()
setOptions nspdfPanel value =
  sendMessage nspdfPanel setOptionsSelector value

-- | @- defaultFileName@
defaultFileName :: IsNSPDFPanel nspdfPanel => nspdfPanel -> IO (Id NSString)
defaultFileName nspdfPanel =
  sendMessage nspdfPanel defaultFileNameSelector

-- | @- setDefaultFileName:@
setDefaultFileName :: (IsNSPDFPanel nspdfPanel, IsNSString value) => nspdfPanel -> value -> IO ()
setDefaultFileName nspdfPanel value =
  sendMessage nspdfPanel setDefaultFileNameSelector (toNSString value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @panel@
panelSelector :: Selector '[] (Id NSPDFPanel)
panelSelector = mkSelector "panel"

-- | @Selector@ for @beginSheetWithPDFInfo:modalForWindow:completionHandler:@
beginSheetWithPDFInfo_modalForWindow_completionHandlerSelector :: Selector '[Id NSPDFInfo, Id NSWindow, Ptr ()] ()
beginSheetWithPDFInfo_modalForWindow_completionHandlerSelector = mkSelector "beginSheetWithPDFInfo:modalForWindow:completionHandler:"

-- | @Selector@ for @accessoryController@
accessoryControllerSelector :: Selector '[] (Id NSViewController)
accessoryControllerSelector = mkSelector "accessoryController"

-- | @Selector@ for @setAccessoryController:@
setAccessoryControllerSelector :: Selector '[Id NSViewController] ()
setAccessoryControllerSelector = mkSelector "setAccessoryController:"

-- | @Selector@ for @options@
optionsSelector :: Selector '[] NSPDFPanelOptions
optionsSelector = mkSelector "options"

-- | @Selector@ for @setOptions:@
setOptionsSelector :: Selector '[NSPDFPanelOptions] ()
setOptionsSelector = mkSelector "setOptions:"

-- | @Selector@ for @defaultFileName@
defaultFileNameSelector :: Selector '[] (Id NSString)
defaultFileNameSelector = mkSelector "defaultFileName"

-- | @Selector@ for @setDefaultFileName:@
setDefaultFileNameSelector :: Selector '[Id NSString] ()
setDefaultFileNameSelector = mkSelector "setDefaultFileName:"

