{-# LANGUAGE PatternSynonyms #-}
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
  , panelSelector
  , beginSheetWithPDFInfo_modalForWindow_completionHandlerSelector
  , accessoryControllerSelector
  , setAccessoryControllerSelector
  , optionsSelector
  , setOptionsSelector
  , defaultFileNameSelector
  , setDefaultFileNameSelector

  -- * Enum types
  , NSPDFPanelOptions(NSPDFPanelOptions)
  , pattern NSPDFPanelShowsPaperSize
  , pattern NSPDFPanelShowsOrientation
  , pattern NSPDFPanelRequestsParentDirectory

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

import ObjC.AppKit.Internal.Classes
import ObjC.AppKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @+ panel@
panel :: IO (Id NSPDFPanel)
panel  =
  do
    cls' <- getRequiredClass "NSPDFPanel"
    sendClassMsg cls' (mkSelector "panel") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- beginSheetWithPDFInfo:modalForWindow:completionHandler:@
beginSheetWithPDFInfo_modalForWindow_completionHandler :: (IsNSPDFPanel nspdfPanel, IsNSPDFInfo pdfInfo, IsNSWindow docWindow) => nspdfPanel -> pdfInfo -> docWindow -> Ptr () -> IO ()
beginSheetWithPDFInfo_modalForWindow_completionHandler nspdfPanel  pdfInfo docWindow completionHandler =
withObjCPtr pdfInfo $ \raw_pdfInfo ->
  withObjCPtr docWindow $ \raw_docWindow ->
      sendMsg nspdfPanel (mkSelector "beginSheetWithPDFInfo:modalForWindow:completionHandler:") retVoid [argPtr (castPtr raw_pdfInfo :: Ptr ()), argPtr (castPtr raw_docWindow :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- accessoryController@
accessoryController :: IsNSPDFPanel nspdfPanel => nspdfPanel -> IO (Id NSViewController)
accessoryController nspdfPanel  =
  sendMsg nspdfPanel (mkSelector "accessoryController") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setAccessoryController:@
setAccessoryController :: (IsNSPDFPanel nspdfPanel, IsNSViewController value) => nspdfPanel -> value -> IO ()
setAccessoryController nspdfPanel  value =
withObjCPtr value $ \raw_value ->
    sendMsg nspdfPanel (mkSelector "setAccessoryController:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- options@
options :: IsNSPDFPanel nspdfPanel => nspdfPanel -> IO NSPDFPanelOptions
options nspdfPanel  =
  fmap (coerce :: CLong -> NSPDFPanelOptions) $ sendMsg nspdfPanel (mkSelector "options") retCLong []

-- | @- setOptions:@
setOptions :: IsNSPDFPanel nspdfPanel => nspdfPanel -> NSPDFPanelOptions -> IO ()
setOptions nspdfPanel  value =
  sendMsg nspdfPanel (mkSelector "setOptions:") retVoid [argCLong (coerce value)]

-- | @- defaultFileName@
defaultFileName :: IsNSPDFPanel nspdfPanel => nspdfPanel -> IO (Id NSString)
defaultFileName nspdfPanel  =
  sendMsg nspdfPanel (mkSelector "defaultFileName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setDefaultFileName:@
setDefaultFileName :: (IsNSPDFPanel nspdfPanel, IsNSString value) => nspdfPanel -> value -> IO ()
setDefaultFileName nspdfPanel  value =
withObjCPtr value $ \raw_value ->
    sendMsg nspdfPanel (mkSelector "setDefaultFileName:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @panel@
panelSelector :: Selector
panelSelector = mkSelector "panel"

-- | @Selector@ for @beginSheetWithPDFInfo:modalForWindow:completionHandler:@
beginSheetWithPDFInfo_modalForWindow_completionHandlerSelector :: Selector
beginSheetWithPDFInfo_modalForWindow_completionHandlerSelector = mkSelector "beginSheetWithPDFInfo:modalForWindow:completionHandler:"

-- | @Selector@ for @accessoryController@
accessoryControllerSelector :: Selector
accessoryControllerSelector = mkSelector "accessoryController"

-- | @Selector@ for @setAccessoryController:@
setAccessoryControllerSelector :: Selector
setAccessoryControllerSelector = mkSelector "setAccessoryController:"

-- | @Selector@ for @options@
optionsSelector :: Selector
optionsSelector = mkSelector "options"

-- | @Selector@ for @setOptions:@
setOptionsSelector :: Selector
setOptionsSelector = mkSelector "setOptions:"

-- | @Selector@ for @defaultFileName@
defaultFileNameSelector :: Selector
defaultFileNameSelector = mkSelector "defaultFileName"

-- | @Selector@ for @setDefaultFileName:@
setDefaultFileNameSelector :: Selector
setDefaultFileNameSelector = mkSelector "setDefaultFileName:"

