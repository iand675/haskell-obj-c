{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | This class configures and displays a media browser panel.
--
-- This class provides no direct access to the panel, and its meaningful contents aren't in the calling process.
--
-- Generated bindings for @NSMediaLibraryBrowserController@.
module ObjC.AppKit.NSMediaLibraryBrowserController
  ( NSMediaLibraryBrowserController
  , IsNSMediaLibraryBrowserController(..)
  , togglePanel
  , sharedMediaLibraryBrowserController
  , visible
  , setVisible
  , frame
  , setFrame
  , mediaLibraries
  , setMediaLibraries
  , togglePanelSelector
  , sharedMediaLibraryBrowserControllerSelector
  , visibleSelector
  , setVisibleSelector
  , frameSelector
  , setFrameSelector
  , mediaLibrariesSelector
  , setMediaLibrariesSelector

  -- * Enum types
  , NSMediaLibrary(NSMediaLibrary)
  , pattern NSMediaLibraryAudio
  , pattern NSMediaLibraryImage
  , pattern NSMediaLibraryMovie

  ) where

import Foreign.Ptr (Ptr, nullPtr, castPtr)
import Foreign.LibFFI
import Foreign.C.Types
import Data.Int (Int8, Int16)
import Data.Word (Word16)
import Data.Coerce (coerce)

import ObjC.Runtime.Types
import ObjC.Runtime.MsgSend (sendMsg, sendClassMsg, sendMsgStret, sendClassMsgStret)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Structs
import ObjC.AppKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- togglePanel:@
togglePanel :: IsNSMediaLibraryBrowserController nsMediaLibraryBrowserController => nsMediaLibraryBrowserController -> RawId -> IO ()
togglePanel nsMediaLibraryBrowserController  sender =
  sendMsg nsMediaLibraryBrowserController (mkSelector "togglePanel:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | @+ sharedMediaLibraryBrowserController@
sharedMediaLibraryBrowserController :: IO (Id NSMediaLibraryBrowserController)
sharedMediaLibraryBrowserController  =
  do
    cls' <- getRequiredClass "NSMediaLibraryBrowserController"
    sendClassMsg cls' (mkSelector "sharedMediaLibraryBrowserController") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- visible@
visible :: IsNSMediaLibraryBrowserController nsMediaLibraryBrowserController => nsMediaLibraryBrowserController -> IO Bool
visible nsMediaLibraryBrowserController  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsMediaLibraryBrowserController (mkSelector "visible") retCULong []

-- | @- setVisible:@
setVisible :: IsNSMediaLibraryBrowserController nsMediaLibraryBrowserController => nsMediaLibraryBrowserController -> Bool -> IO ()
setVisible nsMediaLibraryBrowserController  value =
  sendMsg nsMediaLibraryBrowserController (mkSelector "setVisible:") retVoid [argCULong (if value then 1 else 0)]

-- | @- frame@
frame :: IsNSMediaLibraryBrowserController nsMediaLibraryBrowserController => nsMediaLibraryBrowserController -> IO NSRect
frame nsMediaLibraryBrowserController  =
  sendMsgStret nsMediaLibraryBrowserController (mkSelector "frame") retNSRect []

-- | @- setFrame:@
setFrame :: IsNSMediaLibraryBrowserController nsMediaLibraryBrowserController => nsMediaLibraryBrowserController -> NSRect -> IO ()
setFrame nsMediaLibraryBrowserController  value =
  sendMsg nsMediaLibraryBrowserController (mkSelector "setFrame:") retVoid [argNSRect value]

-- | @- mediaLibraries@
mediaLibraries :: IsNSMediaLibraryBrowserController nsMediaLibraryBrowserController => nsMediaLibraryBrowserController -> IO NSMediaLibrary
mediaLibraries nsMediaLibraryBrowserController  =
  fmap (coerce :: CULong -> NSMediaLibrary) $ sendMsg nsMediaLibraryBrowserController (mkSelector "mediaLibraries") retCULong []

-- | @- setMediaLibraries:@
setMediaLibraries :: IsNSMediaLibraryBrowserController nsMediaLibraryBrowserController => nsMediaLibraryBrowserController -> NSMediaLibrary -> IO ()
setMediaLibraries nsMediaLibraryBrowserController  value =
  sendMsg nsMediaLibraryBrowserController (mkSelector "setMediaLibraries:") retVoid [argCULong (coerce value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @togglePanel:@
togglePanelSelector :: Selector
togglePanelSelector = mkSelector "togglePanel:"

-- | @Selector@ for @sharedMediaLibraryBrowserController@
sharedMediaLibraryBrowserControllerSelector :: Selector
sharedMediaLibraryBrowserControllerSelector = mkSelector "sharedMediaLibraryBrowserController"

-- | @Selector@ for @visible@
visibleSelector :: Selector
visibleSelector = mkSelector "visible"

-- | @Selector@ for @setVisible:@
setVisibleSelector :: Selector
setVisibleSelector = mkSelector "setVisible:"

-- | @Selector@ for @frame@
frameSelector :: Selector
frameSelector = mkSelector "frame"

-- | @Selector@ for @setFrame:@
setFrameSelector :: Selector
setFrameSelector = mkSelector "setFrame:"

-- | @Selector@ for @mediaLibraries@
mediaLibrariesSelector :: Selector
mediaLibrariesSelector = mkSelector "mediaLibraries"

-- | @Selector@ for @setMediaLibraries:@
setMediaLibrariesSelector :: Selector
setMediaLibrariesSelector = mkSelector "setMediaLibraries:"

