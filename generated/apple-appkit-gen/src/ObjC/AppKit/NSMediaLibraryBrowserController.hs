{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , frameSelector
  , mediaLibrariesSelector
  , setFrameSelector
  , setMediaLibrariesSelector
  , setVisibleSelector
  , sharedMediaLibraryBrowserControllerSelector
  , togglePanelSelector
  , visibleSelector

  -- * Enum types
  , NSMediaLibrary(NSMediaLibrary)
  , pattern NSMediaLibraryAudio
  , pattern NSMediaLibraryImage
  , pattern NSMediaLibraryMovie

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Structs
import ObjC.AppKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- togglePanel:@
togglePanel :: IsNSMediaLibraryBrowserController nsMediaLibraryBrowserController => nsMediaLibraryBrowserController -> RawId -> IO ()
togglePanel nsMediaLibraryBrowserController sender =
  sendMessage nsMediaLibraryBrowserController togglePanelSelector sender

-- | @+ sharedMediaLibraryBrowserController@
sharedMediaLibraryBrowserController :: IO (Id NSMediaLibraryBrowserController)
sharedMediaLibraryBrowserController  =
  do
    cls' <- getRequiredClass "NSMediaLibraryBrowserController"
    sendClassMessage cls' sharedMediaLibraryBrowserControllerSelector

-- | @- visible@
visible :: IsNSMediaLibraryBrowserController nsMediaLibraryBrowserController => nsMediaLibraryBrowserController -> IO Bool
visible nsMediaLibraryBrowserController =
  sendMessage nsMediaLibraryBrowserController visibleSelector

-- | @- setVisible:@
setVisible :: IsNSMediaLibraryBrowserController nsMediaLibraryBrowserController => nsMediaLibraryBrowserController -> Bool -> IO ()
setVisible nsMediaLibraryBrowserController value =
  sendMessage nsMediaLibraryBrowserController setVisibleSelector value

-- | @- frame@
frame :: IsNSMediaLibraryBrowserController nsMediaLibraryBrowserController => nsMediaLibraryBrowserController -> IO NSRect
frame nsMediaLibraryBrowserController =
  sendMessage nsMediaLibraryBrowserController frameSelector

-- | @- setFrame:@
setFrame :: IsNSMediaLibraryBrowserController nsMediaLibraryBrowserController => nsMediaLibraryBrowserController -> NSRect -> IO ()
setFrame nsMediaLibraryBrowserController value =
  sendMessage nsMediaLibraryBrowserController setFrameSelector value

-- | @- mediaLibraries@
mediaLibraries :: IsNSMediaLibraryBrowserController nsMediaLibraryBrowserController => nsMediaLibraryBrowserController -> IO NSMediaLibrary
mediaLibraries nsMediaLibraryBrowserController =
  sendMessage nsMediaLibraryBrowserController mediaLibrariesSelector

-- | @- setMediaLibraries:@
setMediaLibraries :: IsNSMediaLibraryBrowserController nsMediaLibraryBrowserController => nsMediaLibraryBrowserController -> NSMediaLibrary -> IO ()
setMediaLibraries nsMediaLibraryBrowserController value =
  sendMessage nsMediaLibraryBrowserController setMediaLibrariesSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @togglePanel:@
togglePanelSelector :: Selector '[RawId] ()
togglePanelSelector = mkSelector "togglePanel:"

-- | @Selector@ for @sharedMediaLibraryBrowserController@
sharedMediaLibraryBrowserControllerSelector :: Selector '[] (Id NSMediaLibraryBrowserController)
sharedMediaLibraryBrowserControllerSelector = mkSelector "sharedMediaLibraryBrowserController"

-- | @Selector@ for @visible@
visibleSelector :: Selector '[] Bool
visibleSelector = mkSelector "visible"

-- | @Selector@ for @setVisible:@
setVisibleSelector :: Selector '[Bool] ()
setVisibleSelector = mkSelector "setVisible:"

-- | @Selector@ for @frame@
frameSelector :: Selector '[] NSRect
frameSelector = mkSelector "frame"

-- | @Selector@ for @setFrame:@
setFrameSelector :: Selector '[NSRect] ()
setFrameSelector = mkSelector "setFrame:"

-- | @Selector@ for @mediaLibraries@
mediaLibrariesSelector :: Selector '[] NSMediaLibrary
mediaLibrariesSelector = mkSelector "mediaLibraries"

-- | @Selector@ for @setMediaLibraries:@
setMediaLibrariesSelector :: Selector '[NSMediaLibrary] ()
setMediaLibrariesSelector = mkSelector "setMediaLibraries:"

