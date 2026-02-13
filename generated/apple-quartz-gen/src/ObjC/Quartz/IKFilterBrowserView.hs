{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | IKFilterBrowserView
--
-- 2006 Apple Inc. All rights reserved.	 Coming to a Leopard installation near you
--
-- View containing the elements of the IKFilterBrowser
--
-- See discussion in IKFilterBrowserPanel
--
-- Generated bindings for @IKFilterBrowserView@.
module ObjC.Quartz.IKFilterBrowserView
  ( IKFilterBrowserView
  , IsIKFilterBrowserView(..)
  , setPreviewState
  , filterName
  , filterNameSelector
  , setPreviewStateSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Quartz.Internal.Classes
import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | setPreviewState:
--
-- Use this method to show and hide the Preview
--
-- Use this method to show and hide the Preview from the program.
--
-- @inState@ — Boolean for visibility of the preview.
--
-- ObjC selector: @- setPreviewState:@
setPreviewState :: IsIKFilterBrowserView ikFilterBrowserView => ikFilterBrowserView -> Bool -> IO ()
setPreviewState ikFilterBrowserView inState =
  sendMessage ikFilterBrowserView setPreviewStateSelector inState

-- | filterName
--
-- Returns the name of the currently selected filter.
--
-- Use this method in response to a IKFilterBrowserFilterSelectedNotification or IKFilterBrowserFilterDoubleClickNotification or afer returning from a modal session.
--
-- ObjC selector: @- filterName@
filterName :: IsIKFilterBrowserView ikFilterBrowserView => ikFilterBrowserView -> IO (Id NSString)
filterName ikFilterBrowserView =
  sendMessage ikFilterBrowserView filterNameSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @setPreviewState:@
setPreviewStateSelector :: Selector '[Bool] ()
setPreviewStateSelector = mkSelector "setPreviewState:"

-- | @Selector@ for @filterName@
filterNameSelector :: Selector '[] (Id NSString)
filterNameSelector = mkSelector "filterName"

