{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A view that extends content to fill its own bounds.
--
-- A background extension view can be laid out to extend outside the safe area, such as under the titlebar, sidebar, or inspector. By default it lays out its content to stay within the safe area, and uses modifications of the content along the edges to fill the container view.
--
-- Generated bindings for @NSBackgroundExtensionView@.
module ObjC.AppKit.NSBackgroundExtensionView
  ( NSBackgroundExtensionView
  , IsNSBackgroundExtensionView(..)
  , contentView
  , setContentView
  , automaticallyPlacesContentView
  , setAutomaticallyPlacesContentView
  , automaticallyPlacesContentViewSelector
  , contentViewSelector
  , setAutomaticallyPlacesContentViewSelector
  , setContentViewSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | The content view to extend to fill the @NSBackgroundExtensionView@.
--
-- The content view will be added as a subview of the extension view and placed within the safe area by default. See @automaticallyPlacesContentView@ to customize the layout.
--
-- ObjC selector: @- contentView@
contentView :: IsNSBackgroundExtensionView nsBackgroundExtensionView => nsBackgroundExtensionView -> IO (Id NSView)
contentView nsBackgroundExtensionView =
  sendMessage nsBackgroundExtensionView contentViewSelector

-- | The content view to extend to fill the @NSBackgroundExtensionView@.
--
-- The content view will be added as a subview of the extension view and placed within the safe area by default. See @automaticallyPlacesContentView@ to customize the layout.
--
-- ObjC selector: @- setContentView:@
setContentView :: (IsNSBackgroundExtensionView nsBackgroundExtensionView, IsNSView value) => nsBackgroundExtensionView -> value -> IO ()
setContentView nsBackgroundExtensionView value =
  sendMessage nsBackgroundExtensionView setContentViewSelector (toNSView value)

-- | Controls the automatic safe area placement of the @contentView@ within the container.
--
-- When @NO@, the frame of the content view must be explicitly set or constraints added. The extension effect will be used to fill the container view around the content.
--
-- Defaults to @YES@.
--
-- ObjC selector: @- automaticallyPlacesContentView@
automaticallyPlacesContentView :: IsNSBackgroundExtensionView nsBackgroundExtensionView => nsBackgroundExtensionView -> IO Bool
automaticallyPlacesContentView nsBackgroundExtensionView =
  sendMessage nsBackgroundExtensionView automaticallyPlacesContentViewSelector

-- | Controls the automatic safe area placement of the @contentView@ within the container.
--
-- When @NO@, the frame of the content view must be explicitly set or constraints added. The extension effect will be used to fill the container view around the content.
--
-- Defaults to @YES@.
--
-- ObjC selector: @- setAutomaticallyPlacesContentView:@
setAutomaticallyPlacesContentView :: IsNSBackgroundExtensionView nsBackgroundExtensionView => nsBackgroundExtensionView -> Bool -> IO ()
setAutomaticallyPlacesContentView nsBackgroundExtensionView value =
  sendMessage nsBackgroundExtensionView setAutomaticallyPlacesContentViewSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @contentView@
contentViewSelector :: Selector '[] (Id NSView)
contentViewSelector = mkSelector "contentView"

-- | @Selector@ for @setContentView:@
setContentViewSelector :: Selector '[Id NSView] ()
setContentViewSelector = mkSelector "setContentView:"

-- | @Selector@ for @automaticallyPlacesContentView@
automaticallyPlacesContentViewSelector :: Selector '[] Bool
automaticallyPlacesContentViewSelector = mkSelector "automaticallyPlacesContentView"

-- | @Selector@ for @setAutomaticallyPlacesContentView:@
setAutomaticallyPlacesContentViewSelector :: Selector '[Bool] ()
setAutomaticallyPlacesContentViewSelector = mkSelector "setAutomaticallyPlacesContentView:"

