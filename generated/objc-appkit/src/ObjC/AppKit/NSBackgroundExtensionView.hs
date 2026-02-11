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
  , contentViewSelector
  , setContentViewSelector
  , automaticallyPlacesContentViewSelector
  , setAutomaticallyPlacesContentViewSelector


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
import ObjC.Foundation.Internal.Classes

-- | The content view to extend to fill the @NSBackgroundExtensionView@.
--
-- The content view will be added as a subview of the extension view and placed within the safe area by default. See @automaticallyPlacesContentView@ to customize the layout.
--
-- ObjC selector: @- contentView@
contentView :: IsNSBackgroundExtensionView nsBackgroundExtensionView => nsBackgroundExtensionView -> IO (Id NSView)
contentView nsBackgroundExtensionView  =
  sendMsg nsBackgroundExtensionView (mkSelector "contentView") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The content view to extend to fill the @NSBackgroundExtensionView@.
--
-- The content view will be added as a subview of the extension view and placed within the safe area by default. See @automaticallyPlacesContentView@ to customize the layout.
--
-- ObjC selector: @- setContentView:@
setContentView :: (IsNSBackgroundExtensionView nsBackgroundExtensionView, IsNSView value) => nsBackgroundExtensionView -> value -> IO ()
setContentView nsBackgroundExtensionView  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsBackgroundExtensionView (mkSelector "setContentView:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls the automatic safe area placement of the @contentView@ within the container.
--
-- When @NO@, the frame of the content view must be explicitly set or constraints added. The extension effect will be used to fill the container view around the content.
--
-- Defaults to @YES@.
--
-- ObjC selector: @- automaticallyPlacesContentView@
automaticallyPlacesContentView :: IsNSBackgroundExtensionView nsBackgroundExtensionView => nsBackgroundExtensionView -> IO Bool
automaticallyPlacesContentView nsBackgroundExtensionView  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsBackgroundExtensionView (mkSelector "automaticallyPlacesContentView") retCULong []

-- | Controls the automatic safe area placement of the @contentView@ within the container.
--
-- When @NO@, the frame of the content view must be explicitly set or constraints added. The extension effect will be used to fill the container view around the content.
--
-- Defaults to @YES@.
--
-- ObjC selector: @- setAutomaticallyPlacesContentView:@
setAutomaticallyPlacesContentView :: IsNSBackgroundExtensionView nsBackgroundExtensionView => nsBackgroundExtensionView -> Bool -> IO ()
setAutomaticallyPlacesContentView nsBackgroundExtensionView  value =
  sendMsg nsBackgroundExtensionView (mkSelector "setAutomaticallyPlacesContentView:") retVoid [argCULong (if value then 1 else 0)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @contentView@
contentViewSelector :: Selector
contentViewSelector = mkSelector "contentView"

-- | @Selector@ for @setContentView:@
setContentViewSelector :: Selector
setContentViewSelector = mkSelector "setContentView:"

-- | @Selector@ for @automaticallyPlacesContentView@
automaticallyPlacesContentViewSelector :: Selector
automaticallyPlacesContentViewSelector = mkSelector "automaticallyPlacesContentView"

-- | @Selector@ for @setAutomaticallyPlacesContentView:@
setAutomaticallyPlacesContentViewSelector :: Selector
setAutomaticallyPlacesContentViewSelector = mkSelector "setAutomaticallyPlacesContentView:"

