{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | WKWindowFeatures specifies optional attributes for the containing window when a new WKWebView is requested.
--
-- Generated bindings for @WKWindowFeatures@.
module ObjC.WebKit.WKWindowFeatures
  ( WKWindowFeatures
  , IsWKWindowFeatures(..)
  , menuBarVisibility
  , statusBarVisibility
  , toolbarsVisibility
  , allowsResizing
  , x
  , y
  , width
  , height
  , allowsResizingSelector
  , heightSelector
  , menuBarVisibilitySelector
  , statusBarVisibilitySelector
  , toolbarsVisibilitySelector
  , widthSelector
  , xSelector
  , ySelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.WebKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | BOOL. Whether the menu bar should be visible. nil if menu bar visibility was not specified.
--
-- ObjC selector: @- menuBarVisibility@
menuBarVisibility :: IsWKWindowFeatures wkWindowFeatures => wkWindowFeatures -> IO (Id NSNumber)
menuBarVisibility wkWindowFeatures =
  sendMessage wkWindowFeatures menuBarVisibilitySelector

-- | BOOL. Whether the status bar should be visible. nil if status bar visibility was not specified.
--
-- ObjC selector: @- statusBarVisibility@
statusBarVisibility :: IsWKWindowFeatures wkWindowFeatures => wkWindowFeatures -> IO (Id NSNumber)
statusBarVisibility wkWindowFeatures =
  sendMessage wkWindowFeatures statusBarVisibilitySelector

-- | BOOL. Whether toolbars should be visible. nil if toolbar visibility was not specified.
--
-- ObjC selector: @- toolbarsVisibility@
toolbarsVisibility :: IsWKWindowFeatures wkWindowFeatures => wkWindowFeatures -> IO (Id NSNumber)
toolbarsVisibility wkWindowFeatures =
  sendMessage wkWindowFeatures toolbarsVisibilitySelector

-- | BOOL. Whether the containing window should be resizable. nil if resizability was not specified.
--
-- ObjC selector: @- allowsResizing@
allowsResizing :: IsWKWindowFeatures wkWindowFeatures => wkWindowFeatures -> IO (Id NSNumber)
allowsResizing wkWindowFeatures =
  sendMessage wkWindowFeatures allowsResizingSelector

-- | CGFloat. The x coordinate of the containing window. nil if the x coordinate was not specified.
--
-- ObjC selector: @- x@
x :: IsWKWindowFeatures wkWindowFeatures => wkWindowFeatures -> IO (Id NSNumber)
x wkWindowFeatures =
  sendMessage wkWindowFeatures xSelector

-- | CGFloat. The y coordinate of the containing window. nil if the y coordinate was not specified.
--
-- ObjC selector: @- y@
y :: IsWKWindowFeatures wkWindowFeatures => wkWindowFeatures -> IO (Id NSNumber)
y wkWindowFeatures =
  sendMessage wkWindowFeatures ySelector

-- | CGFloat. The width coordinate of the containing window. nil if the width was not specified.
--
-- ObjC selector: @- width@
width :: IsWKWindowFeatures wkWindowFeatures => wkWindowFeatures -> IO (Id NSNumber)
width wkWindowFeatures =
  sendMessage wkWindowFeatures widthSelector

-- | CGFloat. The height coordinate of the containing window. nil if the height was not specified.
--
-- ObjC selector: @- height@
height :: IsWKWindowFeatures wkWindowFeatures => wkWindowFeatures -> IO (Id NSNumber)
height wkWindowFeatures =
  sendMessage wkWindowFeatures heightSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @menuBarVisibility@
menuBarVisibilitySelector :: Selector '[] (Id NSNumber)
menuBarVisibilitySelector = mkSelector "menuBarVisibility"

-- | @Selector@ for @statusBarVisibility@
statusBarVisibilitySelector :: Selector '[] (Id NSNumber)
statusBarVisibilitySelector = mkSelector "statusBarVisibility"

-- | @Selector@ for @toolbarsVisibility@
toolbarsVisibilitySelector :: Selector '[] (Id NSNumber)
toolbarsVisibilitySelector = mkSelector "toolbarsVisibility"

-- | @Selector@ for @allowsResizing@
allowsResizingSelector :: Selector '[] (Id NSNumber)
allowsResizingSelector = mkSelector "allowsResizing"

-- | @Selector@ for @x@
xSelector :: Selector '[] (Id NSNumber)
xSelector = mkSelector "x"

-- | @Selector@ for @y@
ySelector :: Selector '[] (Id NSNumber)
ySelector = mkSelector "y"

-- | @Selector@ for @width@
widthSelector :: Selector '[] (Id NSNumber)
widthSelector = mkSelector "width"

-- | @Selector@ for @height@
heightSelector :: Selector '[] (Id NSNumber)
heightSelector = mkSelector "height"

