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
  , menuBarVisibilitySelector
  , statusBarVisibilitySelector
  , toolbarsVisibilitySelector
  , allowsResizingSelector
  , xSelector
  , ySelector
  , widthSelector
  , heightSelector


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

import ObjC.WebKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | BOOL. Whether the menu bar should be visible. nil if menu bar visibility was not specified.
--
-- ObjC selector: @- menuBarVisibility@
menuBarVisibility :: IsWKWindowFeatures wkWindowFeatures => wkWindowFeatures -> IO (Id NSNumber)
menuBarVisibility wkWindowFeatures  =
  sendMsg wkWindowFeatures (mkSelector "menuBarVisibility") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | BOOL. Whether the status bar should be visible. nil if status bar visibility was not specified.
--
-- ObjC selector: @- statusBarVisibility@
statusBarVisibility :: IsWKWindowFeatures wkWindowFeatures => wkWindowFeatures -> IO (Id NSNumber)
statusBarVisibility wkWindowFeatures  =
  sendMsg wkWindowFeatures (mkSelector "statusBarVisibility") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | BOOL. Whether toolbars should be visible. nil if toolbar visibility was not specified.
--
-- ObjC selector: @- toolbarsVisibility@
toolbarsVisibility :: IsWKWindowFeatures wkWindowFeatures => wkWindowFeatures -> IO (Id NSNumber)
toolbarsVisibility wkWindowFeatures  =
  sendMsg wkWindowFeatures (mkSelector "toolbarsVisibility") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | BOOL. Whether the containing window should be resizable. nil if resizability was not specified.
--
-- ObjC selector: @- allowsResizing@
allowsResizing :: IsWKWindowFeatures wkWindowFeatures => wkWindowFeatures -> IO (Id NSNumber)
allowsResizing wkWindowFeatures  =
  sendMsg wkWindowFeatures (mkSelector "allowsResizing") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | CGFloat. The x coordinate of the containing window. nil if the x coordinate was not specified.
--
-- ObjC selector: @- x@
x :: IsWKWindowFeatures wkWindowFeatures => wkWindowFeatures -> IO (Id NSNumber)
x wkWindowFeatures  =
  sendMsg wkWindowFeatures (mkSelector "x") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | CGFloat. The y coordinate of the containing window. nil if the y coordinate was not specified.
--
-- ObjC selector: @- y@
y :: IsWKWindowFeatures wkWindowFeatures => wkWindowFeatures -> IO (Id NSNumber)
y wkWindowFeatures  =
  sendMsg wkWindowFeatures (mkSelector "y") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | CGFloat. The width coordinate of the containing window. nil if the width was not specified.
--
-- ObjC selector: @- width@
width :: IsWKWindowFeatures wkWindowFeatures => wkWindowFeatures -> IO (Id NSNumber)
width wkWindowFeatures  =
  sendMsg wkWindowFeatures (mkSelector "width") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | CGFloat. The height coordinate of the containing window. nil if the height was not specified.
--
-- ObjC selector: @- height@
height :: IsWKWindowFeatures wkWindowFeatures => wkWindowFeatures -> IO (Id NSNumber)
height wkWindowFeatures  =
  sendMsg wkWindowFeatures (mkSelector "height") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @menuBarVisibility@
menuBarVisibilitySelector :: Selector
menuBarVisibilitySelector = mkSelector "menuBarVisibility"

-- | @Selector@ for @statusBarVisibility@
statusBarVisibilitySelector :: Selector
statusBarVisibilitySelector = mkSelector "statusBarVisibility"

-- | @Selector@ for @toolbarsVisibility@
toolbarsVisibilitySelector :: Selector
toolbarsVisibilitySelector = mkSelector "toolbarsVisibility"

-- | @Selector@ for @allowsResizing@
allowsResizingSelector :: Selector
allowsResizingSelector = mkSelector "allowsResizing"

-- | @Selector@ for @x@
xSelector :: Selector
xSelector = mkSelector "x"

-- | @Selector@ for @y@
ySelector :: Selector
ySelector = mkSelector "y"

-- | @Selector@ for @width@
widthSelector :: Selector
widthSelector = mkSelector "width"

-- | @Selector@ for @height@
heightSelector :: Selector
heightSelector = mkSelector "height"

