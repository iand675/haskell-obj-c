{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @OSAScriptView@.
module ObjC.OSAKit.OSAScriptView
  ( OSAScriptView
  , IsOSAScriptView(..)
  , source
  , setSource
  , usesScriptAssistant
  , setUsesScriptAssistant
  , usesTabs
  , setUsesTabs
  , tabWidth
  , setTabWidth
  , wrapsLines
  , setWrapsLines
  , indentsWrappedLines
  , setIndentsWrappedLines
  , indentWidth
  , setIndentWidth
  , sourceSelector
  , setSourceSelector
  , usesScriptAssistantSelector
  , setUsesScriptAssistantSelector
  , usesTabsSelector
  , setUsesTabsSelector
  , tabWidthSelector
  , setTabWidthSelector
  , wrapsLinesSelector
  , setWrapsLinesSelector
  , indentsWrappedLinesSelector
  , setIndentsWrappedLinesSelector
  , indentWidthSelector
  , setIndentWidthSelector


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

import ObjC.OSAKit.Internal.Classes
import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- source@
source :: IsOSAScriptView osaScriptView => osaScriptView -> IO (Id NSString)
source osaScriptView  =
  sendMsg osaScriptView (mkSelector "source") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setSource:@
setSource :: (IsOSAScriptView osaScriptView, IsNSString value) => osaScriptView -> value -> IO ()
setSource osaScriptView  value =
withObjCPtr value $ \raw_value ->
    sendMsg osaScriptView (mkSelector "setSource:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- usesScriptAssistant@
usesScriptAssistant :: IsOSAScriptView osaScriptView => osaScriptView -> IO Bool
usesScriptAssistant osaScriptView  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg osaScriptView (mkSelector "usesScriptAssistant") retCULong []

-- | @- setUsesScriptAssistant:@
setUsesScriptAssistant :: IsOSAScriptView osaScriptView => osaScriptView -> Bool -> IO ()
setUsesScriptAssistant osaScriptView  value =
  sendMsg osaScriptView (mkSelector "setUsesScriptAssistant:") retVoid [argCULong (if value then 1 else 0)]

-- | @- usesTabs@
usesTabs :: IsOSAScriptView osaScriptView => osaScriptView -> IO Bool
usesTabs osaScriptView  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg osaScriptView (mkSelector "usesTabs") retCULong []

-- | @- setUsesTabs:@
setUsesTabs :: IsOSAScriptView osaScriptView => osaScriptView -> Bool -> IO ()
setUsesTabs osaScriptView  value =
  sendMsg osaScriptView (mkSelector "setUsesTabs:") retVoid [argCULong (if value then 1 else 0)]

-- | @- tabWidth@
tabWidth :: IsOSAScriptView osaScriptView => osaScriptView -> IO CULong
tabWidth osaScriptView  =
  sendMsg osaScriptView (mkSelector "tabWidth") retCULong []

-- | @- setTabWidth:@
setTabWidth :: IsOSAScriptView osaScriptView => osaScriptView -> CULong -> IO ()
setTabWidth osaScriptView  value =
  sendMsg osaScriptView (mkSelector "setTabWidth:") retVoid [argCULong (fromIntegral value)]

-- | @- wrapsLines@
wrapsLines :: IsOSAScriptView osaScriptView => osaScriptView -> IO Bool
wrapsLines osaScriptView  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg osaScriptView (mkSelector "wrapsLines") retCULong []

-- | @- setWrapsLines:@
setWrapsLines :: IsOSAScriptView osaScriptView => osaScriptView -> Bool -> IO ()
setWrapsLines osaScriptView  value =
  sendMsg osaScriptView (mkSelector "setWrapsLines:") retVoid [argCULong (if value then 1 else 0)]

-- | @- indentsWrappedLines@
indentsWrappedLines :: IsOSAScriptView osaScriptView => osaScriptView -> IO Bool
indentsWrappedLines osaScriptView  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg osaScriptView (mkSelector "indentsWrappedLines") retCULong []

-- | @- setIndentsWrappedLines:@
setIndentsWrappedLines :: IsOSAScriptView osaScriptView => osaScriptView -> Bool -> IO ()
setIndentsWrappedLines osaScriptView  value =
  sendMsg osaScriptView (mkSelector "setIndentsWrappedLines:") retVoid [argCULong (if value then 1 else 0)]

-- | @- indentWidth@
indentWidth :: IsOSAScriptView osaScriptView => osaScriptView -> IO CULong
indentWidth osaScriptView  =
  sendMsg osaScriptView (mkSelector "indentWidth") retCULong []

-- | @- setIndentWidth:@
setIndentWidth :: IsOSAScriptView osaScriptView => osaScriptView -> CULong -> IO ()
setIndentWidth osaScriptView  value =
  sendMsg osaScriptView (mkSelector "setIndentWidth:") retVoid [argCULong (fromIntegral value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @source@
sourceSelector :: Selector
sourceSelector = mkSelector "source"

-- | @Selector@ for @setSource:@
setSourceSelector :: Selector
setSourceSelector = mkSelector "setSource:"

-- | @Selector@ for @usesScriptAssistant@
usesScriptAssistantSelector :: Selector
usesScriptAssistantSelector = mkSelector "usesScriptAssistant"

-- | @Selector@ for @setUsesScriptAssistant:@
setUsesScriptAssistantSelector :: Selector
setUsesScriptAssistantSelector = mkSelector "setUsesScriptAssistant:"

-- | @Selector@ for @usesTabs@
usesTabsSelector :: Selector
usesTabsSelector = mkSelector "usesTabs"

-- | @Selector@ for @setUsesTabs:@
setUsesTabsSelector :: Selector
setUsesTabsSelector = mkSelector "setUsesTabs:"

-- | @Selector@ for @tabWidth@
tabWidthSelector :: Selector
tabWidthSelector = mkSelector "tabWidth"

-- | @Selector@ for @setTabWidth:@
setTabWidthSelector :: Selector
setTabWidthSelector = mkSelector "setTabWidth:"

-- | @Selector@ for @wrapsLines@
wrapsLinesSelector :: Selector
wrapsLinesSelector = mkSelector "wrapsLines"

-- | @Selector@ for @setWrapsLines:@
setWrapsLinesSelector :: Selector
setWrapsLinesSelector = mkSelector "setWrapsLines:"

-- | @Selector@ for @indentsWrappedLines@
indentsWrappedLinesSelector :: Selector
indentsWrappedLinesSelector = mkSelector "indentsWrappedLines"

-- | @Selector@ for @setIndentsWrappedLines:@
setIndentsWrappedLinesSelector :: Selector
setIndentsWrappedLinesSelector = mkSelector "setIndentsWrappedLines:"

-- | @Selector@ for @indentWidth@
indentWidthSelector :: Selector
indentWidthSelector = mkSelector "indentWidth"

-- | @Selector@ for @setIndentWidth:@
setIndentWidthSelector :: Selector
setIndentWidthSelector = mkSelector "setIndentWidth:"

