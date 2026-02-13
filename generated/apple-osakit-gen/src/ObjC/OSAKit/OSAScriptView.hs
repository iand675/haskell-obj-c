{-# LANGUAGE DataKinds #-}
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
  , indentWidthSelector
  , indentsWrappedLinesSelector
  , setIndentWidthSelector
  , setIndentsWrappedLinesSelector
  , setSourceSelector
  , setTabWidthSelector
  , setUsesScriptAssistantSelector
  , setUsesTabsSelector
  , setWrapsLinesSelector
  , sourceSelector
  , tabWidthSelector
  , usesScriptAssistantSelector
  , usesTabsSelector
  , wrapsLinesSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.OSAKit.Internal.Classes
import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- source@
source :: IsOSAScriptView osaScriptView => osaScriptView -> IO (Id NSString)
source osaScriptView =
  sendMessage osaScriptView sourceSelector

-- | @- setSource:@
setSource :: (IsOSAScriptView osaScriptView, IsNSString value) => osaScriptView -> value -> IO ()
setSource osaScriptView value =
  sendMessage osaScriptView setSourceSelector (toNSString value)

-- | @- usesScriptAssistant@
usesScriptAssistant :: IsOSAScriptView osaScriptView => osaScriptView -> IO Bool
usesScriptAssistant osaScriptView =
  sendMessage osaScriptView usesScriptAssistantSelector

-- | @- setUsesScriptAssistant:@
setUsesScriptAssistant :: IsOSAScriptView osaScriptView => osaScriptView -> Bool -> IO ()
setUsesScriptAssistant osaScriptView value =
  sendMessage osaScriptView setUsesScriptAssistantSelector value

-- | @- usesTabs@
usesTabs :: IsOSAScriptView osaScriptView => osaScriptView -> IO Bool
usesTabs osaScriptView =
  sendMessage osaScriptView usesTabsSelector

-- | @- setUsesTabs:@
setUsesTabs :: IsOSAScriptView osaScriptView => osaScriptView -> Bool -> IO ()
setUsesTabs osaScriptView value =
  sendMessage osaScriptView setUsesTabsSelector value

-- | @- tabWidth@
tabWidth :: IsOSAScriptView osaScriptView => osaScriptView -> IO CULong
tabWidth osaScriptView =
  sendMessage osaScriptView tabWidthSelector

-- | @- setTabWidth:@
setTabWidth :: IsOSAScriptView osaScriptView => osaScriptView -> CULong -> IO ()
setTabWidth osaScriptView value =
  sendMessage osaScriptView setTabWidthSelector value

-- | @- wrapsLines@
wrapsLines :: IsOSAScriptView osaScriptView => osaScriptView -> IO Bool
wrapsLines osaScriptView =
  sendMessage osaScriptView wrapsLinesSelector

-- | @- setWrapsLines:@
setWrapsLines :: IsOSAScriptView osaScriptView => osaScriptView -> Bool -> IO ()
setWrapsLines osaScriptView value =
  sendMessage osaScriptView setWrapsLinesSelector value

-- | @- indentsWrappedLines@
indentsWrappedLines :: IsOSAScriptView osaScriptView => osaScriptView -> IO Bool
indentsWrappedLines osaScriptView =
  sendMessage osaScriptView indentsWrappedLinesSelector

-- | @- setIndentsWrappedLines:@
setIndentsWrappedLines :: IsOSAScriptView osaScriptView => osaScriptView -> Bool -> IO ()
setIndentsWrappedLines osaScriptView value =
  sendMessage osaScriptView setIndentsWrappedLinesSelector value

-- | @- indentWidth@
indentWidth :: IsOSAScriptView osaScriptView => osaScriptView -> IO CULong
indentWidth osaScriptView =
  sendMessage osaScriptView indentWidthSelector

-- | @- setIndentWidth:@
setIndentWidth :: IsOSAScriptView osaScriptView => osaScriptView -> CULong -> IO ()
setIndentWidth osaScriptView value =
  sendMessage osaScriptView setIndentWidthSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @source@
sourceSelector :: Selector '[] (Id NSString)
sourceSelector = mkSelector "source"

-- | @Selector@ for @setSource:@
setSourceSelector :: Selector '[Id NSString] ()
setSourceSelector = mkSelector "setSource:"

-- | @Selector@ for @usesScriptAssistant@
usesScriptAssistantSelector :: Selector '[] Bool
usesScriptAssistantSelector = mkSelector "usesScriptAssistant"

-- | @Selector@ for @setUsesScriptAssistant:@
setUsesScriptAssistantSelector :: Selector '[Bool] ()
setUsesScriptAssistantSelector = mkSelector "setUsesScriptAssistant:"

-- | @Selector@ for @usesTabs@
usesTabsSelector :: Selector '[] Bool
usesTabsSelector = mkSelector "usesTabs"

-- | @Selector@ for @setUsesTabs:@
setUsesTabsSelector :: Selector '[Bool] ()
setUsesTabsSelector = mkSelector "setUsesTabs:"

-- | @Selector@ for @tabWidth@
tabWidthSelector :: Selector '[] CULong
tabWidthSelector = mkSelector "tabWidth"

-- | @Selector@ for @setTabWidth:@
setTabWidthSelector :: Selector '[CULong] ()
setTabWidthSelector = mkSelector "setTabWidth:"

-- | @Selector@ for @wrapsLines@
wrapsLinesSelector :: Selector '[] Bool
wrapsLinesSelector = mkSelector "wrapsLines"

-- | @Selector@ for @setWrapsLines:@
setWrapsLinesSelector :: Selector '[Bool] ()
setWrapsLinesSelector = mkSelector "setWrapsLines:"

-- | @Selector@ for @indentsWrappedLines@
indentsWrappedLinesSelector :: Selector '[] Bool
indentsWrappedLinesSelector = mkSelector "indentsWrappedLines"

-- | @Selector@ for @setIndentsWrappedLines:@
setIndentsWrappedLinesSelector :: Selector '[Bool] ()
setIndentsWrappedLinesSelector = mkSelector "setIndentsWrappedLines:"

-- | @Selector@ for @indentWidth@
indentWidthSelector :: Selector '[] CULong
indentWidthSelector = mkSelector "indentWidth"

-- | @Selector@ for @setIndentWidth:@
setIndentWidthSelector :: Selector '[CULong] ()
setIndentWidthSelector = mkSelector "setIndentWidth:"

