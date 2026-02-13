{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @DOMHTMLDocument@.
module ObjC.WebKit.DOMHTMLDocument
  ( DOMHTMLDocument
  , IsDOMHTMLDocument(..)
  , open
  , close
  , write
  , writeln
  , clear
  , captureEvents
  , releaseEvents
  , createDocumentFragmentWithMarkupString_baseURL
  , createDocumentFragmentWithText
  , embeds
  , plugins
  , scripts
  , width
  , height
  , dir
  , setDir
  , designMode
  , setDesignMode
  , compatMode
  , bgColor
  , setBgColor
  , fgColor
  , setFgColor
  , alinkColor
  , setAlinkColor
  , linkColor
  , setLinkColor
  , vlinkColor
  , setVlinkColor
  , alinkColorSelector
  , bgColorSelector
  , captureEventsSelector
  , clearSelector
  , closeSelector
  , compatModeSelector
  , createDocumentFragmentWithMarkupString_baseURLSelector
  , createDocumentFragmentWithTextSelector
  , designModeSelector
  , dirSelector
  , embedsSelector
  , fgColorSelector
  , heightSelector
  , linkColorSelector
  , openSelector
  , pluginsSelector
  , releaseEventsSelector
  , scriptsSelector
  , setAlinkColorSelector
  , setBgColorSelector
  , setDesignModeSelector
  , setDirSelector
  , setFgColorSelector
  , setLinkColorSelector
  , setVlinkColorSelector
  , vlinkColorSelector
  , widthSelector
  , writeSelector
  , writelnSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.WebKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- open@
open :: IsDOMHTMLDocument domhtmlDocument => domhtmlDocument -> IO ()
open domhtmlDocument =
  sendMessage domhtmlDocument openSelector

-- | @- close@
close :: IsDOMHTMLDocument domhtmlDocument => domhtmlDocument -> IO ()
close domhtmlDocument =
  sendMessage domhtmlDocument closeSelector

-- | @- write:@
write :: (IsDOMHTMLDocument domhtmlDocument, IsNSString text) => domhtmlDocument -> text -> IO ()
write domhtmlDocument text =
  sendMessage domhtmlDocument writeSelector (toNSString text)

-- | @- writeln:@
writeln :: (IsDOMHTMLDocument domhtmlDocument, IsNSString text) => domhtmlDocument -> text -> IO ()
writeln domhtmlDocument text =
  sendMessage domhtmlDocument writelnSelector (toNSString text)

-- | @- clear@
clear :: IsDOMHTMLDocument domhtmlDocument => domhtmlDocument -> IO ()
clear domhtmlDocument =
  sendMessage domhtmlDocument clearSelector

-- | @- captureEvents@
captureEvents :: IsDOMHTMLDocument domhtmlDocument => domhtmlDocument -> IO ()
captureEvents domhtmlDocument =
  sendMessage domhtmlDocument captureEventsSelector

-- | @- releaseEvents@
releaseEvents :: IsDOMHTMLDocument domhtmlDocument => domhtmlDocument -> IO ()
releaseEvents domhtmlDocument =
  sendMessage domhtmlDocument releaseEventsSelector

-- | @- createDocumentFragmentWithMarkupString:baseURL:@
createDocumentFragmentWithMarkupString_baseURL :: (IsDOMHTMLDocument domhtmlDocument, IsNSString markupString, IsNSURL baseURL) => domhtmlDocument -> markupString -> baseURL -> IO (Id DOMDocumentFragment)
createDocumentFragmentWithMarkupString_baseURL domhtmlDocument markupString baseURL =
  sendMessage domhtmlDocument createDocumentFragmentWithMarkupString_baseURLSelector (toNSString markupString) (toNSURL baseURL)

-- | @- createDocumentFragmentWithText:@
createDocumentFragmentWithText :: (IsDOMHTMLDocument domhtmlDocument, IsNSString text) => domhtmlDocument -> text -> IO (Id DOMDocumentFragment)
createDocumentFragmentWithText domhtmlDocument text =
  sendMessage domhtmlDocument createDocumentFragmentWithTextSelector (toNSString text)

-- | @- embeds@
embeds :: IsDOMHTMLDocument domhtmlDocument => domhtmlDocument -> IO (Id DOMHTMLCollection)
embeds domhtmlDocument =
  sendMessage domhtmlDocument embedsSelector

-- | @- plugins@
plugins :: IsDOMHTMLDocument domhtmlDocument => domhtmlDocument -> IO (Id DOMHTMLCollection)
plugins domhtmlDocument =
  sendMessage domhtmlDocument pluginsSelector

-- | @- scripts@
scripts :: IsDOMHTMLDocument domhtmlDocument => domhtmlDocument -> IO (Id DOMHTMLCollection)
scripts domhtmlDocument =
  sendMessage domhtmlDocument scriptsSelector

-- | @- width@
width :: IsDOMHTMLDocument domhtmlDocument => domhtmlDocument -> IO CInt
width domhtmlDocument =
  sendMessage domhtmlDocument widthSelector

-- | @- height@
height :: IsDOMHTMLDocument domhtmlDocument => domhtmlDocument -> IO CInt
height domhtmlDocument =
  sendMessage domhtmlDocument heightSelector

-- | @- dir@
dir :: IsDOMHTMLDocument domhtmlDocument => domhtmlDocument -> IO (Id NSString)
dir domhtmlDocument =
  sendMessage domhtmlDocument dirSelector

-- | @- setDir:@
setDir :: (IsDOMHTMLDocument domhtmlDocument, IsNSString value) => domhtmlDocument -> value -> IO ()
setDir domhtmlDocument value =
  sendMessage domhtmlDocument setDirSelector (toNSString value)

-- | @- designMode@
designMode :: IsDOMHTMLDocument domhtmlDocument => domhtmlDocument -> IO (Id NSString)
designMode domhtmlDocument =
  sendMessage domhtmlDocument designModeSelector

-- | @- setDesignMode:@
setDesignMode :: (IsDOMHTMLDocument domhtmlDocument, IsNSString value) => domhtmlDocument -> value -> IO ()
setDesignMode domhtmlDocument value =
  sendMessage domhtmlDocument setDesignModeSelector (toNSString value)

-- | @- compatMode@
compatMode :: IsDOMHTMLDocument domhtmlDocument => domhtmlDocument -> IO (Id NSString)
compatMode domhtmlDocument =
  sendMessage domhtmlDocument compatModeSelector

-- | @- bgColor@
bgColor :: IsDOMHTMLDocument domhtmlDocument => domhtmlDocument -> IO (Id NSString)
bgColor domhtmlDocument =
  sendMessage domhtmlDocument bgColorSelector

-- | @- setBgColor:@
setBgColor :: (IsDOMHTMLDocument domhtmlDocument, IsNSString value) => domhtmlDocument -> value -> IO ()
setBgColor domhtmlDocument value =
  sendMessage domhtmlDocument setBgColorSelector (toNSString value)

-- | @- fgColor@
fgColor :: IsDOMHTMLDocument domhtmlDocument => domhtmlDocument -> IO (Id NSString)
fgColor domhtmlDocument =
  sendMessage domhtmlDocument fgColorSelector

-- | @- setFgColor:@
setFgColor :: (IsDOMHTMLDocument domhtmlDocument, IsNSString value) => domhtmlDocument -> value -> IO ()
setFgColor domhtmlDocument value =
  sendMessage domhtmlDocument setFgColorSelector (toNSString value)

-- | @- alinkColor@
alinkColor :: IsDOMHTMLDocument domhtmlDocument => domhtmlDocument -> IO (Id NSString)
alinkColor domhtmlDocument =
  sendMessage domhtmlDocument alinkColorSelector

-- | @- setAlinkColor:@
setAlinkColor :: (IsDOMHTMLDocument domhtmlDocument, IsNSString value) => domhtmlDocument -> value -> IO ()
setAlinkColor domhtmlDocument value =
  sendMessage domhtmlDocument setAlinkColorSelector (toNSString value)

-- | @- linkColor@
linkColor :: IsDOMHTMLDocument domhtmlDocument => domhtmlDocument -> IO (Id NSString)
linkColor domhtmlDocument =
  sendMessage domhtmlDocument linkColorSelector

-- | @- setLinkColor:@
setLinkColor :: (IsDOMHTMLDocument domhtmlDocument, IsNSString value) => domhtmlDocument -> value -> IO ()
setLinkColor domhtmlDocument value =
  sendMessage domhtmlDocument setLinkColorSelector (toNSString value)

-- | @- vlinkColor@
vlinkColor :: IsDOMHTMLDocument domhtmlDocument => domhtmlDocument -> IO (Id NSString)
vlinkColor domhtmlDocument =
  sendMessage domhtmlDocument vlinkColorSelector

-- | @- setVlinkColor:@
setVlinkColor :: (IsDOMHTMLDocument domhtmlDocument, IsNSString value) => domhtmlDocument -> value -> IO ()
setVlinkColor domhtmlDocument value =
  sendMessage domhtmlDocument setVlinkColorSelector (toNSString value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @open@
openSelector :: Selector '[] ()
openSelector = mkSelector "open"

-- | @Selector@ for @close@
closeSelector :: Selector '[] ()
closeSelector = mkSelector "close"

-- | @Selector@ for @write:@
writeSelector :: Selector '[Id NSString] ()
writeSelector = mkSelector "write:"

-- | @Selector@ for @writeln:@
writelnSelector :: Selector '[Id NSString] ()
writelnSelector = mkSelector "writeln:"

-- | @Selector@ for @clear@
clearSelector :: Selector '[] ()
clearSelector = mkSelector "clear"

-- | @Selector@ for @captureEvents@
captureEventsSelector :: Selector '[] ()
captureEventsSelector = mkSelector "captureEvents"

-- | @Selector@ for @releaseEvents@
releaseEventsSelector :: Selector '[] ()
releaseEventsSelector = mkSelector "releaseEvents"

-- | @Selector@ for @createDocumentFragmentWithMarkupString:baseURL:@
createDocumentFragmentWithMarkupString_baseURLSelector :: Selector '[Id NSString, Id NSURL] (Id DOMDocumentFragment)
createDocumentFragmentWithMarkupString_baseURLSelector = mkSelector "createDocumentFragmentWithMarkupString:baseURL:"

-- | @Selector@ for @createDocumentFragmentWithText:@
createDocumentFragmentWithTextSelector :: Selector '[Id NSString] (Id DOMDocumentFragment)
createDocumentFragmentWithTextSelector = mkSelector "createDocumentFragmentWithText:"

-- | @Selector@ for @embeds@
embedsSelector :: Selector '[] (Id DOMHTMLCollection)
embedsSelector = mkSelector "embeds"

-- | @Selector@ for @plugins@
pluginsSelector :: Selector '[] (Id DOMHTMLCollection)
pluginsSelector = mkSelector "plugins"

-- | @Selector@ for @scripts@
scriptsSelector :: Selector '[] (Id DOMHTMLCollection)
scriptsSelector = mkSelector "scripts"

-- | @Selector@ for @width@
widthSelector :: Selector '[] CInt
widthSelector = mkSelector "width"

-- | @Selector@ for @height@
heightSelector :: Selector '[] CInt
heightSelector = mkSelector "height"

-- | @Selector@ for @dir@
dirSelector :: Selector '[] (Id NSString)
dirSelector = mkSelector "dir"

-- | @Selector@ for @setDir:@
setDirSelector :: Selector '[Id NSString] ()
setDirSelector = mkSelector "setDir:"

-- | @Selector@ for @designMode@
designModeSelector :: Selector '[] (Id NSString)
designModeSelector = mkSelector "designMode"

-- | @Selector@ for @setDesignMode:@
setDesignModeSelector :: Selector '[Id NSString] ()
setDesignModeSelector = mkSelector "setDesignMode:"

-- | @Selector@ for @compatMode@
compatModeSelector :: Selector '[] (Id NSString)
compatModeSelector = mkSelector "compatMode"

-- | @Selector@ for @bgColor@
bgColorSelector :: Selector '[] (Id NSString)
bgColorSelector = mkSelector "bgColor"

-- | @Selector@ for @setBgColor:@
setBgColorSelector :: Selector '[Id NSString] ()
setBgColorSelector = mkSelector "setBgColor:"

-- | @Selector@ for @fgColor@
fgColorSelector :: Selector '[] (Id NSString)
fgColorSelector = mkSelector "fgColor"

-- | @Selector@ for @setFgColor:@
setFgColorSelector :: Selector '[Id NSString] ()
setFgColorSelector = mkSelector "setFgColor:"

-- | @Selector@ for @alinkColor@
alinkColorSelector :: Selector '[] (Id NSString)
alinkColorSelector = mkSelector "alinkColor"

-- | @Selector@ for @setAlinkColor:@
setAlinkColorSelector :: Selector '[Id NSString] ()
setAlinkColorSelector = mkSelector "setAlinkColor:"

-- | @Selector@ for @linkColor@
linkColorSelector :: Selector '[] (Id NSString)
linkColorSelector = mkSelector "linkColor"

-- | @Selector@ for @setLinkColor:@
setLinkColorSelector :: Selector '[Id NSString] ()
setLinkColorSelector = mkSelector "setLinkColor:"

-- | @Selector@ for @vlinkColor@
vlinkColorSelector :: Selector '[] (Id NSString)
vlinkColorSelector = mkSelector "vlinkColor"

-- | @Selector@ for @setVlinkColor:@
setVlinkColorSelector :: Selector '[Id NSString] ()
setVlinkColorSelector = mkSelector "setVlinkColor:"

