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
  , openSelector
  , closeSelector
  , writeSelector
  , writelnSelector
  , clearSelector
  , captureEventsSelector
  , releaseEventsSelector
  , createDocumentFragmentWithMarkupString_baseURLSelector
  , createDocumentFragmentWithTextSelector
  , embedsSelector
  , pluginsSelector
  , scriptsSelector
  , widthSelector
  , heightSelector
  , dirSelector
  , setDirSelector
  , designModeSelector
  , setDesignModeSelector
  , compatModeSelector
  , bgColorSelector
  , setBgColorSelector
  , fgColorSelector
  , setFgColorSelector
  , alinkColorSelector
  , setAlinkColorSelector
  , linkColorSelector
  , setLinkColorSelector
  , vlinkColorSelector
  , setVlinkColorSelector


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

-- | @- open@
open :: IsDOMHTMLDocument domhtmlDocument => domhtmlDocument -> IO ()
open domhtmlDocument  =
  sendMsg domhtmlDocument (mkSelector "open") retVoid []

-- | @- close@
close :: IsDOMHTMLDocument domhtmlDocument => domhtmlDocument -> IO ()
close domhtmlDocument  =
  sendMsg domhtmlDocument (mkSelector "close") retVoid []

-- | @- write:@
write :: (IsDOMHTMLDocument domhtmlDocument, IsNSString text) => domhtmlDocument -> text -> IO ()
write domhtmlDocument  text =
withObjCPtr text $ \raw_text ->
    sendMsg domhtmlDocument (mkSelector "write:") retVoid [argPtr (castPtr raw_text :: Ptr ())]

-- | @- writeln:@
writeln :: (IsDOMHTMLDocument domhtmlDocument, IsNSString text) => domhtmlDocument -> text -> IO ()
writeln domhtmlDocument  text =
withObjCPtr text $ \raw_text ->
    sendMsg domhtmlDocument (mkSelector "writeln:") retVoid [argPtr (castPtr raw_text :: Ptr ())]

-- | @- clear@
clear :: IsDOMHTMLDocument domhtmlDocument => domhtmlDocument -> IO ()
clear domhtmlDocument  =
  sendMsg domhtmlDocument (mkSelector "clear") retVoid []

-- | @- captureEvents@
captureEvents :: IsDOMHTMLDocument domhtmlDocument => domhtmlDocument -> IO ()
captureEvents domhtmlDocument  =
  sendMsg domhtmlDocument (mkSelector "captureEvents") retVoid []

-- | @- releaseEvents@
releaseEvents :: IsDOMHTMLDocument domhtmlDocument => domhtmlDocument -> IO ()
releaseEvents domhtmlDocument  =
  sendMsg domhtmlDocument (mkSelector "releaseEvents") retVoid []

-- | @- createDocumentFragmentWithMarkupString:baseURL:@
createDocumentFragmentWithMarkupString_baseURL :: (IsDOMHTMLDocument domhtmlDocument, IsNSString markupString, IsNSURL baseURL) => domhtmlDocument -> markupString -> baseURL -> IO (Id DOMDocumentFragment)
createDocumentFragmentWithMarkupString_baseURL domhtmlDocument  markupString baseURL =
withObjCPtr markupString $ \raw_markupString ->
  withObjCPtr baseURL $ \raw_baseURL ->
      sendMsg domhtmlDocument (mkSelector "createDocumentFragmentWithMarkupString:baseURL:") (retPtr retVoid) [argPtr (castPtr raw_markupString :: Ptr ()), argPtr (castPtr raw_baseURL :: Ptr ())] >>= retainedObject . castPtr

-- | @- createDocumentFragmentWithText:@
createDocumentFragmentWithText :: (IsDOMHTMLDocument domhtmlDocument, IsNSString text) => domhtmlDocument -> text -> IO (Id DOMDocumentFragment)
createDocumentFragmentWithText domhtmlDocument  text =
withObjCPtr text $ \raw_text ->
    sendMsg domhtmlDocument (mkSelector "createDocumentFragmentWithText:") (retPtr retVoid) [argPtr (castPtr raw_text :: Ptr ())] >>= retainedObject . castPtr

-- | @- embeds@
embeds :: IsDOMHTMLDocument domhtmlDocument => domhtmlDocument -> IO (Id DOMHTMLCollection)
embeds domhtmlDocument  =
  sendMsg domhtmlDocument (mkSelector "embeds") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- plugins@
plugins :: IsDOMHTMLDocument domhtmlDocument => domhtmlDocument -> IO (Id DOMHTMLCollection)
plugins domhtmlDocument  =
  sendMsg domhtmlDocument (mkSelector "plugins") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- scripts@
scripts :: IsDOMHTMLDocument domhtmlDocument => domhtmlDocument -> IO (Id DOMHTMLCollection)
scripts domhtmlDocument  =
  sendMsg domhtmlDocument (mkSelector "scripts") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- width@
width :: IsDOMHTMLDocument domhtmlDocument => domhtmlDocument -> IO CInt
width domhtmlDocument  =
  sendMsg domhtmlDocument (mkSelector "width") retCInt []

-- | @- height@
height :: IsDOMHTMLDocument domhtmlDocument => domhtmlDocument -> IO CInt
height domhtmlDocument  =
  sendMsg domhtmlDocument (mkSelector "height") retCInt []

-- | @- dir@
dir :: IsDOMHTMLDocument domhtmlDocument => domhtmlDocument -> IO (Id NSString)
dir domhtmlDocument  =
  sendMsg domhtmlDocument (mkSelector "dir") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setDir:@
setDir :: (IsDOMHTMLDocument domhtmlDocument, IsNSString value) => domhtmlDocument -> value -> IO ()
setDir domhtmlDocument  value =
withObjCPtr value $ \raw_value ->
    sendMsg domhtmlDocument (mkSelector "setDir:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- designMode@
designMode :: IsDOMHTMLDocument domhtmlDocument => domhtmlDocument -> IO (Id NSString)
designMode domhtmlDocument  =
  sendMsg domhtmlDocument (mkSelector "designMode") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setDesignMode:@
setDesignMode :: (IsDOMHTMLDocument domhtmlDocument, IsNSString value) => domhtmlDocument -> value -> IO ()
setDesignMode domhtmlDocument  value =
withObjCPtr value $ \raw_value ->
    sendMsg domhtmlDocument (mkSelector "setDesignMode:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- compatMode@
compatMode :: IsDOMHTMLDocument domhtmlDocument => domhtmlDocument -> IO (Id NSString)
compatMode domhtmlDocument  =
  sendMsg domhtmlDocument (mkSelector "compatMode") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- bgColor@
bgColor :: IsDOMHTMLDocument domhtmlDocument => domhtmlDocument -> IO (Id NSString)
bgColor domhtmlDocument  =
  sendMsg domhtmlDocument (mkSelector "bgColor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setBgColor:@
setBgColor :: (IsDOMHTMLDocument domhtmlDocument, IsNSString value) => domhtmlDocument -> value -> IO ()
setBgColor domhtmlDocument  value =
withObjCPtr value $ \raw_value ->
    sendMsg domhtmlDocument (mkSelector "setBgColor:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- fgColor@
fgColor :: IsDOMHTMLDocument domhtmlDocument => domhtmlDocument -> IO (Id NSString)
fgColor domhtmlDocument  =
  sendMsg domhtmlDocument (mkSelector "fgColor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setFgColor:@
setFgColor :: (IsDOMHTMLDocument domhtmlDocument, IsNSString value) => domhtmlDocument -> value -> IO ()
setFgColor domhtmlDocument  value =
withObjCPtr value $ \raw_value ->
    sendMsg domhtmlDocument (mkSelector "setFgColor:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- alinkColor@
alinkColor :: IsDOMHTMLDocument domhtmlDocument => domhtmlDocument -> IO (Id NSString)
alinkColor domhtmlDocument  =
  sendMsg domhtmlDocument (mkSelector "alinkColor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setAlinkColor:@
setAlinkColor :: (IsDOMHTMLDocument domhtmlDocument, IsNSString value) => domhtmlDocument -> value -> IO ()
setAlinkColor domhtmlDocument  value =
withObjCPtr value $ \raw_value ->
    sendMsg domhtmlDocument (mkSelector "setAlinkColor:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- linkColor@
linkColor :: IsDOMHTMLDocument domhtmlDocument => domhtmlDocument -> IO (Id NSString)
linkColor domhtmlDocument  =
  sendMsg domhtmlDocument (mkSelector "linkColor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setLinkColor:@
setLinkColor :: (IsDOMHTMLDocument domhtmlDocument, IsNSString value) => domhtmlDocument -> value -> IO ()
setLinkColor domhtmlDocument  value =
withObjCPtr value $ \raw_value ->
    sendMsg domhtmlDocument (mkSelector "setLinkColor:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- vlinkColor@
vlinkColor :: IsDOMHTMLDocument domhtmlDocument => domhtmlDocument -> IO (Id NSString)
vlinkColor domhtmlDocument  =
  sendMsg domhtmlDocument (mkSelector "vlinkColor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setVlinkColor:@
setVlinkColor :: (IsDOMHTMLDocument domhtmlDocument, IsNSString value) => domhtmlDocument -> value -> IO ()
setVlinkColor domhtmlDocument  value =
withObjCPtr value $ \raw_value ->
    sendMsg domhtmlDocument (mkSelector "setVlinkColor:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @open@
openSelector :: Selector
openSelector = mkSelector "open"

-- | @Selector@ for @close@
closeSelector :: Selector
closeSelector = mkSelector "close"

-- | @Selector@ for @write:@
writeSelector :: Selector
writeSelector = mkSelector "write:"

-- | @Selector@ for @writeln:@
writelnSelector :: Selector
writelnSelector = mkSelector "writeln:"

-- | @Selector@ for @clear@
clearSelector :: Selector
clearSelector = mkSelector "clear"

-- | @Selector@ for @captureEvents@
captureEventsSelector :: Selector
captureEventsSelector = mkSelector "captureEvents"

-- | @Selector@ for @releaseEvents@
releaseEventsSelector :: Selector
releaseEventsSelector = mkSelector "releaseEvents"

-- | @Selector@ for @createDocumentFragmentWithMarkupString:baseURL:@
createDocumentFragmentWithMarkupString_baseURLSelector :: Selector
createDocumentFragmentWithMarkupString_baseURLSelector = mkSelector "createDocumentFragmentWithMarkupString:baseURL:"

-- | @Selector@ for @createDocumentFragmentWithText:@
createDocumentFragmentWithTextSelector :: Selector
createDocumentFragmentWithTextSelector = mkSelector "createDocumentFragmentWithText:"

-- | @Selector@ for @embeds@
embedsSelector :: Selector
embedsSelector = mkSelector "embeds"

-- | @Selector@ for @plugins@
pluginsSelector :: Selector
pluginsSelector = mkSelector "plugins"

-- | @Selector@ for @scripts@
scriptsSelector :: Selector
scriptsSelector = mkSelector "scripts"

-- | @Selector@ for @width@
widthSelector :: Selector
widthSelector = mkSelector "width"

-- | @Selector@ for @height@
heightSelector :: Selector
heightSelector = mkSelector "height"

-- | @Selector@ for @dir@
dirSelector :: Selector
dirSelector = mkSelector "dir"

-- | @Selector@ for @setDir:@
setDirSelector :: Selector
setDirSelector = mkSelector "setDir:"

-- | @Selector@ for @designMode@
designModeSelector :: Selector
designModeSelector = mkSelector "designMode"

-- | @Selector@ for @setDesignMode:@
setDesignModeSelector :: Selector
setDesignModeSelector = mkSelector "setDesignMode:"

-- | @Selector@ for @compatMode@
compatModeSelector :: Selector
compatModeSelector = mkSelector "compatMode"

-- | @Selector@ for @bgColor@
bgColorSelector :: Selector
bgColorSelector = mkSelector "bgColor"

-- | @Selector@ for @setBgColor:@
setBgColorSelector :: Selector
setBgColorSelector = mkSelector "setBgColor:"

-- | @Selector@ for @fgColor@
fgColorSelector :: Selector
fgColorSelector = mkSelector "fgColor"

-- | @Selector@ for @setFgColor:@
setFgColorSelector :: Selector
setFgColorSelector = mkSelector "setFgColor:"

-- | @Selector@ for @alinkColor@
alinkColorSelector :: Selector
alinkColorSelector = mkSelector "alinkColor"

-- | @Selector@ for @setAlinkColor:@
setAlinkColorSelector :: Selector
setAlinkColorSelector = mkSelector "setAlinkColor:"

-- | @Selector@ for @linkColor@
linkColorSelector :: Selector
linkColorSelector = mkSelector "linkColor"

-- | @Selector@ for @setLinkColor:@
setLinkColorSelector :: Selector
setLinkColorSelector = mkSelector "setLinkColor:"

-- | @Selector@ for @vlinkColor@
vlinkColorSelector :: Selector
vlinkColorSelector = mkSelector "vlinkColor"

-- | @Selector@ for @setVlinkColor:@
setVlinkColorSelector :: Selector
setVlinkColorSelector = mkSelector "setVlinkColor:"

